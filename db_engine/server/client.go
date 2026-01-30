package server

import (
	"ShardDB/proto"
	"ShardDB/server/config"
	"ShardDB/util"
	"context"
	"flag"
	"fmt"
	"github.com/eiannone/keyboard"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
	"log"
	"os"
	"sync"
	"sync/atomic"
	"time"
)

type GenRequestFunc func(threadId int, count int, isReady bool) (int, map[string]string, int)
type Client struct {
	id int

	GenRequest GenRequestFunc
	PrintChain []string

	// GRPC hopNode client
	Shards      []proto.HopNodeClient
	ShardNumber int

	// Experiment Parameters
	Concurrency int
	Verbose     bool

	// Evaluation Metrics
	completeChan chan int64
	lat          int64

	status []Status

	numFinished int32
	TxId        uint64

	ExpTime    int
	WarmUpTime float64
	ready      bool
}
type Status struct {
	numFinished int64
	lat         int64
	avgLat      float64
	throughput  float64
}

func newClient(conf config.Options, id int, verbose bool) *Client {
	server := &Client{Shards: nil, ShardNumber: conf.ShardNumber, id: id}

	log.Println("Connecting to nodes...")
	var shards []proto.HopNodeClient
	for _, shard := range conf.Shards {
		for {
			conn, err := grpc.Dial(shard.Addr, grpc.WithTransportCredentials(insecure.NewCredentials()))

			if err != nil {
				time.Sleep(50 * time.Millisecond)
				continue
			}

			hopnode := proto.NewHopNodeClient(conn)
			// Wait til hop node server is ready
		check:
			_, err = hopnode.Tick(context.Background(), &proto.Empty{})

			if err != nil {
				time.Sleep(500 * time.Millisecond)
				goto check
			}
			shards = append(shards, hopnode)
			break
		}
	}

	server.Shards = shards
	return server
}

func SetupClient(numChains int) *Client {
	configFile := flag.String("config", "config.toml", "The path to the config file")
	mode := flag.Int("mode", 0, "Running mode")
	id := flag.Int("id", 0, "cluster id")
	concurrency := flag.Int("rate", 100, "Maximum number of concurrent requests")
	verbose := flag.Bool("v", false, "Print out debug msg or not")

	flag.Parse()

	var conf config.Options
	err := config.ReadConfig(&conf, *configFile) //default config file "config.toml"
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to read config: %v\n", err)
		os.Exit(1)
	}

	client := newClient(conf, *id, *verbose)
	client.Verbose = *verbose
	client.Concurrency = *concurrency
	client.numFinished = 0
	client.completeChan = make(chan int64, *concurrency)
	client.lat = 0
	client.ExpTime = 60
	client.WarmUpTime = 10
	client.ready = false
	client.PrintChain = nil

	client.status = make([]Status, numChains)
	util.ConcurrentMode = *mode

	modeStr := "2PC"
	switch *mode {
	case 1:
		modeStr = "2PC"
	case 2:
		modeStr = "IC3"
	case 3:
		modeStr = "DIC3"
	case 4:
		modeStr = "IC3 + Ordered2PL"
	case 5:
		modeStr = "IC3 + MessagePulling"
	}

	if client.Verbose {
		log.Println("Number of shards:", client.ShardNumber, "mode:", modeStr, "rate:", *concurrency, "concurrent request")
	}

	return client
}

func (Client *Client) StartExperiment() {
	// Open keyboard listener
	var done chan bool
	if Client.Verbose {
		if err := keyboard.Open(); err != nil {
			log.Fatal(err)
		}
		defer keyboard.Close()

		done = make(chan bool)
		go func() {
			log.Println("Press any key to exit...")
			keyboard.GetKey()
			done <- true
		}()
	}

	ctx, cancel := context.WithTimeout(context.Background(), time.Duration(Client.ExpTime)*time.Second)
	defer cancel()

	secTicker := time.NewTicker(100000 * time.Second)
	log.Println("Warming up")
	var begin time.Time
	time.AfterFunc(time.Duration(Client.WarmUpTime)*time.Second, func() {
		log.Println("Warm up finished")
		Client.ready = true
		begin = time.Now()
		secTicker.Reset(time.Second)
	})

	for i := 0; i < Client.Concurrency; i++ {
		go Client.workerThread(ctx, i)
	}

	timeout := false
	txnCount := int32(0)

	for !timeout {
		select {
		case <-ctx.Done():
			timeout = true
		case <-done:
			timeout = true
		case finT := <-Client.completeChan:
			Client.numFinished += 1
			Client.lat += finT
		case <-secTicker.C:
			if !Client.ready {
				continue
			}
			log.Println(Client.numFinished-txnCount, " txn/s")
			txnCount = Client.numFinished
		}
	}

	Client.computeResults(begin)

	numAbort := make([]int32, len(Client.status))
	numMessage := make([]int32, len(Client.status))
	wg := sync.WaitGroup{}
	wg.Add(len(Client.Shards))
	for _, shard := range Client.Shards {
		go func(nodeClient proto.HopNodeClient) {
			stat, err := nodeClient.Shutdown(context.Background(), &proto.Empty{})
			if err != nil {
				wg.Done()
				return
			}

			for i := 0; i < len(Client.status); i++ {
				atomic.AddInt32(&numAbort[i], stat.NumAbort[i])
				atomic.AddInt32(&numMessage[i], stat.NumMessage[i])
			}
			wg.Done()
		}(shard)
	}
	wg.Wait()

	Client.writeResults(numAbort, numMessage)
}

func (Client *Client) workerThread(ctx context.Context, id int) {
	totalTime := int64(0)
	count := 0
	abortCount := 0
	for {
		chainId, params, shardId := Client.GenRequest(id, count, Client.ready)

		rpcInfo := &proto.TrxInfo{
			Trxid:   util.TxId(atomic.AddUint64(&Client.TxId, 1), chainId),
			Chainid: int32(chainId),
			Hopid:   0,
		}

		req := &proto.TrxReq{
			Info:       rpcInfo,
			Params:     params,
			Dependency: nil,
		}

		txBegin := time.Now()

		succeed := false
		var response *proto.TrxRes
		var err error

		for !succeed {
			response, err = Client.Shards[shardId].ProcessRequest(ctx, req)

			if Client.ready && err == nil && response.Status != proto.Status_Success {
				abortCount += 1
			}
			if err == nil && response.Status == proto.Status_Success {
				succeed = true
			}

			select {
			case <-ctx.Done():
				return
			default:
				continue
			}
		}

		if Client.ready && succeed {
			// Chain finished
			t := time.Since(txBegin).Milliseconds()
			totalTime += t
			atomic.AddInt64(&Client.status[rpcInfo.Chainid].numFinished, 1)
			atomic.AddInt64(&Client.status[rpcInfo.Chainid].lat, t)
			Client.completeChan <- t
		}

		count += 1

		select {
		case <-ctx.Done():
			return
		default:
			continue
		}
	}
}

var totalStatus Status

func (Client *Client) computeResults(begin time.Time) {
	totalTime := time.Since(begin).Seconds()
	totalStatus.numFinished = 0
	totalStatus.lat = 0
	for i, _ := range Client.status {
		Client.status[i].throughput = float64(Client.status[i].numFinished) / totalTime
		Client.status[i].avgLat = float64(Client.status[i].lat) / float64(Client.status[i].numFinished)

		totalStatus.numFinished += Client.status[i].numFinished
		totalStatus.lat += Client.status[i].lat
	}
	totalStatus.throughput = float64(totalStatus.numFinished) / totalTime
	totalStatus.avgLat = float64(totalStatus.lat) / float64(totalStatus.numFinished)
}

func (Client *Client) writeResults(numAborts []int32, numMessages []int32) {
	if Client.Verbose {
		fmt.Printf("Chain, Throughput, Avg latency, Messages per Tx, Retries per Tx, Total retries\n")
	}

	for i, status := range Client.status {
		if status.numFinished == 0 {
			continue
		}

		var ChainName string

		if Client.PrintChain == nil || i >= len(Client.PrintChain) || Client.PrintChain[i] == "" {
			ChainName = fmt.Sprintf("Chain %d", i)
		} else {
			ChainName = Client.PrintChain[i]
		}

		fmt.Printf("%s, %f, %f, %f, %f, %d\n", ChainName, status.throughput, status.avgLat,
			float64(numMessages[i])/float64(status.numFinished),
			float64(numAborts[i])/float64(status.numFinished), numAborts[i])
	}

	fmt.Printf("Summary, %f, %f\n", totalStatus.throughput, totalStatus.avgLat)
}
