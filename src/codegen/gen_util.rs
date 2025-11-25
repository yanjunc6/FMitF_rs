use super::{util::write_go_file_header, GoProgram};
use std::error::Error;
use std::fmt::Write;

pub fn generate_util() -> Result<GoProgram, Box<dyn Error>> {
    let content = build_util_source()?;
    Ok(GoProgram::new("util.go", content))
}

fn build_util_source() -> Result<String, std::fmt::Error> {
    let mut out = String::new();

    write_go_file_header(
        &mut out,
        &[
            "ShardDB/proto",
            "ShardDB/util",
            "github.com/boltdb/bolt",
            "encoding/json",
            "sync",
        ],
    )?;

    // Constants
    writeln!(out, "const TPCCNewOrderChainId = 0")?;
    writeln!(out)?;

    // NextReqFunc type
    writeln!(out, "type NextReqFunc func(in *proto.TrxRes, params map[string]string, shards int) (*proto.TrxReq, map[string]string, int)")?;

    // Chain type
    writeln!(out, "type Chain struct {{")?;
    writeln!(out, "\tdb   *bolt.DB")?;
    writeln!(out, "\thops []*Hop")?;
    writeln!(out)?;
    writeln!(out, "\tNextReq NextReqFunc")?;
    writeln!(out, "}}")?;
    writeln!(out)?;
    writeln!(out)?;

    // Global scheduler maps
    writeln!(out, "var LockSchedulers map[string]*Scheduler")?;
    writeln!(out, "var AccessSchedulers map[string]*Scheduler")?;
    writeln!(out, "var AccessDSchedulers map[string]*Scheduler")?;
    writeln!(out)?;
    writeln!(out)?;

    // ChainManager type
    writeln!(out, "type ChainManager struct {{")?;
    writeln!(out, "\tchains map[string]*Chain")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    // ProcessFunc type
    writeln!(
        out,
        "type ProcessFunc func(tx *bolt.Tx, in *proto.TrxReq) (*proto.TrxRes, error)"
    )?;
    writeln!(out)?;

    // AggregateFunc type for global hops
    writeln!(
        out,
        "type AggregateFunc func(results [][]string, params map[string]string) map[string]string"
    )?;
    writeln!(out)?;

    // Hop type
    writeln!(out, "type Hop struct {{")?;
    writeln!(out, "\tid         int32")?;
    writeln!(out, "\tisReadOnly bool")?;
    writeln!(out, "\thopType    int")?;
    writeln!(out, "\tconflicts  map[int32]int32")?;
    writeln!(out, "\tprocess    ProcessFunc")?;
    writeln!(out, "\taggregate  AggregateFunc // For global hops")?;
    writeln!(out)?;
    writeln!(out, "\tisWaitTx    bool")?;
    writeln!(out, "\tisRecordDep bool")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    // Scheduler type
    writeln!(out, "type Scheduler struct {{")?;
    writeln!(out, "\tName     string")?;
    writeln!(out, "\tschedKey []string")?;
    writeln!(out, "\tendKey   []string")?;
    writeln!(out, "\tendHopId int32")?;
    writeln!(out, "\tmaps     *sync.Map")?;
    writeln!(out, "}}")?;

    // NewScheduler function
    writeln!(out, "func NewScheduler() *Scheduler {{")?;
    writeln!(out, "\treturn &Scheduler{{maps: &sync.Map{{}}}}")?;
    writeln!(out, "}}")?;
    writeln!(out)?;
    writeln!(out)?;

    // AddRWSet function
    writeln!(
        out,
        "func AddRWSet(rwSet []*proto.RWSet, tableName string, key []byte) []*proto.RWSet {{"
    )?;
    writeln!(
        out,
        "\treturn append(rwSet, &proto.RWSet{{TableName: tableName, Key: string(key)}})"
    )?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    // ListConcatenate function for global hop aggregation
    writeln!(
        out,
        "// ListConcatenate concatenates results from multiple shards into a single list"
    )?;
    writeln!(
        out,
        "func ListConcatenate[T any](src [][]string, dest []T) string {{"
    )?;
    writeln!(out, "\tvar tmp []T")?;
    writeln!(out, "\tfor _, v := range src {{")?;
    writeln!(out, "\t\tif len(v) == 0 {{")?;
    writeln!(out, "\t\t\tcontinue")?;
    writeln!(out, "\t\t}}")?;
    writeln!(out)?;
    writeln!(out, "\t\tjson.Unmarshal([]byte(v[0]), &tmp)")?;
    writeln!(out, "\t\tdest = append(dest, tmp...)")?;
    writeln!(out, "\t}}")?;
    writeln!(out)?;
    writeln!(out, "\tresult, _ := json.Marshal(dest)")?;
    writeln!(out)?;
    writeln!(out, "\treturn string(result)")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    // putData function
    writeln!(
        out,
        "func putData(tx *bolt.Bucket, key []byte, value any) {{"
    )?;
    writeln!(out, "\tvalueBytes, _ := json.Marshal(value)")?;
    writeln!(out, "\ttx.Put(key, valueBytes)")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    // hashStr function for string hashing
    writeln!(out, "// hashStr computes a hash of a string")?;
    writeln!(out, "func hashStr(s string) uint64 {{")?;
    writeln!(out, "\tvar hash uint64 = 0")?;
    writeln!(out, "\tfor _, c := range s {{")?;
    writeln!(out, "\t\thash = hash*31 + uint64(c)")?;
    writeln!(out, "\t}}")?;
    writeln!(out, "\treturn hash")?;
    writeln!(out, "}}")?;
    writeln!(out)?;

    Ok(out)
}
