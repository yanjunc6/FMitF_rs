package config

import (
	"github.com/spf13/pflag"
	"github.com/spf13/viper"
)

type Options struct {
	ShardNumber int `mapstructure:"shard-number"`
	Clients     []Client
	Shards      []Shard
}

type Client struct {
	ID   string `mapstructure:"client-id"`
	ADDR string `mapstructure:"client-addr"`
}

type Shard struct {
	ID   string `mapstructure:"shard-id"`
	Addr string `mapstructure:"shard-addr"`
}

func ReadConfig(opts interface{}, configfile string) (err error) {
	err = viper.BindPFlags(pflag.CommandLine)
	if err != nil {
		return err
	}
	if configfile != "" {
		//viper.SetConfigFile(configfile)
		viper.SetConfigName(configfile)
	} else {
		viper.SetConfigName("node")
	}

	viper.AddConfigPath(".")
	err = viper.ReadInConfig()
	if err != nil {
		return err
	}
	err = viper.Unmarshal(opts)
	if err != nil {
		return err
	}

	return nil
}
