use super::{util::write_go_file_header, GoProgram};
use std::error::Error;
use std::fmt::Write;

pub(super) fn generate_util() -> Result<GoProgram, Box<dyn Error>> {
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

    // Hop type
    writeln!(out, "type Hop struct {{")?;
    writeln!(out, "\tid         int32")?;
    writeln!(out, "\tisReadOnly bool")?;
    writeln!(out, "\thopType    int")?;
    writeln!(out, "\tconflicts  map[int32]int32")?;
    writeln!(out, "\tprocess    ProcessFunc")?;
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

    Ok(out)
}
