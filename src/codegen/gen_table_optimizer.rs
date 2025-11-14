//! Table Access Optimizer for Go Code Generation
//!
//! This module provides optimization for consecutive table get/set operations.
//! It transforms a sequence of instructions into optimized operations, treating
//! optimization as a preprocessing stage before code generation.

use crate::cfg::{self, Instruction, InstructionKind, Operand};
use std::collections::HashMap;

/// Optimized operation - either a single instruction or a combined table access group
#[derive(Debug, Clone)]
pub enum OptimizedOp {
    /// A single instruction that doesn't participate in optimization
    Single(Instruction),
    /// A combined table access group (multiple gets/sets merged into one)
    CombinedTableAccess(TableAccessGroup),
}

/// Represents a group of consecutive table accesses that can be combined
#[derive(Debug, Clone)]
pub struct TableAccessGroup {
    /// The table being accessed
    pub table_id: cfg::TableId,
    /// The key operands (must be identical for all accesses in the group)
    pub keys: Vec<Operand>,
    /// The individual accesses in this group
    pub accesses: Vec<TableAccess>,
    /// Whether this group contains any writes (TableSet)
    pub has_writes: bool,
}

/// Represents a single table access (get or set)
#[derive(Debug, Clone)]
pub enum TableAccess {
    Get {
        dest: cfg::VariableId,
        field: Option<cfg::FieldId>,
        instruction_index: usize,
    },
    Set {
        field: Option<cfg::FieldId>,
        value: Operand,
        instruction_index: usize,
    },
}

impl TableAccessGroup {
    /// Check if this group only accesses specific fields (not whole rows)
    pub fn all_field_accesses(&self) -> bool {
        self.accesses.iter().all(|access| match access {
            TableAccess::Get { field, .. } => field.is_some(),
            TableAccess::Set { field, .. } => field.is_some(),
        })
    }

    /// Check if this group has at least 2 accesses (worth combining)
    pub fn is_worth_combining(&self) -> bool {
        self.accesses.len() >= 2
    }
}

/// Analyze a sequence of instructions and group consecutive table accesses
pub fn group_table_accesses(instructions: &[Instruction]) -> Vec<TableAccessGroup> {
    let mut groups = Vec::new();
    let mut current_group: Option<TableAccessGroup> = None;

    for (idx, inst) in instructions.iter().enumerate() {
        match &inst.kind {
            InstructionKind::TableGet {
                dest,
                table,
                keys,
                field,
            } => {
                let can_extend = if let Some(ref group) = current_group {
                    group.table_id == *table && operands_equal(&group.keys, keys)
                } else {
                    false
                };

                if can_extend {
                    // Extend current group
                    if let Some(ref mut group) = current_group {
                        group.accesses.push(TableAccess::Get {
                            dest: *dest,
                            field: *field,
                            instruction_index: idx,
                        });
                    }
                } else {
                    // Start new group (after finishing current one)
                    if let Some(group) = current_group.take() {
                        groups.push(group);
                    }
                    current_group = Some(TableAccessGroup {
                        table_id: *table,
                        keys: keys.clone(),
                        accesses: vec![TableAccess::Get {
                            dest: *dest,
                            field: *field,
                            instruction_index: idx,
                        }],
                        has_writes: false,
                    });
                }
            }

            InstructionKind::TableSet {
                table,
                keys,
                field,
                value,
            } => {
                let can_extend = if let Some(ref group) = current_group {
                    group.table_id == *table && operands_equal(&group.keys, keys)
                } else {
                    false
                };

                if can_extend {
                    // Extend current group
                    if let Some(ref mut group) = current_group {
                        group.accesses.push(TableAccess::Set {
                            field: *field,
                            value: value.clone(),
                            instruction_index: idx,
                        });
                        group.has_writes = true;
                    }
                } else {
                    // Start new group (after finishing current one)
                    if let Some(group) = current_group.take() {
                        groups.push(group);
                    }
                    current_group = Some(TableAccessGroup {
                        table_id: *table,
                        keys: keys.clone(),
                        accesses: vec![TableAccess::Set {
                            field: *field,
                            value: value.clone(),
                            instruction_index: idx,
                        }],
                        has_writes: true,
                    });
                }
            }

            // Any other instruction breaks the current group
            _ => {
                if let Some(group) = current_group.take() {
                    groups.push(group);
                }
            }
        }
    }

    // Don't forget the last group
    if let Some(group) = current_group {
        groups.push(group);
    }

    groups
}

/// Optimize a sequence of instructions into optimized operations
/// This is the main preprocessing function that should be called before code generation
pub fn optimize_instructions(instructions: &[Instruction]) -> Vec<OptimizedOp> {
    let groups = group_table_accesses(instructions);
    let inst_to_group = create_instruction_to_group_map(&groups);

    let mut result = Vec::new();
    let mut processed_groups = std::collections::HashSet::new();

    for (inst_idx, inst) in instructions.iter().enumerate() {
        if let Some(&(group_idx, access_idx_in_group)) = inst_to_group.get(&inst_idx) {
            // This instruction is part of a group
            if access_idx_in_group == 0 && !processed_groups.contains(&group_idx) {
                // First instruction in the group - emit the optimized op
                processed_groups.insert(group_idx);
                let group = &groups[group_idx];

                if group.is_worth_combining() && group.all_field_accesses() {
                    // Worth optimizing - emit combined operation
                    result.push(OptimizedOp::CombinedTableAccess(group.clone()));
                } else {
                    // Not worth optimizing - emit as single instruction
                    result.push(OptimizedOp::Single(inst.clone()));
                }
            } else if !processed_groups.contains(&group_idx) {
                // Not first in group but group not processed - shouldn't happen, fallback
                result.push(OptimizedOp::Single(inst.clone()));
            }
            // else: already processed as part of optimized group, skip
        } else {
            // Not part of any group - emit as single instruction
            result.push(OptimizedOp::Single(inst.clone()));
        }
    }

    result
}

/// Check if two operand sequences are equal
fn operands_equal(a: &[Operand], b: &[Operand]) -> bool {
    if a.len() != b.len() {
        return false;
    }
    a.iter().zip(b.iter()).all(|(x, y)| match (x, y) {
        (Operand::Variable(v1), Operand::Variable(v2)) => v1 == v2,
        (Operand::Constant(c1), Operand::Constant(c2)) => c1 == c2,
        (Operand::Global(g1), Operand::Global(g2)) => g1 == g2,
        (Operand::Table(t1), Operand::Table(t2)) => t1 == t2,
        _ => false,
    })
}

/// Create a mapping from instruction index to group index for quick lookup
pub fn create_instruction_to_group_map(
    groups: &[TableAccessGroup],
) -> HashMap<usize, (usize, usize)> {
    let mut map = HashMap::new();
    for (group_idx, group) in groups.iter().enumerate() {
        for (access_idx, access) in group.accesses.iter().enumerate() {
            let inst_idx = match access {
                TableAccess::Get {
                    instruction_index, ..
                } => *instruction_index,
                TableAccess::Set {
                    instruction_index, ..
                } => *instruction_index,
            };
            map.insert(inst_idx, (group_idx, access_idx));
        }
    }
    map
}
