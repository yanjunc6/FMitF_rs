use crate::verification::Boogie::VerificationNodeId;

pub fn canonical_commutative_name(file_name: &str) -> &str {
    let trimmed = file_name.strip_suffix(".bpl").unwrap_or(file_name);
    trimmed.split("__split_").next().unwrap_or(trimmed)
}

pub fn parse_node_from_parts(
    f_part: &str,
    i_part: &str,
    h_part: &str,
) -> Option<VerificationNodeId> {
    let function_id = f_part.strip_prefix('f')?.parse().ok()?;
    let instance = i_part.strip_prefix('i')?.parse().ok()?;
    let hop_id = h_part.strip_prefix('h')?.parse().ok()?;

    Some(VerificationNodeId {
        function_id,
        instance,
        hop_id,
    })
}

pub fn parse_commutative_nodes(
    file_name: &str,
) -> Option<(VerificationNodeId, VerificationNodeId)> {
    let canonical = canonical_commutative_name(file_name);
    if !canonical.starts_with("commutative_") {
        return None;
    }

    let parts: Vec<&str> = canonical.split('_').collect();
    if parts.len() != 8 || parts.get(4).copied() != Some("vs") {
        return None;
    }

    let source = parse_node_from_parts(parts[1], parts[2], parts[3])?;
    let target = parse_node_from_parts(parts[5], parts[6], parts[7])?;
    Some((source, target))
}

pub fn parse_commutative_edge_ids(
    file_name: &str,
) -> Option<(usize, u32, usize, usize, u32, usize)> {
    let (source, target) = parse_commutative_nodes(file_name)?;
    Some((
        source.function_id,
        source.instance,
        source.hop_id,
        target.function_id,
        target.instance,
        target.hop_id,
    ))
}

pub fn split_child_program_name(parent_name: &str, next_depth: u32, branch: char) -> String {
    format!("{}__split_d{}_{}", parent_name, next_depth, branch)
}
