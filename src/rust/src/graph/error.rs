// SPDX-License-Identifier: MIT
//! Error types for graph operations.

/// Errors that can occur when constructing or validating an ADMG.
#[derive(Debug, Clone)]
pub enum AdmgError {
    /// The directed part of the graph contains a cycle.
    DirectedCycle,
    /// An invalid edge type was found (only directed and bidirected are allowed).
    InvalidEdgeType { found: String },
    /// A node index is out of bounds.
    InvalidNodeIndex { index: u32, max: u32 },
}

impl std::fmt::Display for AdmgError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DirectedCycle => write!(f, "ADMG contains a directed cycle"),
            Self::InvalidEdgeType { found } => write!(
                f,
                "ADMG can only contain directed and bidirected edges, found: {}",
                found
            ),
            Self::InvalidNodeIndex { index, max } => {
                write!(f, "Node index {} out of bounds (max: {})", index, max)
            }
        }
    }
}

impl std::error::Error for AdmgError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn error_display_directed_cycle() {
        let err = AdmgError::DirectedCycle;
        assert_eq!(err.to_string(), "ADMG contains a directed cycle");
    }

    #[test]
    fn error_display_invalid_edge_type() {
        let err = AdmgError::InvalidEdgeType {
            found: "---".to_string(),
        };
        assert!(err
            .to_string()
            .contains("ADMG can only contain directed and bidirected edges"));
        assert!(err.to_string().contains("---"));
    }

    #[test]
    fn error_display_invalid_node_index() {
        let err = AdmgError::InvalidNodeIndex { index: 10, max: 5 };
        assert!(err.to_string().contains("10"));
        assert!(err.to_string().contains("5"));
    }
}
