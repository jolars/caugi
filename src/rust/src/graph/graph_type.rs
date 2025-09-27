use std::str::FromStr;
use crate::edges::EdgeClass;

#[derive(Debug)]
pub enum GraphType {
    Dag,
    Pdag,
    Unknown,
} // ADMG, MAG, PAG to be added

impl FromStr for GraphType {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, String> {
        let original = s.to_string();
        match s.to_ascii_uppercase().as_str() {
            "DAG" => Ok(Self::Dag),
            "PDAG" | "CPDAG" => Ok(Self::Pdag),
            "" | "UNKNOWN" | "<UNKNOWN>" => Ok(Self::Unknown),
            _ => Err(format!("unknown graph class '{original}'")),
        }
    }
}

impl GraphType {
    pub fn spec(&self) -> (&'static str, &'static [EdgeClass], bool) {
        use EdgeClass::*;
        const DAG_ALLOWED: &[EdgeClass] = &[Directed];
        const PDAG_ALLOWED: &[EdgeClass] = &[Directed, Undirected];
        match self {
            GraphType::Dag => ("DAG", DAG_ALLOWED, true),
            GraphType::Pdag => ("PDAG", PDAG_ALLOWED, true),
            GraphType::Unknown => ("UNKNOWN", &[], false),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn graph_type_from_str_all_paths() {
        assert!(matches!("DAG".parse::<GraphType>().unwrap(), GraphType::Dag));
        assert!(matches!("pdag".parse::<GraphType>().unwrap(), GraphType::Pdag));
        assert!(matches!("CPDAG".parse::<GraphType>().unwrap(), GraphType::Pdag));
        assert!(matches!("".parse::<GraphType>().unwrap(), GraphType::Unknown));
        assert!(matches!("unknown".parse::<GraphType>().unwrap(), GraphType::Unknown));
        assert!(matches!("<UNKNOWN>".parse::<GraphType>().unwrap(), GraphType::Unknown));
        let e = "weird".parse::<GraphType>().unwrap_err();
        assert_eq!(e, "unknown graph class 'weird'");
    }

    #[test]
    fn graph_type_spec_all_paths() {
        let (name, allowed, acyclic) = GraphType::Dag.spec();
        assert_eq!(name, "DAG");
        assert_eq!(allowed, &[EdgeClass::Directed]);
        assert!(acyclic);

        let (name, allowed, acyclic) = GraphType::Pdag.spec();
        assert_eq!(name, "PDAG");
        assert_eq!(allowed, &[EdgeClass::Directed, EdgeClass::Undirected]);
        assert!(acyclic);

        let (name, allowed, acyclic) = GraphType::Unknown.spec();
        assert_eq!(name, "UNKNOWN");
        assert_eq!(allowed, &[]);
        assert!(!acyclic);
    }
}