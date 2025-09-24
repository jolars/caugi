// SPDX-License-Identifier: MIT
//! Edge specification and registry with endpoint Marks.

use std::{collections::HashMap, error::Error, fmt, result::Result, str::FromStr};

mod query_flags;
pub use query_flags::QueryFlags;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mark {
    Arrow,
    Tail,
    Circle,
    Other,
}

impl FromStr for Mark {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "arrow" => Ok(Mark::Arrow),
            "tail" => Ok(Mark::Tail),
            "circle" => Ok(Mark::Circle),
            "other" => Ok(Mark::Other),
            _ => Err(format!("Unknown mark '{}'", s)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EdgeClass {
    Directed,
    Undirected,
    Bidirected,
    PartiallyDirected,
    PartiallyUndirected,
    Partial, // for o-o
}

impl FromStr for EdgeClass {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "directed" => Ok(EdgeClass::Directed),
            "undirected" => Ok(EdgeClass::Undirected),
            "bidirected" => Ok(EdgeClass::Bidirected),
            "partial" => Ok(EdgeClass::Partial),
            _ => Err(format!("Unknown class '{}'", s)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EdgeSpec {
    pub glyph: String,
    pub tail: Mark,
    pub head: Mark,
    pub symmetric: bool,
    pub class: EdgeClass,
    pub flags: QueryFlags,
}

impl EdgeSpec {
    pub fn glyph<S: Into<String>>(mut self, g: S) -> Self {
        self.glyph = g.into();
        self
    }
}

#[derive(Debug)]
#[non_exhaustive]
pub enum RegistryError {
    Sealed,
    TooManyTypes,
    Conflict { glyph: String },
    UnknownGlyph(String),
    InvalidCode(u8),
}

impl fmt::Display for RegistryError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use RegistryError::*;
        match self {
            Sealed => write!(f, "Edge registry is sealed"),
            TooManyTypes => write!(f, "Edge registry full (max 256)"),
            Conflict { glyph } => write!(
                f,
                "The glyph '{}' already registered with different semantics",
                glyph
            ),
            UnknownGlyph(g) => write!(f, "Unknown glyph '{}'", g),
            InvalidCode(c) => write!(f, "Invalid edge code {}", c),
        }
    }
}

impl Error for RegistryError {}

#[derive(Debug, Clone, Default)]
pub struct EdgeRegistry {
    glyph_to_code: HashMap<String, u8>,
    code_to_spec: Vec<EdgeSpec>, // index = code
    sealed: bool,
}

impl EdgeRegistry {
    const MAX_TYPES: usize = 256;

    pub fn new() -> Self {
        Self {
            glyph_to_code: HashMap::new(),
            code_to_spec: Vec::new(),
            sealed: false,
        }
    }
    pub fn is_sealed(&self) -> bool {
        self.sealed
    }
    pub fn is_empty(&self) -> bool {
        self.code_to_spec.is_empty()
    }
    pub fn seal(&mut self) {
        self.sealed = true;
    }

    pub fn len(&self) -> usize {
        self.code_to_spec.len()
    }

    pub fn register(&mut self, spec: EdgeSpec) -> Result<u8, RegistryError> {
        if self.sealed {
            return Err(RegistryError::Sealed);
        }

        if let Some(&code) = self.glyph_to_code.get(&spec.glyph) {
            let existing = &self.code_to_spec[code as usize];
            return if existing == &spec {
                Ok(code)
            } else {
                Err(RegistryError::Conflict {
                    glyph: spec.glyph.clone(),
                })
            };
        }

        if self.code_to_spec.len() >= Self::MAX_TYPES {
            return Err(RegistryError::TooManyTypes);
        }

        let code = self.code_to_spec.len() as u8;
        self.glyph_to_code.insert(spec.glyph.clone(), code);
        self.code_to_spec.push(spec);
        Ok(code)
    }

    pub fn code_of(&self, glyph: &str) -> Result<u8, RegistryError> {
        match self.glyph_to_code.get(glyph) {
            Some(&code) => Ok(code),
            None => Err(RegistryError::UnknownGlyph(glyph.to_string())),
        }
    }

    pub fn spec_of_code(&self, code: u8) -> Result<&EdgeSpec, RegistryError> {
        match self.code_to_spec.get(code as usize) {
            Some(spec) => Ok(spec),
            None => Err(RegistryError::InvalidCode(code)),
        }
    }

    /// Convenience: fetch spec directly by glyph.
    pub fn spec_of(&self, glyph: &str) -> Result<&EdgeSpec, RegistryError> {
        let code = self.code_of(glyph)?;
        self.spec_of_code(code)
    }

    /// Register built-ins. Idempotent if called multiple times.
    pub fn register_builtins(&mut self) -> Result<(), RegistryError> {
        use EdgeClass as C;
        use Mark as M;
        use QueryFlags as F;

        let mut add = |glyph: &str,
                       tail: M,
                       head: M,
                       symmetric: bool,
                       class: C,
                       flags: F|
         -> Result<(), RegistryError> {
            self.register(EdgeSpec {
                glyph: glyph.to_string(),
                tail,
                head,
                symmetric,
                class,
                flags,
            })
            .map(|_| ())
        };

        // Built-in mappings with retained flags
        add(
            "-->",
            M::Tail,
            M::Arrow,
            false,
            C::Directed,
            F::TRAVERSABLE_WHEN_CONDITIONED,
        )?;
        add(
            "---",
            M::Tail,
            M::Tail,
            true,
            C::Undirected,
            F::TRAVERSABLE_WHEN_CONDITIONED,
        )?;
        add(
            "<->",
            M::Arrow,
            M::Arrow,
            true,
            C::Bidirected,
            F::TRAVERSABLE_WHEN_CONDITIONED | F::LATENT_CONFOUNDING,
        )?;
        add(
            "o-o",
            M::Circle,
            M::Circle,
            true,
            C::Partial,
            F::TRAVERSABLE_WHEN_CONDITIONED,
        )?;
        add(
            "--o",
            M::Tail,
            M::Circle,
            false,
            C::PartiallyUndirected,
            F::TRAVERSABLE_WHEN_CONDITIONED,
        )?;
        add(
            "o->",
            M::Circle,
            M::Arrow,
            false,
            C::PartiallyDirected,
            F::TRAVERSABLE_WHEN_CONDITIONED,
        )?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn built_reg() -> EdgeRegistry {
        let mut r = EdgeRegistry::new();
        r.register_builtins().unwrap();
        r
    }

    #[test]
    fn builtins_present_and_len() {
        let r = built_reg();
        assert_eq!(r.len(), 6);
        for g in ["-->", "---", "<->", "o-o", "--o", "o->"] {
            assert!(r.code_of(g).is_ok(), "missing builtin {}", g);
        }
    }

    #[test]
    fn register_new_and_fetch_spec() {
        let mut r = built_reg();
        let before = r.len();
        let spec = EdgeSpec {
            glyph: "--<".into(),
            tail: Mark::Tail,
            head: Mark::Arrow,
            symmetric: false,
            class: EdgeClass::Directed,
            flags: QueryFlags::TRAVERSABLE_WHEN_CONDITIONED,
        };
        let code = r.register(spec.clone()).unwrap();
        assert_eq!(r.len(), before + 1);
        let got = r.spec_of_code(code).unwrap();
        assert_eq!(got, &spec);
    }

    #[test]
    fn idempotent_same_spec_returns_same_code() {
        let mut r = built_reg();
        let spec = EdgeSpec {
            glyph: "o-O".into(),
            tail: Mark::Circle,
            head: Mark::Other,
            symmetric: false,
            class: EdgeClass::Partial,
            flags: QueryFlags::TRAVERSABLE_WHEN_CONDITIONED,
        };
        let c1 = r.register(spec.clone()).unwrap();
        let c2 = r.register(spec).unwrap();
        assert_eq!(c1, c2);
    }

    #[test]
    fn conflict_and_seal_behavior() {
        let mut r = built_reg();
        // conflict: redefine existing glyph with different semantics
        let bad = EdgeSpec {
            glyph: "-->".into(),
            tail: Mark::Tail,
            head: Mark::Tail,
            symmetric: true,
            class: EdgeClass::Undirected,
            flags: QueryFlags::TRAVERSABLE_WHEN_CONDITIONED,
        };
        assert!(matches!(
            r.register(bad),
            Err(RegistryError::Conflict { .. })
        ));

        // seal prevents any further registrations
        r.seal();
        let new = EdgeSpec {
            glyph: "x".into(),
            tail: Mark::Other,
            head: Mark::Other,
            symmetric: true,
            class: EdgeClass::Undirected,
            flags: QueryFlags::TRAVERSABLE_WHEN_CONDITIONED,
        };
        assert!(matches!(r.register(new), Err(RegistryError::Sealed)));
    }

    #[test]
    fn unknown_glyph_and_invalid_code_errors() {
        let r = built_reg();
        assert!(matches!(
            r.code_of("does-not-exist"),
            Err(RegistryError::UnknownGlyph(_))
        ));
        assert!(matches!(
            r.spec_of_code(250),
            Err(RegistryError::InvalidCode(_))
        ));
    }
}
