// SPDX-License-Identifier: MIT
//! Edge specification and registry.

use std::{collections::HashMap, error::Error, fmt, result::Result};

mod query_flags;
pub use query_flags::QueryFlags;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mark { Line, Circle, Arrow, Other }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Orientation { LeftHead, RightHead, BothHeads, None }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EdgeClass { Directed, Undirected, Bidirected, Partial }

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EdgeSpec {
    pub glyph: String,
    pub left_mark: Mark,
    pub right_mark: Mark,
    pub orientation: Orientation,
    pub class: EdgeClass,
    pub symmetric: bool,
    pub flags: QueryFlags,
}

impl EdgeSpec {
    pub fn glyph<S: Into<String>>(mut self, g: S) -> Self { 
        self.glyph = g.into(); 
        self 
    }
}

#[derive(Debug)]
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
            Sealed => write!(f, "edge registry is sealed"),
            TooManyTypes => write!(f, "edge registry full (max 256)"),
            Conflict { glyph } => write!(f, "glyph '{}' already registered with different semantics", glyph),
            UnknownGlyph(g) => write!(f, "unknown glyph '{}'", g),
            InvalidCode(c) => write!(f, "invalid edge code {}", c),
        }
    }
}
impl Error for RegistryError {}

#[derive(Debug, Clone)]
pub struct EdgeRegistry {
    glyph_to_code: HashMap<String, u8>,
    code_to_spec: Vec<EdgeSpec>, // index = code
    sealed: bool,
}

impl EdgeRegistry {
    pub fn new() -> Self {
        Self { glyph_to_code: HashMap::new(), code_to_spec: Vec::new(), sealed: false }
    }
    pub fn is_sealed(&self) -> bool { self.sealed }
    pub fn seal(&mut self) { self.sealed = true; }

    pub fn len(&self) -> usize { self.code_to_spec.len() }

    pub fn register(&mut self, spec: EdgeSpec) -> Result<u8, RegistryError> {
        if self.sealed { return Err(RegistryError::Sealed); }
        if let Some(&code) = self.glyph_to_code.get(&spec.glyph) {
            let existing = &self.code_to_spec[code as usize];
            if existing == &spec { return Ok(code); }
            return Err(RegistryError::Conflict { glyph: spec.glyph.clone() });
        }
        if self.code_to_spec.len() == 256 { return Err(RegistryError::TooManyTypes); }
        let code = self.code_to_spec.len() as u8;
        self.glyph_to_code.insert(spec.glyph.clone(), code);
        self.code_to_spec.push(spec);
        Ok(code)
    }

    pub fn code_of(&self, glyph: &str) -> Result<u8, RegistryError> {
        self.glyph_to_code.get(glyph).copied().ok_or_else(|| RegistryError::UnknownGlyph(glyph.into()))
    }
    pub fn spec_of_code(&self, code: u8) -> Result<&EdgeSpec, RegistryError> {
        self.code_to_spec.get(code as usize).ok_or(RegistryError::InvalidCode(code))
    }

    /// Register built-ins. Idempotent if called multiple times.
/// Register built-ins. Idempotent if called multiple times.
    pub fn register_builtins(&mut self) -> Result<(), RegistryError> {
        use EdgeClass as C; use Mark as M; use Orientation as O; use QueryFlags as F;
        let mut add = |glyph: &str, left: M, right: M, ori: O, class: C, symmetric: bool, flags: QueryFlags| {
            let spec = EdgeSpec { 
                glyph: glyph.to_string(), 
                left_mark: left, 
                right_mark: right,
                orientation: ori, 
                class, 
                symmetric, 
                flags };

            let _ = self.register(spec);
        };

        // Helpers
        let base = F::TRAVERSABLE_WHEN_CONDITIONED;

        // Directed arrow right: tail(child), head(parent)
        add("-->", M::Line, M::Arrow, O::RightHead, C::Directed, false,
            base | F::TAIL_CHILD | F::HEAD_PARENT);

        // Undirected line-line
        add("---", M::Line, M::Line, O::None, C::Undirected, true,
            base | F::TAIL_UNDIR | F::HEAD_UNDIR);

        // Bidirected arrows both sides -> treat as undirected for splits
        add("<->", M::Arrow, M::Arrow, O::BothHeads, C::Bidirected, true,
             base | F::TAIL_UNDIR | F::HEAD_UNDIR | F::LATENT_CONFOUNDING);

        // Circle-circle undirected
        add("o-o", M::Circle, M::Circle, O::None, C::Undirected, true,
            base | F::TAIL_UNDIR | F::HEAD_UNDIR);

        // Circle-line partial (no head): undirected on both sides for splits
        add("o--", M::Circle, M::Line,  O::None, C::Partial, false,
            base | F::TAIL_UNDIR | F::HEAD_UNDIR);

        // Circle-arrow partial: tail undirected, head parent
        add("o->", M::Circle, M::Arrow, O::RightHead, C::Partial, false,
            base | F::TAIL_UNDIR | F::HEAD_UNDIR | F::TAIL_POSS_CHILD | F::HEAD_POSS_PARENT);

        Ok(())
    }
}

pub(crate) fn parse_mark(s: &str) -> Result<Mark, String> {
    match s {
        "line" => Ok(Mark::Line),
        "circle" => Ok(Mark::Circle),
        "arrow" => Ok(Mark::Arrow),
        "other" => Ok(Mark::Other),
        _ => Err(format!("unknown mark '{}'", s)),
    }
}

pub(crate) fn parse_orientation(s: &str) -> Result<Orientation, String> {
    match s {
        "left_head" => Ok(Orientation::LeftHead),
        "right_head" => Ok(Orientation::RightHead),
        "both_heads" => Ok(Orientation::BothHeads),
        "none" => Ok(Orientation::None),
        _ => Err(format!("unknown orientation '{}'", s)),
    }
}

pub(crate) fn parse_class(s: &str) -> Result<EdgeClass, String> {
    match s {
        "directed" => Ok(EdgeClass::Directed),
        "undirected" => Ok(EdgeClass::Undirected),
        "bidirected" => Ok(EdgeClass::Bidirected),
        "partial" => Ok(EdgeClass::Partial),
        _ => Err(format!("unknown class '{}'", s)),
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
        for g in ["-->", "---", "<->", "o-o", "o--", "o->"] {
            assert!(r.code_of(g).is_ok(), "missing builtin {}", g);
        }
    }

    #[test]
    fn register_new_and_fetch_spec() {
        let mut r = built_reg();
        let before = r.len();
        let spec = EdgeSpec {
            glyph: "--<".into(),
            left_mark: Mark::Line,
            right_mark: Mark::Arrow,
            orientation: Orientation::LeftHead,
            class: EdgeClass::Directed,
            symmetric: false,
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
            left_mark: Mark::Circle,
            right_mark: Mark::Other,
            orientation: Orientation::RightHead,
            class: EdgeClass::Partial,
            symmetric: false,
            flags: QueryFlags::TRAVERSABLE_WHEN_CONDITIONED,
        };
        let c1 = r.register(spec.clone()).unwrap();
        let c2 = r.register(spec).unwrap();
        assert_eq!(c1, c2);
    }

    #[test]
    fn conflict_and_seal_behaviour() {
        let mut r = built_reg();
        // conflict: redefine existing glyph with different semantics
        let bad = EdgeSpec {
            glyph: "-->".into(),
            left_mark: Mark::Line,
            right_mark: Mark::Line,
            orientation: Orientation::None,
            class: EdgeClass::Undirected,
            symmetric: true,
            flags: QueryFlags::TRAVERSABLE_WHEN_CONDITIONED,
        };
        assert!(matches!(r.register(bad), Err(RegistryError::Conflict { .. })));

        // seal prevents any further registrations
        r.seal();
        let new = EdgeSpec {
            glyph: "x".into(),
            left_mark: Mark::Line,
            right_mark: Mark::Line,
            orientation: Orientation::None,
            class: EdgeClass::Undirected,
            symmetric: true,
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

    #[test]
    fn parse_helpers() {
        assert_eq!(parse_mark("line").unwrap(), Mark::Line);
        assert!(parse_mark("zzz").is_err());

        assert_eq!(parse_orientation("left_head").unwrap(), Orientation::LeftHead);
        assert!(parse_orientation("zzz").is_err());

        assert_eq!(parse_class("directed").unwrap(), EdgeClass::Directed);
        assert!(parse_class("zzz").is_err());
    }
}
