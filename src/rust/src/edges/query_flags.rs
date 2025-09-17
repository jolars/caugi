// SPDX-License-Identifier: MIT
use bitflags::bitflags;

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct QueryFlags: u16 {
        // Per-side role bits for SPLIT_INDEX placement
        const TAIL_PARENT = 0b0000_0001;
        const TAIL_CHILD  = 0b0000_0010;
        const TAIL_UNDIR  = 0b0000_0100;
        const HEAD_PARENT = 0b0000_1000;
        const HEAD_CHILD  = 0b0001_0000;
        const HEAD_UNDIR  = 0b0010_0000;
        
        // Does the edge indicate latent confounding?
        const LATENT_CONFOUNDING = 0b1000_0000;

        // Per-side possible-role bits for queries
        const HEAD_POSS_CHILD  = 0b0000_1000_0000;
        const HEAD_POSS_PARENT = 0b0001_0000_0000;
        const TAIL_POSS_PARENT = 0b0000_0100_0000;
        const TAIL_POSS_CHILD  = 0b0010_0000_0000;

        // Extra semantics (e.g., d/m-sep traversal tweaks)
        const TRAVERSABLE_WHEN_CONDITIONED = 0b0100_0000;
    }
}
