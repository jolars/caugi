// SPDX-License-Identifier: MIT
use bitflags::bitflags;

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct QueryFlags: u32 {
        // definite per-side roles
        const TAIL_PARENT   = 1 << 0;
        const TAIL_CHILD    = 1 << 1;
        const TAIL_UNDIR    = 1 << 2;
        const HEAD_PARENT   = 1 << 3;
        const HEAD_CHILD    = 1 << 4;
        const HEAD_UNDIR    = 1 << 5;
        const TAIL_UNKNOWN  = 1 << 6;
        const HEAD_UNKNOWN  = 1 << 7;

        // possible per-side roles
        const TAIL_POSS_PARENT = 1 << 8;
        const TAIL_POSS_CHILD  = 1 << 9;
        const HEAD_POSS_PARENT = 1 << 10;
        const HEAD_POSS_CHILD  = 1 << 11;

        // extras
        const TRAVERSABLE_WHEN_CONDITIONED = 1 << 12;
        const LATENT_CONFOUNDING           = 1 << 13;
    }
}
