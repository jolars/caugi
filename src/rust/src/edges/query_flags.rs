// SPDX-License-Identifier: MIT
use bitflags::bitflags;

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct QueryFlags: u32 {
        const TRAVERSABLE_WHEN_CONDITIONED = 1 << 0;
        const LATENT_CONFOUNDING           = 1 << 1;
    }
}

// todo: remove query flags