// SPDX-License-Identifier: MIT
use bitflags::bitflags;

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct QueryFlags: u16 {
        /// Edge is traversable when conditioned-on nodes appear (for d/m-sep logic).
        const TRAVERSABLE_WHEN_CONDITIONED = 0b0000_0001;
        // Reserve bits for future semantics as needed.
    }
}
