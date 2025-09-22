// SPDX-License-Identifier: MIT
//! Unified graph view with dispatch to class-specific APIs.

use std::sync::Arc;
use std::str::FromStr;
use super::{CaugiGraph};
use super::dag::Dag;
use super::pdag::Pdag;

pub enum GraphKind { Dag, Pdag, Unknown } // ADMG, MAG, PAG to be added

impl FromStr for GraphKind{ type Err=String; fn from_str(s:&str)-> Result<Self,String>{
    match s.to_ascii_uppercase().as_str(){ 
        "DAG"=>Ok(Self::Dag),
        "PDAG"|"CPDAG"=>Ok(Self::Pdag),
      ""|"UNKNOWN"|"<UNKNOWN>"=>Ok(Self::Unknown), 
      other=>Err(format!("unknown graph class '{other}'"))}}}


#[derive(Debug, Clone)]
pub enum GraphView {
    Dag(Arc<Dag>),
    Pdag(Arc<Pdag>),
    Raw(Arc<CaugiGraph>),
    // ADMG, MAG, PAG to be added
}

pub trait GraphApi {
    fn parents_of(&self, _i: u32) -> Result<&[u32], String> { Err("parents_of not implemented for this class".into()) }
    fn children_of(&self, _i: u32) -> Result<&[u32], String> { Err("children_of not implemented for this class".into()) }
    fn undirected_of(&self, _i: u32) -> Result<&[u32], String> { Err("undirected_of not implemented for this class".into()) }
    fn n(&self) -> u32;
}

impl GraphApi for GraphView {
    fn parents_of(&self, i: u32) -> Result<&[u32], String> {
        match self {
            GraphView::Dag(g) => Ok(g.parents_of(i)),
            GraphView::Pdag(g) => Ok(g.parents_of(i)),
            GraphView::Raw(_)=>Err("parents_of not implemented for UNKNOWN class".into())
        }
    }
    fn children_of(&self, i: u32) -> Result<&[u32], String> {
        match self {
            GraphView::Dag(g) => Ok(g.children_of(i)),
            GraphView::Pdag(g) => Ok(g.children_of(i)),
            GraphView::Raw(_)=>Err("children_of not implemented for UNKNOWN class".into())
        }
    }
    fn undirected_of(&self, i: u32) -> Result<&[u32], String> {
        match self {
            GraphView::Dag(_) => Err("undirected_of not defined for Dag".into()),
            GraphView::Pdag(g) => Ok(g.undirected_of(i)),
            GraphView::Raw(_)=>Err("undirected_of not implemented for UNKNOWN class".into())
        }
    }

    fn n(&self) -> u32 {
        match self {
            GraphView::Dag(g) => g.n(),
            GraphView::Pdag(g) => g.n(),
            GraphView::Raw(core) => core.n(), 
        }
    }
}