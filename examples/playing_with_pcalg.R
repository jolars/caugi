# experiment_pcalg.R
#
# A self-contained script to explore pcalg’s GraphNEL objects,
# plot them with Rgraphviz, and see how they map into caugi_graph.

# Install required packages if you haven’t already:
# install.packages(c("pcalg", "graph", "Rgraphviz", "caugi"))
devtools::load_all()
library(pcalg) # for PC algorithm and simulation
library(graph) # GraphNEL class & helpers
library(Rgraphviz) # for plotting GraphNEL
library(caugi) # your caugi_graph converters
library(tibble)

set.seed(123)
p <- 10
## true DAG
myDAG <- randomDAG(p, prob = 0.3)
## true CPDAG
myCPDAG <- dag2cpdag(myDAG)
## true PDAG with added background knowledge 5 -> 6
myPDAG <- addBgKnowledge(myCPDAG, 5, 6)
if (require(Rgraphviz)) {
  par(mfrow = c(1, 3))
  plot(myDAG)
  plot(myPDAG)
  plot(myCPDAG) ## plot of the graphs
}


# simulate linear Gaussian data with no latent confounders
sim <- rmvDAG(1000, myDAG)

# wrap data for pcalg
suffStat <- list(C = cor(sim), n = 1000)
indepTest <- gaussCItest

# 2. Run the PC algorithm to recover the CPDAG
pcFit <- pc(
  suffStat = suffStat,
  indepTest = indepTest,
  alpha = 0.01,
  labels = colnames(sim),
  verbose = FALSE
)

# pcFit is of class "pcAlgo", and pcFit@graph is a GraphNEL
gNEL <- pcFit@graph

# 3. Inspect the GraphNEL
print(gNEL)
cat("Nodes:\n")
print(nodes(gNEL))
cat("Edges (adjacency list):\n")
str(edges(gNEL))

# 4. Plot the GraphNEL
#    (this will open a graph window if run interactively)
plot(gNEL, main = "PC-recovered CPDAG (GraphNEL)")

# 5. Convert to caugi_graph via your R converter
caugi_from_graphNEL(gNEL, collapse = TRUE, collapse_to = "---")
caugi_from_graphNEL(gNEL, collapse = FALSE)

caugi_from_graphNEL(gNEL, collapse = TRUE, collapse_to = "---")
caugi_from_amat(as(pcFit, "amat"))

igraph::graph_from_graphnel(gNEL)
