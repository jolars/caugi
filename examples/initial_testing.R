# initial_testing.R
#
# smoke‐test the core sprig_graph constructor and as_tibble()
devtools::load_all()
library(sprig)
# same tiny graph as before:
nodes <- tibble::tibble(name = c("A", "B", "C", "D"))
edges <- tibble::tibble(
  from      = c("A", "A", "B", "C"),
  to        = c("B", "C", "D", "A"),
  edge_type = c("-->", "o->", "<->", "--o")
)

# get integer IDs & codes:
from_i <- as.integer(match(edges$from, nodes$name))
to_i <- as.integer(match(edges$to, nodes$name))
tc <- as.integer(factor(edges$edge_type,
  levels = c("-->", "<--", "<->", "o->", "<-o", "o--", "--o", "o-o")
))

# call the C++ function directly
res <- sprig_create_csr(from_i, to_i, tc, as.integer(nrow(nodes)))
res
g <- sprig_graph(nodes, edges)
g
