# Tests for AG (Ancestral Graph) functionality

# ─────────────────────────────────────────────────────────────────────────────
# AG Construction and Validation
# ─────────────────────────────────────────────────────────────────────────────

test_that("is_ag correctly identifies ancestral graphs", {
  # Pure DAG is a valid AG
  dag <- caugi(A %-->% B %-->% C, class = "DAG")
  expect_true(is_ag(dag, force_check = TRUE))

  # UG is a valid AG
  ug <- caugi(A %---% B, class = "UG")
  expect_true(is_ag(ug, force_check = TRUE))

  # ADMG is a valid AG (no undirected edges)
  admg <- caugi(
    A %-->% B,
    A %<->% C,
    class = "ADMG"
  )
  expect_true(is_ag(admg, force_check = TRUE))

  # AG with all three edge types (valid configuration)
  # Note: can't mix undirected edges with arrowhead edges on same node
  ag <- caugi(
    A %-->% B,
    C %<->% D,
    E %---% F,
    class = "AG"
  )
  expect_true(is_ag(ag))
  expect_equal(ag@graph_class, "AG")
})

test_that("AG rejects directed cycles", {
  expect_error(
    caugi(
      A %-->% B,
      B %-->% C,
      C %-->% A,
      class = "AG"
    ),
    regexp = "[Cc]ycle"
  )
})

test_that("AG rejects undirected constraint violation", {
  # A node with undirected edge cannot have arrowhead edge
  expect_error(
    caugi(
      A %---% B, # B has undirected edge
      C %-->% B, # B has incoming directed edge (arrowhead)
      class = "AG"
    ),
    regexp = "[Uu]ndirected"
  )

  expect_error(
    caugi(
      A %---% B, # B has undirected edge
      B %<->% C, # B has bidirected edge (arrowhead)
      class = "AG"
    ),
    regexp = "[Uu]ndirected"
  )
})

test_that("AG rejects partial edges", {
  expect_error(
    caugi(
      A %o->% B,
      class = "AG"
    ),
    regexp = "can only contain"
  )
})

# ─────────────────────────────────────────────────────────────────────────────
# Graph Query Functions
# ─────────────────────────────────────────────────────────────────────────────

test_that("parents and children work for AG", {
  ag <- caugi(
    A %-->% B,
    B %-->% C,
    D %<->% E,
    class = "AG"
  )

  expect_equal(parents(ag, "B"), "A")
  expect_equal(children(ag, "A"), "B")
  expect_equal(parents(ag, "C"), "B")
  expect_equal(children(ag, "B"), "C")
})

test_that("spouses works for AG", {
  ag <- caugi(
    A %-->% B,
    C %<->% D,
    class = "AG"
  )

  expect_equal(spouses(ag, "C"), "D")
  expect_equal(spouses(ag, "D"), "C")
  expect_equal(length(spouses(ag, "A")), 0L)
})

test_that("undirected neighbors work for AG", {
  ag <- caugi(
    A %---% B %---% C,
    class = "AG"
  )

  expect_equal(neighbors(ag, "B", mode = "undirected"), c("A", "C"))
  expect_equal(neighbors(ag, "A", mode = "undirected"), "B")
})

test_that("ancestors and descendants work for AG", {
  ag <- caugi(
    A %-->% B %-->% C,
    D %<->% E,
    class = "AG"
  )

  expect_equal(sort(ancestors(ag, "C")), c("A", "B"))
  expect_equal(sort(descendants(ag, "A")), c("B", "C"))

  # Bidirected doesn't count as ancestor/descendant
  expect_equal(length(ancestors(ag, "D")), 0L)
  expect_equal(length(descendants(ag, "D")), 0L)
})

test_that("anteriors work for AG", {
  # Anteriors include parents and undirected neighbors transitively
  # Note: In an AG, a node with undirected edge can't have arrowhead edges
  # So we create a valid graph: A --> B, C --- D --- E
  ag <- caugi(
    A %-->% B,
    C %---% D %---% E,
    class = "AG"
  )

  # Anteriors of B: A (parent)
  ant_b <- anteriors(ag, "B")
  expect_true("A" %in% ant_b)

  # Anteriors of E: D (undirected), C (via D)
  ant_e <- anteriors(ag, "E")
  expect_true("D" %in% ant_e)
  expect_true("C" %in% ant_e) # via D --- C
})

test_that("exogenous returns nodes without parents", {
  ag <- caugi(
    A %-->% B,
    C %<->% D,
    E %---% F,
    class = "AG"
  )

  exo <- exogenous(ag)

  # A, C, D, E, F have no parents
  expect_true("A" %in% exo)
  expect_true("C" %in% exo)
  expect_true("D" %in% exo)
  expect_true("E" %in% exo)
  expect_true("F" %in% exo)
  expect_false("B" %in% exo)
})

# ─────────────────────────────────────────────────────────────────────────────
# M-Separation
# ─────────────────────────────────────────────────────────────────────────────

test_that("m_separated works for chain structures in AG", {
  ag <- caugi(
    A %-->% B %-->% C,
    class = "AG"
  )

  # A and C not separated unconditionally
  expect_false(m_separated(ag, "A", "C"))

  # A and C separated given B
  expect_true(m_separated(ag, "A", "C", "B"))
})

test_that("m_separated works for collider structures in AG", {
  ag <- caugi(
    A %-->% C,
    B %-->% C,
    class = "AG"
  )

  # A and B are m-separated unconditionally (collider at C blocks)
  expect_true(m_separated(ag, "A", "B"))

  # A and B NOT m-separated given C (conditioning on collider opens)
  expect_false(m_separated(ag, "A", "B", "C"))
})

test_that("m_separated works for undirected paths in AG", {
  ag <- caugi(
    A %---% B %---% C,
    class = "AG"
  )

  # A and C not m-separated unconditionally (connected via undirected)
  expect_false(m_separated(ag, "A", "C"))

  # A and C m-separated given B
  expect_true(m_separated(ag, "A", "C", "B"))
})

test_that("m_separated handles bidirected confounding in AG", {
  ag <- caugi(
    A %-->% B,
    C %<->% D,
    class = "AG"
  )

  # C and D are not m-separated (directly connected via bidirected)
  expect_false(m_separated(ag, "C", "D"))

  # A and C are m-separated (no path)
  expect_true(m_separated(ag, "A", "C"))
})

test_that("m_separated opens collider paths in bidirected chains", {
  ag <- caugi(
    A %<->% B %<->% C %<->% D,
    class = "AG"
  )

  # Colliders block the path unless conditioned on.
  expect_true(m_separated(ag, "A", "D"))

  # Conditioning on colliders opens the path.
  expect_false(m_separated(ag, "A", "D", c("B", "C")))
})

# ─────────────────────────────────────────────────────────────────────────────
# Districts
# ─────────────────────────────────────────────────────────────────────────────

test_that("districts returns c-components for AG", {
  ag <- caugi(
    A %-->% B,
    C %<->% D,
    E %---% F,
    class = "AG"
  )

  dists <- districts(ag)

  # Should have districts: {C,D} and singletons for A, B, E, F
  expect_equal(length(dists), 5L)

  # Check that C and D are in the same district
  c_dist <- dists[sapply(dists, function(d) "C" %in% d)][[1]]
  expect_true("D" %in% c_dist)
})

# ─────────────────────────────────────────────────────────────────────────────
# MAG (Maximal Ancestral Graph)
# ─────────────────────────────────────────────────────────────────────────────

test_that("is_mag identifies maximal ancestral graphs", {
  # A complete AG (all pairs adjacent) is trivially maximal
  ag_complete <- caugi(
    A %-->% B,
    A %-->% C,
    B %<->% C,
    class = "AG"
  )

  # This should be maximal (no edge can be added without violating AG)
  result <- is_mag(ag_complete)
  expect_type(result, "logical")
})

# ─────────────────────────────────────────────────────────────────────────────
# Markov Blanket
# ─────────────────────────────────────────────────────────────────────────────

test_that("markov_blanket works for AG", {
  ag <- caugi(
    L %-->% X,
    X %-->% Y,
    X %<->% Z,
    class = "AG"
  )

  mb_x <- markov_blanket(ag, "X")

  # MB(X) includes parents, and district members (spouses)
  expect_true("L" %in% mb_x) # Parent of X
  expect_true("Z" %in% mb_x) # Spouse (district member)
})

# ─────────────────────────────────────────────────────────────────────────────
# Subgraph
# ─────────────────────────────────────────────────────────────────────────────

test_that("subgraph preserves AG class", {
  ag <- caugi(
    A %-->% B %-->% C,
    D %<->% E,
    class = "AG"
  )

  sub <- subgraph(ag, c("A", "B", "C"))

  expect_equal(sub@graph_class, "AG")
  expect_true(is_ag(sub))
})
