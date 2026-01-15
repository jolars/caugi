test_that("Marginalization and conditioning work", {

  ## Examples from Figure 10 of Richardson and Spirtes 2002

  mg <- caugi(U %-->% X + Y,
              A %-->% X,
              B %-->% Y, class = "DAG")

  mgmu <- condition_marginalize(mg, margvars = "U")
  resmu <- caugi(A %-->% X, B %-->% Y, X %<->% Y)

  same_graphs <- function(cg1, cg2) {

    if(!cg1@graph_class == cg2@graph_class) FALSE
    if(!setequal(nodes(cg1)$name, nodes(cg2)$name)) FALSE

    shd(cg1, cg2) == 0

  }

  expect_true(same_graphs(mgmu, resmu))

  rescu <- caugi(A %-->% X, B %-->% Y)
  mgcu <- condition_marginalize(mg, condvars = "U")

  expect_true(same_graphs(rescu, mgcu))

  # Figure 11

  f11 <- caugi(A %-->% L1,
               L1 %-->% B,
               L2 %-->% B + C,
               B %-->% S,
               S %-->% D,
               D %-->% C)

  f11.cS <- condition_marginalize(f11, condvars = "S")
  f11.mL1L2 <- condition_marginalize(f11, margvars = c("L1", "L2"))
  f11.cSmL1L2 <- condition_marginalize(f11, margvars = c("L1", "L2"), condvars = "S")

  f11.ii <- caugi(A %---% L1, L1 %---% L2 + B, L2 %---% B,
                  L2 %-->% C, D %-->% C, class = "PDAG")
  f11.iii <- caugi(A %-->% B + C, B %-->% S + C,
                   S %-->% D, D %-->% C, class = "DAG")
  f11.iv <- caugi(A %---% B, A %-->% C, B %-->% C,
                   D %-->% C, class = "PDAG")

  same_graphs(f11.cS, f11.ii)
  same_graphs(f11.mL1L2, f11.iii)
  same_graphs(f11.cSmL1L2, f11.iv)

  cg1 <- caugi(L2 %---% B, L2 %-->% C, D %-->% C, class = "PDAG")
  cg2 <- caugi(D %-->% C, L2 %---% B, L2 %-->% C, class = "PDAG")
  shd(cg1, cg2)

})
