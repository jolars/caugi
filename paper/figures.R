#!/usr/bin/env Rscript
# Generates the PDF figures referenced by paper.md into paper/figures/.

library(caugi)

out_dir <- file.path("figures")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

cg <- caugi(
  A %-->% B + C,
  B %-->% D,
  C %-->% D,
  class = "DAG"
)

pdf(file.path(out_dir, "example-plot.pdf"), width = 2, height = 2)
plot(cg)
dev.off()
