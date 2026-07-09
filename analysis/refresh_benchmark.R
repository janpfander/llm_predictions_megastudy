# =============================================================================
# analysis/refresh_benchmark.R
#
# Clears the benchmark render cache (analysis/results/*.rds) so the next render
# of preregistration_benchmark.qmd recomputes the heavy analyses (subgroup
# moderator fits, cluster-bootstrap forest data) from scratch via cached().
# (The cached() keys keep their historical "amendment_" prefix.)
#
# Run after changing any benchmark analysis code or the placeholder simulation:
#   Rscript analysis/refresh_benchmark.R
# =============================================================================

library(here)

results_dir <- here("analysis/results")
files <- list.files(results_dir, pattern = "\\.rds$", full.names = TRUE)
file.remove(files)
message(sprintf("cleared %d cached result(s) from %s", length(files), results_dir))
