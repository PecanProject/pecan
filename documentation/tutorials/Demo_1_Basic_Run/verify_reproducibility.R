#!/usr/bin/env Rscript
# verify_reproducibility.R
#
# PURPOSE: Verify that set.seed(20260325) in run_pecan.qmd (and uncertainty.qmd)
# causes all randomly-generated plot images to be byte-identical across two renders.
#
# USAGE (inside the PEcAn Docker container):
#   Rscript verify_reproducibility.R
#
# REQUIREMENTS: quarto must be available in PATH (it is inside the pecan container)

library(tools)  # for md5sum() -- base R, always available

render_and_collect_hashes <- function(qmd_file, outdir) {
  message("\n=== Rendering: ", basename(qmd_file), " ===")
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

  # Render the notebook
  ret <- system2(
    "quarto",
    args = c("render", qmd_file, "--output-dir", outdir, "--quiet"),
    stdout = TRUE, stderr = TRUE
  )

  # Collect all generated image files (PNGs produced by knitr/quarto)
  img_files <- list.files(outdir, pattern = "\\.(png|svg|pdf)$",
                          recursive = TRUE, full.names = TRUE)
  if (length(img_files) == 0) {
    message("  WARNING: No image files found in output directory.")
    return(character(0))
  }

  hashes <- md5sum(img_files)
  names(hashes) <- basename(img_files)
  hashes
}

compare_hashes <- function(h1, h2, label) {
  message("\n--- Comparing hashes for: ", label, " ---")
  all_names <- union(names(h1), names(h2))
  ok <- TRUE
  for (nm in all_names) {
    if (is.na(h1[nm])) {
      message("  ONLY IN RUN 2: ", nm)
      ok <- FALSE
    } else if (is.na(h2[nm])) {
      message("  ONLY IN RUN 1: ", nm)
      ok <- FALSE
    } else if (h1[nm] != h2[nm]) {
      message("  MISMATCH: ", nm)
      message("    Run 1: ", h1[nm])
      message("    Run 2: ", h2[nm])
      ok <- FALSE
    } else {
      message("  OK (identical): ", nm)
    }
  }
  ok
}

# ────────────────────────────────────────────────────────────────────────────
# Targets
# ────────────────────────────────────────────────────────────────────────────
script_dir  <- dirname(normalizePath(commandArgs(trailingOnly = FALSE)[4],
                                     mustWork = FALSE))
tutorials   <- normalizePath(file.path(script_dir, ".."), mustWork = FALSE)

targets <- list(
  list(
    qmd   = file.path(script_dir, "run_pecan.qmd"),
    out1  = file.path(script_dir, "_verify_run1"),
    out2  = file.path(script_dir, "_verify_run2"),
    label = "Demo 1 — run_pecan.qmd"
  ),
  list(
    qmd   = file.path(tutorials, "Demo_02_Uncertainty_Analysis", "uncertainty.qmd"),
    out1  = file.path(tutorials, "Demo_02_Uncertainty_Analysis", "_verify_run1"),
    out2  = file.path(tutorials, "Demo_02_Uncertainty_Analysis", "_verify_run2"),
    label = "Demo 2 — uncertainty.qmd"
  )
)

# ────────────────────────────────────────────────────────────────────────────
# Run verification
# ────────────────────────────────────────────────────────────────────────────
results <- list()
for (t in targets) {
  if (!file.exists(t$qmd)) {
    message("SKIP (file not found): ", t$qmd)
    next
  }
  h1 <- render_and_collect_hashes(t$qmd, t$out1)
  h2 <- render_and_collect_hashes(t$qmd, t$out2)
  results[[t$label]] <- compare_hashes(h1, h2, t$label)
}

# ────────────────────────────────────────────────────────────────────────────
# Summary
# ────────────────────────────────────────────────────────────────────────────
message("\n========== VERIFICATION SUMMARY ==========")
all_passed <- TRUE
for (label in names(results)) {
  status <- if (isTRUE(results[[label]])) "PASS ✅" else "FAIL ❌"
  message(sprintf("  %-45s %s", label, status))
  if (!isTRUE(results[[label]])) all_passed <- FALSE
}
message("==========================================")

if (all_passed) {
  message("\nAll notebooks render byte-identical images. Reproducibility confirmed.")
  quit(status = 0)
} else {
  message("\nSome images differ between renders. Check output above.")
  quit(status = 1)
}
