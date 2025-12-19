# Test script to validate unit conversion changes
# This verifies that ud_convert works with the new unit strings

library(PEcAn.utils)

cat("Testing unit string validity with UDUNITS2...\n\n")

# Test cases from the modified files
test_conversions <- list(
  list(value = 100, u1 = "g/m2", u2 = "kg/m2", expected = 0.1, desc = "DALEC/SIPNET pool: g/m2 to kg/m2"),
  list(value = 86400, u1 = "g/m2/d", u2 = "kg/m2/s", expected = 0.001, desc = "DALEC/SIPNET flux: g/m2/d to kg/m2/s"),
  list(value = 10, u1 = "Mg/ha", u2 = "kg/m2", expected = 1, desc = "GDAY: Mg/ha to kg/m2"),
  list(value = 1000, u1 = "J/mol", u2 = "kJ/mol", expected = 1, desc = "FATES: J/mol to kJ/mol")
)

results <- list()
for (i in seq_along(test_conversions)) {
  test <- test_conversions[[i]]
  tryCatch({
    result <- ud_convert(test$value, test$u1, test$u2)
    passed <- abs(result - test$expected) < 1e-10
    results[[i]] <- list(
      desc = test$desc,
      passed = passed,
      result = result,
      expected = test$expected
    )
    cat(sprintf("[%s] %s\n", if(passed) "PASS" else "FAIL", test$desc))
    if (!passed) {
      cat(sprintf("  Expected: %f, Got: %f\n", test$expected, result))
    }
  }, error = function(e) {
    results[[i]] <<- list(desc = test$desc, passed = FALSE, error = e$message)
    cat(sprintf("[FAIL] %s\n", test$desc))
    cat(sprintf("  Error: %s\n", e$message))
  })
}

cat("\n")
passed_count <- sum(sapply(results, function(x) x$passed))
total_count <- length(results)
cat(sprintf("Summary: %d/%d tests passed\n", passed_count, total_count))

if (passed_count < total_count) {
  cat("\nFAILED TESTS DETECTED - These must be fixed before proceeding\n")
  quit(status = 1)
} else {
  cat("\nAll unit strings are valid!\n")
}
