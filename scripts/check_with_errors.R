#!/usr/bin/env Rscript

arg <- commandArgs(trailingOnly = TRUE)
pkg <- arg[1]

log_level <- Sys.getenv("LOGLEVEL", unset = NA)
die_level <- Sys.getenv("DIELEVEL", unset = NA)
redocument <- as.logical(Sys.getenv("REBUILD_DOCS", unset = NA))
runtests <- as.logical(Sys.getenv("RUN_TESTS", unset = TRUE))

old_file <- file.path(pkg, "tests", "Rcheck_reference.log")
if (file.exists(old_file)) {
    # Package has old unfixed warnings that we should ignore by default
    # (but if log/die level are explicitly set, respect them)
    if (is.na(log_level)) log_level <- "error"
    if (is.na(die_level)) die_level <- "error"
} else {
    if (is.na(log_level)) log_level <- "all"
    if (is.na(die_level)) die_level <- "note"
}

log_level <- match.arg(log_level, c("error", "warning", "note", "all"))
die_level <- match.arg(die_level, c("never", "error", "warning", "note"))

log_warn <- log_level %in% c("warning", "note", "all")
log_notes <- log_level %in% c("note", "all")

# should test se run
if (!runtests) {
    args <- c("--no-tests", "--timings")
} else {
    args <- c("--timings")
}

chk <- devtools::check(pkg, args = args, quiet = TRUE,
    error_on = die_level, document = redocument)

errors <- chk[["errors"]]
n_errors <- length(errors)
if (n_errors > 0) {
    cat(errors, "\n")
    stop(n_errors, " errors found in ", pkg, ".")
}

warns <- chk[["warnings"]]
n_warns <- length(warns)
message(n_warns, " warnings found in ", pkg, ".")
if ((log_warn) && n_warns > 0) {
    cat(warns, "\n")
}

notes <- chk[["notes"]]
n_notes <- length(notes)
message(n_notes, " notes found in ", pkg, ".")
if (log_notes && n_notes > 0) {
    cat(notes, "\n")
}


######

# PEcAn has a lot of legacy code that issues check warnings,
# such that it's not yet practical to break the build on every warning.
# Cleaning this up is a long-term goal, but will take time.
# Meanwhile, we compare against a cached historic check output to enforce that
# no *new* warnings are added. As historic warnings are removed, we will update
# the cached results to ensure they stay gone.
#
# To compare checks, we take a two-level approach:
# First by comparing results with rcmdcheck::compare_checks to find any new
# top-level warnings (e.g. "checking Rd cross-references ... WARNING"),
# then if those are OK we get fussier and check for new *instances* of existing
# warnings (e.g. new check increases from 2 bad Rd cross-references to 3).

###
# To update reference files after fixing an old warning:
# * Run check_with_errors.R to be sure the check is currently passing
# * Delete the file you want to update
# * Uncomment this section
# * run `DIELEVEL=never Rscript scripts/check_with_errors.R path/to/package`
# * recomment this section
# * Commit updated file
# if (!file.exists(old_file)) {
#     cat("No reference check file found. Saving current results as the new standard\n")
#     cat(chk$stdout, file = old_file)
#     quit("no")
# }
###

# everything beyond this point is comparing to old version
if (!file.exists(old_file)) {
    quit("no")
}

old <- rcmdcheck::parse_check(old_file)
cmp <- rcmdcheck::compare_checks(old, chk)

msg_lines <- function(msg) {
    # leading double-space indicates wrapped line -> rejoin
    msg <- gsub("\n  ", " ", msg, fixed = TRUE)

    #split lines, delete empty ones
    msg <- strsplit(msg, split = "\n", fixed = TRUE)
    msg <- lapply(msg, function(x)x[x != ""])

    # prepend message title (e.g. "checking Rd files ... NOTE") to each line
    unlist(lapply(
        msg,
        function(x) {
            if (length(x) > 1) {
                paste(x[[1]], x[-1], sep = ": ")
            } else {
                x
            }
        }))
}

if (cmp$status != "+") {
    # rcmdcheck found new messages, so check has failed
    print(cmp)
    cat("R check of", pkg, "reports the following new problems.",
        "Please fix these and resubmit:\n")
    cat(cmp$cmp$output[cmp$cmp$change == 1], sep = "\n")
    stop("Please fix these and resubmit.")
} else {
    # No new messages, but need to check details of pre-existing ones
    # We stopped earlier for errors, so all entries here are WARNING or NOTE
    cur_msgs <- msg_lines(cmp$cmp$output[cmp$cmp$which == "new"])
    prev_msgs <- msg_lines(cmp$cmp$output[cmp$cmp$which == "old"])

    # avoids false positives from tempdir changes
    cur_msgs <- gsub(chk$checkdir, "...", cur_msgs)
    prev_msgs <- gsub(old$checkdir, "...", prev_msgs)

    # R 3.6.0 switched style for lists of packages
    # from space-separated fancy quotes to comma-separated straight quotes
    # We'll meet halfway, with space-separated straight quotes
    cur_msgs <- gsub("[‘’]", "'", cur_msgs)
    cur_msgs <- gsub("', '", "' '", cur_msgs)
    prev_msgs <- gsub("[‘’]", "'", prev_msgs)
    prev_msgs <- gsub("', '", "' '", prev_msgs)

    # Compression warnings report slightly different sizes on different R
    # versions. If the only difference is in the numbers, don't complain
    cmprs_msg <- grepl("significantly better compression", cur_msgs)
    if (any(cmprs_msg)) {
        prev_cmprs_msg <- grepl("significantly better compression", prev_msgs)
        cur_cmprs_nodigit <- gsub("[0-9]", "", cur_msgs[cmprs_msg])
        prev_cmprs_nodigit <- gsub("[0-9]", "", prev_msgs[prev_cmprs_msg])
        if (all(cur_cmprs_nodigit %in% prev_cmprs_nodigit)) {
            cur_msgs <- cur_msgs[!cmprs_msg]
        }
    }

    # These lines are redundant summaries of issues also reported individually
    # and create false positives when an existing issue is fixed
    cur_msgs <- cur_msgs[!grepl(
        "NOTE: Undefined global functions or variables:", cur_msgs)]
    cur_msgs <- cur_msgs[!grepl("NOTE: Consider adding importFrom", cur_msgs)]

    lines_changed <- setdiff(cur_msgs, prev_msgs)

    # Crude hack:
    # Some messages are locale-dependent in complex ways,
    # e.g. the note about undocumented datasets concatenates CSV names
    # (ordered in the current locale) and objects in RData files (always
    # ordered in C locale), and so on.
    # As a last effort, we look for pre-existing lines that contain the same
    # words in a different order
    if (length(lines_changed) > 0) {
        prev_words <- strsplit(prev_msgs, " ")
        changed_words <- strsplit(lines_changed, " ")
        is_reordered <- function(v1, v2) {
            length(v1[v1 != ""]) == length(v2[v2 != ""]) && setequal(v1, v2)
        }
        is_in_prev <- function(line) {
            any(vapply(
                X = prev_words,
                FUN = is_reordered,
                FUN.VALUE = logical(1),
                line))
        }
        in_prev <- vapply(
            X = changed_words,
            FUN = is_in_prev,
            FUN.VALUE = logical(1))
        lines_changed <- lines_changed[!in_prev]
    }
    if (length(lines_changed) > 0) {
        cat("R check of", pkg, "returned new problems:\n")
        cat(lines_changed, sep = "\n")
        stop("Please fix these and resubmit.")
    }
}
