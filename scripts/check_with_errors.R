library(devtools)
arg <- commandArgs(trailingOnly = TRUE)
pkg <- arg[1]

log_level <- Sys.getenv('LOGLEVEL', unset = NA)
die_level <- Sys.getenv('DIELEVEL', unset = NA)

message('log_level = ', log_level)
message('die_level = ', die_level)

valid_log_levels <- c('warn', 'all')
if (!is.na(log_level) && !log_level %in% valid_log_levels) {
    stop('Invalid log_level "', log_level, '". Select one of: ', 
         paste(valid_log_levels, collapse = ', '))
}

if (!is.na(die_level) && !die_level == 'warn') {
    stop('Invalid die_level "', die_level, 
         '". Use either "warn" for warnings or leave blank for error.')
}

log_warn <- !is.na(log_level) && log_level %in% c('warn', 'all')
die_warn <- !is.na(die_level) && die_level == 'warn'

log_notes <- !is.na(log_level) && log_level == 'all'

chk <- check(pkg, quiet = TRUE)

errors <- chk[['errors']]
n_errors <- length(errors)

if (n_errors > 0) {
    cat(errors, '\n')
    stop(n_errors, ' errors found in ', pkg, '. See above for details')
}

warns <- chk[['warnings']]
n_warns <- length(warns)
message(n_warns, ' warnings found in ', pkg, '.')

if ((log_warn|die_warn) && n_warns > 0) {
    cat(warns, '\n')
    if (die_warn && n_warns > 0) {
        stop('Killing process because ', n_warns, ' warnings found in ', pkg, '.')
    }
}

notes <- chk[['notes']]
n_notes <- length(notes)
message(n_notes, ' notes found in ', pkg, '.')

if (log_notes && n_notes > 0) {
    cat(notes, '\n')
}
