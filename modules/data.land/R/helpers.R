stop <- function(...) {
    if (requireNamespace(PEcAn.utils)) {
        PEcAn.utils::logger.error(...)
    } else {
        stop(...)
    }
}
