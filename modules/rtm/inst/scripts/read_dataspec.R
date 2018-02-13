fname <- "src/RTM/modules/dataSpec/dataSpec_prospectd.f90"
raw_file <- readLines(fname)

get_values <- function(raw_file, string) {
  pattern <- sprintf("^[[:space:]]+DATA \\(%s\\(", string)
  all_mentions <- grep(pattern, raw_file)
  data_lines <- all_mentions + 1
  raw_lines <- trimws(raw_file[data_lines])
  proc_lines <- gsub("/$", "", raw_lines)
  num_list <- strsplit(proc_lines, ",")
  num_char <- Reduce(c, num_list)
  as.numeric(num_char)
}

wl <- 400:2500
pars <- c(
  "refractive",
  "k_cab",
  "k_car",
  "k_canth",
  "k_cbrown",
  "k_cw",
  "k_cm"
)
dat_list <- c(list(wavelength = 400:2500),
              lapply(pars, get_values, raw_file = raw_file))
dataSpec_prospectd <- do.call(cbind, dat_list)
colnames(dataSpec_prospectd) <- c("wavelength", pars)
save(dataSpec_prospectd, file = "data/dataSpec_prospectd.RData")

normalize <- function(x) x / max(x)
dat_mat_normalized <- apply(dataSpec_prospectd[, -1], 2, normalize)
matplot(dataSpec_prospectd[, 1], dat_mat_normalized, type = "l", lty = "solid")
