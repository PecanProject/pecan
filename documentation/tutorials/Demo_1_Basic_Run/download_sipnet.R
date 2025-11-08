# This script downloads the SIPNET binary for the appropriate operating system
# and sets it up for use in the Demo_Run tutorial.
os <- Sys.info()["sysname"]
if(os == "Darwin") {
  os <- "MacOS"
}

base_url <- "https://github.com/PecanProject/sipnet/releases/download/v1.3.0/"

if (os == "Linux") {
  download_url <- paste0(base_url, "sipnet-linux-v1.3.0")
} else if (os == "MacOS") {
  download_url <- paste0(base_url, "sipnet-macos-v1.3.0")
} else {
  PEcAn.logger::logger.error("Unsupported operating system: ", os)
}

demo_outdir <- here::here("documentation/tutorials/Demo_1_Basic_Run/demo_outdir")
dest_path <- file.path(demo_outdir, "sipnet")
if (!dir.exists(demo_outdir)) {
    # using if(!dir.exists) instead of `showWarnings = FALSE`
    # to allow warnings like 'cannot create dir ...'
    dir.create(demo_outdir,
        recursive = TRUE
    )
}

PEcAn.logger::logger.info(
    "Downloading SIPNET binary for", os, "..."
)

download.file(
    url = download_url, 
    destfile = dest_path,
    mode = "wb"
)

# Make executable
Sys.chmod(dest_path, mode = "0755")

## Now we are run, lets just check that `sipnet -h` works
tryCatch(
  {
    # This block runs if system2 succeeds with exit code 0 (status attribute is NULL).
    # Exit code 0 means successful execution.
    system2(dest_path, "-h", stderr = TRUE, stdout = TRUE)
    PEcAn.logger::logger.info("SIPNET has been installed!")
  },
  warning = function(w) {
    # This block runs if system2 returns a non-zero exit code.
    # We check the warning message for the expected status of 1.
    if (grepl("had status 1", w$message, fixed = TRUE)) {
      PEcAn.logger::logger.info("SIPNET has been installed!")
    } else {
      PEcAn.logger::logger.error("SIPNET ran but failed with an unexpected status.", "Details:", w$message)
    }
  },
  error = function(e) {
    # This block runs if system2 fails to execute the command at all.
    PEcAn.logger::logger.error(
      "SIPNET command failed to execute. The binary may be incompatible with your system.",
      "Details:", e$message
    )
  }
)

dir.create("dbfiles", showWarnings = FALSE)
# Download demo .clim file
climfile <- "https://gist.githubusercontent.com/dlebauer/8aea1146dc8f915e1dea7a7335d7ec24/raw/4cc127098b0b42a0d428fc7de580e17aafca4e8b/AMF_US-NR1_BASE_HH_23-5.2004-01-01.2004-12-31.clim"
clim_dest <- file.path("dbfiles", basename(climfile))
if (!file.exists(clim_dest)) {
  download.file(
    url = climfile,
    destfile = clim_dest,
    mode = "wb"
  )
}
