# This script downloads the SIPNET binary for the appropriate operating system
# and sets it up for use in the Demo_Run tutorial.
os <- Sys.info()["sysname"]
if(os == "Darwin") {
  os <- "MacOS"
}
arch <- Sys.info()["machine"]

sipnet_version <- "v2.2.0"
base_url <- paste0(
  "https://github.com/PecanProject/sipnet/releases/download/",
  sipnet_version, "/"
)

# SIPNET v2 releases are tarballs containing a single `sipnet` binary.
# Only Linux x86_64 and macOS arm64 builds are provided.
if (os == "Linux" && arch == "x86_64") {
  asset <- paste0("sipnet-linux-x86_64-", sipnet_version, ".tar.gz")
} else if (os == "MacOS" && arch == "arm64") {
  asset <- paste0("sipnet-macos-arm64-", sipnet_version, ".tar.gz")
} else {
  PEcAn.logger::logger.severe(
    "No prebuilt SIPNET ", sipnet_version, " binary for ", os, " (", arch, "). ",
    "See https://github.com/PecanProject/sipnet/releases ",
    "or build SIPNET from source."
  )
}
download_url <- paste0(base_url, asset)

demo_outdir <- file.path("demo_outdir")
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

tarball <- file.path(demo_outdir, asset)
download.file(
    url = download_url,
    destfile = tarball,
    mode = "wb"
)
utils::untar(tarball, files = "sipnet", exdir = demo_outdir)
if (!file.exists(dest_path)) {
  PEcAn.logger::logger.severe(
    "Extracting ", tarball, " did not produce ", dest_path, ". ",
    "The layout of the SIPNET release archive may have changed."
  )
}

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
