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

status <- suppressWarnings(
    system2(dest_path, "-h", stderr = TRUE, stdout = TRUE) |>
        attr("status")
)

if(status == 1){
    # 1 is expected for `sipnet -h`
    PEcAn.logger::logger.info("SIPNET has been installed!")
} else {
    PEcAn.logger::logger.error("SIPNET installation has failed with status:", status)
}

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
