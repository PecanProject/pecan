README.md
=========
Keep stuff in this folder that is needed to customize
a dvmdostem run for working with PEcAn. For instance, 
template files for configuration, and other config or
setup files that need to be arranged a particular way
for working with PEcAn.

One example of this is the output_spec.csv file. This
file ships with dvmdostem. While we could copy the file
from the main dvmdostem install folder, and modify it, 
we can simply keep a copy in this inst/ folder that has
the appropriate outputs enabled for a PEcAn run. During
the PEcAn run the copy kept here will be copied to the
run directory for the model. That allows us to 
essentially have a default set of outputs enabled 
particular to PEcAn. In the future I suppose this could
be template-ized to allow certain outputs to be toggled
on/off via the PEcAn web interface...

For testing and debugging work, here is one reccomended
setup that has been used successfully on the VM:
  - Make a `~/scratch` directory.
  - Copy the `pecan/web/workflow.R` file into the `~/scratch`.
    Alternatively you can symlink the file so that as you
    pull in updates to the pecan repo, your file in 
    `~/scratch` stays up to date.
  - Copy one of the example pecan.xml files (from 
    `pecan/models/dvmdostem/inst/`) into `~/scratch`.
  - Open RStudio (or a basical terminal) and set the 
    working directory to `~/scratch`.
  - Run the `workflow.R` script, either by sourcing in
    Rstudio, or by running with `Rscript` in a basic
    terminal.