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

