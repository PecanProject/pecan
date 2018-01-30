## SIPNET configuration

SIPNET is configured using 3 files which are placed in the run folder, as well as a symbolic link to the met file.

* **sipnet.in** : template for this file is located at models/sipnet/inst/sipnet.in and is not modified.  
* **sipnet.param-spatial** : template for this file is located at models/sipnet/inst/template.param-spatial and is not modified.  
* **sipnet.param** : template for this file is in models/sipnet/inst/template.param or it is specified in the \<model\> section as \<default.param\>. The values in this template are replaced by those computed in the earlier stages of PEcAN.
