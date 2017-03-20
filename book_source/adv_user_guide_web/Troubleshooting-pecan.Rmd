## cookies and pecan web pages

You may need to disable cookies specifically for the pecan webserver in your browser. This shouldn't be a problem running from the virtual machine, but your installation of php can include a 'PHPSESSID' that is quite long, and this can overflow the params field of the workflows table, depending on how long your hostname, model name, site name, etc are. 

## Warning: mkdir() [function.mkdir]: No such file or directory

If you are seeing: `Warning: mkdir() [function.mkdir]: No such file or directory in /path/to/pecan/web/runpecan.php at line 169` it is because you have used a relative path for $output_folder in system.php.

## `Can't insert workflow : Incorrect integer value: ''

`Can't insert workflow : Incorrect integer value: '' for column 'advanced_edit' at row 1`, you are running mysql in strict mode. Open /usr/local/mysql/my.cnf or wherever you have installed mysql ('locate my.cnf' may help you) and comment out "sql_mode=NO_ENGINE_SUBSTITUTION,STRICT_TRANS_TABLES". Restart your mysql daemon using 'sudo /Library/StartupItems/MySQLCOM/MySQLCOM restart' and try again.

## After creating a new PFT the <num> tag for PFT not passed to config.xml in ED

This is a result of the rather clunky way we currently have adding PFTs to PEcAn. This is happening because you need to edit the ./pecan/models/ed/data/pftmapping.csv file to include your new PFTs.

This is what the file looks like:

PEcAn;ED
ebifarm.acru;11
ebifarm.acsa3;11
...

You just need to edit this file (in a text editor no Excel) and add your PFT names and associated number to the end of the file. Once you do this, recompile PEcAn and it should then work for you. We currently need to reference this file in order to properly set the PFT number and maintain internal consistency between PEcAn and ED2.
