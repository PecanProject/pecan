Contents of PEcAn scripts/ folder
---------------------------------


## Building PEcAn

#### `./install.dependencies.R`

Installs R functions required by R

#### `$PEcAn_root/scripts/quickbuild.R`

Uses `devtools` to quickly load all PEcAn packages in `dev_mode()`

#### `./build.sh`

Builds PEcAn


## Documentation

#### `./dependencies.R` 

makes graph of package dependencies for all PEcAn packages

#### `./updateVersion.sh 0.0.1`

Bumps Version number


#### `./roxygenize.R`

Updates documentation for all packages

## Testing / Demonstration

## Helper Functions 
 
#### `./Rfcn.R` 

executable for running R functions on remote machine at command line

  ```r
 ssh <machine> <package> <function> <args>
  ```

#### `workflow.R` test workflows

 ```r
 ./workflow.R --settings pecan.xml
```



### Loading Database

#### `./dump.bety.sh`

#### `./load.bety.sh`

#### `./add.models.sh`

#### `./add.data.sh`

#### `./add.util.sh`

convenience functions used in add.data.sh and add.models.sh
