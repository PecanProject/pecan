## Download and Install Models
This page contains instructions on how to download and install ecosystem models that have been or are being coupled to PEcAn.
These instructions have been tested on the PEcAn unbuntu VM. Commands may vary on other operating systems.
Also, some model downloads require permissions before downloading, making them unavailable to the general public. Please contact the PEcAn team if you would like access to a model that is not already installed on the default PEcAn VM.

- [BioCro](Installing-PEcAn-models.md#biocro)
- [CLM 4.5](Installing-PEcAn-models.md#clm4.5)
- [DALEC](Installing-PEcAn-models.md#dalec)
- [ED2](Installing-PEcAn-models.md#ed2)
- [GDAY](Installing-PEcAn-models.md#gday)
- [JULES](Installing-PEcAn-models.md#jules)
- [LINKAGES](Installing-PEcAn-models.md#linkages)
- [LPJ-GUESS](Installing-PEcAn-models.md#lpjguess)
- [MAESPA](Installing-PEcAn-models.md#maespa)
- [SIPNET](Installing-PEcAn-models.mdsipnet#sipnet)


## Install instructions



### ED2

#### ED2.2 r46 (used in PEcAn manuscript)

```bash
# ----------------------------------------------------------------------
# Get version r46 with a few patches for ubuntu
cd
curl -o ED.r46.tgz http://isda.ncsa.illinois.edu/~kooper/EBI/ED.r46.tgz
tar zxf ED.r46.tgz
rm ED.r46.tgz
# ----------------------------------------------------------------------
# configure and compile ed
cd ~/ED.r46/ED/build/bin
curl -o include.mk.VM http://isda.ncsa.illinois.edu/~kooper/EBI/include.mk.opt.`uname -s`
make OPT=VM
sudo cp ../ed_2.1-VM /usr/local/bin/ed2.r46
```

Perform a test run using pre configured ED settings for ED2.2 r46

```bash
# ----------------------------------------------------------------------
# Create sample run
cd
mkdir testrun.ed.r46
cd testrun.ed.r46
curl -o ED2IN http://isda.ncsa.illinois.edu/~kooper/EBI/ED2IN.r46
sed -i -e "s#\$HOME#$HOME#" ED2IN
curl -o config.xml  http://isda.ncsa.illinois.edu/~kooper/EBI/config.r46.xml
# execute test run
time ed2.r46
```

#### ED 2.2 r82

```bash
cd
curl -o ED.r82.tgz http://isda.ncsa.illinois.edu/~kooper/EBI/ED.r82.tgz
tar zxf ED.r82.tgz
rm ED.r82.tgz

cd ED.r82
curl -o ED.r82.patch http://isda.ncsa.illinois.edu/~kooper/EBI/ED.r82.patch
patch -p1 < ED.r82.patch
cd ED/build/bin
curl -o include.mk.VM http://isda.ncsa.illinois.edu/~kooper/EBI/include.mk.opt.`uname -s`
make OPT=VM
sudo cp ../ed_2.1-VM /usr/local/bin/ed2.r82
```

Perform a test run using pre configured ED settings for ED2.2 r82

```bash
cd
mkdir testrun.ed.r82
cd testrun.ed.r82
curl -o ED2IN http://isda.ncsa.illinois.edu/~kooper/EBI/ED2IN.r82
sed -i -e "s#\$HOME#$HOME#" ED2IN
curl -o config.xml  http://isda.ncsa.illinois.edu/~kooper/EBI/config.r82.xml
# execute test run
time ed2.r82
```

#### ED 2.2 bleeding edge

```bash
cd
git clone https://github.com/EDmodel/ED2.git

cd ED2/ED/build/bin
curl -o include.mk.VM http://isda.ncsa.illinois.edu/~kooper/EBI/include.mk.opt.`uname -s`
./generate_deps.sh
make OPT=VM
sudo cp ../ed_2.1-VM /usr/local/bin/ed2.git
```

### SIPNET

#### Sipnet Installation

```bash
cd
curl -o sipnet_unk.tar.gz http://isda.ncsa.illinois.edu/~kooper/EBI/sipnet_unk.tar.gz
tar zxf sipnet_unk.tar.gz
rm sipnet_unk.tar.gz

cd sipnet_unk
make
sudo cp sipnet /usr/local/bin/sipnet.runk
```

#### SIPNET testrun

```bash
cd
curl -o testrun.sipnet.tar.gz http://isda.ncsa.illinois.edu/~kooper/EBI/testrun.sipnet.tar.gz
tar zxf testrun.sipnet.tar.gz
rm testrun.sipnet.tar.gz
cd testrun.sipnet
sipnet.runk
```

### BioCro

#### Installation 
```r
# Public
echo 'devtools::install_github("ebimodeling/biocro")' | R --vanilla
# Development:
echo 'devtools::install_github("ebimodeling/biocro-dev")' | R --vanilla
```

_BioCro Developers:_ request from [@dlebauer on GitHub](https://github.com/dlebauer)

### Linkages

#### Installation 
```r
# Public
echo 'devtools::install_github("araiho/linkages_package")' | R --vanilla
```

### DALEC

#### Installation

```bash
cd
curl -o dalec_EnKF_pub.tgz http://isda.ncsa.illinois.edu/~kooper/EBI/dalec_EnKF_pub.tgz
tar zxf dalec_EnKF_pub.tgz
rm dalec_EnKF_pub.tgz

cd dalec_EnKF_pub
make dalec_EnKF
make dalec_seqMH
sudo cp dalec_EnKF dalec_seqMH /usr/local/bin
```

### LINKAGES

#### Installation

```r
#FORTRAN VERSION
cd
git clone https://github.com/araiho/Linkages.git
cd Linkages
gfortran -o linkages linkages.f
sudo cp linkages /usr/local/bin/linkages.git

#R Version
git clone https://github.com/araiho/linkages_package.git
R CMD INSTALL --no-multiarch --with-keep.source linkages_package
```

### CLM 4.5

The version of CLM installed on PEcAn is the ORNL branch provided by Dan Ricciuto. This version includes Dan's point-level CLM processing scripts

Download the code (~300M compressed), input data (1.7GB compressed and expands to 14 GB), and a few misc inputs.

```bash
mkdir models
cd models
wget ftp://nacp.ornl.gov/synthesis/2008/firenze/site/clm4_5_1_r085.tar.gz
wget ftp://nacp.ornl.gov/synthesis/2008/firenze/site/clm/ccsm_inputdata.tar.gz
tar -xvzf clm4_5*
tar -xvzf ccsm_inputdata.tar.gz

#Parameter file:
mkdir /home/carya/models/ccsm_inputdata/lnd/clm2/paramdata
cd  /home/carya/models/ccsm_inputdata/lnd/clm2/paramdata
wget ftp://nacp.ornl.gov/synthesis/2008/firenze/site/clm_params.c130821.nc
wget ftp://nacp.ornl.gov/synthesis/2008/firenze/site/clm_params.c140423.nc

#Domain file:
cd /home/carya/models/ccsm_inputdata/share/domains/domain.clm/
wget ftp://nacp.ornl.gov/synthesis/2008/firenze/site/domain.lnd.1x1pt_US-UMB_navy.nc

#Aggregated met data file:
cd /home/carya/models/ccsm_inputdata/atm/datm7/CLM1PT_data/1x1pt_US-UMB
wget ftp://nacp.ornl.gov/synthesis/2008/firenze/site/all_hourly.nc 

## lightning database
cd /home/carya/models/ccsm_inputdata/atm/datm7/NASA_LIS/
wget ftp://nacp.ornl.gov/synthesis/2008/firenze/site/clmforc.Li_2012_climo1995-2011.T62.lnfm_Total_c140423.nc

## surface data
cd /home/carya/models/ccsm_inputdata/lnd/clm2/surfdata
wget ftp://nacp.ornl.gov/synthesis/2008/firenze/site/clm/surfdata_360x720cru_simyr1850_c130927.nc
cd /home/carya/models/ccsm_inputdata/lnd/clm2/surfdata_map
wget ftp://nacp.ornl.gov/synthesis/2008/firenze/site/clm/surfdata_1x1pt_US-UMB_I1850CLM45CN_simyr1850.nc_new
mv surfdata_1x1pt_US-UMB_I1850CLM45CN_simyr1850.nc_new surfdata_1x1pt_US-UMB_I1850CLM45CN_simyr1850.nc
```
Required libraries
```bash
sudo apt-get install mercurial csh tcsh subversion cmake

sudo ln -s /usr/bin/make /usr/bin/gmake
```
Compile and build default inputs
```bash
cd ~/carya/models/clm4_5_1_r085/scripts
python runCLM.py --site US-UMB ––compset I1850CLM45CN --mach ubuntu --ccsm_input /home/carya/models/ccsm_inputdata --tstep 1 --nopointdata --coldstart --cpl_bypass --clean_build
```

#### CLM Test Run
You will see a new directory in scripts:  US-UMB_I1850CLM45CN
Enter this directory and run (you shouldn’t have to do this normally, but there is a bug with the python script and doing this ensures all files get to the right place):
```bash
./US-UMB_I1850CLM45CN.build
```

Next you are ready to go to the run directory:
```bash
/home/carya/models/clm4_5_1_r085/run/US-UMB_I1850CLM45CN/run
```
Open to edit file: datm.streams.txt.CLM1PT.CLM_USRDAT and check file paths such that all paths start with /home/carya/models/ccsm_inputdata

From this directory, launch the executable that resides in the bld directory:
```bash
/home/carya/clm4_5_1_r085/run/US-UMB_I1850CLM45CN/bld/cesm.exe
```    
not sure this was the right location, but wherever the executable is

You should begin to see output files that look like this:
US-UMB_I1850CLM45CN.clm2.h0.yyyy-mm.nc (yyyy is year, mm is month)
These are netcdf files containing monthly averages of lots of variables.

The lnd_in file in the run directory can be modified to change the output file frequency and variables.


### JULES

INSTALL STEPS:
    1) Download JULES and FCM
JULES:
    Model requires registration to download. Not to be put on PEcAn VM
Registration: https://jules.jchmr.org/software-and-documentation
Documentation: http://jules-lsm.github.io/vn4.2/index.html  

FCM:
```bash
    https://github.com/metomi/fcm/
    wget https://github.com/metomi/fcm/archive/2015.05.0.tar.gz
```

2) edit makefile
```bash
open etc/fcm-make/make.cfg

set JULES_NETCDF = actual instead of dummy
set path (e.g. /usr/) and lib_path /lib64 to netCDF libraries
```

3) compile JULES 

```bash
cd etc/fcm-make/
{path.to.fcm}/fcm make -f etc/fcm-make/make.cfg --new

```

```bash
UBUNTU VERSION: installed without having to add any perl libraries
#perl stuff that I had to install on pecan2 not PEcAN VM
sudo yum install perl-Digest-SHA
sudo yum install perl-Time-modules
sudo yum install cpan
curl -L http://cpanmin.us | perl - --sudo App::cpanminus
sudo cpanm Time/Piece.pm
sudo cpanm IO/Uncompress/Gunzip.pm
```


Executable is under build/bin/jules.exe

Example rundir:  examples/point_loobos

### MAESPA

Navigate to a directory you would like store MAESPA and run the following:

```bash
git clone https://bitbucket.org/remkoduursma/maespa.git

cd maespa

make
```

```maespa.out``` is your executable. Example input files can be found in the ```inpufiles``` directory. Executing measpa.out from within one of the example directories will produce output.

MAESPA developers have also developed a wrapper package called Maeswrap. The usual R package installation method ```install.packages``` may present issues with downloading an unpacking a dependency package called ```rgl```. Here are a couple of solutions:

#### Solution 1

```bash
### From the Command Line
sudo apt-get install r-cran-rgl
```
then from within R
```R
install.packages("Maeswrap")
```

#### Solution 2

```bash
### From the Command line 
sudo apt-get install libglu1-mesa-dev
```
then from within R
```R
install.packages("Maeswrap")
```
### GDAY

Navigate to a directory you would like to store GDAY and run the following:

```bash
git clone https://github.com/mdekauwe/GDAY.git

cd GDAY

cd src

make
```

```gday``` is your executable.

### LPJ-GUESS

Instructions to download source code

```bash
wget http://stormbringerii.nateko.lu.se/public/guess_download/guess_3.1.tar.gz
tar -xvzf guess_3.1.tar.gz
```

