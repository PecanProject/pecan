## PalEON version of PEcAn

Install the following packages: gnome2, xfce4, firefox

```
sudo apt-get -y install gdm gnome-shell xfce4 firefox
```

Install Rstudio-desktop (+ libjpeg62 seems to be a dependency)

```
PROC=$( uname -m | sed -e 's/x86_64/amd64/' -e 's/i686/i386/' )
wget http://download1.rstudio.org/rstudio-0.98.507-${PROC}.deb
sudo apt-get install libjpeg62
sudo dpkg -i rstudio-*.deb
rm rstudio-*.deb
```

Additional pieces of software

```
wget http://chrono.qub.ac.uk/blaauw/LinBacon_2.2.zip
unzip LinBacon_2.2.zip
rm LinBacon_2.2.zip

wget http://chrono.qub.ac.uk/blaauw/clam.zip
unzip clam.zip
rm clam.zip
```

Additional R packages:
 
```
cat << EOF | R --vanilla
list.of.packages <- c('R2jags', 'RCurl', 'RJSONIO', 'reshape2', 'plyr', 'fields',
                      'maps', 'maptools', 'ggplot2', 'mvtnorm', 'devtools')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  print("installing : ")
  print(new.packages)
  install.packages(new.packages, repos="http://cran.rstudio.com/")
}

require(devtools)
install_github("neotoma", "ropensci")
EOF
```
