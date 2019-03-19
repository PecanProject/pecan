nneo
====



[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Build Status](https://travis-ci.org/ropenscilabs/nneo.svg?branch=master)](https://travis-ci.org/ropenscilabs/nneo)
[![codecov.io](https://codecov.io/github/ropenscilabs/nneo/coverage.svg?branch=master)](https://codecov.io/github/ropenscilabs/nneo?branch=master)
[![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/nneo)](https://github.com/metacran/cranlogs.app)


`nneo` - R client for [NEON API](http://data.neonscience.org/data-api)

Routes and R methods

* `/products` - `nneo_products()`/`nneo_product()`
* `/sites` - `nneo_sites()`/`nneo_site()`
* `/locations` - `nneo_locations()`/`nneo_location()`
* `/data` - `nneo_data()`/`nneo_file()`
* `nneo_wrangle()` - uses `nneo_site()`, `nneo_data()`, and 
`nneo_file()` internally


## installation

cran version


```r
install.packages("nneo")
```

Development version


```r
devtools::install_github("ropenscilabs/nneo")
```


```r
library("nneo")
```

## list products


```r
nneo_products()
#> # A tibble: 184 x 14
#>      keywords productStatus
#>  *     <list>         <chr>
#>  1  <chr [8]>        FUTURE
#>  2     <NULL>        FUTURE
#>  3 <chr [20]>        FUTURE
#>  4  <chr [8]>        ACTIVE
#>  5  <chr [8]>        FUTURE
#>  6 <chr [12]>        ACTIVE
#>  7     <NULL>        ACTIVE
#>  8  <chr [4]>        FUTURE
#>  9  <chr [6]>        FUTURE
#> 10  <chr [9]>        FUTURE
#> # ... with 174 more rows, and 12 more variables: productDescription <chr>,
#> #   productCode <chr>, productCategory <chr>, themes <list>,
#> #   productScienceTeam <chr>, productName <chr>,
#> #   productCodePresentation <chr>, productScienceTeamAbbr <chr>,
#> #   productCodeLong <chr>, specs <list>, productHasExpanded <lgl>,
#> #   siteCodes <list>
```

## list sites


```r
nneo_sites()
#> # A tibble: 81 x 11
#>                          siteDescription siteLongitude    siteType
#>  *                                 <chr>         <dbl>       <chr>
#>  1                          Jornada LTER    -106.84254 RELOCATABLE
#>  2                      Oksrukuyik Creek    -149.14302        CORE
#>  3                             Lewis Run     -77.98322 RELOCATABLE
#>  4        Wind River Experimental Forest    -121.95191        CORE
#>  5     Ordway-Swisher Biological Station     -81.99343        CORE
#>  6                LBJ National Grassland     -97.57000        CORE
#>  7                                 Healy    -149.21334 RELOCATABLE
#>  8 Niwot Ridge Mountain Research Station    -105.58237        CORE
#>  9                       Red Butte Creek    -111.79765        CORE
#> 10                             Hop Brook     -72.32963        CORE
#> # ... with 71 more rows, and 8 more variables: stateName <chr>,
#> #   stateCode <chr>, siteLatitude <dbl>, domainName <chr>,
#> #   domainCode <chr>, siteCode <chr>, dataProducts <list>, siteName <chr>
```

## list a location


```r
res <- nneo_location("HARV")
names(res)
#>  [1] "locationDescription"      "locationElevation"       
#>  [3] "locationUtmEasting"       "locationUtmHemisphere"   
#>  [5] "locationUtmZone"          "locationName"            
#>  [7] "siteCode"                 "locationParent"          
#>  [9] "locationChildren"         "locationChildrenUrls"    
#> [11] "locationType"             "locationProperties"      
#> [13] "locationDecimalLatitude"  "locationParentUrl"       
#> [15] "locationUtmNorthing"      "domainCode"              
#> [17] "locationDecimalLongitude"
```

## data


```r
nneo_data(product_code = "DP1.00098.001", site_code = "HEAL",
          year_month = "2016-05")
#> $data
#> $data$files
#> # A tibble: 5 x 4
#>        crc32                                                       name
#> *      <chr>                                                      <chr>
#> 1 0x78a83344 NEON.D19.HEAL.DP1.00098.001.00000.000.040.030.RH_30min.csv
#> 2 0x80296475          NEON.D19.HEAL.DP1.00098.001.20160501-20160521.xml
#> 3  0x560b9ba NEON.D19.HEAL.DP1.00098.001.00000.003.000.030.RH_30min.csv
#> 4 0xd6e86fd9  NEON.D19.HEAL.DP1.00098.001.00000.000.040.001.RH_1min.csv
#> 5 0xb2506dd5  NEON.D19.HEAL.DP1.00098.001.00000.003.000.001.RH_1min.csv
#> # ... with 2 more variables: url <chr>, size <chr>
#> 
#> $data$productCode
#> [1] "DP1.00098.001"
#> 
#> $data$month
#> [1] "2016-05"
#> 
#> $data$siteCode
#> [1] "HEAL"
```

## Contributors

* [Scott Chamberlain](https://github.com/sckott)
* [Josh Roberti](https://github.com/jaroberti)

## Meta

* Please [report any issues or bugs](https://github.com/ropenscilabs/nneo/issues).
* License: MIT
* Get citation information for `nneo` in R doing `citation(package = nneo')`
* Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md).
By participating in this project you agree to abide by its terms.
