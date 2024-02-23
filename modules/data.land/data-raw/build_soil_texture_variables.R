### Build soil texture table variables
### Code from ED2/R-utils/soilutils.r

#==========================================================================================#
#==========================================================================================#
#      This variable has the "edges" of all soil types.                                    #
#------------------------------------------------------------------------------------------#
stext.lines       = list()
stext.lines[[ 1]] = list(sand=c(0.900,0.850),clay=c(0.100,0.000))
stext.lines[[ 2]] = list(sand=c(0.850,0.700),clay=c(0.150,0.000))
stext.lines[[ 3]] = list(sand=c(0.800,0.525),clay=c(0.200,0.200))
stext.lines[[ 4]] = list(sand=c(0.520,0.525),clay=c(0.200,0.075))
stext.lines[[ 5]] = list(sand=c(0.425,0.525),clay=c(0.075,0.075))
stext.lines[[ 6]] = list(sand=c(0.225,0.500),clay=c(0.275,0.000))
stext.lines[[ 7]] = list(sand=c(0.200,0.075),clay=c(0.000,0.125))
stext.lines[[ 8]] = list(sand=c(0.075,0.000),clay=c(0.125,0.125))
stext.lines[[ 9]] = list(sand=c(0.525,0.450),clay=c(0.200,0.275))
stext.lines[[10]] = list(sand=c(0.450,0.000),clay=c(0.275,0.275))
stext.lines[[11]] = list(sand=c(0.200,0.200),clay=c(0.275,0.400))
stext.lines[[12]] = list(sand=c(0.650,0.450),clay=c(0.350,0.350))
stext.lines[[13]] = list(sand=c(0.450,0.450),clay=c(0.275,0.550))
stext.lines[[14]] = list(sand=c(0.450,0.000),clay=c(0.400,0.400))
stext.lines[[15]] = list(sand=c(0.200,0.000),clay=c(0.400,0.600))
stext.lines[[16]] = list(sand=c(0.300,0.000),clay=c(0.400,0.700))
stext.lines[[17]] = list(sand=c(0.300,0.300),clay=c(0.400,0.700))
stext.lines[[18]] = list(sand=c(0.300,0.000),clay=c(0.700,0.700))
nstext.lines      = length(stext.lines)
for(n in 1:nstext.lines){
  stext.lines[[n]]$silt = pmax(0,pmin(1,1.-stext.lines[[n]]$sand-stext.lines[[n]]$clay))
}#end for
#==========================================================================================#
#==========================================================================================#




#==========================================================================================#
#==========================================================================================#
#      This variable has the "polygons" for all soil types.                                #
#------------------------------------------------------------------------------------------#
stext.polygon       = list()
stext.polygon[[ 1]] = list(sand = c(1.000,0.900,0.850)
                           ,clay = c(0.000,0.100,0.000)
)#end list
stext.polygon[[ 2]] = list(sand = c(0.900,0.850,0.700,0.850)
                           ,clay = c(0.100,0.150,0.000,0.000)
)#end list
stext.polygon[[ 3]] = list(sand = c(0.850,0.800,0.525,0.525,0.425,0.500,0.700)
                           ,clay = c(0.150,0.200,0.200,0.075,0.075,0.000,0.000)
)#end list
stext.polygon[[ 4]] = list(sand = c(0.500,0.225,0.000,0.000,0.075,0.200)
                           ,clay = c(0.000,0.275,0.275,0.125,0.125,0.000)
)#end list
stext.polygon[[ 5]] = list(sand = c(0.525,0.450,0.225,0.425,0.525)
                           ,clay = c(0.200,0.275,0.275,0.075,0.075)
)#end list
stext.polygon[[ 6]] = list(sand = c(0.800,0.650,0.450,0.450,0.525)
                           ,clay = c(0.200,0.350,0.350,0.275,0.200)
)#end list
stext.polygon[[ 7]] = list(sand = c(0.200,0.000,0.000,0.200)
                           ,clay = c(0.400,0.400,0.275,0.275)
)#end list
stext.polygon[[ 8]] = list(sand = c(0.450,0.200,0.200,0.450)
                           ,clay = c(0.400,0.400,0.275,0.275)
)#end list
stext.polygon[[ 9]] = list(sand = c(0.650,0.450,0.450)
                           ,clay = c(0.350,0.550,0.350)
)#end list
stext.polygon[[10]] = list(sand = c(0.200,0.000,0.000)
                           ,clay = c(0.400,0.600,0.400)
)#end list
stext.polygon[[11]] = list(sand = c(0.300,0.300,0.000)
                           ,clay = c(0.400,0.700,0.700)
)#end list
stext.polygon[[12]] = list(sand = c(NA,NA)
                           ,clay = c(NA,NA)
)#end list
stext.polygon[[13]] = list(sand = c(NA,NA)
                           ,clay = c(NA,NA)
)#end list
stext.polygon[[14]] = list(sand = c(0.200,0.075,0.000,0.000)
                           ,clay = c(0.000,0.125,0.125,0.000)
)#end list
stext.polygon[[15]] = list(sand = c(0.300,0.000,0.000)
                           ,clay = c(0.700,1.000,0.700)
)#end list
stext.polygon[[16]] = list(sand = c(0.450,0.300,0.300,0.450)
                           ,clay = c(0.550,0.700,0.400,0.400)
)#end list
stext.polygon[[17]] = list(sand = c(0.300,0.000,0.000,0.200)
                           ,clay = c(0.400,0.700,0.600,0.400)
)#end list
nstext.polygon      = length(stext.polygon)

for(n in 1:nstext.polygon){
  sand.now = stext.polygon[[n]]$sand
  clay.now = stext.polygon[[n]]$clay
  stext.polygon[[n]]$silt = pmax(0,pmin(1,1.-sand.now-clay.now))
}#end for
#==========================================================================================#
#==========================================================================================#

#----- Define some prescribed fractions. -----------------------------------------------#
xsand.def = c( 0.920, 0.825, 0.660, 0.200, 0.410, 0.590
               , 0.100, 0.320, 0.520, 0.060, 0.200, 0.200
               , 0.333, 0.075, 0.100, 0.375, 0.125)
xclay.def = c( 0.030, 0.060, 0.110, 0.160, 0.170, 0.270
               , 0.340, 0.340, 0.420, 0.470, 0.600, 0.200
               , 0.333, 0.050, 0.800, 0.525, 0.525)

soil.name = c("Sand","Loamy sand","Sandy loam","Silt loam","Loam","Sandy clay loam"
              ,"Silty clay loam","Clayey loam","Sandy clay","Silty clay","Clay"
              ,"Peat","Bedrock","Silt","Heavy clay","Clayey sand","Clayey silt")

texture <- read.csv("texture.csv",header=TRUE,stringsAsFactors = FALSE)



soil.key  = c("Sa","LSa","SaL","SiL","L","SaCL","SiCL","CL","SaC","SiC","C","P","BR"
              ,"Si","CC","CSa","CSi")
#----- Define some constants. ----------------------------------------------------------#
fieldcp.K  = PEcAn.utils::ud_convert(0.1,"mm/day","meters/second")  
                  # hydraulic conduct. at field capacity                       [ mm/day]
soilcp.MPa = 3.1  # soil-water potential for air dry soil                      [    MPa]
soilwp.MPa = 1.5  # soil-water potential at wilting point                      [    MPa]
soilld.MPa = 0.75 # soil-water potential that plants start dropping leaves     [    MPa]
theta.crit = 0.11 # fractional soil moisture that plants start dropping leaves [  m3/m3]
grav      <- 9.80665         # Gravity acceleration                            [     m/s2]
#---------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------#
# Soil heat capacity.  Didn't find silt values, using average between sand and clay     #
#---------------------------------------------------------------------------------------#
sand.hcap = 2.128e6
clay.hcap = 2.385e6
silt.hcap = .5 * (sand.hcap + clay.hcap)
air.hcap  = 1212
#---------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------#
# Soil heat capacity.  Didn't find silt values, using average between sand and clay     #
#---------------------------------------------------------------------------------------#
sand.cond = 8.80
clay.cond = 2.92
silt.cond = .5 * (sand.cond + clay.cond)
air.cond  = 0.025
h2o.cond  = 0.57

ksand <- 3. * h2o.cond / ( 2. * h2o.cond + sand.cond )
ksilt <- 3. * h2o.cond / ( 2. * h2o.cond + silt.cond )
kclay <- 3. * h2o.cond / ( 2. * h2o.cond + clay.cond )
kair  <- 3. * h2o.cond / ( 2. * h2o.cond +  air.cond )

# TODO may be more useful to collect related variables into sublists...
soil_class <- list(
  air.cond = air.cond,
  air.hcap = air.hcap,
  clay.cond = clay.cond,
  clay.hcap = clay.hcap,
  fieldcp.K = fieldcp.K,
  grav = grav,
  h2o.cond = h2o.cond,
  kair = kair,
  kclay = kclay,
  ksand = ksand,
  ksilt = ksilt,
  sand.cond = sand.cond,
  sand.hcap = sand.hcap,
  silt.cond = silt.cond,
  silt.hcap = silt.hcap,
  soil.key = soil.key,
  soil.name = soil.name,
  soilcp.MPa = soilcp.MPa,
  soilld.MPa = soilld.MPa,
  soilwp.MPa = soilwp.MPa,
  stext.lines = stext.lines,
  stext.polygon = stext.polygon,
  texture = texture,
  theta.crit = theta.crit,
  xclay.def = xclay.def,
  xsand.def = xsand.def)

save(soil_class, file = "../data/soil_class.rda")
