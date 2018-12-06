extract_palsar_metadata_function <-function(inpath){
  
######
##Author Brady S. Hardiman
## 04/24/2013

##Parameters I (Metadata) will be extracted from these files:
workreport <- read.delim(file.path(inpath,"workreport"), header=F, sep="=", fill=T)

##scene ID
scnid   <- as.character(workreport$V2[grep("Scs_SceneID",workreport$V1)])

##Parameters II (Metadata) will be extracted from these files:
txtfile <- read.delim(file.path(inpath, paste(scnid,".LED.txt",sep="")),header=F, sep="\t", fill=T)

##scene centroid date time (Format is YYYYMMDD HH:MM:SS.SSS)
scndate <- as.character(workreport$V2[grep("Img_SceneCenterDateTime",workreport$V1)])

##scene UTM zone
scnUTMzone <- as.numeric(as.character(workreport$V2[grep("Pds_UTM_ZoneNo",workreport$V1)]))

##scene Pixel spacing (m)
scnpix <- as.numeric(as.character(workreport$V2[grep("Pds_PixelSpacing",workreport$V1)]))

##scene calibration factor 
## Note: Level 1.5  s0  = 10*log10 <DN 2> + CF where DN is the pixel value of level 1.5 products 
##  and CF is calibration factor
scncf_raw <- as.matrix(txtfile$V1[grep("Calibration factor", txtfile$V1)],nrow=2)
library(stringr)
scncf <- as.numeric(str_extract_all(scncf_raw[1,1],"\\(?[0-9,.-]+\\)?")[[1]])

## Coefficients to convert a line & pixel to a projection reference  
## To convert a line (L) and pixel (P) position to the map projection frame of reference, say (E, N) where:
##   E = A11 + A12*L + A13*P + A14*L*P
##   N = A21 + A22*L + A23*P + A24*L*P
options(digits=8)
scn_pix2coord_a11 <- as.numeric(as.character(txtfile$V2[grep("a11", txtfile$V1)]))
scn_pix2coord_a12 <- as.numeric(as.character(txtfile$V2[grep("a12", txtfile$V1)]))
scn_pix2coord_a13 <- as.numeric(as.character(txtfile$V2[grep("a13", txtfile$V1)]))
scn_pix2coord_a14 <- as.numeric(as.character(txtfile$V2[grep("a14", txtfile$V1)]))
scn_pix2coord_a21 <- as.numeric(as.character(txtfile$V2[grep("a21", txtfile$V1)]))
scn_pix2coord_a22 <- as.numeric(as.character(txtfile$V2[grep("a22", txtfile$V1)]))
scn_pix2coord_a23 <- as.numeric(as.character(txtfile$V2[grep("a23", txtfile$V1)]))
scn_pix2coord_a24 <- as.numeric(as.character(txtfile$V2[grep("a24", txtfile$V1)]))

##Coefficients to convert a projection reference to a line & pixel
## To convert from the map projection (E, N) of the pixel at the upper left to line (L) and pixel (P) position in the image, say (L, P) where: corner and (E, N) show alongitude (deg.) and a latitude
##  L = B11 + B12*E + B13*N + B14*E*N 
##  P = B21 + B22*E + B23*N + B24*E*N
scn_coord2pix_b11 <- as.numeric(as.character(txtfile$V2[grep("b11", txtfile$V1)]))
scn_coord2pix_b12 <- as.numeric(as.character(txtfile$V2[grep("b12", txtfile$V1)]))
scn_coord2pix_b13 <- as.numeric(as.character(txtfile$V2[grep("b13", txtfile$V1)]))
scn_coord2pix_b14 <- as.numeric(as.character(txtfile$V2[grep("b14", txtfile$V1)]))
scn_coord2pix_b21 <- as.numeric(as.character(txtfile$V2[grep("b21", txtfile$V1)]))
scn_coord2pix_b22 <- as.numeric(as.character(txtfile$V2[grep("b22", txtfile$V1)]))
scn_coord2pix_b23 <- as.numeric(as.character(txtfile$V2[grep("b23", txtfile$V1)]))
scn_coord2pix_b24 <- as.numeric(as.character(txtfile$V2[grep("b24", txtfile$V1)]))

##More coordinates to convert dec degree LatLon to line and pixel
##P = a0＋a1*N＋a2*E＋a3*N*E＋a4*N^2＋a5*E^2＋a6*N^2*E＋a7*N*E^2＋a8*N^3＋a9*E^3
##L = b0＋b1*N＋b2*E＋b3*N*E＋b4*N^2＋b5*E^2＋b6*N^2*E＋b7*N*E^2＋b8*N^3＋b9*E^3

scn_coord2pix_a0 <- as.numeric(as.character(txtfile$V3[grep("Map to line/sample coefficients..a.0.", txtfile$V1)]))
scn_coord2pix_a1 <- as.numeric(as.character(txtfile$V3[grep("Map to line/sample coefficients..a.1.", txtfile$V1)]))
scn_coord2pix_a2 <- as.numeric(as.character(txtfile$V3[grep("Map to line/sample coefficients..a.2.", txtfile$V1)]))
scn_coord2pix_a3 <- as.numeric(as.character(txtfile$V3[grep("Map to line/sample coefficients..a.3.", txtfile$V1)]))
scn_coord2pix_a4 <- as.numeric(as.character(txtfile$V3[grep("Map to line/sample coefficients..a.4.", txtfile$V1)]))
scn_coord2pix_a5 <- as.numeric(as.character(txtfile$V3[grep("Map to line/sample coefficients..a.5.", txtfile$V1)]))
scn_coord2pix_a6 <- as.numeric(as.character(txtfile$V3[grep("Map to line/sample coefficients..a.6.", txtfile$V1)]))
scn_coord2pix_a7 <- as.numeric(as.character(txtfile$V3[grep("Map to line/sample coefficients..a.7.", txtfile$V1)]))
scn_coord2pix_a8 <- as.numeric(as.character(txtfile$V3[grep("Map to line/sample coefficients..a.8.", txtfile$V1)]))
scn_coord2pix_a9 <- as.numeric(as.character(txtfile$V3[grep("Map to line/sample coefficients..a.9.", txtfile$V1)]))
scn_coord2pix_b0 <- as.numeric(as.character(txtfile$V3[grep("Map to line/sample coefficients..b.0.", txtfile$V1)]))
scn_coord2pix_b1 <- as.numeric(as.character(txtfile$V3[grep("Map to line/sample coefficients..b.1.", txtfile$V1)]))
scn_coord2pix_b2 <- as.numeric(as.character(txtfile$V3[grep("Map to line/sample coefficients..b.2.", txtfile$V1)]))
scn_coord2pix_b3 <- as.numeric(as.character(txtfile$V3[grep("Map to line/sample coefficients..b.3.", txtfile$V1)]))
scn_coord2pix_b4 <- as.numeric(as.character(txtfile$V3[grep("Map to line/sample coefficients..b.4.", txtfile$V1)]))
scn_coord2pix_b5 <- as.numeric(as.character(txtfile$V3[grep("Map to line/sample coefficients..b.5.", txtfile$V1)]))
scn_coord2pix_b6 <- as.numeric(as.character(txtfile$V3[grep("Map to line/sample coefficients..b.6.", txtfile$V1)]))
scn_coord2pix_b7 <- as.numeric(as.character(txtfile$V3[grep("Map to line/sample coefficients..b.7.", txtfile$V1)]))
scn_coord2pix_b8 <- as.numeric(as.character(txtfile$V3[grep("Map to line/sample coefficients..b.8.", txtfile$V1)]))
scn_coord2pix_b9 <- as.numeric(as.character(txtfile$V3[grep("Map to line/sample coefficients..b.9.", txtfile$V1)]))
  
##Extract scene corner and center coordinates (UTM)
scn_nwlat  <- as.numeric(as.character(workreport$V2[grep("Img_ImageSceneLeftTopLatitude",workreport$V1)]))
scn_nwlon  <- as.numeric(as.character(workreport$V2[grep("Img_ImageSceneLeftTopLongitude",workreport$V1)]))
scn_nelat  <- as.numeric(as.character(workreport$V2[grep("Img_ImageSceneRightTopLatitude",workreport$V1)]))
scn_nelon  <- as.numeric(as.character(workreport$V2[grep("Img_ImageSceneRightTopLongitude",workreport$V1)]))
scn_swlat  <- as.numeric(as.character(workreport$V2[grep("Img_ImageSceneLeftBottomLatitude",workreport$V1)]))
scn_swlon  <- as.numeric(as.character(workreport$V2[grep("Img_ImageSceneLeftBottomLongitude",workreport$V1)]))
scn_selat  <- as.numeric(as.character(workreport$V2[grep("Img_ImageSceneRightBottomLatitude",workreport$V1)]))
scn_selon  <- as.numeric(as.character(workreport$V2[grep("Img_ImageSceneRightBottomLongitude",workreport$V1)]))
scn_centlat <-as.numeric(as.character(workreport$V2[grep("Img_ImageSceneCenterLatitude",workreport$V1)]))
scn_centlon <-as.numeric(as.character(workreport$V2[grep("Img_ImageSceneCenterLongitude",workreport$V1)]))


scn_vector <- c(scnid,
                scndate, 
                scnUTMzone, 
                scnpix,
                scncf,
                scn_pix2coord_a11,
                scn_pix2coord_a12,
                scn_pix2coord_a13,
                scn_pix2coord_a14,
                scn_pix2coord_a21,
                scn_pix2coord_a22,
                scn_pix2coord_a23,
                scn_pix2coord_a24,
                scn_coord2pix_b11,
                scn_coord2pix_b12,
                scn_coord2pix_b13,
                scn_coord2pix_b14,
                scn_coord2pix_b21,
                scn_coord2pix_b22,
                scn_coord2pix_b23,
                scn_coord2pix_b24,
                scn_coord2pix_a0,
                scn_coord2pix_a1,
                scn_coord2pix_a2,
                scn_coord2pix_a3,
                scn_coord2pix_a4,
                scn_coord2pix_a5,
                scn_coord2pix_a6,
                scn_coord2pix_a7,
                scn_coord2pix_a8,
                scn_coord2pix_a9,
                scn_coord2pix_b0,
                scn_coord2pix_b1,
                scn_coord2pix_b2,
                scn_coord2pix_b3,
                scn_coord2pix_b4,
                scn_coord2pix_b5,
                scn_coord2pix_b6,
                scn_coord2pix_b7,
                scn_coord2pix_b8,
                scn_coord2pix_b9,
                scn_nwlat,          
                scn_nwlon,
                scn_nelat,   
                scn_nelon,
                scn_swlat,
                scn_swlon,
                scn_selat,
                scn_selon,
                scn_centlat,
                scn_centlon)

return(scn_vector)
}