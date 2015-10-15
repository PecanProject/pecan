#
#--------------------------------------------------#
# This file contains subroutines for calculating   #
# solar radiation for date, hour and latitude      #
# Originally programmed by Timo Pukkala            #
# and based on paper by Oker-Blom, Pukkala and     #
# Kuuluvainen: The relationship between radiation  # 
# interception and photosynthesis in forest        #
# canopies: effect of stand structure and latitude #
# Ecol. Modelling 1989.                            #
#-------------------------------------------------#
#-------------------------------------------------#
# CALCULATE IRRADIANCE ABOVE CANOPY               #
#-------------------------------------------------#
# TIMO PUKKALA 3.7.1987
#
elev <-  function(GLAT,DAT,HOUR,HKI,A){
#
# GLAT,DAT,HOUR (LATIT,DATE,HOUR)     IN
# ALTI (ALTITUDE, DEGREES)	      OUT
# RADI (IRRADIANCE, MJ/M2/H)          OUT
#
	AL = ALTU(DAT,GLAT,HOUR)	
	if(AL < 0.01) {
	ALTI=0.
	RADI=0.} else{
	RADI=SI(AL,HKI,A)
	ALTI=AST(AL)}
	return(list(ALTI=ALTI,RADI=RADI))
}

#---------------------------------------------------#
# CALCULATES ZENITH ANGLE OF SUN                    #
#---------------------------------------------------#
# TIMO PUKKALA 24.3.1986              F77
#
#	SUBROUTINE VINTI(ALT)
#
# ALTI=ZENITH ANGLE, DEGREES
#
#100	FORMAT ('$',(A),' >')
#1	PRINT 100,'GIVE DATE (day number from start of year)'
#	READ (5,*,ERR=1,END=2) DAT
#	PRINT 100,'Give hour'
#	READ (5,*,ERR=1,END=2) H
#	PRINT 100,'Give latitude'
#	READ (5,*,ERR=1,END=2) PL
#	DE = DEK(DAT)
#	HA = HOUR(H)
#	AL = ALTI(DE,HA,PL)	
#	ALT=AST(AL)
#2	RETURN
#	END
#--------------------------------------------------------------#
# CALCULATES ZENITH ANGLE ON THE BASIS OF HOUR                 #
#--------------------------------------------------------------#
# TIMO PUKKALA 2.4.1986              F77
#
	ALTU <- function(DAT,POHJ,HOA){
#
# ALTU=ZENITH ANGLE, RADIANS
#
	DE = DEK(DAT)
	HA = HOUR(HOA)
	ALTU = ALTI(DE,HA,POHJ)	
	return(ALTU)
	}
#------------------------------------------
# DECLINATION OF SUN (RAD)
#--------------------------------------------
# TIMO PUKKALA
	DEK <- function(DAT){
# DAT = DAY NUMBER
	A <- 23.45; B <- .9848; C <- 80.
	AA = RAD(A)
	DEK = AA * sin(RAD(B*DAT-C))
	return(DEK)
	}
#-----------------------------------------------
# HOURLY ANGLE (RAD)
	HOUR <- function(H){
# H = HOUR
	B <- 12.; C <- 15.
	HOUR = RAD((H-B) * C)
	return(HOUR)
	}
#---------------------------------------------------
# ZENITH ANGLE OF SUN (RAD)
  	ALTI <- function(DE,HA,PL){
# DE = DECLINATION, RAD
# HA = HOURLY ANGLE, RAD
# PL = LATITUDE, DEGREES
	P=RAD(PL)
	ALTI = asin(sin(DE)*sin(P) + cos(DE)*cos(P)*cos(HA))
	return(ALTI)
	}
#-------------------------------------
# SOLAR IRRADIANCE MJ/m2/h
	SI <- function(AL,HKI,A){
# AL=ZENITH ANGLE, RAD
   	SI = HKI * exp(A/sin(AL)) * sin(AL)
	return(SI)
	}
#--------------------------------------------
# RADIANS TO DEGREES
	AST <- function(R){
	A <- 57.29578
	AST = A * R
	return(AST)
	}
#--------------------------------------------
# DEGREES TO RADIANS
	RAD <- function(A){
	B <- 0.0174533
	RAD = B * A
	return(RAD)
	}


##

