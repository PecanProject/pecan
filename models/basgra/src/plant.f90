module plant

use parameters_site
use parameters_plant
use environment
implicit none

integer :: NOHARV
real :: CRESMX,DAYLGE,FRACTV,GLVSI,GSTSI,LERG,LERV,LUEMXQ,NELLVG,PHENRF,PHOT
real :: RDRFROST,RDRS,RDRT,RDRTOX,RESPGRT,RESPGSH,RESPHARD,RESPHARDSI,RESNOR,RLEAF,RplantAer,SLANEW
real :: RATEH,reHardPeriod,TV2TIL
real :: fNCgrowth,fNgrowth,FSPOT,KN,KNMAX,NSHNOR,RGRTV
real :: NSHK
real :: GLAISI, SINK1T
real :: NSOURCE, NSINK

Contains

Subroutine Harvest(CLV,CRES,CST,year,doy,DAYS_HARVEST,HARVEST_PARAMS, LAI,PHEN,TILG1,TILG2,TILV, &
     GSTUB,HARVLA,HARVLV,HARVLVP,HARVPH,HARVRE,HARVREP,HARVST,HARVSTP,HARVTILG2, if_cut_only)
  integer, intent(in) :: doy,year
  integer, intent(in), dimension(300,2) :: DAYS_HARVEST
  real, intent(in), dimension(300, 2) :: HARVEST_PARAMS
  real, intent(in)    :: CLV, CRES, CST, LAI, PHEN, TILG1, TILG2, TILV
  real, intent(out)    :: GSTUB, HARVLV, HARVLVP, HARVLA, HARVRE, HARVREP, HARVTILG2, HARVST, HARVSTP, HARVPH
  logical, intent(out) :: if_cut_only ! if the cut grass is left on the field and not to be added to the yield pool

  real    :: CLAIV, CLAI, HARVFR, TV1
  integer :: HARV, HARVP, i
 
  HARVP  = 1 
  HARV   = 0
  NOHARV = 1
  CLAIV  = 0.5  
  do i=1,300    
    if ( (year==DAYS_HARVEST(i,1)) .and. (doy==DAYS_HARVEST(i,2)) ) then
      HARV   = 1
      NOHARV = 0	
      CLAIV  = HARVEST_PARAMS(i,1)
      if_cut_only = HARVEST_PARAMS(i,2) > 0.0
      exit
    end if
  end do
  FRACTV = (TILV+TILG1) / (TILV+TILG1+TILG2)
  CLAI   = FRACTV * CLAIV
  if (LAI <= CLAI) then
    HARVFR = 0.0
  else
    HARVFR = 1.0 - CLAI/LAI
  end if
  HARVLA    = (HARV   * LAI * HARVFR) / DELT
  HARVLV    = (HARV   * CLV * HARVFR) / DELT
  HARVLVP   = (HARVP  * CLV * HARVFR) / DELT
  HARVPH    = (HARV   * PHEN        ) / DELT
  TV1       = (HARVFR * FRACTV) + (1-FRACTV)*HAGERE
  HARVRE    = (HARV   * TV1 * CRES  ) / DELT
  HARVREP   = (HARVP  * TV1 * CRES  ) / DELT
  HARVST    = (HARV   * CST         ) / DELT
  HARVSTP   = (HARVP  * CST         ) / DELT
  GSTUB     =  HARVST * (1-HAGERE)
  HARVTILG2 = (HARV   * TILG2       ) / DELT
end Subroutine Harvest

Subroutine Biomass(CLV,CRES,CST)
  real :: CLV, CRES, CST
  CRESMX = COCRESMX*(CLV + CRES + CST)
  RESNOR = max(0.,min(1., CRES/CRESMX ))
end Subroutine Biomass

Subroutine Phenology(DAYL,PHEN, DPHEN,GPHEN)
  real :: DAYL,PHEN
  real :: DPHEN,GPHEN
  GPHEN = max(0., (DAVTMP-0.01)*0.000144*24. * (min(DAYLP,DAYL)-0.24) )
  DPHEN = 0.
  if (DAYL < DAYLB) DPHEN = PHEN / DELT
  PHENRF = (1 - PHEN)/(1 - PHENCR)
  if (PHENRF > 1.0) PHENRF = 1.0
  if (PHENRF < 0.0) PHENRF = 0.0
  DAYLGE = max(0.,min(1., (DAYL - DAYLB)/(DLMXGE - DAYLB) ))
end Subroutine Phenology

Subroutine Foliage1
  real :: EFFTMP, SLAMIN
  EFFTMP = max(TBASE, DAVTMP)
  LERV   =          max(0., (-0.76 + 0.52*EFFTMP)/1000. )
  LERG   = DAYLGE * max(0., (-5.46 + 2.80*EFFTMP)/1000. )
  SLAMIN = SLAMAX * FSLAMIN
  SLANEW = SLAMAX - RESNOR*(SLAMAX-SLAMIN)
end Subroutine Foliage1

Subroutine LUECO2TM(PARAV)
!=============================================================================
! Calculate LUEMXQ (mol CO2 mol-1 PAR quanta)
! Inputs : PARAV (micromol PAR quanta m-2 s-)
!=============================================================================
  real :: PARAV
  real :: CO2I, EA, EAKMC, EAKMO, EAVCMX, EFF, GAMMAX, KC25, KMC, KMC25
  real :: KMO, KMO25, KOKC, O2, PMAX, R, RUBISCN, T, TMPFAC, VCMAX
  T      = DAVTMP                                            !(degC)
  RUBISCN = RUBISC * (1.E6/550000.)
  EAVCMX =  68000                                            !(J mol-1)
  EAKMC  =  65800                                            !(J mol-1)
  EAKMO  =   1400                                            !(J mol-1)
  KC25   =     20                                            !(mol CO2 mol-1 Rubisco s-1)
  KMC25  =    460                                            !(ppm CO2)
  KMO25  =     33                                            !(% O2)
  KOKC   =      0.21                                         !(-)
  O2     =     21                                            !(% O2)
  R      =      8.314                                        !(J K-1 mol-1)
  CO2I   = 0.7 * CO2A                                        !(ppm CO2)
  VCMAX  = RUBISCN * KC25 * exp((1/298.-1/(T+273))*EAVCMX/R) !(micromol CO2 m-2 s-1)
  KMC    =          KMC25 * exp((1/298.-1/(T+273))*EAKMC /R) !(ppm CO2)
  KMO    =          KMO25 * exp((1/298.-1/(T+273))*EAKMO /R) !(% O2)
  GAMMAX = 0.5 * KOKC * KMC * O2 / KMO                       !(ppm CO2)
  PMAX   = VCMAX * (CO2I-GAMMAX) / (CO2I + KMC * (1+O2/KMO)) !(micromol CO2 m-2 s-1)
  TMPFAC = max( 0., min( 1., (T+4.)/5. ) )                   !(-)
  EFF    = TMPFAC * (1/2.1) * (CO2I-GAMMAX) / (4.5*CO2I+10.5*GAMMAX) !(mol CO2 mol-1 PAR quanta)
  LUEMXQ = EFF*PMAX*(1+KLUETILG*(1-FRACTV)) / (EFF*K*PARAV + PMAX)   !(mol CO2 mol-1 PAR quanta)  
end Subroutine LUECO2TM
  
Subroutine HardeningSink(CLV,DAYL,doy,LT50,Tsurf)
  integer :: doy
  real :: CLV,DAYL,LT50,Tsurf
  real :: doySinceStart, reHardRedStart
  if( LAT > 0 ) then
    reHardRedStart = modulo( reHardRedEnd       - reHardRedDay, 365. )
  else
    reHardRedStart = modulo( reHardRedEnd + 183 - reHardRedDay, 365. )
  end if  
  doySinceStart  = modulo( doy-reHardRedStart       , 365. )
  if ( doySinceStart < (reHardRedDay+0.5*(365.-reHardRedDay)) ) then
    reHardPeriod = max( 0., 1.-doySinceStart/reHardRedDay )
  else
    reHardPeriod = 1.
  end if
  if ( (Tsurf>THARDMX) .or. (LT50<LT50MN) ) then
    RATEH = 0.
  else
    RATEH = reHardPeriod * Hparam * (THARDMX-Tsurf) * (LT50-LT50MN)
  end if
  RESPHARDSI = RATEH * CLV * KRESPHARD * max(0.,min(1., RESNOR*5. ))
end Subroutine HardeningSink

Subroutine Growth(LAI,NSH,NMIN,CLV,CRES,CST,PARINT,TILG1,TILG2,TILV, TRANRF, &
     RESMOB, NSINK, NSHmob, nupt_max, ALLOTOT, GRESSI)
  real, intent(in) :: LAI, NSH, NMIN, CLV, CRES, CST, PARINT, TILG1, TILG2
  real, intent(in) :: TILV, TRANRF
  real, intent(out) :: RESMOB
  real, intent(out) :: NSINK
  real, intent(out) :: NSHmob
  real, intent(out) :: nupt_max
  real, intent(out) :: ALLOTOT
  real, intent(out) :: GRESSI
  
  real :: CSTAV,SOURCE
  real :: NSHEXCESS
  
  PHOT     = PARINT * TRANRF * 12. * LUEMXQ * NOHARV
  RESMOB   = (CRES * NOHARV / TCRES) * max(0.,min( 1.,DAVTMP/5. ))
  SOURCE   = RESMOB + PHOT
  RESPHARD = min(SOURCE,RESPHARDSI)
  ALLOTOT  = SOURCE - RESPHARD
  GRESSI   = 0.5 * (RESMOB + max(0.,(CRESMX-CRES)/DELT))
  if (TILG2 /= 0.0) then 
    CSTAV  = CST/TILG2 
  else 
    CSTAV  = 0.
  end if
  SINK1T   = max(0., 1 - (CSTAV/CSTAVM)) * SIMAX1T
  NELLVG   = PHENRF * NELLVM 
  GLAISI   = ((LERV*(TILV+TILG1)*NELLVM*LFWIDV)  + &
              (LERG*      TILG2 *NELLVG*LFWIDG)) * SHAPE * TRANRF
  GLVSI    = max(0., (GLAISI * NOHARV / SLANEW) / YG )
!  GLVSI    = (GLAISI * NOHARV / SLANEW) / YG
  GSTSI    = max(0., (SINK1T * TILG2 * TRANRF * NOHARV) / YG )
!  GSTSI    = (SINK1T * TILG2 * TRANRF * NOHARV) / YG
  
  NSHK     = (CLV+CST)*NCSHMAX * (1.-exp(-K*LAI))/(K*LAI)
  NSHEXCESS = max( 0., NSH-NSHK )
  NSHmob   = NOHARV * NSHEXCESS / TCNSHMOB
  NSINK    = max(0., (GLVSI+GSTSI)*NCSHMAX )
  NUPT_MAX = min(NSINK-NSHmob, NMIN / TCNUPT)
  
end Subroutine Growth

Subroutine Allocation(use_nitrogen, ALLOTOT, GRESSI, NSOURCE, NSINK, GRES,GRT,GLV,GST)
  logical, intent(in) :: use_nitrogen
  real, intent(in) :: ALLOTOT
  real, intent(in) :: GRESSI
  real, intent(in) :: NSOURCE ! N available for growth from reserves and soil
  real, intent(in) :: NSINK ! N required for maximum (N-unlimited) growth
  real, intent(out) :: GRES, GRT, GLV, GST
  
  real :: GSHSI, ALLOSH, ALLORT, ALLOLV, ALLOST

  if (use_nitrogen) then
     fNgrowth = min( 1., NSOURCE / NSINK )
  else
     fNgrowth = 1.0
  end if
  GLAISI   = GLAISI * fNgrowth
  GLVSI    = GLVSI  * fNgrowth
  GSTSI    = GSTSI  * fNgrowth

  GSHSI = GLVSI + GSTSI
  if (DAYLGE >= 0.1) then
     ! Situation 1: Growth has priority over storage (spring and growth period)
     ! Calculate amount of assimilates allocated to shoot
     ALLOSH = min( ALLOTOT, GSHSI )
     ! Calculate amount of assimilates allocated to reserves    
     GRES   = min( ALLOTOT - ALLOSH, GRESSI)
  else
     ! Calculate amount of assimilates allocated to reserves
     GRES   = min( ALLOTOT, GRESSI )
     ! Calculate amount of assimilates allocated to shoot
     ALLOSH = min( ALLOTOT - GRES, GSHSI )
  end if
  ! All surplus carbohydrate goes to roots
  ALLORT  = ALLOTOT - ALLOSH - GRES
  if (GSHSI == 0.) GSHSI = 1
  ALLOLV  = GLVSI * (ALLOSH / GSHSI)
  ALLOST  = GSTSI * (ALLOSH / GSHSI)
  GLV     = ALLOLV * YG
  GST     = ALLOST * YG
  GRT     = ALLORT * YG
  RESPGSH = (ALLOLV + ALLOST) * (1-YG)
  RESPGRT =  ALLORT           * (1-YG)
end Subroutine Allocation
    
Subroutine PlantRespiration(FO2,RESPHARD)
  real :: FO2,RESPHARD
  real :: fAer
  fAer      = max(0.,min(1., FO2/FO2MX ))
  RplantAer = fAer * ( RESPGRT + RESPGSH + RESPHARD )
end Subroutine PlantRespiration

Subroutine Senescence(CLV,CRT,CSTUB,LAI,LT50,PERMgas,TANAER,TILV,Tsurf, &
                      DeHardRate,DLAI,DLV,DRT,DSTUB,dTANAER,DTILV,HardRate)
  integer :: doy
  real :: CLV,CRT,CSTUB,DAYL,LAI,LT50,PERMgas,TANAER,TILV,Tsurf
  real :: DeHardRate,DLAI,DLV,DRT,DSTUB,dTANAER,DTILV,HardRate
  real :: TV1, TV2
  call AnaerobicDamage(LT50,PERMgas,TANAER, dTANAER)
  call Hardening(CLV,LT50,Tsurf, DeHardRate,HardRate)
  if (LAI < LAICR) then
    TV1 = 0.0 
  else 
    TV1 = RDRSCO*(LAI-LAICR)/LAICR
  end if
  RDRS   = min(TV1, RDRSMX)
  RDRT   = max(RDRTMIN, RDRTEM * Tsurf)
  TV2    = NOHARV * max(RDRS,RDRT,RDRFROST,RDRTOX)
  TV2TIL = NOHARV * max(RDRS,     RDRFROST,RDRTOX)
  DLAI   = LAI    * TV2
  DLV    = CLV    * TV2
  DSTUB  = CSTUB  * RDRSTUB
  DTILV  = TILV   * TV2TIL
  DRT    = CRT    * RDRROOT

end Subroutine Senescence

Subroutine AnaerobicDamage(LT50,PERMgas,TANAER, dTANAER)
  real :: LT50,PERMgas,TANAER
  real :: dTANAER,LD50
  if (PERMgas==0.) then
     dTANAER = 1.
  else
     dTANAER = -TANAER / DELT
  end if
  LD50 = LDT50A + LDT50B * LT50
  if (TANAER > 0.) then
     RDRTOX = KRDRANAER / (1.+exp(-KRDRANAER*(TANAER-LD50)))
  else
     RDRTOX = 0.
  end if
end Subroutine AnaerobicDamage

Subroutine Hardening(CLV,LT50,Tsurf, DeHardRate,HardRate)
  real :: CLV,LT50,Tsurf
  real :: DeHardRate,HardRate
  real :: RATED,RSR3H,RSRDAY
  RSR3H      = 1. / (1.+exp(-KRSR3H*(Tsurf-LT50)))
  ! RDRFROST should be less than 1 to avoid numerical problems
  ! (loss of all biomass but keeping positive reserves). We cap it at 0.5.
  RSRDAY     = RSR3H ! In previous versions we had RSRDAY = RSR3H^8 which understimated survival
  RDRFROST   = min( 0.5, 1. - RSRDAY )
  RATED      = min( Dparam*(LT50MX-LT50)*(Tsurf+TsurfDiff), (LT50MX-LT50)/DELT )
  DeHardRate = max(0.,min( RATEDMX, RATED ))
  !     HardRate   = RESPHARD / (CLV * KRESPHARD)
  if (CLV > 0.) then
     HardRate   = RESPHARD / (CLV * KRESPHARD)
  else
     HardRate   = 0.
  end if
end Subroutine Hardening

Subroutine Foliage2(DAYL,GLV,LAI,TILV,TILG1,TRANRF,Tsurf,VERN, GLAI,GTILV,TILVG1,TILG1G2)
  real    :: DAYL,GLV,LAI,TILV,TILG1,TRANRF,Tsurf
  integer :: VERN
  real    :: GLAI,GTILV,TILVG1,TILG1G2
  real    :: RGRTVG1,TGE,TV1
  GLAI    = SLANEW * GLV
  if (Tsurf < TBASE) then 
    TV1   = 0. 
  else 
    TV1   = Tsurf/PHY
  end if
  RLEAF   = TV1 * NOHARV * TRANRF * DAYLGE * ( FRACTV + PHENRF*(1-FRACTV) ) * fNgrowth
  FSPOT = LAITIL - LAIEFT*LAI
    if (FSPOT > FSMAX) FSPOT = FSMAX
    if (FSPOT < 0.)    FSPOT = 0.
  RGRTV   = max( 0., FSPOT * RESNOR * RLEAF )
  GTILV   = TILV  * RGRTV
  TGE     = max( 0., 1 - (abs(DAVTMP - TOPTGE))/(TOPTGE-TBASE))
  RGRTVG1 = min( 1.-TV2TIL, NOHARV * DAYLGE * TGE * RGENMX ) * VERN
  TILVG1  = TILV  * RGRTVG1
  if (DAYL > DAYLG1G2) then
    TILG1G2 = TILG1 * RGRTG1G2
  else
    TILG1G2 = 0.
  end if
end Subroutine Foliage2

Subroutine Nplant(NSHmob,CLV,CRT,CST,DLAI,DLV,DRT,GLAI,GLV,GRT,GST,HARVLA,HARVLV,HARVST, &
                  LAI,NRT,NSH, &
                  DNRT,DNSH,GNRT,GNSH,HARVNSH,NCDSH,NCGSH,NCHARVSH,NSHmobsoil,Nupt)
  real :: NSHmob,CLV,CRT,CST,DLAI,DLV,DRT,GLAI,GLV,GRT,GST,HARVLA,HARVLV,HARVST
  real :: LAI,NRT,NSH
  real :: DNRT,DNSH,GNRT,GNSH,HARVNSH,NCDSH,NCGSH,NCHARVSH,NSHmobsoil,Nupt
  real :: GNmob
  NSHNOR = NSH / ((CLV+CST)*NCSHMAX)
  if(NSHNOR*LAI > 0) then
    KNMAX  = 1./(NSHNOR*LAI)
  else
    KNMAX  = 1.E10
  end if
  if (NSHNOR < (5./8.)) then
    KN = KNMAX
  else
    KN = ( 1.5 - 3. * sqrt(0.25 - (1.-NSHNOR)*2./3.) ) / LAI
  end if
  KN = max( 0., min(KNMAX,KN) )
  fNCgrowth = FNCGSHMIN + (1-FNCGSHMIN) * fNgrowth
  if (GLAI > 0) then
    if (KN > 0) then
      GNSH = fNCgrowth * NCSHMAX * (GLV+GST) * (1-exp(-KN*GLAI)) / (KN*GLAI)
	else
	  GNSH = fNCgrowth * NCSHMAX * (GLV+GST)
    end if      
	NCGSH = GNSH / (GLV+GST)
  else
    GNSH  = 0
	NCGSH = 0
  end if
  if (DLAI > 0) then
    if (KN > 0) then
      DNSH = NSH * (CLV/(CLV+CST)) * (1 - (1-exp(-KN*(LAI-DLAI))) / (1-exp(-KN*LAI)) )
	else
	  DNSH = NSH * (CLV/(CLV+CST)) * DLAI/LAI
    end if      
	DNSH  = max( DNSH, NSH-NCSHMAX*((CLV+CST)-DLV) )
	NCDSH = DNSH / DLV
  else
	DNSH  = 0
	NCDSH = 0
  end if
  if (HARVLA > 0) then
    if (KN > 0) then
      HARVNSH = NSH * (1-exp(-KN*HARVLA)) / (1-exp(-KN*LAI))
	else
	  HARVNSH = NSH * HARVLA/LAI
    end if      
	HARVNSH  = max( HARVNSH, NSH-NCSHMAX*((CLV+CST)-(HARVLV+HARVST)) )
	NCHARVSH = HARVNSH / (HARVLV+HARVST)
  else
    HARVNSH  = 0  
	NCHARVSH = 0
  end if
!  GNRT = NCR * GRT
  GNRT = NCR * GRT * fNgrowth
!  GNRT = NCR * GRT * NSHNOR
!  DNRT = NCR * DRT
  DNRT = DRT * NRT/CRT
  GNmob      = min( NSHmob, GNSH+GNRT )
  NSHmobsoil = NSHmob - GNmob
  Nupt       = GNSH + GNRT - GNmob
end Subroutine Nplant

Subroutine Digestibility(DM,DMLV,DMRES,DMSH,DMST,DMSTUB,PHEN, &
                         F_WALL_DM,F_WALL_DMSH,F_WALL_LV,F_WALL_ST, &
                         F_DIGEST_DM,F_DIGEST_DMSH,F_DIGEST_LV,F_DIGEST_ST,F_DIGEST_WALL)
  real :: PHEN,DMLV,DMST,DMSTUB,DM,DMSH,DMRES
  real :: F_WALL_DM,F_WALL_DMSH,F_WALL_LV,F_WALL_ST
  real :: F_DIGEST_DM,F_DIGEST_DMSH,F_DIGEST_LV,F_DIGEST_ST,F_DIGEST_WALL
  real :: F_DIGEST_WALL_MIN,F_WALL_LV_MIN,F_WALL_ST_MIN
  F_WALL_LV_MIN     = F_WALL_LV_FMIN * F_WALL_LV_MAX
  F_WALL_ST_MIN     = F_WALL_ST_FMIN * F_WALL_ST_MAX
  F_WALL_LV         = F_WALL_LV_MIN + PHEN * ( F_WALL_LV_MAX - F_WALL_LV_MIN )
  F_WALL_ST         = F_WALL_ST_MIN + PHEN * ( F_WALL_ST_MAX - F_WALL_ST_MIN )  
  F_WALL_DM         = ( F_WALL_LV*DMLV + F_WALL_ST*DMST + DMSTUB ) / DM
  F_WALL_DMSH       = ( F_WALL_LV*DMLV + F_WALL_ST*DMST          ) / DMSH
  F_DIGEST_WALL_MIN = F_DIGEST_WALL_FMIN * F_DIGEST_WALL_MAX
  F_DIGEST_WALL     = F_DIGEST_WALL_MAX - PHEN * ( F_DIGEST_WALL_MAX - F_DIGEST_WALL_MIN )
  F_DIGEST_LV       = 1 - F_WALL_LV + F_DIGEST_WALL * F_WALL_LV
  F_DIGEST_ST       = 1 - F_WALL_ST + F_DIGEST_WALL * F_WALL_ST
  F_DIGEST_DMSH     = ( F_DIGEST_LV * DMLV + F_DIGEST_ST * DMST + DMRES                          ) / DMSH
  F_DIGEST_DM       = ( F_DIGEST_LV * DMLV + F_DIGEST_ST * DMST + DMRES + F_DIGEST_WALL * DMSTUB ) / DM
end Subroutine Digestibility

end module plant
