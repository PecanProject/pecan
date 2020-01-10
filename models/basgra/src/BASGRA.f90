subroutine BASGRA( PARAMS, MATRIX_WEATHER, &
                   CALENDAR_FERT, CALENDAR_NDEP, DAYS_HARVEST, &
				   NDAYS, NOUT, &
				   y)
!-------------------------------------------------------------------------------
! This is the BASic GRAss model originally written in MATLAB/Simulink by Marcel
! van Oijen, Mats Hoglind, Stig Morten Thorsen and Ad Schapendonk.
! 2011-07-13: Translation to FORTRAN by David Cameron and Marcel van Oijen.
! 2014-03-17: Extra category of tillers added
! 2014-04-03: Vernalization added
! 2014-04-03: Lower limit of temperature-driven leaf senescence no longer zero
! 2015-03-26: Introducing N, following example of BASFOR for the soil part.
!             Plant N is restricted to LV, ST, RT, not present in STUB and RES.
! 2015-09-24: More N-processes added
! 2016-12-09: Digestibility and fibre content added
!-------------------------------------------------------------------------------

use parameters_site
use parameters_plant
use environment
use resources
use soil
use plant
implicit none

integer, dimension(100,2) :: DAYS_HARVEST
real                      :: PARAMS(120)
#ifdef weathergen  
  integer, parameter      :: NWEATHER =  7
#else
  integer, parameter      :: NWEATHER =  8
#endif
real                      :: MATRIX_WEATHER(NMAXDAYS,NWEATHER)
real   , dimension(100,3) :: CALENDAR_FERT, CALENDAR_NDEP
integer, dimension(100,2) :: DAYS_FERT    , DAYS_NDEP
real   , dimension(100)   :: NFERTV       , NDEPV

integer                   :: day, doy, i, NDAYS, NOUT, year
real                      :: y(NDAYS,NOUT)

! State variables plants
real    :: CLV, CLVD, CRES, CRT, CST, CSTUB, LAI, LT50, PHEN
real    :: ROOTD, TILG1, TILG2, TILV
integer :: VERN
real    :: YIELD, YIELD_LAST, YIELD_TOT
real    :: NRT, NSH

! Output variables constructed from plant state variables
real    :: DM, DMLV, DMRES, DMSH, DMST, DMSTUB, DM_MAX, TILTOT
real    :: NSH_DMSH
real    :: ENERGY_DM, F_ASH, F_PROTEIN, PROTEIN

! State variables soil
real    :: CLITT, CSOMF, CSOMS, DRYSTOR, Fdepth
real    :: NLITT, NSOMF, NSOMS, NMIN, O2, Sdepth
real    :: TANAER, WAL, WAPL, WAPS, WAS, WETSTOR
real    :: Nfert_TOT

! Intermediate and rate variables
real :: DeHardRate, DLAI, DLV, DPHEN, DRT, DSTUB, dTANAER, DTILV, EVAP, EXPLOR
real :: Frate, FREEZEL, FREEZEPL, GLAI, GLV, GPHEN, GRES, GRT, GST, GSTUB, GTILV, HardRate
real :: HARVLA, HARVLV, HARVPH, HARVRE, HARVST, HARVTILG2, INFIL, IRRIG, O2IN
real :: O2OUT, PackMelt, poolDrain, poolInfil, Psnow, reFreeze, RESMOB
real :: RGRTVG1, RROOTD, SnowMelt, THAWPS, THAWS, TILVG1, TILG1G2, TRAN, Wremain
real :: NCSHI, NCGSH, NCDSH, NCHARVSH, GNSH, DNSH, HARVNSH, GNRT, DNRT
real :: NSHmob, NSHmobsoil, Nupt

real :: Ndep, Nfert

real :: F_DIGEST_DM, F_DIGEST_DMSH, F_DIGEST_LV, F_DIGEST_ST, F_DIGEST_WALL
real :: F_WALL_DM  , F_WALL_DMSH  , F_WALL_LV  , F_WALL_ST

! Parameters
call set_params(PARAMS)

! Calendar & weather
YEARI  = MATRIX_WEATHER(:,1)
DOYI   = MATRIX_WEATHER(:,2)
GRI    = MATRIX_WEATHER(:,3)
TMMNI  = MATRIX_WEATHER(:,4)
TMMXI  = MATRIX_WEATHER(:,5)
#ifdef weathergen  
  RAINI = MATRIX_WEATHER(:,6)
  PETI  = MATRIX_WEATHER(:,7)
#else
  VPI   = MATRIX_WEATHER(:,6)
  RAINI = MATRIX_WEATHER(:,7)
  WNI   = MATRIX_WEATHER(:,8)
#endif

! Calendars
DAYS_FERT  = CALENDAR_FERT (:,1:2)
DAYS_NDEP  = CALENDAR_NDEP (:,1:2)
NFERTV     = CALENDAR_FERT (:,3) * NFERTMULT
NDEPV      = CALENDAR_NDEP (:,3)

! Initial constants for plant state variables
CLV        = CLVI
CLVD       = CLVDI
CRES       = CRESI
CRT        = CRTI
CST        = CSTI
CSTUB      = CSTUBI
LAI        = LAII
LT50       = LT50I
NRT        = NCR * CRTI
  NCSHI    = NCSHMAX * (1-EXP(-K*LAII)) / (K*LAII)
NSH        = NCSHI * (CLVI+CSTI)
PHEN       = PHENI
ROOTD      = ROOTDM
TILG1      = TILTOTI *       FRTILGI *    FRTILGG1I
TILG2      = TILTOTI *       FRTILGI * (1-FRTILGG1I)
TILV       = TILTOTI * (1. - FRTILGI)
VERN       = 1
YIELD      = YIELDI
YIELD_LAST = YIELDI
YIELD_TOT  = YIELDI

Nfert_TOT  = 0
DM_MAX     = 0

! Initial constants for soil state variables
CLITT      = CLITT0
CSOMF      = CSOM0 * FCSOMF0
CSOMS      = CSOM0 * (1-FCSOMF0)
DRYSTOR    = DRYSTORI
Fdepth     = FdepthI
NLITT      = CLITT0 / CNLITT0
NSOMF      = (CSOM0 *    FCSOMF0)  / CNSOMF0
NSOMS      = (CSOM0 * (1-FCSOMF0)) / CNSOMS0
NMIN       = NMIN0
O2         = FGAS * ROOTDM * FO2MX * 1000./22.4
Sdepth     = SDEPTHI
TANAER     = TANAERI
WAL        = 1000. * ROOTDM * WCI
WAPL       = WAPLI
WAPS       = WAPSI
WAS        = WASI
WETSTOR    = WETSTORI

do day = 1, NDAYS

  ! Environment
  call DDAYL          (doy)
  call set_weather_day(day,DRYSTOR,                    year,doy)
  call SoilWaterContent(Fdepth,ROOTD,WAL)
  call Physics        (DAVTMP,Fdepth,ROOTD,Sdepth,WAS, Frate)
  call MicroClimate   (doy,DRYSTOR,Fdepth,Frate,LAI,Sdepth,Tsurf,WAPL,WAPS,WETSTOR, &
                                                       FREEZEPL,INFIL,PackMelt,poolDrain,poolInfil, &
                                                       pSnow,reFreeze,SnowMelt,THAWPS,wRemain)
#ifdef weathergen  
  call PEVAPINPUT     (LAI)
#else
  call PENMAN         (LAI)
#endif
  ! Resources
  call Light          (DAYL,DTR,LAI,PAR)
  call EVAPTRTRF      (Fdepth,PEVAP,PTRAN,ROOTD,WAL,   EVAP,TRAN)
  call ROOTDG         (Fdepth,ROOTD,WAL,               EXPLOR,RROOTD)
  ! Soil
  call FRDRUNIR       (EVAP,Fdepth,Frate,INFIL,poolDRAIN,ROOTD,TRAN,WAL,WAS, &
                                                       FREEZEL,IRRIG,THAWS)
  call O2status       (O2,ROOTD)
  ! Plant
  call Harvest        (CLV,CRES,CST,year,doy,DAYS_HARVEST,LAI,PHEN,TILG1,TILG2,TILV, &
                                                       GSTUB,HARVLA,HARVLV,HARVPH,HARVRE,HARVST,HARVTILG2)
  call Biomass        (CLV,CRES,CST)
  call Phenology      (DAYL,PHEN,                      DPHEN,GPHEN)
  call Foliage1
  call LUECO2TM       (PARAV)
  call HardeningSink  (CLV,DAYL,doy,LT50,Tsurf)
  call Growth         (LAI,NSH,NMIN,CLV,CRES,CST,PARINT,TILG1,TILG2,TILV,TRANRF, &
                                                       GLV,GRES,GRT,GST,RESMOB,NSHmob)
  call PlantRespiration(FO2,RESPHARD)
  call Senescence     (CLV,CRT,CSTUB,LAI,LT50,PERMgas,TANAER,TILV,Tsurf, &
                                                       DeHardRate,DLAI,DLV,DRT,DSTUB,dTANAER,DTILV,HardRate)
  call Foliage2       (DAYL,GLV,LAI,TILV,TILG1,TRANRF,Tsurf,VERN, &
                                                       GLAI,GTILV,TILVG1,TILG1G2)
  ! Soil 2
  call O2fluxes       (O2,PERMgas,ROOTD,RplantAer,     O2IN,O2OUT)
  call N_fert         (year,doy,DAYS_FERT,NFERTV,      Nfert)
  call N_dep          (year,doy,DAYS_NDEP,NDEPV,       Ndep)
  call CNsoil         (ROOTD,RWA,WFPS,WAL,GRT,CLITT,CSOMF,NLITT,NSOMF,NSOMS,NMIN,CSOMS) 
  call Nplant         (NSHmob,CLV,CRT,CST,DLAI,DLV,DRT,GLAI,GLV,GRT,GST, &
                                                       HARVLA,HARVLV,HARVST,LAI,NRT,NSH, &
                                                       DNRT,DNSH,GNRT,GNSH,HARVNSH, &
													   NCDSH,NCGSH,NCHARVSH,NSHmobsoil,Nupt)

! Extra variables
  DMLV      = CLV   / 0.45           ! Leaf dry matter; g m-2
  DMST      = CST   / 0.45           ! Stem dry matter; g m-2
  DMSTUB    = CSTUB / 0.45
  DMRES     = CRES  / 0.40
  DMSH      = DMLV + DMST + DMRES
  DM        = DMSH + DMSTUB
  TILTOT    = TILG1 + TILG2 + TILV

  NSH_DMSH  = NSH / DMSH             ! N content in shoot DM; g N g-1 DM
  
  PROTEIN   = NSH * 6.25             ! Crude protein; g m-2
  F_PROTEIN = PROTEIN / DMSH         ! Crude protein in shoot dry matter; g g-1   
  F_ASH	    = 0.069 + 0.14*F_PROTEIN

  call Digestibility  (DM,DMLV,DMRES,DMSH,DMST,DMSTUB,PHEN, &
                       F_WALL_DM,F_WALL_DMSH,F_WALL_LV,F_WALL_ST, &
                       F_DIGEST_DM,F_DIGEST_DMSH,F_DIGEST_LV,F_DIGEST_ST,F_DIGEST_WALL)

  !================
  ! Outputs
  !================
  y(day, 1) = year + (doy-0.5)/366 ! "Time" = Decimal year (approximation)
  y(day, 2) = year
  y(day, 3) = doy
  y(day, 4) = DAVTMP
  
  y(day, 5) = CLV
  y(day, 6) = CLVD
  y(day, 7) = YIELD_LAST
  y(day, 8) = CRES
  y(day, 9) = CRT
  y(day,10) = CST
  y(day,11) = CSTUB
  y(day,12) = DRYSTOR
  y(day,13) = Fdepth
  y(day,14) = LAI
  y(day,15) = LT50
  y(day,16) = O2
  y(day,17) = PHEN
  y(day,18) = ROOTD
  y(day,19) = Sdepth
  y(day,20) = TANAER
  y(day,21) = TILG1 + TILG2          ! "TILG"
  y(day,22) = TILV
  y(day,23) = WAL
  y(day,24) = WAPL
  y(day,25) = WAPS
  y(day,26) = WAS
  y(day,27) = WETSTOR
  
  y(day,28) = DM                     ! "DM"      = Aboveground dry matter in g m-2
  y(day,29) = DMRES / DM             ! "RES"     = Reserves in g g-1 aboveground dry matter
  y(day,30) = LERG                   !
  y(day,31) = NELLVG                 !
  y(day,32) = RLEAF                  !
  y(day,33) = LAI / DMLV             ! "SLA"     = m2 leaf area g-1 dry matter leaves
  y(day,34) = TILTOT                 ! "TILTOT"  = Total tiller number in # m-2
  y(day,35) = (TILG1+TILG2) / TILTOT ! "FRTILG"  = Fraction of tillers that is generative
  y(day,36) =  TILG1        / TILTOT ! "FRTILG1" = Fraction of tillers that is in TILG1
  y(day,37) =        TILG2  / TILTOT ! "FRTILG2" = Fraction of tillers that is in TILG2
  y(day,38) = RDRT
  y(day,39) = VERN

  y(day,40) = CLITT           ! g C m-2
  y(day,41) = CSOMF           ! g C m-2
  y(day,42) = CSOMS           ! g C m-2
  y(day,43) = NLITT           ! g N m-2
  y(day,44) = NSOMF           ! g N m-2
  y(day,45) = NSOMS           ! g N m-2
  y(day,46) = NMIN            ! g N m-2
  y(day,47) = Rsoil           ! g C m-2 d-1
  y(day,48) = NemissionN2O    ! g N m-2 d-1
  y(day,49) = NemissionNO     ! g N m-2 d-1
  y(day,50) = Nfert           ! g N m-2 d-1
  y(day,51) = Ndep            ! g N m-2 d-1
  y(day,52) = RWA             ! -
  y(day,53) = NSH             ! g N m-2
  y(day,54) = GNSH
  y(day,55) = DNSH
  y(day,56) = HARVNSH 
  y(day,57) = NSH / (CLV+CST) ! - "NCSH"
  y(day,58) = NCGSH
  y(day,59) = NCDSH
  y(day,60) = NCHARVSH
  y(day,61) = fNgrowth
  y(day,62) = RGRTV
  y(day,63) = FSPOT
  y(day,64) = RESNOR
  y(day,65) = TV2TIL
  y(day,66) = NSHNOR
  y(day,67) = KNMAX
  y(day,68) = KN

  y(day,69) = DMLV
  y(day,70) = DMST
  y(day,71) = NSH_DMSH
  y(day,72) = Nfert_TOT
  y(day,73) = YIELD_TOT
  y(day,74) = DM_MAX
  
  y(day,75) = F_PROTEIN
  y(day,76) = F_ASH

  y(day,77) = F_WALL_DM
  y(day,78) = F_WALL_DMSH
  y(day,79) = F_WALL_LV
  y(day,80) = F_WALL_ST
  y(day,81) = F_DIGEST_DM
  y(day,82) = F_DIGEST_DMSH
  y(day,83) = F_DIGEST_LV
  y(day,84) = F_DIGEST_ST
  y(day,85) = F_DIGEST_WALL

  y(day,86) = RDRS
  y(day,87) = RAIN
  y(day,88) = Nleaching
  
  y(day,89) = NSHmob
  y(day,90) = NSHmobsoil
  y(day,91) = Nfixation
  y(day,92) = Nupt
  y(day,93) = Nmineralisation

  y(day,94) = NSOURCE
  y(day,95) = NSINK

  y(day,96) = NRT
  y(day,97) = NRT / CRT
  
  y(day,98) = rNLITT
  y(day,99) = rNSOMF
  
  y(day,100) = DAYL

! State equations plants
  CLV     = CLV     + GLV   - DLV    - HARVLV
  CLVD    = CLVD            + DLV
  CRES    = CRES    + GRES  - RESMOB - HARVRE
  CRT     = CRT     + GRT   - DRT
  CST     = CST     + GST           - HARVST
  CSTUB   = CSTUB   + GSTUB - DSTUB
  LAI     = LAI     + GLAI - DLAI   - HARVLA
  LT50    = LT50    + DeHardRate - HardRate
  PHEN    = min(1., PHEN + GPHEN - DPHEN - HARVPH)
  ROOTD   = ROOTD   + RROOTD
  TILG1   = TILG1           + TILVG1 - TILG1G2
  TILG2   = TILG2                    + TILG1G2 - HARVTILG2
  TILV    = TILV    + GTILV - TILVG1           - DTILV
  if((LAT>0).AND.(doy==305)) VERN = 0  
  if((LAT<0).AND.(doy==122)) VERN = 0  
  if(DAVTMP<TVERN)           VERN = 1
  YIELD     = (HARVLV + HARVST*HAGERE)/0.45 + HARVRE/0.40
  if(YIELD>0) YIELD_LAST = YIELD
  YIELD_TOT = YIELD_TOT + YIELD
  
  NRT       = NRT   + GNRT - DNRT
  NSH       = NSH   + GNSH - DNSH - HARVNSH - NSHmob

  Nfert_TOT = Nfert_TOT + Nfert
  DM_MAX    = max( DM, DM_MAX )
  
! State equations soil
  CLITT   = CLITT + DLV + DSTUB              - rCLITT - dCLITT
  CSOMF   = CSOMF + DRT         + dCLITTsomf - rCSOMF - dCSOMF
  CSOMS   = CSOMS               + dCSOMFsoms          - dCSOMS
  DRYSTOR = DRYSTOR + reFreeze + Psnow - SnowMelt
  Fdepth  = Fdepth  + Frate
  NLITT   = NLITT + DNSH             - rNLITT - dNLITT
  NSOMF   = NSOMF + DNRT + NLITTsomf - rNSOMF - dNSOMF 
  NSOMS   = NSOMS        + NSOMFsoms          - dNSOMS
  NMIN    = NMIN  + Ndep + Nfert + Nmineralisation + Nfixation + NSHmobsoil &
            - Nupt - Nleaching - Nemission
  NMIN    = max(0.,NMIN)
  O2      = O2      + O2IN - O2OUT
  Sdepth  = Sdepth  + Psnow/RHOnewSnow - PackMelt
  TANAER  = TANAER  + dTANAER
  WAL     = WAL  + THAWS  - FREEZEL  + poolDrain + INFIL +EXPLOR+IRRIG-DRAIN-RUNOFF-EVAP-TRAN
  WAPL    = WAPL + THAWPS - FREEZEPL + poolInfil - poolDrain
  WAPS    = WAPS - THAWPS + FREEZEPL
  WAS     = WAS  - THAWS  + FREEZEL
  WETSTOR = WETSTOR + Wremain - WETSTOR

enddo

end