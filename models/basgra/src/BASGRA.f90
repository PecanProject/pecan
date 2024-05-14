subroutine BASGRA(PARAMS, MATRIX_WEATHER, &
     CALENDAR_FERT, CALENDAR_NDEP, DAYS_HARVEST, &
     HARVEST_PARAMS, &
     NPARAMS, NDAYS, NOUT, &
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
use yasso
use set_params_mod
implicit none
#ifdef weathergen  
  integer, parameter      :: NWEATHER =  7
#else
  integer, parameter      :: NWEATHER =  9
#endif
real, intent(in)                      :: PARAMS(NPARAMS)
real, intent(in)                      :: MATRIX_WEATHER(NMAXDAYS,NWEATHER)
real, intent(in)                      :: CALENDAR_FERT(300, 6)
real, intent(in)                      :: CALENDAR_NDEP(300, 3)
integer, intent(in)                   :: DAYS_HARVEST(300, 2)
real, intent(in)                      :: HARVEST_PARAMS(300, 2) ! (day, 1=CLAIV, 2={if > 0, cut only})
integer, intent(in)                   :: NPARAMS, NDAYS, NOUT
real, intent(out)                     :: y(NDAYS,NOUT)


integer, dimension(300,2) :: DAYS_FERT    , DAYS_NDEP
real   , dimension(300,4) :: NFERTV ! (day,[mineral-N, organic-N, soluble-C, compost-C])
real   , dimension(300)   :: NDEPV

integer                   :: day, doy, i, year

! State variables plants
real    :: CLV, CLVD, CRES, CRT, CST, CSTUB, LAI, LT50, PHEN
real    :: ROOTD, TILG1, TILG2, TILV
integer :: VERN
real    :: YIELD, YIELD_POT, YIELD_LAST, YIELD_TOT
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
real :: HARVLA, HARVLV, HARVLVP, HARVPH, HARVRE, HARVREP, HARVST, HARVSTP, HARVTILG2, INFIL, IRRIG, O2IN
real :: O2OUT, PackMelt, poolDrain, poolInfil, Psnow, reFreeze, RESMOB
real :: RGRTVG1, RROOTD, SnowMelt, THAWPS, THAWS, TILVG1, TILG1G2, TRAN, Wremain
real :: NCSHI, NCGSH, NCDSH, NCHARVSH, GNSH, DNSH, HARVNSH, GNRT, DNRT
real :: ALLOTOT, GRESSI
real :: NSHmob, NSHmobsoil, Nupt
real :: input_soluble_c, input_compost_c ! from organic amendments/fertilizers
real :: input_org_n
real :: nupt_max, nupt_max_adj
real :: Ndep, Nfert

real :: F_DIGEST_DM, F_DIGEST_DMSH, F_DIGEST_LV, F_DIGEST_ST, F_DIGEST_WALL
real :: F_WALL_DM  , F_WALL_DMSH  , F_WALL_LV  , F_WALL_ST
logical :: if_cut_only ! add harvested biomass to litter, not yield fluxes
real :: harv_c_to_litt, harv_n_to_litt
real :: harv_c_exported
! yasso
real :: yasso_cstate(statesize_yasso)
real :: yasso_ctend(statesize_yasso)
real :: runoff_cstate(statesize_yasso)
real :: yasso_nstate
real :: yasso_ntend
real :: yasso_met_state(2, 31) ! for calculating 30-day averages of tempr & precip
real :: yasso_met(2) ! 30-day rolling tempr, precip
integer :: yasso_met_ind ! counter for averaging the met variables
real :: cflux_to_yasso(statesize_yasso)
real :: yasso_param(num_params_y20)
real :: org_n_to_yasso

if (NOUT < 118) then
   call rexit('NOUT < 118 too small')
end if

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
  CO2   = MATRIX_WEATHER(:,9)
#endif

! Calendars
DAYS_FERT  = CALENDAR_FERT (:,1:2)
DAYS_NDEP  = CALENDAR_NDEP (:,1:2)
NFERTV     = CALENDAR_FERT (:,3:6) * NFERTMULT

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
NRT        = NRTI
  NCSHI    = NCSHMAX * (1-EXP(-K*LAII)) / (K*LAII)
NSH        = NSHI
PHEN       = PHENI
ROOTD      = ROOTDM
TILG1      = TILG1I
TILG2      = TILG2I
TILV       = TILVI
VERN       = 1
YIELD      = YIELDI
YIELD_LAST = YIELDI
YIELD_TOT  = YIELDI

Nfert_TOT  = 0
DM_MAX     = 0

if (use_yasso) then
   ! Yasso currently requires DELT = 1
   if (abs(DELT - 1.0) > 1e-6) then
      call rexit('Yasso soil model requires DELT = 1')
   end if
   yasso_met_ind = 1
   call get_params(param_y20_map, yasso_alpha_awen, yasso_beta12, yasso_decomp_pc, yasso_param)
   ! call initialize(&
   !      yasso_param, &
   !      0.3 * hist_carbon_input / 365.0, &
   !      0.7 * hist_carbon_input / 365.0, &
   !      hist_carbon_input * 0.02 / 365.0, & ! C:N 50 for carbon input
   !      hist_mean_tempr, hist_yearly_precip / 365.0, hist_tempr_ampl, &
   !      totc_min_init, &
   !      yasso_cstate, &
   !      yasso_nstate)
   if (sum(yasso_cstate_init) > 0.0) then
      yasso_cstate = yasso_cstate_init
      if (.not. yasso_nstate_init > 0.0) then
         call rexit('yasso initial nstate is zero but cstate is not')
      end if
      yasso_nstate = yasso_nstate_init
   else
      call initialize_totc(&
           yasso_param, &
           totc_init, &
           cn_input = 50.0, &
           fract_root_input = 0.7, &
           fract_legacy_soc = fract_legacy_c, &
           tempr_c = hist_mean_tempr, &
           precip_day = hist_yearly_precip / 365.0, &
           tempr_ampl = hist_tempr_ampl, &
           cstate = yasso_cstate, nstate = yasso_nstate)
   end if
else
   ! Initial constants for soil state variables
   CLITT      = CLITT0
   CSOMF      = CSOMF0
   CSOMS      = CSOMS0 
   NLITT      = NLITT0
   NSOMF      = NSOMF0
   NSOMS      = NSOMS0
   ! missing values for yasso variables
   yasso_cstate = -1e35
   yasso_nstate = -1e35
   yasso_met = -1e35
end if

DRYSTOR    = DRYSTORI
Fdepth     = FdepthI
NLITT      = NLITT0
NSOMF      = NSOMF0
NSOMS      = NSOMS0
NMIN       = NMIN0
O2         = O2I
Sdepth     = SDEPTHI
TANAER     = TANAERI
WAL        = WALI
WAPL       = WAPLI
WAPS       = WAPSI
WAS        = WASI
WETSTOR    = WETSTORI

do day = 1, NDAYS
  ! Environment
  call set_weather_day(day,DRYSTOR,                    year,doy)
  call DDAYL          (doy)
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
  call Harvest        (CLV,CRES,CST,year,doy,DAYS_HARVEST,HARVEST_PARAMS, LAI,PHEN,TILG1,TILG2,TILV, &
       GSTUB,HARVLA,HARVLV,HARVLVP,HARVPH,HARVRE,HARVREP,HARVST,HARVSTP,HARVTILG2,if_cut_only)
  if (if_cut_only) then
     harv_c_to_litt = HARVLV + HARVST*HAGERE + HARVRE
     harv_n_to_litt = HARVNSH
  else
     harv_c_to_litt = 0.0
     harv_n_to_litt = 0.0
  end if
      
  
  call Biomass        (CLV,CRES,CST)
  call Phenology      (DAYL,PHEN,                      DPHEN,GPHEN)
  call Foliage1
  call LUECO2TM       (PARAV)
  call HardeningSink  (CLV,DAYL,doy,LT50,Tsurf)
  call Growth(LAI, NSH, NMIN, CLV, CRES, CST,&
       PARINT,TILG1,TILG2,TILV,TRANRF, &
       RESMOB, NSINK, NSHmob, nupt_max, &
       ALLOTOT, GRESSI)
  
  call Senescence     (CLV,CRT,CSTUB,LAI,LT50,PERMgas,TANAER,TILV,Tsurf, &
       DeHardRate,DLAI,DLV,DRT,DSTUB,dTANAER,DTILV,HardRate)

  call N_fert         (year,doy,DAYS_FERT,NFERTV,      Nfert, input_soluble_c, input_compost_c, input_org_n)

  if (use_yasso) then
     if (use_met_ema) then
        if (day == 1) then
           yasso_met(1:2) = yasso_met_init(1:2)
        end if
        call average_met_ema((/DAVTMP, RAIN/), yasso_met)
     else
        call average_met((/DAVTMP, RAIN/), yasso_met, 30, yasso_met_state, yasso_met_ind)
     end if
     call decompose(&
          yasso_param, &
          DELT, & ! timestep
!          cflux_to_yasso, & ! segregated by the AWENH fraction
!          org_n_to_yasso, & ! total organic N input
          yasso_met(1), &! 30-day temperature
          yasso_met(2), &! 30-day precip,
          yasso_cstate, &
          yasso_nstate, &
          yasso_ctend, &
          yasso_ntend)
     
     ! The nitrogen-uptake-related fluxes:
     ! NSINK = how much N the plant can potentially use for growth
     ! NSHmob = how much N can be mobilized internally
     ! NSOURCE_ADJ = how much N the plant can receive after accounting for microbial N immobilization
     ! NSOURCE = how much N the plant is able to use, from all sources
     call adjust_nmin_fluxes(&
          use_yasso, &
          NMIN, &
          nupt_max, & 
          yasso_ntend, &
          nupt_max_adj, &
          nmin_immob_yasso = Nmineralisation)
     call Allocation(&
          use_nitrogen, ALLOTOT, GRESSI, &
          nupt_max_adj + NSHmob, & ! NSOURCE
          NSINK, GRES, GRT, GLV, GST)
     call CNSoil_stub(ROOTD, RWA, WFPS, WAL, GRT, yasso_cstate, yasso_nstate, NMIN, runoff_cstate)
     Rsoil = -(sum(yasso_ctend))
     
  else
     call adjust_nmin_fluxes(&
          use_yasso, &
          NMIN, &
          nupt_max, & 
          yasso_ntend, &
          nupt_max_adj = nupt_max_adj)
     call Allocation(&
          use_nitrogen, ALLOTOT, GRESSI, &
          nupt_max_adj + NSHmob, & ! NSOURCE
          NSINK, GRES, GRT, GLV, GST)
     ! The inputs are zeroed because they are currently ignored when run without YASSO.
     input_soluble_c = 0.0
     input_compost_c = 0.0
     call CNsoil      (ROOTD,RWA,WFPS,WAL,GRT,CLITT,CSOMF,NLITT,NSOMF,NSOMS,NMIN,CSOMS)
  end if
  
  call PlantRespiration(FO2,RESPHARD) ! must be after allocation
  
  call Foliage2       (DAYL,GLV,LAI,TILV,TILG1,TRANRF,Tsurf,VERN, &
                                                       GLAI,GTILV,TILVG1,TILG1G2)
  ! Soil 2
  call O2fluxes       (O2,PERMgas,ROOTD,RplantAer,     O2IN,O2OUT)
  call N_dep          (year,doy,DAYS_NDEP,NDEPV,       Ndep)
  
  
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
  

  NSH_DMSH  = NSH / DMSH             ! N content in shoot DM; g N g-1 DM
  
  PROTEIN   = NSH * 6.25             ! Crude protein; g m-2
  F_PROTEIN = PROTEIN / DMSH         ! Crude protein in shoot dry matter; g g-1   
  F_ASH	    = 0.069 + 0.14*F_PROTEIN

  call Digestibility  (DM,DMLV,DMRES,DMSH,DMST,DMSTUB,PHEN, &
                       F_WALL_DM,F_WALL_DMSH,F_WALL_LV,F_WALL_ST, &
                       F_DIGEST_DM,F_DIGEST_DMSH,F_DIGEST_LV,F_DIGEST_ST,F_DIGEST_WALL)

! State equations plants
  CLV     = CLV     + GLV   - DLV    
  CLVD    = CLVD            + DLV
  CRES    = CRES    + GRES  - RESMOB 
  CRT     = CRT     + GRT   - DRT
  CST     = CST     + GST          
  CSTUB   = CSTUB   + GSTUB - DSTUB
  LAI     = LAI     + GLAI - DLAI   
  LT50    = LT50    + DeHardRate - HardRate
  PHEN    = min(1., PHEN + GPHEN - DPHEN)
  ROOTD   = ROOTD   + RROOTD
  TILG1   = TILG1           + TILVG1 - TILG1G2
  TILG2   = TILG2                    + TILG1G2 
  TILV    = TILV    + GTILV - TILVG1           - DTILV
  TILTOT  = TILG1 + TILG2 + TILV
  if((LAT>0).AND.(doy==305)) VERN = 0  
  if((LAT<0).AND.(doy==122)) VERN = 0  
  if(DAVTMP<TVERN)           VERN = 1
  if (.not. if_cut_only) then
     ! the other case handled elsewhere
     YIELD     = YIELD+(HARVLV + HARVST*HAGERE)/0.45 + HARVRE/0.40
     harv_c_exported = HARVLV + HARVRE + HARVST
  else
     harv_c_exported = 0.0
  end if
  YIELD_POT = (HARVLVP + HARVSTP*HAGERE)/0.45 + HARVREP/0.40
  if(YIELD>0) YIELD_LAST = YIELD
  YIELD_TOT = YIELD_TOT + YIELD
  
  NRT       = NRT   + GNRT - DNRT
  NSH       = NSH   + GNSH - DNSH - NSHmob
  
  NCR       = NRT / CRT

  Nfert_TOT = Nfert_TOT + Nfert
  DM_MAX    = max( DM, DM_MAX )

  CLV     = CLV     - HARVLV
  CRES    = CRES    - HARVRE
  CST     = CST     - HARVST
  LAI     = LAI     - HARVLA
  PHEN    = min(1., PHEN - HARVPH)
  TILG2   = TILG2   - HARVTILG2
  NSH       = NSH   - HARVNSH 

  
  ! State equations soil
  if (use_yasso) then
     call inputs_to_fractions(&
          leaf = DSTUB + DLV + harv_c_to_litt, &
          root = DRT, &
          soluble = input_soluble_c, &
          compost = input_compost_c, &
          fract = cflux_to_yasso)
     org_n_to_yasso = DNSH + DNRT + input_org_n + harv_n_to_litt
     yasso_cstate = yasso_cstate + yasso_ctend + cflux_to_yasso - runoff_cstate
     yasso_nstate = yasso_nstate + yasso_ntend + org_n_to_yasso - rNSOMF
  else
     CLITT   = CLITT + DLV + DSTUB + harv_c_to_litt               - rCLITT - dCLITT
     CSOMF   = CSOMF + DRT         + dCLITTsomf - rCSOMF - dCSOMF
     CSOMS   = CSOMS               + dCSOMFsoms          - dCSOMS
     NLITT   = NLITT + DNSH        + harv_n_to_litt               - rNLITT - dNLITT
     NSOMF   = NSOMF + DNRT + NLITTsomf - rNSOMF - dNSOMF 
     NSOMS   = NSOMS        + NSOMFsoms          - dNSOMS
  end if
  
  DRYSTOR = DRYSTOR + reFreeze + Psnow - SnowMelt
  Fdepth  = Fdepth  + Frate
  if (.not. use_nitrogen) then
     NMIN = -1e35
  else
     NMIN    = NMIN  + Ndep + Nfert + Nmineralisation + Nfixation + NSHmobsoil &
          - Nupt - Nleaching - Nemission
     NMIN    = max(0.,NMIN)
  end if
  O2      = O2      + O2IN - O2OUT
  Sdepth  = Sdepth  + Psnow/RHOnewSnow - PackMelt
  TANAER  = TANAER  + dTANAER
  WAL     = WAL  + THAWS  - FREEZEL  + poolDrain + INFIL +EXPLOR+IRRIG-DRAIN-RUNOFF-EVAP-TRAN
  WAPL    = WAPL + THAWPS - FREEZEPL + poolInfil - poolDrain
  WAPS    = WAPS - THAWPS + FREEZEPL
  WAS     = WAS  - THAWS  + FREEZEL
  WETSTOR = WETSTOR + Wremain - WETSTOR
  

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
  y(day,30) = PHENCR                   !
  y(day,31) = NELLVG                 !
  y(day,32) = NELLVM                  !
  y(day,33) = LAI / DMLV             ! "SLA"     = m2 leaf area g-1 dry matter leaves
  y(day,34) = TILTOT                 ! "TILTOT"  = Total tiller number in # m-2
  y(day,35) = (TILG1+TILG2) / TILTOT ! "FRTILG"  = Fraction of tillers that is generative
  y(day,36) = TILG1        
  y(day,37) = TILG2  
  y(day,38) = RDRT
  y(day,39) = VERN

  y(day,40) = CLITT           ! g C m-2
  y(day,41) = CSOMF           ! g C m-2
  y(day,42) = CSOMS           ! g C m-2
  y(day,43) = NLITT           ! g N m-2
  y(day,44) = NSOMF           ! g N m-2
  y(day,45) = NSOMS           ! g N m-2
  y(day,46) = NMIN            ! g N m-2
  y(day,47) = PHOT            ! g C m-2 d-1
  y(day,48) = RplantAer       ! g C m-2 d-1
  y(day,49) = Rsoil           ! g C m-2 d-1
  y(day,50) = NemissionN2O    ! g N m-2 d-1
  y(day,51) = NemissionNO     ! g N m-2 d-1
  y(day,52) = Nfert           ! g N m-2 d-1
  y(day,53) = Ndep            ! g N m-2 d-1
  y(day,54) = RWA             ! -
  y(day,55) = NSH             ! g N m-2
  y(day,56) = GNSH
  y(day,57) = DNSH
  y(day,58) = HARVNSH 
  y(day,59) = NSH / (CLV+CST) ! - "NCSH"
  y(day,60) = NCGSH
  y(day,61) = NCDSH
  y(day,62) = NCHARVSH
  y(day,63) = fNgrowth
  y(day,64) = RGRTV
  y(day,65) = FSPOT
  y(day,66) = RESNOR
  y(day,67) = TV2TIL
  y(day,68) = NSHNOR
  y(day,69) = KNMAX
  y(day,70) = KN

  y(day,71) = DMLV
  y(day,72) = DMST
  y(day,73) = NSH_DMSH
  y(day,74) = Nfert_TOT
  y(day,75) = YIELD_POT
  y(day,76) = DM_MAX
  
  y(day,77) = F_PROTEIN
  y(day,78) = F_ASH

  y(day,79) = F_WALL_DM
  y(day,80) = F_WALL_DMSH
  y(day,81) = F_WALL_LV
  y(day,82) = F_WALL_ST
  y(day,83) = F_DIGEST_DM
  y(day,84) = F_DIGEST_DMSH
  y(day,85) = F_DIGEST_LV
  y(day,86) = F_DIGEST_ST
  y(day,87) = F_DIGEST_WALL

  y(day,88) = RDRS
  y(day,89) = RAIN
  y(day,90) = Nleaching
  
  y(day,91) = NSHmob
  y(day,92) = NSHmobsoil
  y(day,93) = Nfixation
  y(day,94) = Nupt
  y(day,95) = Nmineralisation

  y(day,96) = NSOURCE
  y(day,97) = NSINK

  y(day,98) = NRT
  y(day,99) = NRT / CRT
  
  y(day,100) = rNLITT
  y(day,101) = rNSOMF
  
  y(day,102) = DAYL
  y(day,103) = EVAP
  y(day,104) = TRAN

  y(day,105) = DLV+DSTUB ! FLITTC_LEAF
  y(day,106) = DRT       ! FLITTC_ROOT

  y(day,107) = Rsoil - GRT - GST - GLV - GRES + RESMOB ! NEE
  y(day,108) = harv_c_exported
  y(day,109) = rCLITT + rCSOMF ! C RUNOFF

  ! yasso outputs
  y(day,110:114) = yasso_cstate
  y(day,115) = yasso_nstate
  y(day,116:117) = yasso_met

  y(day,118) = input_soluble_c + input_compost_c ! C added in soil amendments

enddo

end
