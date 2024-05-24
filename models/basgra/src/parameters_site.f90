module parameters_site

! Simulation period and time step
real, parameter       :: DELT   =   1.0

! Geography
real                  :: LAT

! Atmospheric conditions
! real, parameter       :: CO2A   = 350   

! Soil
real, parameter       :: DRATE  =  50
real                  :: WCI
real                  :: FWCAD, FWCWP, FWCFC, FWCWET, WCST
real                  ::  WCAD,  WCWP,  WCFC,  WCWET
real, parameter       :: KNFIX = 0, RRUNBULK = 0.05

! Soil - WINTER PARAMETERS
real                  :: FGAS, FO2MX, gamma, KRTOTAER, KSNOW, O2I
real, parameter       :: LAMBDAice      = 1.9354e+005
real                  :: LAMBDAsoil
real, parameter       :: LatentHeat     = 335000.
real, parameter       :: poolInfilLimit =      0.2
real                  :: RHOnewSnow, RHOpack
real, parameter       :: RHOwater       =   1000.
real                  :: SWret, SWrf, TmeltFreeze, TrainSnow
real                  :: WpoolMax

! Soil initial values (parameters)
real :: CLITT0, CSOM0, CNLITT0, CNSOMF0, CNSOMS0, FCSOMF0, NMIN0
real :: CSOMF0, CSOMS0, NLITT0, NSOMF0, NSOMS0
real :: FLITTSOMF, FSOMFSOMS, RNLEACH, KNEMIT
real :: TCLITT, TCSOMF, TCSOMS, TMAXF, TSIGMAF, RFN2O
real :: WFPS50N2O

! Soil initial constants
real  :: DRYSTORI 
real  :: FdepthI  
real  :: SDEPTHI  
real  :: TANAERI  
real  :: WAPLI    
real  :: WAPSI    
real  :: WASI     
real  :: WETSTORI 
real  :: WALI

! Management: harvest dates and irrigation
integer, dimension(3) :: doyHA
real, parameter       :: IRRIGF = 0.

! Mathematical constants
real, parameter       :: pi   = 3.141592653589793
!  real, parameter       :: Freq = 2.*pi / 365.
real, parameter       :: Kmin = 4.
real, parameter       :: Ampl = 0.625
real, parameter       :: Bias = Kmin + Ampl

! SA parameters
real                  :: NFERTMULT

! Yasso
logical               :: use_yasso
logical               :: use_nitrogen
real                  :: hist_carbon_input
real                  :: fract_legacy_c
real                  :: hist_mean_tempr
real                  :: hist_yearly_precip
real                  :: hist_tempr_ampl
real                  :: totc_init ! used in initialization
real                  :: yasso_alpha_awen(4) ! base decomposition rates for the faster-cycling pools
real                  :: yasso_beta12(2)     ! temperature sensitivity parameters
! principal component parametrization for the decomposition rates and temperature sensitivity
real                  :: yasso_decomp_pc(2)  
! initial YASSO C & N state
real                  :: yasso_cstate_init(5)
real                  :: yasso_nstate_init
real                  :: yasso_met_init(2)
end module parameters_site

