module yasso
implicit none
private

real, parameter :: days_yr = 365.0
integer, parameter, public :: statesize_yasso = 5

! The yasso parameter vector:
! 1-16 matrix A entries: 4*alpha, 12*p
! 17-21 Leaching parameters: w1,...,w5 IGNORED IN THIS FUNCTION
! 22-23 Temperature-dependence parameters for AWE fractions: beta_1, beta_2
! 24-25 Temperature-dependence parameters for N fraction: beta_N1, beta_N2
! 26-27 Temperature-dependence parameters for H fraction: beta_H1, beta_H2
! 28-30 Precipitation-dependence parameters for AWE, N and H fraction: gamma, gamma_N, gamma_H
! 31-32 Humus decomposition parameters: p_H, alpha_H (Note the order!)
! 33-35 Woody parameters: theta_1, theta_2, r 

! The Yasso20 maximum a posteriori parameters:
integer, parameter, public :: num_params_y20 = 35
real, parameter, public :: param_y20_map(num_params_y20) = (/ &
     0.51, &
     5.19, &
     0.13, &
     0.1, &
     0.5, &
     0., &
     1., &
     1., &
     0.99, &
     0., &
     0., &
     0., &
     0., &
     0., &
     0.163, &
     0., &
     -0., &
     0., &
     0., &
     0., &
     0., &
     0.158, &
     -0.002, &
     0.17, &
     -0.005, &
     0.067, &
     -0., &
     -1.44, &
     -2.0, &
     -6.9, &
     0.0042, &
     0.0015, &
     -2.55, &
     1.24, &
     0.25/)

! Nitrogen-specific parameters
!
real, parameter, public :: nc_mb = 0.1 ! N-C ratio of the microbial biomass 
real, parameter, public :: cue_min = 0.1 ! minimum microbial carbon use efficiency
real, public :: nc_h_max = 0.1 ! N-C ratio of the H pool

! AWENH composition from Palosuo et al. (2015), for grasses. For now, we'll use the same
! composition for both above and below ground inputs. The last values (H) are always 0.
real, parameter :: awenh_fineroot(statesize_yasso) = (/0.46, 0.32, 0.04, 0.18, 0.0/)
real, parameter :: awenh_leaf(statesize_yasso) = (/0.46, 0.32, 0.04, 0.18, 0.0/)
! A soil amendment consisting of soluble carbon (and nitrogen)
real, parameter :: awenh_soluble(statesize_yasso) = (/0.0, 1.0, 0.0, 0.0, 0.0/)
! From Heikkinen et al 2021, composted horse manure with straw litter
real, parameter :: awenh_compost(statesize_yasso) = (/0.69, 0.09, 0.02, 0.20, 0.0/)

integer, parameter, public :: met_ind_init = 1

! whether use the exponentially weighted averaging for meteorological parameters
logical, parameter, public :: use_met_ema = .true.

public get_params
public decompose
public initialize
public initialize_totc
public average_met
public average_met_ema
public partition_nitr
public inputs_to_fractions

contains

  subroutine get_params(param_base, alpha_awen, beta12, decomp_pc, param_final)
    real, intent(in) :: param_base(:) ! base params, e.g. the yasso20 MAP vector
    real, intent(in) :: alpha_awen(4) ! base decomposition rate for the AWEN pools
    real, intent(in) :: beta12(2)     ! temperature sensitivity parameters
    real, intent(in) :: decomp_pc(2)  ! perturbations along principal components for (1) AWE rates and (2) beta1 and beta2
    real, intent(out) :: param_final(:) ! the modified parameters

    real, parameter :: pc_rate(3) = (/-0.101565, -1.017149, -0.024609/)
    real, parameter :: pc_tresp(2) = (/0.023387, -0.000753/)
    
    param_final = param_base
    param_final(1:3) = alpha_awen(1:3) + pc_rate * decomp_pc(1)
    param_final(4) = alpha_awen(4)
    param_final(22:23) = beta12 + pc_tresp * decomp_pc(2)
    ! hard constraints on parameter values
    param_final(1:4) = max(param_final(1:4), 1e-6) ! decomposition rates must be strictly positive
    param_final(22) = max(param_final(22), 0.0) ! first order temperature response must be positive
    param_final(23) = min(param_final(23), 0.0) ! second order temperature response must be negative
  end subroutine get_params
  
  subroutine initialize(param, flux_leafc_day, flux_rootc_day, flux_nitr_day, &
       tempr_c, precip_day, tempr_ampl, totc_min, cstate, nstate)
    ! A simple algorithm to initialize the SOC pools into a steady state or a partial
    ! steady-state. First, the equilibrium SOC is evaluated. Then, if totc_min is > 0 and
    ! greater than the equilibrium, the deficit will be covered by increasing the H
    ! pool. The nitrogen pool is left unconstrained and is equal to the equilibrium N +
    ! the possible contribution from the extra H. 
    real, intent(in) :: param(:) ! parameter vector
    real, intent(in) :: flux_leafc_day ! carbon input with "leaf" composition per day
    real, intent(in) :: flux_rootc_day ! carbon input with "fineroot" composition per day
    real, intent(in) :: flux_nitr_day  ! organic nitrogen input per day
    real, intent(in) :: tempr_c
    real, intent(in) :: tempr_ampl
    real, intent(in) :: precip_day ! mm
    real, intent(in) :: totc_min ! see above
    real, intent(out) :: cstate(statesize_yasso)
    real, intent(out) :: nstate ! nitrogen
    
    real :: neg_c_input_yr(statesize_yasso)
    real :: matrix(statesize_yasso, statesize_yasso)
    real :: totc

    
    ! Carbon
    ! 
    neg_c_input_yr = -(flux_leafc_day * awenh_leaf + flux_rootc_day * awenh_fineroot) * 365.0
    call evaluate_matrix_mean_tempr(param, tempr_c, precip_day * days_yr,tempr_ampl,  matrix)
    ! Solve the equilibrium condition Ax + b = 0
    call solve(matrix, neg_c_input_yr, cstate)
    totc = sum(cstate)
    !dblepr1('totc before adjust', -1, totc)
    !dblepr1('totc_min before adjust', -1, totc_min)
    if (totc_min > 0.0 .and. totc < totc_min) then
       cstate(5) = cstate(5) + totc_min - totc
    end if
    !dblepr1('totc after adjust', -1, sum(cstate))

    ! Nitrogen
    !
    call eval_steadystate_nitr(&
         cstate, &
         -sum(neg_c_input_yr), & ! respiration equal to C input in equilibrium
         flux_nitr_day * days_yr, & 
         matrix, &
         nstate)
    
  end subroutine initialize

  subroutine eval_steadystate_nitr(cstate, resp_yr, nitr_input_yr, matrix, nstate)
    ! evaluate the steady state N pool based on the steady state C pools.
    real, intent(in) :: cstate(statesize_yasso)
    real, intent(in) :: resp_yr ! respiration in steady state == negative input
    real, intent(in) :: nitr_input_yr ! nitrogen input
    real, intent(in) :: matrix(statesize_yasso, statesize_yasso) ! the matrix used in steady state computation
    real, intent(out) :: nstate ! steady state N

    integer, parameter :: max_cue_iter = 10

    real :: decomp_h
    real :: cue
    real :: cupt_awen
    real :: nc_awen
    real :: growth_c
    integer :: cue_iter
    real :: nc_som
    
    decomp_h = matrix(5,5) * cstate(5)
    cue = 0.43 ! initially
    
    do cue_iter = 1, max_cue_iter
       cupt_awen = (resp_yr - decomp_h) / (1.0 - cue)
       growth_c = cue * cupt_awen
       ! Solve nc_awen from the state equation (below) such that nstate becomes stationary:
       nc_awen = (1.0 / cupt_awen) * (nc_mb * cue * cupt_awen - nc_h_max*decomp_h + nitr_input_yr)
       nstate = sum(cstate(1:4)) * nc_awen + nc_h_max * cstate(5)
       nc_som = nstate / sum(cstate)
       cue = max(min(0.43 * (nc_som / nc_mb) ** 0.6, 1.0), cue_min)
    end do
    
  end subroutine eval_steadystate_nitr
  
  subroutine initialize_totc(param, totc, cn_input, fract_root_input, fract_legacy_soc, &
       tempr_c, precip_day, tempr_ampl, cstate, nstate)
    ! Another, simpler initialization method which enforces the total C and N stocks
    ! strictly and requires setting the fraction of "legacy" carbon explicitly. Given a
    ! total C, the C pools are set as a weighted combination of an equilibrated
    ! partitioning and a "legacy" partitioning where all C is assigned to the H pool. The
    ! weighting is given by the fract_legacy_soc parameter. The N pool is set analoguously
    ! with the equilibrium N depending on the given C:N ratio of input.
    real, intent(in) :: param(:) ! parameter vector
    real, intent(in) :: totc ! total C pool
    real, intent(in) :: cn_input ! C:N ratio of the steady-state input 
    real, intent(in) :: fract_root_input ! fraction of input C with the fineroot composition
    real, intent(in) :: fract_legacy_soc
    real, intent(in) :: tempr_c
    real, intent(in) :: tempr_ampl
    real, intent(in) :: precip_day ! mm
    real, intent(out) :: cstate(statesize_yasso)
    real, intent(out) :: nstate ! nitrogen

    real, parameter :: legacy_state(statesize_yasso) = (/0.0, 0.0, 0.0, 0.0, 1.0/)
    real :: matrix(statesize_yasso, statesize_yasso)
    real :: unit_input(statesize_yasso)
    real :: tmpstate(statesize_yasso)
    real :: eqstate(statesize_yasso)
    real :: eqfac
    real :: eqnitr
    
    call evaluate_matrix_mean_tempr(param, tempr_c, precip_day * days_yr,tempr_ampl, matrix)
    if (fract_root_input < 0.0 .or. fract_root_input > 1) then
       call dblepr1('Bad fract_root_input:', -1, fract_root_input)
       call rexit('Bad fract_root_input')
    end if
    if (fract_legacy_soc < 0.0 .or. fract_legacy_soc > 1) then
       call dblepr1('Bad fract_legacy_soc:', -1, fract_legacy_soc)
       call rexit('Bad fract_legacy_soc')
    end if
    
    unit_input = fract_root_input * awenh_fineroot + (1.0 - fract_root_input) * awenh_leaf
    call solve(matrix, -unit_input, tmpstate)
    eqfac = totc / sum(tmpstate)
    eqstate = eqfac * tmpstate
    call eval_steadystate_nitr(eqstate, eqfac, eqfac / cn_input, matrix, eqnitr)
    
    cstate = fract_legacy_soc * legacy_state * totc + (1.0 - fract_legacy_soc) * eqstate
    nstate = fract_legacy_soc * totc * nc_h_max + (1.0 - fract_legacy_soc) * eqnitr

    call labelpr('TOTC INITIALIZATION', -1)
    call dblepr('CSTATE:', -1, cstate, statesize_yasso)
    call dblepr1('C:N ratio:', -1, sum(cstate)/nstate)
    call dblepr1('Equlibrium C input:', -1, eqfac)
    call dblepr1('legacy fraction:', -1, fract_legacy_soc)
    call dblepr('equilibrium state:', -1, eqstate, statesize_yasso)
  end subroutine initialize_totc

  subroutine inputs_to_fractions(leaf, root, soluble, compost, fract)
    ! Split C in various types of inputs into the (here hard-coded) YASSO fractions
    ! ("AWEN"). The fifth pool (H) never receives external input. Only C needs to be
    ! split, nitrogen always goes to the total pool.
    real, intent(in) :: leaf
    real, intent(in) :: root
    real, intent(in) :: soluble
    real, intent(in) :: compost
    real, intent(out) :: fract(statesize_yasso)

    fract = leaf * awenh_leaf &
         + root * awenh_fineroot &
         + soluble * awenh_soluble &
         + compost * awenh_compost
    
  end subroutine inputs_to_fractions
  
  subroutine decompose(param, timestep_days, tempr_c, &
       precip_day, cstate, nstate, ctend, ntend)
    real, intent(in) :: param(:) ! parameter vector
    real, intent(in) :: timestep_days
    real, intent(in) :: tempr_c ! air temperature
    real, intent(in) :: precip_day ! precipitation mm / day
    real, intent(in) :: cstate(:) ! AWENH
    real, intent(in) :: nstate ! nitrogen, single pool
    real, intent(out) :: ctend(:) ! AWENH time derivative
    real, intent(out) :: ntend ! nitrogen, single pool    
    
    real :: matrix(statesize_yasso, statesize_yasso)
    real :: totc ! total C, step beginning
    real :: decomp_h ! C mineralization from the H pool
    real :: cue ! carbon use (growth) efficiency
    real :: nc_som ! current N:C ratio of the SOM
    real :: growth_c ! microbial growth in C
    real :: cupt_awen ! C uptake from AWEN
    real :: cupt_h ! C uptake from H
    real :: timestep_yr
    real :: nc_awen ! N:C ratio of the AWEN pools
    real :: nitr_awen ! nitrogen remaining after subtracting H nitrogen from the total
    real :: nc_h ! N:C of the H pool
    real :: resp ! heterotrophic respiration
    
    totc = sum(cstate)

    ! Carbon
    !
    call evaluate_matrix(param, tempr_c, precip_day * days_yr, matrix)
    ! The equation is in form of dx/dt = Ax + b. The standalone Y20 uses a matrix
    ! exponential in yearly or longer steps, but here with a daily timestep this is not
    ! needed and explicit 1st order time stepping is used instead.
    timestep_yr = timestep_days / days_yr
    ctend = matmul(matrix, cstate) * timestep_yr   ! (matmul(matrix, cstate) + c_input_yr) * timestep_yr
    resp = sum(-ctend)
    
    ! Nitrogen
    !
    if (totc < 1e-6) then
       ! No SOM, no need for N dynamics
       ntend = 0.0
    else
       decomp_h = matrix(5,5) * cstate(5) * timestep_yr
       if (cstate(5) * nc_h_max > nstate) then
          ! This should require very unusual inputs or parameters. Handle it nevertheless:
          nc_h = nstate / totc
       else
          nc_h = nc_h_max
       end if
       nitr_awen = nstate - cstate(5) * nc_h
       nc_awen = nitr_awen / (totc - cstate(5) + 1e-9)
       nc_som = nstate / totc
       cue = max(min(0.43 * (nc_som / nc_mb) ** 0.6, 1.0), cue_min)
       ! resp_from_awen = uptake_from_awen * (1 - CUE), and thus: 
       cupt_awen = (resp - decomp_h) / (1.0 - cue)
       ! Yasso has no C flow from H to AWEN so we assume no C uptake from H.
       growth_c = cue * cupt_awen
       ! The immobilization / mineralization is equal to the difference of nitrogen needed for
       ! microbial growth and the nitrogen released from the decomposed organic matter.
       ntend = nc_mb * growth_c - nc_awen * cupt_awen - nc_h * decomp_h
    end if
    
  end subroutine decompose

  subroutine partition_nitr(cstate, nstate, nitr_awen, nitr_h)
    real, intent(in) :: nstate
    real, intent(in) :: cstate(statesize_yasso)
    real, intent(out) :: nitr_awen
    real, intent(out) :: nitr_h

    real :: totc
    real :: nc_h
    
    if (cstate(5) * nc_h_max > nstate) then
       ! This should require very unusual inputs or parameters. Handle it nevertheless:
       nc_h = nstate / totc
    else
       nc_h = nc_h_max
    end if
    nitr_h = nc_h * cstate(5)
    nitr_awen = nstate - nitr_h
  end subroutine partition_nitr
  
  subroutine evaluate_matrix(param, tempr, precip, matrix)
    real, intent(in) :: param(:) ! parameter vector
    real, intent(in) :: tempr ! temperature deg C
    real, intent(in) :: precip ! mm / yr
    real, intent(out) :: matrix(:,:) ! decomposition matrix, "A" in YASSO publications

    real :: temprm  ! temperature modifier for AWE
    real :: temprmN ! temperature modifier for N
    real :: temprmH ! temperature modifier for H
    real :: decm  ! rate modifier for AWE
    real :: decmN ! rate modifier for N
    real :: decmH ! rate modifier for H

    integer :: ii
    
    temprm = exp(param(22)*tempr + param(23)*tempr**2)
    temprmN = exp(param(24)*tempr + param(25)*tempr**2)
    temprmH = exp(param(26)*tempr + param(27)*tempr**2)

    ! The full rate modifiers including precipitation. The Y20 code has here division
    ! by 12 due to monthly averaging of the temperature modifer, which is not done here.
    decm = temprm * (1.0 - exp(param(28) * precip * 0.001))
    decmN = temprmN * (1.0 - exp(param(29) * precip * 0.001))
    decmH = temprmH * (1.0 - exp(param(30) * precip * 0.001))
    
    ! Calculating matrix A (will work ok despite the sign of alphas)
    DO ii = 1,3
       matrix(ii,ii) = -ABS(param(ii))*decm
    END DO
    matrix(4,4) = -ABS(param(4))*decmN
    
    matrix(1,2) = param(5)*ABS(matrix(2,2))
    matrix(1,3) = param(6)*ABS(matrix(3,3))
    matrix(1,4) = param(7)*ABS(matrix(4,4))
    matrix(1,5) = 0.0 ! no mass flows from H -> AWEN
    matrix(2,1) = param(8)*ABS(matrix(1,1))
    matrix(2,3) = param(9)*ABS(matrix(3,3))
    matrix(2,4) = param(10)*ABS(matrix(4,4))
    matrix(2,5) = 0.0
    matrix(3,1) = param(11)*ABS(matrix(1,1))
    matrix(3,2) = param(12)*ABS(matrix(2,2))
    matrix(3,4) = param(13)*ABS(matrix(4,4))
    matrix(3,5) = 0.0
    matrix(4,1) = param(14)*ABS(matrix(1,1))
    matrix(4,2) = param(15)*ABS(matrix(2,2))
    matrix(4,3) = param(16)*ABS(matrix(3,3))
    matrix(4,5) = 0.0
    matrix(5,5) = -ABS(param(32))*decmH ! no size effect in humus
    DO ii = 1,4
       matrix(5,ii) = param(31)*ABS(matrix(ii,ii)) ! mass flows AWEN -> H (size effect is present here)
    END DO
    
  end subroutine evaluate_matrix

  subroutine evaluate_matrix_mean_tempr(param, tempr, precip, tempr_ampl, matrix)
    ! Evaluate the matrix as above, but use the old YASSO-15 temperature averaging
    real, intent(in) :: param(:) ! parameter vector
    real, intent(in) :: tempr ! temperature deg C
    real, intent(in) :: precip ! mm / yr
    real, intent(in) :: tempr_ampl ! temperature yearly amplitude, deg C
    real, intent(out) :: matrix(:,:) ! decomposition matrix, "A" in YASSO publications

    real :: temprm  ! temperature modifier for AWE
    real :: temprmN ! temperature modifier for N
    real :: temprmH ! temperature modifier for H
    real :: decm  ! rate modifier for AWE
    real :: decmN ! rate modifier for N
    real :: decmH ! rate modifier for H
    real :: te(4) ! temperature for averaging the temperature modifier
    
    integer :: ii
    real, parameter :: pi = 3.141592653589793
    
    ! temperature annual cycle approximation
    te(1) = tempr+4*tempr_ampl*(1/sqrt(2.0)-1)/pi
    te(2) = tempr-4*tempr_ampl/sqrt(2.0)/pi
    te(3) = tempr+4*tempr_ampl*(1-1/sqrt(2.0))/pi
    te(4) = tempr+4*tempr_ampl/sqrt(2.0)/pi

    ! average over the 4 temperature points
    temprm = 0.25 * sum(exp(param(22)*te + param(23)*te**2))
    temprmN = 0.25 * sum(exp(param(24)*te + param(25)*te**2))
    temprmH = 0.25 * sum(exp(param(26)*te + param(27)*te**2))

    ! The full rate modifiers including precipitation. The Y20 code has here division
    ! by 12 due to monthly averaging of the temperature modifer, which is not done here.
    decm = temprm * (1.0 - exp(param(28) * precip * 0.001))
    decmN = temprmN * (1.0 - exp(param(29) * precip * 0.001))
    decmH = temprmH * (1.0 - exp(param(30) * precip * 0.001))
    
    ! Calculating matrix A (will work ok despite the sign of alphas)
    DO ii = 1,3
       matrix(ii,ii) = -ABS(param(ii))*decm
    END DO
    matrix(4,4) = -ABS(param(4))*decmN
    
    matrix(1,2) = param(5)*ABS(matrix(2,2))
    matrix(1,3) = param(6)*ABS(matrix(3,3))
    matrix(1,4) = param(7)*ABS(matrix(4,4))
    matrix(1,5) = 0.0 ! no mass flows from H -> AWEN
    matrix(2,1) = param(8)*ABS(matrix(1,1))
    matrix(2,3) = param(9)*ABS(matrix(3,3))
    matrix(2,4) = param(10)*ABS(matrix(4,4))
    matrix(2,5) = 0.0
    matrix(3,1) = param(11)*ABS(matrix(1,1))
    matrix(3,2) = param(12)*ABS(matrix(2,2))
    matrix(3,4) = param(13)*ABS(matrix(4,4))
    matrix(3,5) = 0.0
    matrix(4,1) = param(14)*ABS(matrix(1,1))
    matrix(4,2) = param(15)*ABS(matrix(2,2))
    matrix(4,3) = param(16)*ABS(matrix(3,3))
    matrix(4,5) = 0.0
    matrix(5,5) = -ABS(param(32))*decmH ! no size effect in humus
    DO ii = 1,4
       matrix(5,ii) = param(31)*ABS(matrix(ii,ii)) ! mass flows AWEN -> H (size effect is present here)
    END DO
    
  end subroutine evaluate_matrix_mean_tempr

  subroutine average_met(met_daily, met_rolling, aver_size, met_state, met_ind)
    ! Evaluate a rolling window average for given met quantities. Used for scaling met
    ! parameters from daily to monthly level.
    real, intent(in) :: met_daily(:)
    real, intent(out) :: met_rolling(:)
    integer, intent(in) :: aver_size ! number of days to average over, must not change
    real, intent(inout) :: met_state(:,:) ! size(met_daily), aver_size + 1
    integer, intent(inout) :: met_ind     ! a counter, must be 1 on first call, not changed outside

    if (met_ind < 1 .or. met_ind > aver_size+1) then
       call intpr1('something wrong with met_ind:', -1, met_ind)
       call rexit('something wrong with met_ind')
    end if
    if (size(met_state, 2) /= aver_size + 1 .or. size(met_state, 1) /= size(met_rolling)) then
       call intpr('met_state has wrong size', -1, shape(met_state), 2)
       call rexit('met_state has wrong size')
    end if
    
    if (met_ind <= aver_size) then
       ! For the first aver_size days average as many values as have been input.
       met_state(:,met_ind) = met_daily
       met_ind = met_ind + 1
    else
       ! met_ind now stays as aver_size+1
       met_state(:,aver_size+1) = met_daily
       met_state(:,1:aver_size) = met_state(:,2:aver_size+1)
    end if

    met_rolling = sum(met_state(:,1:met_ind-1), dim=2) / (met_ind-1)
    
  end subroutine average_met

  subroutine average_met_ema(met_daily, met_rolling)
    ! Evaluate an exponentially weighted moving average for the daily met quantities to
    ! scale them to monthly level.
    real, intent(in) :: met_daily(:)
    real, intent(out) :: met_rolling(:)
    
    real :: alpha_smooth1=0.05, alpha_smooth2=0.005
    
    if (size(met_rolling) /= 2) then
       call rexit('met_rolling has wrong shape')
    end if
    if (size(met_daily) /= 2) then
       call rexit('met_daily has wrong shape')
    end if
    
    met_rolling(1) = alpha_smooth1 * met_daily(1) + (1-alpha_smooth1) * met_rolling(1)
    met_rolling(2) = alpha_smooth2 * met_daily(2) + (1-alpha_smooth2) * met_rolling(2)
    
  end subroutine average_met_ema

  !************************************************************************************
  ! Linear algebra for the steady state computation
  
  SUBROUTINE solve(A, b, x)
    ! Solve linear system A*x = b
    IMPLICIT NONE
    INTEGER,PARAMETER :: n = 5
    REAL,DIMENSION(n,n),INTENT(IN) :: A
    REAL,DIMENSION(n),INTENT(IN) :: b
    REAL,DIMENSION(n),INTENT(OUT) :: x
    REAL,DIMENSION(n,n) :: U
    REAL,DIMENSION(n) :: c
    INTEGER :: i

    ! transform the problem to upper diagonal form
    CALL pgauss(A, b, U, c)

    ! solve U*x = c via back substitution
    x(n) = c(n)/U(n,n)
    DO i = n-1,1,-1
       x(i) = (c(i) - DOT_PRODUCT(U(i,i+1:n),x(i+1:n)))/U(i,i)
    END DO
  END SUBROUTINE solve

  SUBROUTINE pgauss(A, b, U, c)
    ! Transform the lin. system to upper diagonal form using gaussian elimination
    ! with pivoting
    IMPLICIT NONE
    INTEGER,PARAMETER :: n = 5
    REAL,DIMENSION(n,n),INTENT(IN) :: A
    REAL,DIMENSION(n),INTENT(IN) :: b
    REAL,DIMENSION(n,n),INTENT(OUT) :: U
    REAL,DIMENSION(n),INTENT(OUT) :: c
    INTEGER :: k, j
    REAL,PARAMETER :: tol = 1E-12

    U = A
    c = b
    DO k = 1,n-1
       CALL pivot(U,c,k) ! do pivoting (though may not be necessary in our case)
       IF (ABS(U(k,k)) <= tol) THEN
          call rwarn('Warning!!! Matrix is singular to working precision!')
       END IF
       U(k+1:n,k) = U(k+1:n,k)/U(k,k)
       DO j = k+1,n
          U(j,k+1:n) = U(j,k+1:n) - U(j,k)*U(k,k+1:n)
       END DO
       c(k+1:n) = c(k+1:n) - c(k)*U(k+1:n,k)
    END DO
  END SUBROUTINE pgauss

  SUBROUTINE pivot(A, b, k)
    ! perform pivoting to matrix A and vector b at row k
    IMPLICIT NONE
    INTEGER,PARAMETER :: n = 5
    REAL,DIMENSION(n,n),INTENT(INOUT) :: A
    REAL,DIMENSION(n),INTENT(INOUT) :: b
    INTEGER,INTENT(IN) :: k
    INTEGER :: q, pk

    !call dblepr('Pivot elements are: ', -1, A(k:n,k), size(A(k:n,k)))
    q = MAXLOC(ABS(A(k:n,k)),1)
    !call intpr('', -1, q, 1)
    IF (q > 1) THEN
       pk = k-1+q
       A(k:pk:pk-k,:) = A(pk:k:k-pk,:)
       b(k:pk:pk-k) = b(pk:k:k-pk)
    END IF
    !call dblepr('Pivot elements are: ', -1, A(k:n,k), size(A(k:n,k)))
  END SUBROUTINE pivot
  
end module yasso

