! PROSPECT model
!Returns reflectance as a function of 4 parameters:
! N - Effective number of leaf layers
! Cab - Chlorophyll content (ug/mL)
! Cw - Equivalent water thickness (cm2)
! Cm - Dry matter content (ug/mL)

subroutine PROSP(N, Cab, Cw, Cm, Refl)
    implicit none
    double precision N, Cab, Cw, Cm
    double precision TAU
    integer wl, i

    double precision, dimension(2101) :: CabAbs, CwAbs, CmAbs
    double precision, dimension(2101) :: tao1, tao2, rho1, rho2
    double precision, dimension(2101) :: x, y
    double precision, dimension(2101) :: k, theta, Refl

    double precision, dimension(2101) :: rhoa, taoa, rho90, tao90
    double precision, dimension(2101) :: d90, a90, b90, nmR, dmRT

    common CabAbs, CwAbs, CmAbs, tao1, tao2, rho1, rho2, x, y

    wl = 2101

    k = 1.0/N * (Cab * CabAbs + Cw * CwAbs + Cm * CmAbs)
    do 10 i = 1, wl
        theta(i) = TAU(k(i))
10  continue
        
    ! Reflectance and transmittance of first layer (N=1)
    rhoa = rho1 + (tao1 * tao2 * rho2 * theta**2) / (1 - rho2**2 * theta**2)
    taoa = tao1 * tao2 * theta / (1 - rho2**2 * theta**2)
    rho90 = (rhoa - y) / x
    tao90 = taoa / x
    
    ! Reflectance and transmittance of N layers (Stokes coefficients)
    d90 = sqrt((tao90**2 - rho90**2 - 1.0)*(tao90**2 - rho90**2 - 1.0) - 4.0*rho90**2)
    a90 = (1.0 + rho90**2 - tao90**2 + d90) / (2.0*rho90)
    b90 = (1.0 - rho90**2 + tao90**2 + d90) / (2.0*tao90)
    nmR = taoa * tao90 * (b90**(N-1.0) - b90**(1.0-N))
    dmRT = a90*b90**(N-1.0) - b90**(1.0-N)/a90 - rho90 * (b90**(N-1.0) - b90**(1.0-N))
    Refl = rhoa + nmR / dmRT

    return
    end

include 'exp_int.f90'

