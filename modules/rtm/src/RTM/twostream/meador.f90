!!!!!
!!! This subroutine implements the two-stream solution for plane-parallel atmospheres from:
!!!     Meador and Weaver (1980). Two-stream approximations to radiative transfer in planetary atmospheres: A unified description of 
!!!     existing methods and a new improvement. Journal of the Atmospheric Sciences 37: 630-643.
!!!!!
subroutine meador_plane_parallel(w0, mu0, tau, &
        gamma1, gamma2, gamma3, gamma4, &
        R, Tdif)

    use mod_dataspec_wavelength
    implicit none

    !! Inputs
    real*8, intent(in) :: mu0, tau
    real*8, intent(in), dimension(nw) :: w0
    real*8, intent(in), dimension(nw) :: gamma1, gamma2, gamma3, gamma4

    !! Outputs
    real*8, intent(out), dimension(nw) :: R, Tdif

    !! Derived terms
    real*8 :: Tdir
    real*8, dimension(nw) :: alpha1, alpha2, k, ksquare
    real*8, dimension(nw) :: expktau, kmu0, kgamma3, kgamma4
    real*8, dimension(nw) :: term1, term2, term3, term4

    !! Direct transmittance follows a simple exponential formula
    Tdir = exp(-tau / mu0)

    !! Define alphas and k
    alpha1 = gamma1 * gamma4 + gamma2 * gamma3      !! (16)
    alpha2 = gamma1 * gamma3 + gamma2 * gamma4      !! (17)
    ksquare = gamma1 * gamma1 - gamma2 * gamma2     !! (18)
    k = sqrt(ksquare)

    !! Reflectance (14)
    expktau = exp(k * tau)
    kmu0 = k * mu0
    kgamma3 = k * gamma3
    kgamma4 = k * gamma4

    term1 = w0 * 1.0d0 / ((1 - kmu0 * kmu0) * &
        ((k + gamma1) * expktau + (k * gamma2) * expktau)) 
    term2 = (1.0d0 - kmu0) * (alpha2 + kgamma3) * expktau
    term3 = (1.0d0 + kmu0) * (alpha2 - kgamma3) / expktau
    term4 = 2.0d0 * k * (gamma3 - alpha2 * mu0) * Tdir

    R = term1 * (term2 - term3 - term4)

    !! Diffuse transmittance (15)
    !! term1 is reused from Reflectance
    term2 = (1.0d0 + kmu0) * (alpha1 + kgamma4) * expktau
    term3 = (1.0d0 - kmu0) * (alpha1 - kgamma4) / expktau
    term4 = 2.0d0 * k * (gamma4 + alpha1 * mu0) / Tdir
    Tdif = Tdir * (1.0d0 - (term1 * (term2 - term3 - term4)))

end subroutine meador_plane_parallel
