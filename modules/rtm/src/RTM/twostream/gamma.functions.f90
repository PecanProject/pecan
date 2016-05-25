!!!!!
! This subroutine calculates the gamma coefficients for the Meador & 
! Weaver two-stream plane-parallel atmosphere radiative transfer code.  
! The calculations are taken directly from Pinty et al. (2006) Table 
! 4.
!
! Pinty, B., Lavergne, T., Dickinson, R.E., Widlowski, J.-L., Gobron, 
! N., Verstraete, M.M., 2006. Simplifying the interaction of land 
! surfaces with radiation for relating remote sensing products to 
! climate models. J. Geophys. Res. 111, D02116. 
! doi:10.1029/2005JD005952
!!!!!

subroutine pinty(rl, tl, mu0, gammas)
    use mod_dataspec_wavelength
    implicit none

    real*8, intent(in) :: mu0
    real*8, intent(in), dimension(nw) :: rl, tl
    real*8, intent(out), dimension(nw, 4) :: gammas

    real*8, dimension(nw) :: w0, wd, w02, wd6

    w0 = rl + tl
    wd = rl - tl

    w02 = w0 / 2.0d0
    wd6 = wd / 6.0d0

    gammas(:,1) = 2.0d0 * (1 - w02 + wd6)
    gammas(:,2) = 2.0d0 * (w02 + wd6)
    gammas(:,3) = 2.0d0 * (w02 / 2.0d0 + mu0 * wd6) / w0
    gammas(:,4) = 1.0d0 - gammas(:,3)

end subroutine pinty
