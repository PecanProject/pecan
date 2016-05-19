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
