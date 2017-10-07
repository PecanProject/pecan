subroutine re_model(model, inits, npars, ipars, &
        rand, nrand, cons, ncons, icons, spec)
    use mod_types
    use mod_dataspec_wavelength
    implicit none

    ! Inputs
    Procedure() :: model
    integer(kind=i2), intent(in) :: npars, nrand, ncons
    integer(kind=i2), intent(in) :: ipars(npars), icons(ncons)
    real(kind=r2), intent(in) :: inits(npars), rand(npars,nrand), cons(ncons)

    ! Outputs
    real(kind=r2), intent(out) :: spec(nw,nrand)

    ! Internals
    integer :: i
    real(kind=r2) :: inpars(npars)

    spec = 0
    do i = 1,nrand
        inpars(:) = inits + rand(:,i)
        call model(inpars, npars, ipars, cons, ncons, icons, spec(:,i))
    end do
    return
end subroutine


