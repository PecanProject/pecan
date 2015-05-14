subroutine mh_sample(observed, nspec, model, &
            inits, npars, ipars, cons, ncons, icons, rsd, &
            Jump, pmu, psd, plog, pmin, PrevError, ar)
    use mod_types
    use mod_statistics
    use mod_dataspec_wavelength
    implicit none

    ! Inputs -- unchanged
    integer(kind=i2), intent(in) :: npars, nspec, ncons
    integer(kind=i2), intent(in) :: ipars(npars), icons(ncons)
    real(kind=r2), intent(in) :: observed(nw,nspec), cons(ncons), rsd, Jump(npars) 
    real(kind=r2), intent(in), dimension(npars) :: pmin, pmu, psd
    logical, intent(in) :: plog(npars)
    procedure() :: model

    ! Input/Output -- modified
    real(kind=r1) :: ar(npars)
    real(kind=r2) :: inits(npars), PrevError(nw,nspec)

    ! Internals
    integer(kind=i1) :: p, i, j
    real(kind=r2) :: tvec(npars), a, u
    real(kind=r2) :: TryError(nw,nspec), TrySpec(nw)
    real(kind=r2) :: TryPost, PrevPost
    
    do p=1,npars
        tvec = inits
        tvec(p) = rnorm(inits(p),Jump(p))

        if(tvec(p) < pmin(p)) cycle
        call model(tvec, npars, ipars, cons, ncons, icons, TrySpec)
        do i = 1,nspec
            TryError(:,i) = TrySpec - observed(:,i)
        enddo
        call prior(tvec(p), pmu(p), psd(p), plog(p), TryPost)
        call prior(inits(p), pmu(p), psd(p), plog(p), PrevPost)
        do i=1,nw
            do j=1,nspec
                TryPost = TryPost + ldnorm(TryError(i,j), 0d0, rsd)
                PrevPost = PrevPost + ldnorm(PrevError(i,j), 0d0, rsd)
            enddo
        enddo
        ! rnorm is symmetrical, so shouldn't need to normalize
        ! JN = dnorm(tvec(p), inits(p), Jump(p), pm(p))
        ! JD = dnorm(inits(p), tvec(p), Jump(p), pm(p))
        ! a = exp((TryPost - JN) - (PrevPost - JD))
        a = exp(TryPost - PrevPost)
        call random_number(u)
        if(a > u) then
            inits(p) = tvec(p)
            PrevError = TryError
            ar(p) = ar(p) + 1
        endif
    end do
end subroutine 
