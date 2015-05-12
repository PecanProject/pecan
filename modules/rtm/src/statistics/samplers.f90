subroutine mh_sample(npars, inits, rsd, observed, nspec, &
                    Jump, pmin, PrevError, ar)
    use mod_types
    use mod_statistics
    use mod_dataspec_wavelength
    implicit none

    ! Inputs -- unchanged
    integer(kind=i1), intent(in) :: npars, nspec
    real(kind=r2), intent(in) :: rsd, Jump(npars), pmin, observed(nw,nspec)

    ! Input/Output -- modified
    integer(kind=i1), intent(inout) :: ar(npars)
    real(kind=r2), intent(inout) :: inits(npars), PrevError(nw,nspec)

    ! Internals
    integer(kind=i1) :: p, i, j
    real(kind=r2) :: tvec(npars), LRT(nw,2), a, u
    real(kind=r2) :: TryError(nw,nspec), TrySpec(nw)
    real(kind=r2) :: TryPost, PrevPost
    
    print *, "Entered sampler"
    do p=1,npars
        tvec = inits
        tvec(p) = rnorm(inits(p),Jump(p))
        print *, p, " inits : ", inits(p), "jump : ", Jump(p)

        if(tvec(p) < pmin) cycle
        call prospect_5b(tvec(1), tvec(2), tvec(3), tvec(4), tvec(5), tvec(6), LRT)
        TrySpec = LRT(:,1)
        do i = 1,nspec
            TryError(:,i) = TrySpec - observed(:,i)
        enddo
        call p5b_prior(p, tvec(p), TryPost)
        call p5b_prior(p, inits(p), PrevPost)
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
