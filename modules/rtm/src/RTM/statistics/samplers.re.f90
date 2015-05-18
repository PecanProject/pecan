subroutine mh_sample_re(observed, nspec, model, &
            inits, npars, ipars, rand, & 
            cons, ncons, icons, rsd, tausd, &
            Jump, Jump_re, pmu, psd, plog, pmin, PrevError, ar)
    use mod_types
    use mod_statistics
    use mod_dataspec_wavelength
    implicit none

    ! Inputs -- unchanged
    integer(kind=i2), intent(in) :: npars, nspec, ncons, &
        ipars(npars), icons(ncons)
    real(kind=r2), intent(in) :: observed(nw,nspec), cons(ncons), rsd, & 
        tausd(npars), Jump(npars), Jump_re(npars), &
        pmu(npars), psd(npars), pmin(npars)
    logical, intent(in) :: plog(npars)
    procedure() :: model

    ! Input/Output -- modified
    real(kind=r1) :: ar(npars), ar_re(npars)
    real(kind=r2) :: inits(npars), PrevError(nw,nspec), rand(npars,nspec)

    ! Internals
    integer(kind=i1) :: p, r, i, j
    real(kind=r2) :: tvec(npars), inpars(npars), rmat(npars,nspec), &
        a, u, TryError(nw,nspec), TrySpec(nw), TryPost, PrevPost
    logical :: minflag
    
    do p=1,npars
        !! Sample parameter p
        minflag = .false.
        tvec = inits
        tvec(p) = rnorm(inits(p),Jump(p))

        if(tvec(p) < pmin(p)) cycle
        do i = 1,nspec
            inpars(:) = tvec + rand(:,i)
            if(inpars(p) < pmin(p)) then
                minflag = .true.
                exit
            end if
            call model(inpars, npars, ipars, cons, ncons, icons, TrySpec)
            TryError(:,i) = TrySpec - observed(:,i)
        end do
        if(minflag) cycle
        call prior(tvec(p), pmu(p), psd(p), plog(p), TryPost)
        call prior(inits(p), pmu(p), psd(p), plog(p), PrevPost)
        do i=1,nw
            do j=1,nspec
                TryPost = TryPost + ldnorm(TryError(i,j), 0d0, rsd)
                PrevPost = PrevPost + ldnorm(PrevError(i,j), 0d0, rsd)
            enddo
        enddo
        a = exp(TryPost - PrevPost)
        call random_number(u)
        if(a > u) then
            inits(p) = tvec(p)
            PrevError = TryError
            ar(p) = ar(p) + 1
        endif

        !! Sample random effects for parameter p
        do r=1,nspec
            minflag = .false.
            rmat = rand
            rmat(p,r) = rnorm(rand(p,r), Jump_re(p))
            do i = 1,nspec
                inpars(:) = inits + rmat(:,i)
                if(inpars(p) < pmin(p)) then
                    minflag = .true.
                    exit
                end if
                call model(inpars, npars, ipars, cons, ncons, icons, TrySpec)
                TryError(:,i) = TrySpec - observed(:,i)
            end do
            if(minflag) cycle
            call prior_re(rmat(p,r), tausd(p), TryPost)
            call prior_re(rand(p,r), tausd(p), PrevPost)
            do i=1,nw
                do j=1,nspec
                    TryPost = TryPost + ldnorm(TryError(i,j), 0d0, rsd)
                    PrevPost = PrevPost + ldnorm(PrevError(i,j), 0d0, rsd)
                enddo
            enddo
            a = exp(TryPost - PrevPost)
            call random_number(u)
            if(a > u) then
                rand(p,r) = rmat(p,r)
                PrevError = TryError
                ar_re(p) = ar_re(p) + 1
            end if
        end do
    end do
end subroutine  

