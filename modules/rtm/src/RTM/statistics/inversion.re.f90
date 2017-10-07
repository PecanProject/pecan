subroutine invert_re(observed, nspec, modcode, &
            inits, npars, ipars, rand, &
            cons, ncons, icons, &
            pmu, psd, plog, pmin, ngibbs, results, seed)
    use mod_types
    use mod_statistics
    use mod_selectmodel
    use mod_dataspec_wavelength
    implicit none

    ! Inputs
    integer(kind=i2), intent(in) :: nspec, npars, ncons, modcode, seed(100)
    integer(kind=i2), intent(in) :: ipars(npars), icons(ncons)
    real(kind=r2), intent(in) :: observed(nw,nspec), inits(npars), &
        cons(ncons), rand(npars,nspec)
    real(kind=r2), intent(in), dimension(npars) :: pmin, pmu, psd
    logical, intent(in) :: plog(npars)
    integer(kind=i2), intent(in) :: ngibbs

    ! Internals
    integer(kind=i2) :: nseed
    integer(kind=i1) :: i, p, ng, adapt 
    real(kind=r2) :: rp1, rp2, rinv, rsd, tp1, tp2, tinv
    real(kind=r2), dimension(npars) :: inpars, tausd
    real(kind=r2) :: PrevError(nw,nspec), PrevSpec(nw)
    real(kind=r2) :: Jump(npars), Jump_re(npars)
    real(kind=r1) :: adj_min
    real(kind=r1), dimension(npars) :: adj, ar, adj_re, ar_re
    procedure(), pointer :: model

    ! Outputs
    real(kind=r2), intent(out) :: results(ngibbs, npars*(nspec+2)+1)

    nseed = 100
    call random_seed(size=nseed)  
    call random_seed(put=seed)
    call model_select(modcode, model) 

    rp1 = 0.001 + nspec*nw/2
    rsd = 0.5
    
    tp1 = 0.001 + nw/2
    tausd = 0.1

    PrevSpec = 0.0d0
    do i = 1,nspec
        inpars(:) = inits + rand(:,i)
        call model(inpars, npars, ipars, cons, ncons, icons, PrevSpec)
        PrevError(:,i) = PrevSpec - observed(:,i)
    end do
    Jump = inits * 0.05
    Jump_re = 0.1
    adapt = 20
    adj_min = 0.1
    ar = 0
    ar_re = 0
    do ng=1,ngibbs
        if(mod(ng, adapt) < 1) then
            adj = ar / adapt / 0.44
            adj_re = ar_re / nspec / adapt / 0.44
            where(adj < adj_min) 
                adj = adj_min
            end where
            where(adj_re < adj_min)
                adj_re = adj_min
            end where
            Jump = Jump * adj
            Jump_re = Jump_re * adj_re
            ar = 0
            ar_re = 0
        endif
        call mh_sample_re(observed, nspec, model, &
            inits, npars, ipars, rand, &
            cons, ncons, icons, rsd, tausd, &
            Jump, Jump_re, pmu, psd, plog, pmin, PrevError, ar, ar_re)
        results(ng,1:npars) = inits
        results(ng,(npars+1):(npars*(nspec+1))) = reshape(rand,(/npars*nspec/))
        do p=1,npars
            tp2 = 0.001 + sum(rand(p,:) * rand(p,:))/2
            tinv = rgamma(tp1, 1/tp2)
            tausd(p) = 1/sqrt(tinv)
            results(ng,p+npars*(nspec+1)) = tausd(p)
        end do
        rp2 = 0.001 + sum(PrevError * PrevError)/2
        rinv = rgamma(rp1, 1/rp2)
        rsd = 1/sqrt(rinv)
        results(ng,npars*(nspec+2)+1) = rsd
    enddo
end subroutine
