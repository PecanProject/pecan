subroutine invert_basic(observed, nspec, inits, npars, &
            pmu, psd, plog, pmin, ngibbs, results)
    use mod_types
    use mod_statistics
    use mod_dataspec_wavelength
    implicit none

    ! Inputs
    integer(kind=i1), intent(in) :: nspec, npars
    real(kind=r2), intent(in) :: observed(nw,nspec), inits(npars)
    real(kind=r2), intent(in), dimension(npars) :: pmin, pmu, psd
    logical, intent(in) :: plog(npars)
    integer(kind=i2), intent(in) :: ngibbs

    ! Internals
    integer(kind=i1) :: i, ng, adapt
    real(kind=r2) :: rp1, rp2, rinv, rsd
    real(kind=r2) :: LRT(nw,2), PrevError(nw,nspec), PrevSpec(nw)
    real(kind=r2) :: Jump(npars)
    real(kind=r1) :: adj_min
    real(kind=r1), dimension(npars) :: adj, ar

    ! Outputs
    real(kind=r2), intent(out) :: results(ngibbs, npars+1)

    rp1 = 0.001 + nspec*nw/2
    rsd = 0.5
    call prospect_5b(inits(1), inits(2), inits(3), inits(4), inits(5), inits(6), LRT)
    PrevSpec = LRT(:,1)
    do i=1,nspec
        PrevError(:,i) = PrevSpec - observed(:,i)
    enddo
    Jump = inits * 0.05
    adapt = 20
    adj_min = 0.1
    do ng=1,ngibbs
        if(mod(ng, adapt) < 1) then
            adj = ar / adapt / 0.75
            where(adj < adj_min) 
                adj = adj_min
            end where
            Jump = Jump * adj
            ar = ar * 0
        endif
        call mh_sample(inits, npars, rsd, observed, nspec, &
                    Jump, pmu, psd, plog, pmin, PrevError, ar)
        results(ng,1:npars) = inits
        rp2 = 0.001 + sum(PrevError * PrevError)/2
        rinv = rgamma(rp1, 1/rp2)
        rsd = 1/sqrt(rinv)
        results(ng,npars+1) = rsd
    enddo
end subroutine
