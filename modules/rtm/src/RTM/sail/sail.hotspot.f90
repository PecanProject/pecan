subroutine HotSpot(lai,q,tss,ks,ko,dso, &
        tsstoo,sumint)

    use mod_types
    implicit none
    real(kind=r2), intent(in) :: lai,q,tss,ks,ko,dso
    real(kind=r2), intent(out) :: tsstoo, sumint
    
    integer :: i
    real(kind=r2) :: fhot,alf,ca,x1,y1,f1,fint,x2,y2,f2

    !Treatment of the hotspot-effect
    alf = 1d6
    !Apply correction 2/(K+k) suggested by F.-M. Bréon
    IF (q.gt.0.) alf = (dso/q)*2./(ks+ko)
    IF (alf.GT.200.) alf = 200 !inserted H. Bach 1/3/04 
    IF (alf.eq.0.) THEN
        !The pure hotspot - no shadow
        tsstoo = tss
        sumint = (1-tss)/(ks*lai)
    ELSE
        !Outside the hotspot
        fhot   = lai*SQRT(ko*ks)

        !Integrate by exponential Simpson method in 20 steps
        !the steps are arranged according to equal partitioning
        !of the slope of the joint probability function

        x1     = 0.
        y1     = 0.
        f1     = 1.
        ca=exp(alf*(-1.))
        fint   = (1.-ca)*.05
        sumint = 0.

        DO i=1,20
        IF (i.lt.20) THEN
            x2=-LOG(1.-i*fint)/alf
        ELSE
            x2=1.
        ENDIF
        y2     = -(ko+ks)*lai*x2+fhot*(1.-EXP(-alf*x2))/alf
        f2     = EXP(y2)
        sumint = sumint+(f2-f1)*(x2-x1)/(y2-y1)
        x1     = x2
        y1     = y2
        f1     = f2
        ENDDO
        tsstoo = f1
    ENDIF

    return
end subroutine HotSpot
