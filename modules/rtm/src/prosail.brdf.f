subroutine sail_BDRF(nw,w,lai,sumint,tsstoo,rsoil, &
        rdd,tdd,tsd,rsd,tdo,rdo,tss,too,rsod, &
        rddt,rsdt,rdot,rsot)

    implicit none
    integer,intent(in) :: nw
    real*8,intent(in) :: lai,tss,too,sumint,tsstoo
    real*8,intent(in),dimension(nw) :: w,rsoil,rdd,tdd,tsd,rsd,tdo,rdo,rsod
    real*8,intent(out),dimension(nw) :: rddt,rsdt,rdot,rsot

    real*8,dimension(nw) :: rsos,rso,dn,rsodt,rsost

    rsos  = w*lai*sumint        ! Single scattering contribution
    rso   = rsos+rsod           !Total canopy contribution
    dn    = 1.-rsoil*rdd            !Interaction with the soil

    ! rddt: bi-hemispherical reflectance factor
    rddt  = rdd+tdd*rsoil*tdd/dn 

    ! rsdt: directional-hemispherical reflectance factor for solar incident flux
    rsdt  = rsd+(tsd+tss)*rsoil*tdd/dn

    ! rdot: hemispherical-directional reflectance factor in viewing direction    
    rdot  = rdo+tdd*rsoil*(tdo+too)/dn

    ! rsot: bi-directional reflectance factor
    rsodt = rsod+((tss+tsd)*tdo+(tsd+tss*rsoil*rdd)*too)*rsoil/dn
    rsost = rsos+tsstoo*rsoil
    rsot  = rsost+rsodt

    return
end subroutine sail_BDRF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!****************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE Jfunc1(k,l,t,Jout)

    USE MOD_dataSpec_P5B
    IMPLICIT NONE

    !J1 function with avoidance of singularity problem
    !
    REAL*8,INTENT(in) :: k,l(nw),t
    REAL*8,INTENT(out) :: Jout(nw)
    REAL*8 :: del(nw)


    del=(k-l)*t
    WHERE (ABS(del)>1e-3)
        Jout=(EXP(-l*t)-EXP(-k*t))/(k-l)
    ELSEWHERE
        Jout=0.5*t*(EXP(-k*t)+EXP(-l*t))*(1.-del*del/12.)
    END WHERE

END SUBROUTINE Jfunc1


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!****************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE Jfunc2(k,l,t,Jout)

    USE MOD_dataSpec_P5B
    IMPLICIT NONE

    !J2 function

    REAL*8,INTENT(in) :: k,l(nw),t
    REAL*8,INTENT(out) :: Jout(nw)
    REAL*8 :: del(nw)

    Jout=(1.-EXP(-(k+l)*t))/(k+l)

END SUBROUTINE Jfunc2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!****************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


FUNCTION Jfunc3(k,l,t)

    USE MOD_dataSpec_P5B
    IMPLICIT NONE

    !J2 function

    REAL*8 :: k,l,t
    REAL*8 :: Jfunc3

    Jfunc3=(1.-EXP(-(k+l)*t))/(k+l)

END FUNCTION Jfunc3
