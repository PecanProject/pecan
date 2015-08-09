subroutine ReflTrans(rho,tau,lai,att,m,sigb,ks,ko,sf,sb,vf,vb, &
        rdd,tdd,tsd,rsd,tdo,rdo,tss,too,rsod)

    use mod_dataSpec_wavelength
    implicit none
    real*8,intent(in) :: lai,ks,ko
    real*8,intent(in),dimension(nw) :: rho,tau,att,m,sigb,sf,sb,vf,vb
    real*8,intent(out),dimension(nw) :: rdd,tdd,tsd,rsd,tdo,rdo,rsod
    real*8,intent(out) :: tss,too

    real*8,dimension(nw) :: e1,e2,rinf,rinf2,re,denom,J1ks,J2ks,J1ko,J2ko
    real*8,dimension(nw) :: Ps,Qs,Pv,Qv,g1,g2,TV1,TV2,T1,T2,T3
    real*8 :: Jfunc3, z

    e1    = EXP(-m*lai)
    e2    = e1*e1
    rinf  = (att-m)/sigb
    rinf2 = rinf*rinf
    re    = rinf*e1
    denom = 1.-rinf2*e2

    CALL Jfunc1(ks,m,lai,J1ks)
    CALL Jfunc2(ks,m,lai,J2ks)
    CALL Jfunc1(ko,m,lai,J1ko)
    CALL Jfunc2(ko,m,lai,J2ko)

    Ps    = (sf+sb*rinf)*J1ks
    Qs    = (sf*rinf+sb)*J2ks
    Pv    = (vf+vb*rinf)*J1ko
    Qv    = (vf*rinf+vb)*J2ko

    rdd   = rinf*(1.-e2)/denom
    tdd   = (1.-rinf2)*e1/denom
    tsd   = (Ps-re*Qs)/denom
    rsd   = (Qs-re*Ps)/denom
    tdo   = (Pv-re*Qv)/denom
    rdo   = (Qv-re*Pv)/denom

    tss   = EXP(-ks*lai)
    too   = EXP(-ko*lai)
    z     = Jfunc3(ks,ko,lai)
    g1    = (z-J1ks*too)/(ko+m)
    g2    = (z-J1ko*tss)/(ks+m)

    Tv1   = (vf*rinf+vb)*g1
    Tv2   = (vf+vb*rinf)*g2
    T1    = Tv1*(sf+sb*rinf)
    T2    = Tv2*(sf*rinf+sb)
    T3    = (rdo*Qs+tdo*Ps)*rinf

    !Multiple scattering contribution to bidirectional canopy reflectance
    rsod  = (T1+T2-T3)/(1.-rinf2)

    return
end subroutine ReflTrans

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!****************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE Jfunc1(k,l,t,Jout)

    USE mod_dataSpec_wavelength
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

    USE mod_dataSpec_wavelength
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

    IMPLICIT NONE

    !J2 function

    REAL*8 :: k,l,t
    REAL*8 :: Jfunc3

    Jfunc3=(1.-EXP(-(k+l)*t))/(k+l)

END FUNCTION Jfunc3
