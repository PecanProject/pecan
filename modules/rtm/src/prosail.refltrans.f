subroutine ReflTrans(nw,rho,tau,lai,att,m,sigb,ks,ko,sf,sb,vf,vb, &
        rdd,tdd,tsd,rsd,tdo,rdo,tss,too,rsod)

    implicit none
    integer,intent(in) :: nw
    real*8,intent(in) :: lai,ks,ko
    real*8,intent(in),dimension(nw) :: rho,tau,att,m,sigb,sf,sb,vf,vb
    real*8,intent(out),dimension(nw) :: rdd,tdd,tsd,rsd,tdo,rdorsod
    real*8,intent(out) :: tss,too

    real*8,dimension(nw) :: e1,e2,rinf,rinf2,re,denom,J1ks,J2ks,J1ko,J2ko
    real*8,dimension(nw) :: Ps,Qs,Pv,Qv,z,g1,g2,TV1,TV2,T1,T2,T3
    real*8 :: Jfunc3

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
