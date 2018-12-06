! Calculate SUITS coefficients
subroutine SUITS(na,litab,lidf,tts,tto,cts,cto,psi,ctscto, &
        ks,ko,sob,sof,sdb,sdf,dob,dof,ddb,ddf)

    implicit none
    integer, intent(in) :: na
    REAL*8, INTENT(in) :: litab(na),lidf(na),tts,tto,cts,cto,psi,ctscto
    REAL*8, INTENT(out) :: ks,ko,sob,sof,sdb,sdf,dob,dof,ddb,ddf

    real*8 :: pi,rd
    common /angle/ pi,rd
    REAL*8 :: bf,ttl,ctl,chi_s,chi_o,frho,ftau,ksli,koli,sobli,sofli,bfli
    integer :: i

    !	Calculate geometric factors associated with extinction and scattering 
    !	Initialise sums
    ks  = 0
    ko  = 0
    bf  = 0
    sob = 0
    sof = 0

    !	Weighted sums over LIDF
    DO i = 1,na
        ttl = litab(i)      ! leaf inclination discrete values
        ctl = COS(rd*ttl)
        !	SAIL volume scattering phase function gives interception and portions to be 
        !	multiplied by rho and tau
        CALL volscatt(tts,tto,psi,ttl,chi_s,chi_o,frho,ftau)

        !********************************************************************************
        !*                   SUITS SYSTEM COEFFICIENTS 
        !*
        !*	ks  : Extinction coefficient for direct solar flux
        !*	ko  : Extinction coefficient for direct observed flux
        !*	att : Attenuation coefficient for diffuse flux
        !*	sigb : Backscattering coefficient of the diffuse downward flux
        !*	sigf : Forwardscattering coefficient of the diffuse upward flux
        !*	sf  : Scattering coefficient of the direct solar flux for downward diffuse flux
        !*	sb  : Scattering coefficient of the direct solar flux for upward diffuse flux
        !*	vf   : Scattering coefficient of upward diffuse flux in the observed direction
        !*	vb   : Scattering coefficient of downward diffuse flux in the observed direction
        !*	w   : Bidirectional scattering coefficient
        !********************************************************************************

        !	Extinction coefficients
        ksli = chi_s/cts
        koli = chi_o/cto

        !	Area scattering coefficient fractions
        sobli = frho*pi/ctscto
        sofli = ftau*pi/ctscto
        bfli  = ctl*ctl
        ks    = ks+ksli*lidf(i)
        ko    = ko+koli*lidf(i)
        bf    = bf+bfli*lidf(i)
        sob   = sob+sobli*lidf(i)
        sof   = sof+sofli*lidf(i)

    ENDDO

    !	Geometric factors to be used later with rho and tau
    sdb = 0.5*(ks+bf)
    sdf = 0.5*(ks-bf)
    dob = 0.5*(ko+bf)
    dof = 0.5*(ko-bf)
    ddb = 0.5*(1.+bf)
    ddf = 0.5*(1.-bf)

    return
end subroutine SUITS

! *****************

!! Correct rho and tau using SUITS factors
subroutine RTgeom(rho,tau,ddb,ddf,sdb,sdf,dob,dof,sob,sof, &
        sigb,att,m,sb,sf,vb,vf,w)

    use mod_dataSpec_wavelength
    implicit none
    real*8,intent(in),dimension(nw) :: rho,tau
    real*8,intent(in) :: ddb,ddf,sdb,sdf,dob,dof,sob,sof
    real*8,intent(out),dimension(nw) :: sigb,att,m,sb,sf,vb,vf,w

    real*8,dimension(nw) :: m2,sigf

    sigb = ddb*rho+ddf*tau
    sigf = ddf*rho+ddb*tau
    att  = 1.-sigf
    m2   = (att+sigb)*(att-sigb)
    WHERE (m2 < 0) 
        m2 = 0
    ENDWHERE
    m  = SQRT(m2)
    sb = sdb*rho+sdf*tau
    sf = sdf*rho+sdb*tau
    vb = dob*rho+dof*tau
    vf = dof*rho+dob*tau
    w  = sob*rho+sof*tau

    return
end subroutine RTgeom
