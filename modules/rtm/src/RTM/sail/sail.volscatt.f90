SUBROUTINE volscatt(tts,tto,psi,ttl,chi_s,chi_o,frho,ftau)

    !********************************************************************************
    !*tts= solar zenith
    !*tto= viewing zenith
    !*psi= azimuth
    !*ttl= leaf inclination angle
    !*chi_s= interception functions
    !*chi_o= interception functions
    !*frho= function to be multiplied by leaf reflectance rho
    !*ftau= functions to be multiplied by leaf transmittance tau
    !********************************************************************************

    !Compute volume scattering functions and interception coefficients
    !for given solar zenith, viewing zenith, azimuth and leaf inclination angle.

    !chi_s and chi_o are the interception functions.
    !frho and ftau are the functions to be multiplied by leaf reflectance rho and
    !leaf transmittance tau, respectively, in order to obtain the volume scattering
    !function.

    !Wout Verhoef, april 2001, for CROMA

    IMPLICIT NONE

    REAL*8,INTENT(in) :: tts,tto,psi,ttl
    REAL*8,INTENT(inout) :: chi_s,chi_o,frho,ftau

    REAL*8 :: pi,rd
    common /angle/ pi,rd

    REAL*8 costs,costo,sints,sinto,cospsi
    REAL*8 psir
    REAL*8 costl,sintl,cs,co,ss,so,ds
    REAL*8 cosbts,cosbto,bts,bto
    REAL*8 btran1,btran2,bt1,bt2,bt3,t1,t2
    REAL*8 doo,denom

    costs=COS(rd*tts)
    costo=COS(rd*tto)
    sints=SIN(rd*tts)
    sinto=SIN(rd*tto)
    cospsi=COS(rd*psi)

    psir=rd*psi

    costl=COS(rd*ttl)
    sintl=SIN(rd*ttl)
    cs=costl*costs
    co=costl*costo
    ss=sintl*sints
    so=sintl*sinto

    !c ..............................................................................
    !c     betas -bts- and betao -bto- computation
    !c     Transition angles (beta) for solar (betas) and view (betao) directions
    !c     if thetav+thetal>pi/2, bottom side of the leaves is observed for leaf azimut 
    !c     interval betao+phi<leaf azimut<2pi-betao+phi.
    !c     if thetav+thetal<pi/2, top side of the leaves is always observed, betao=pi
    !c     same consideration for solar direction to compute betas
    !c ..............................................................................


    cosbts=5.
    IF (ABS(ss).gt.1e-6) THEN
        cosbts=-cs/ss
    ENDIF

    cosbto=5.
    IF (ABS(so).gt.1e-6) THEN
        cosbto=-co/so
    ENDIF

    IF (ABS(cosbts).lt.1.d0) THEN
        bts=ACOS(cosbts)
        ds=ss
    ELSE
        bts=pi
        ds=cs
    ENDIF

    chi_s=2./pi*((bts-pi*.5)*cs+sin(bts)*ss)

    IF (ABS(cosbto).lt.1.d0) THEN
        bto=ACOS(cosbto)
        doo=so
    ELSEIF(tto.lt.90.) THEN
        bto=pi
        doo=co
    ELSE
        bto=0
        doo=-co
    ENDIF

    chi_o=2./pi*((bto-pi*.5)*co+SIN(bto)*so)

    !c ..............................................................................
    !c   Computation of auxiliary azimut angles bt1, bt2, bt3 used          
    !c   for the computation of the bidirectional scattering coefficient w              
    !c .............................................................................


    btran1=abs(bts-bto)
    btran2=pi-abs(bts+bto-pi)

    IF (psir.le.btran1) THEN
        bt1=psir
        bt2=btran1
        bt3=btran2
    ELSE
        bt1=btran1
        IF (psir.le.btran2) THEN
            bt2=psir
            bt3=btran2
        ELSE
            bt2=btran2
            bt3=psir
        ENDIF
    ENDIF

    t1=2.*cs*co+ss*so*cospsi
    t2=0.
    IF (bt2.gt.0.) THEN
        t2=SIN(bt2)*(2.*ds*doo+ss*so*COS(bt1)*COS(bt3))
    ENDIF

    denom=2.*pi*pi
    frho=((pi-bt2)*t1+t2)/denom
    ftau=    (-bt2*t1+t2)/denom

    IF (frho.lt.0) THEN
        frho=0
    ENDIF

    IF (ftau.lt.0) THEN
        ftau=0
    ENDIF

    RETURN
END SUBROUTINE volscatt
