! ********************************************************************************
! tav_abs.f90
! ********************************************************************************
! computation of the average transmittivity at the leaf surface within a given
! solid angle. teta is the incidence solid angle (in radian). The average angle
! that works in most cases is 40deg*pi/180. ref is the refaction index.
! ********************************************************************************
! Stern F. (1964), Transmission of isotropic radiation across an interface between
! two dielectrics, Applied Optics, 3:111-113.
! Allen W.A. (1973), Transmission of isotropic light across a dielectric surface in
! two and three dimensions, Journal of the Optical Society of America, 63:664-666.
! ********************************************************************************
! version (12 April 2015)
! ********************************************************************************

subroutine tav_abs(theta,refr,res)

    use mod_dataSpec_wavelength
    IMPLICIT none
    REAL*8, intent(in) :: theta, refr(nw)
    REAL*8, intent(out) :: res(nw)

    REAL*8 :: pi, thetarad
    REAL*8 :: refr2(nw)
    REAL*8 :: ax(nw),bx(nw),b0(nw),b1(nw),b2(nw)
    REAL*8 :: ts(nw),tp(nw),tp1(nw),tp2(nw),tp3(nw),tp4(nw),tp5(nw)

    pi       = atan(1.)*4.
    thetarad = pi*theta/180.

    if (theta.eq.0.) then
        res=4.*refr/(refr+1.)**2
        return
    endif

    refr2 = refr*refr
    ax    = (refr+1.)**2/2.
    bx    = -(refr2-1.)**2/4.

    if (thetarad.eq.pi/2.) then
        b1=0.
    else
        b1=dsqrt((sin(thetarad)**2-(refr2+1.)/2.)**2+bx)
    endif
    b2  = sin(thetarad)**2-(refr2+1.)/2.
    b0  = b1-b2
    ts  = (bx**2/(6.*b0**3)+bx/b0-b0/2.)-(bx**2/(6.*ax**3)+bx/ax-ax/2.)
    tp1 = -2.*refr2*(b0-ax)/(refr2+1.)**2
    tp2 = -2.*refr2*(refr2+1.)*dlog(b0/ax)/(refr2-1.)**2
    tp3 = refr2*(1./b0-1./ax)/2.
    tp4 = 16.*refr2**2*(refr2**2+1.)*dlog((2.*(refr2+1.)*b0-(refr2-1.)**2)/ &
        (2.*(refr2+1.)*ax-(refr2-1.)**2))/((refr2+1.)**3*(refr2-1.)**2)
    tp5 = 16.*refr2**3*(1./(2.*(refr2+1.)*b0-((refr2-1.)**2))-1./(2.*(refr2+1.) &
        *ax-(refr2-1.)**2))/(refr2+1.)**3
    tp  = tp1+tp2+tp3+tp4+tp5
    res = (ts+tp)/(2.*sin(thetarad)**2)

    return
    end
