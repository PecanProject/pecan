subroutine gpm(k,N,RT)
    use MOD_dataSpec_wavelength
    use MOD_dataSpec_refractive
    implicit none

    real*8, intent(in) :: k(nw), N
    real*8, intent(out) :: RT(nw,2)

    real*8 :: theta1, theta2
    real*8, dimension(nw) :: tau,t1,t2,x1,x2,x3,x4,x5,x6,r,t,ra,ta 
    real*8, dimension(nw) :: delta,beta,va,vb,s1,s2,s3

    ! ********************************************************************************
    ! reflectance and transmittance of one layer
    ! ********************************************************************************
    ! Allen W.A., Gausman H.W., Richardson A.J., Thomas J.R. (1969), Interaction of
    ! isotropic ligth with a compact plant leaf, Journal of the Optical Society of
    ! American, 59:1376-1379.
    ! ********************************************************************************

    Call expint(k,tau)
    ! transmissivity of the layer
    
    theta1  = 90.
    call tav_abs(theta1,refractive,t1)
    theta2  = 40.
    call tav_abs(theta2,refractive,t2)
    x1      = 1-t1
    x2      = t1**2*tau**2*(refractive**2-t1)
    x3      = t1**2*tau*refractive**2
    x4      = refractive**4-tau**2*(refractive**2-t1)**2
    x5      = t2/t1
    x6      = x5*(t1-1)+1-t2
    r       = x1+x2/x4
    t       = x3/x4
    ra      = x5*r+x6
    ta      = x5*t
    
    ! ********************************************************************************
    ! reflectance and transmittance of N layers
    ! ********************************************************************************
    ! Stokes G.G. (1862), On the intensity of the light reflected from or transmitted
    ! through a pile of plates, Proceedings of the Royal Society of London, 11:545-556.
    ! ********************************************************************************
    
    delta   = (t**2-r**2-1)**2-4*r**2
    beta    = (1+r**2-t**2-sqrt(delta))/(2*r)
    va      = (1+r**2-t**2+sqrt(delta))/(2*r)
    vb      = sqrt(beta*(va-r)/(va*(beta-r)))
    s1      = ra*(va*vb**(N-1)-va**(-1)*vb**(-(N-1)))+(ta*t-ra*r)*(vb**(N-1)-vb**(-(N-1)))
    s2      = ta*(va-va**(-1))
    s3      = va*vb**(N-1)-va**(-1)*vb**(-(N-1))-r*(vb**(N-1)-vb**(-(N-1)))
    RT(:,1) = s1/s3
    RT(:,2) = s2/s3
    
end subroutine
