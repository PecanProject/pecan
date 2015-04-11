! ********************************************************************************
! prospect_5B.f90
! ********************************************************************************
!
! Jean-Baptiste FERET
! 
! Department of Global Ecology / Carnegie Institution for Sciences
! 260 Panama Street
! Stanford, CA 94305, USA
! E-mail: jbferet@stanford.edu
!
! Stéphane JACQUEMOUD
!
! Université Paris Diderot / Institut de Physique du Globe de Paris
! 35 rue Hélène Brion
! 75013 Paris, France
! E-mail: jacquemoud@ipgp.fr
!
! http://teledetection.ipgp.fr/prosail/
!
! ********************************************************************************
! Féret J.B., François C., Asner G.P., Gitelson A.A., Martin R.E., Bidel L.P.R.,
! Ustin S.L., le Maire G., Jacquemoud S. (2008), PROSPECT-4 and 5: Advances in the
! leaf optical properties model separating photosynthetic pigments, Remote Sensing
! of Environment, 112:3030-3043.
! Jacquemoud S., Ustin S.L., Verdebout J., Schmuck G., Andreoli G., Hosgood B.
! (1996), Estimating leaf biochemistry using the PROSPECT leaf optical properties
! model, Remote Sensing of Environment, 56:194-202.
! Jacquemoud S., Baret F. (1990), PROSPECT: a model of leaf optical properties
! spectra, Remote Sensing of Environment, 34:75-91.
! ********************************************************************************
! version 5.02 (25 July 2011)
! ********************************************************************************

SUBROUTINE prospect_5B(N,Cab,Car,Cbrown,Cw,Cm,RT)

	USE MOD_dataSpec_P5B
	IMPLICIT none

REAL*8, intent(in) :: N,Cab,Car,Cbrown,Cw,Cm
REAL*8, intent(out) :: RT(nw,2)

REAL*8 :: k(nw), tau(nw), xx(nw), yy(nw)
REAL*8 :: x1(nw),x2(nw),x3(nw),x4(nw),x5(nw),x6(nw)
REAL*8 :: theta1, theta2, t1(nw),t2(nw)
REAL*8 :: r(nw),t(nw),ra(nw),ta(nw)
REAL*8 :: delta(nw),beta(nw),va(nw),vb(nw),s1(nw),s2(nw),s3(nw)

k=(Cab*k_Cab+Car*k_Car+Cbrown*k_Brown+Cw*k_Cw+Cm*k_Cm)/N

! ********************************************************************************
! reflectance and transmittance of one layer
! ********************************************************************************
! Allen W.A., Gausman H.W., Richardson A.J., Thomas J.R. (1969), Interaction of
! isotropic ligth with a compact plant leaf, Journal of the Optical Society of
! American, 59:1376-1379.
! ********************************************************************************

! exponential integral: S13AAF routine from the NAG library

where (k.le.0.0)
	tau=1
end where
where (k.gt.0.0.and.k.le.4.0)
	xx=0.5*k-1.0
	yy=(((((((((((((((-3.60311230482612224d-13 &
		*xx+3.46348526554087424d-12)*xx-2.99627399604128973d-11) &
		*xx+2.57747807106988589d-10)*xx-2.09330568435488303d-9) &
		*xx+1.59501329936987818d-8)*xx-1.13717900285428895d-7) &
		*xx+7.55292885309152956d-7)*xx-4.64980751480619431d-6) &
		*xx+2.63830365675408129d-5)*xx-1.37089870978830576d-4) &
		*xx+6.47686503728103400d-4)*xx-2.76060141343627983d-3) &
		*xx+1.05306034687449505d-2)*xx-3.57191348753631956d-2) &
		*xx+1.07774527938978692d-1)*xx-2.96997075145080963d-1
	yy=(yy*xx+8.64664716763387311d-1)*xx+7.42047691268006429d-1
	yy=yy-log(k)
	tau=(1.0-k)*dexp(-k)+k**2*yy
end where
where (k.gt.4.0.and.k.le.85.0)
	xx=14.5/(k+3.25)-1.0
	yy=(((((((((((((((-1.62806570868460749d-12 &
		*xx-8.95400579318284288d-13)*xx-4.08352702838151578d-12) &
		*xx-1.45132988248537498d-11)*xx-8.35086918940757852d-11) &
		*xx-2.13638678953766289d-10)*xx-1.10302431467069770d-9) &
		*xx-3.67128915633455484d-9)*xx-1.66980544304104726d-8) &
		*xx-6.11774386401295125d-8)*xx-2.70306163610271497d-7) &
		*xx-1.05565006992891261d-6)*xx-4.72090467203711484d-6) &
		*xx-1.95076375089955937d-5)*xx-9.16450482931221453d-5) &
		*xx-4.05892130452128677d-4)*xx-2.14213055000334718d-3
	yy=((yy*xx-1.06374875116569657d-2)*xx-8.50699154984571871d-2)*xx+9.23755307807784058d-1
	yy=exp(-k)*yy/k
	tau=(1.0-k)*dexp(-k)+k**2*yy
end where
where (k.gt.85.0)
	tau=0
end where

! transmissivity of the layer

theta1=90.
call tav_abs(theta1,refractive,t1)
theta2=40.
call tav_abs(theta2,refractive,t2)
x1=1-t1
x2=t1**2*tau**2*(refractive**2-t1)
x3=t1**2*tau*refractive**2
x4=refractive**4-tau**2*(refractive**2-t1)**2
x5=t2/t1
x6=x5*(t1-1)+1-t2
r=x1+x2/x4
t=x3/x4
ra=x5*r+x6
ta=x5*t

! ********************************************************************************
! reflectance and transmittance of N layers
! ********************************************************************************
! Stokes G.G. (1862), On the intensity of the light reflected from or transmitted
! through a pile of plates, Proceedings of the Royal Society of London, 11:545-556.
! ********************************************************************************

delta=(t**2-r**2-1)**2-4*r**2
beta=(1+r**2-t**2-sqrt(delta))/(2*r)
va=(1+r**2-t**2+sqrt(delta))/(2*r)
vb=sqrt(beta*(va-r)/(va*(beta-r)))
s1=ra*(va*vb**(N-1)-va**(-1)*vb**(-(N-1)))+(ta*t-ra*r)*(vb**(N-1)-vb**(-(N-1)))
s2=ta*(va-va**(-1))
s3=va*vb**(N-1)-va**(-1)*vb**(-(N-1))-r*(vb**(N-1)-vb**(-(N-1)))
RT(:,1)=s1/s3
RT(:,2)=s2/s3

end subroutine
