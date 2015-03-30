      subroutine volscatt(tts,tto,psi,ttl,chi_s,chi_o,frho,ftau)
c
c	Compute volume scattering functions and interception coefficients
c	for given solar zenith, viewing zenith, azimuth and leaf inclination angle.
c
c	chi_s and chi_o are the interception functions, 
c	frho and ftau are the functions to be multiplied by leaf reflectance rho and
c	leaf transmittance tau, respectively, in order to obtain the volume scattering
c	function.
c
c	Wout Verhoef, april 2001, for CROMA
c
	implicit real (a-z)
c
      real*8 pi,rd

	pi= 3.14159265358979
	rd=pi/180.d0
c
      costs=cos(rd*tts)
      costo=cos(rd*tto)
      sints=sin(rd*tts)
      sinto=sin(rd*tto)
      cospsi=cos(rd*psi)
c
      psir=rd*psi
c
      costl=cos(rd*ttl)
      sintl=sin(rd*ttl)
      cs=costl*costs
      co=costl*costo
      ss=sintl*sints
      so=sintl*sinto
c
	cosbts=5.
      if (abs(ss).gt.1e-6) cosbts=-cs/ss
	cosbto=5.
      if (abs(so).gt.1e-6) cosbto=-co/so
c
      if (abs(cosbts).lt.1.d0) then
		bts=acos(cosbts)
		ds=ss
      else
		bts=pi
		ds=cs
      end if
      chi_s=2./pi*((bts-pi*.5)*cs+sin(bts)*ss)
c
      if (abs(cosbto).lt.1.d0) then
		bto=acos(cosbto)
		do=so
      else
		if (tto.lt.90.) then
			bto=pi
			do=co
		else
			bto=0
			do=-co
		end if		
      end if
      chi_o=2./pi*((bto-pi*.5)*co+sin(bto)*so)
c
      btran1=abs(bts-bto)
      btran2=pi-abs(bts+bto-pi)
c
      if (psir.le.btran1) then
		bt1=psir
		bt2=btran1
		bt3=btran2
      else
   		bt1=btran1
		if (psir.le.btran2) then
			bt2=psir
			bt3=btran2
		else
			bt2=btran2
			bt3=psir
		endif
      endif
c
      t1=2.*cs*co+ss*so*cospsi
	t2=0.
      if (bt2.gt.0.) t2=sin(bt2)*(2.*ds*do+ss*so*cos(bt1)*cos(bt3))
      denom=2.*pi*pi
      frho=((pi-bt2)*t1+t2)/denom
      ftau=    (-bt2*t1+t2)/denom
	if (frho.lt.0) frho=0
	if (ftau.lt.0) ftau=0
c	  
      return
      end
