      subroutine FourSAIL2
c
c     4SAIL reflectance model (with singularity interception) for two leaf canopy layers
c     including treatment of the hot spot effect. The canopy layers are supposed to have 
c	identical LIDFs and hot spot parameter. The layers differ only in
c	leaf optical properties and LAI. The soil can be non-Lambertian.
c
c	Clumping as in forests can be modelled by specifying a crown coverage Cv less than one,
c	and a shape factor zeta indicating the ratio of crown diameter / crown height 
c     (at the crown's centre).
c
c	For SPECTRA validation project.
c	Developed out of its predecessor GeoSAIL, extended with recent 4SAIL-improvements and 
c	elements of the FLIM model.
c
c     Wout Verhoef
c     July 2003
c
c	Improved robustness by interception of conservative scattering case
c	March 2004
c
c	Improved crown weight contribution to bidirectional reflectance, (1 - Fos) instead of Fcd
c	May 2006
c
c     Second improved crown weight function Fcdc, which is based on Fcd, but goes to one for large
c	zenith angles
c	May 2008
c
c     Input parameters :
c
c     lai     : total leaf area index of canopy layer (brown + green)
c     fb      : fraction brown lai
c     diss    : dissociation factor [0,1]
c     LIDFa   : average leaf slope indicator [-1,+1]
c     LIDFb   : bimodality parameter of lidf [-1,+1]
c     hot	  : hot spot effect parameter, estimated as ratio of
c               average leaf width and canopy height
c     rg      : green leaf reflectance
c     rb      : brown leaf reflectance 
c     tg      : green leaf transmittance
c     tb      : brown leaf transmittance
c     rsosoil : background rso reflectance
c	rdosoil : background rdo reflectance
c	rsdsoil : background rsd reflectance
c	rddsoil : background rdd reflectance
c     tts     : solar zenith angle in degrees
c     tto     : viewing zenith angle in degrees
c     psi     : sun-view azimuth difference in degrees (relative azimuth)
c	Cv      : vertical crown coverage [0,1]
c	zeta    : tree shape factor (diameter/height)
c 
c     Output parameters :
c
c     rsot    : bidirectional reflectance
c     rdot    : directional reflectance for diffuse incidence
c	rsdt    : diffuse reflectance for direct solar incidence
c	rddt    : diffuse reflectance for diffuse incidence
c
	implicit integer (i-n), real (a-h,o-z)
c
      real lai1,lai2,lai_old,li(13),lidf(13),ks,ko,m,m2
	real LIDFa_old,LIDFb_old
      real Jfunc1,Jfunc2,Jfunc3,J3,J1ks,J1ko,J2ks,J2ko,ksli,koli

	real*8 alf,ca,fint,x1,x2,y1,y2,s1,s2,f1,f2
c
c
	logical delta_geom,delta_lidf,delta_fb,delta_lai,delta_hot,
     +    	  delta_forest
	logical do_scatter,do_hotspot
c
c	commom blocks FourSAIL2	
c
	common /staticvar/cts,cto,ctscto,lidf,ks,ko,dso,
     +						ddb,ddf,sdb,sdf,dob,dof,sob,sof,
     +						Cs,Co,Fcd,Fcs,Fod,Fos,Fcdc
	common /oldpar/LIDFa_old,LIDFb_old,lai_old,hot_old,fb_old,
     +					tts_old,tto_old,psi_old,Cv_old,zeta_old
c
	include 'SLC.h'
c
c
      data li/5.,15.,25.,35.,45.,55.,65.,75.,81.,83.,85.,87.,89./
	data ifirst/1/

c
c	Immediate return for lai = 0 
c
	if (lai.le.0.) then
c
		rdot=rdosoil
		rsot=rsosoil
		rsdt=rsdsoil
		rddt=rddsoil
		alfast=0.
		alfadt=0.
		tooc=1.
c
		return	
c
	end if
c
c	Detect which parameters have changed
c
	delta_geom=(tts.ne.tts_old).or.(tto.ne.tto_old).
     +				or.(psi.ne.psi_old)
	delta_lidf=(LIDFa.ne.LIDFa_old).or.(LIDFb.ne.LIDFb_old)
	delta_hot=(hot.ne.hot_old)
	delta_fb=(fb.ne.fb_old)
	delta_lai=(lai.ne.lai_old)
	delta_forest=(Cv.ne.Cv_old).or.(zeta.ne.zeta_old)
c
	do_scatter=delta_geom.or.delta_lidf.or.(ifirst.eq.1)
	do_hotspot=delta_geom.or.delta_hot.or.delta_lidf.or.delta_fb.
     +			  or.delta_lai.or.(ifirst.eq.1)
c
	if (delta_geom.or.delta_forest.or.(ifirst.eq.1)) then
c
c		Angular factors
c									
		cts=cos(rd*tts)
		cto=cos(rd*tto)
		ctscto=cts*cto
		tants=tan(rd*tts)
		tanto=tan(rd*tto)
		cspsi=cos(rd*psi)
		dso=sqrt(tants*tants+tanto*tanto-2.*tants*tanto*cspsi)
c
c		Clumping effects
c
		Cs=1
		Co=1
c
		If (Cv.lt.1) then
c
			Cs=1-(1-Cv)**(1./cts)
			Co=1-(1-Cv)**(1./cto)
c
		End if
c
		Overlap=0
c
		If (zeta.gt.0) Overlap=min(Cs*(1-Co),Co*(1-Cs))*exp(-dso/zeta)
c
		Fcd=Cs*Co+Overlap
		Fcs=(1-Cs)*Co-Overlap
		Fod=Cs*(1-Co)-Overlap
		Fos=(1-Cs)*(1-Co)+Overlap
c
		Fcdc= 1.-(1-Fcd)**(.5/cts+.5/cto)
c
	end if
c
c	Part depending on diss, fb, and leaf optical properties
c	First save the input fb as the old fb, as the following change is only artificial
c
c	Better define an fb that is actually used: fbu, so that the input is not modified!
c
	fbu=fb
c
	if (fb.eq.0) then
c
		fbu=0.5
		rb=rg
		tb=tg
c
	end if
c
	if (fb.eq.1) then
c
		fbu=0.5
		rg=rb
		tg=tb
c
	end if

      s=(1.-diss)*fbu*(1.-fbu)
c
      rho1=((1-fbu-s)*rg+s*rb)/(1-fbu)
      tau1=((1-fbu-s)*tg+s*tb)/(1-fbu)
      rho2=(s*rg+(fbu-s)*rb)/fbu
      tau2=(s*tg+(fbu-s)*tb)/fbu
c
c	Generate LIDF from (a,b) parameters
c
	if (delta_lidf.or.(ifirst.eq.1)) call ladgen(LIDFa,LIDFb,lidf)
c
	if (do_scatter) then
c
c		Calculate geometric factors associated with extinction and scattering 
c
c		Initialise sums
c
		ks=0
		ko=0
		bf=0
		sob=0
		sof=0
c
c		Weighted sums over LIDF
c      
		do ili=1,13
c
			ttl=li(ili)
			ctl=cos(rd*ttl)
c
c			SAIL volscatt function gives interception coefficients 
c			and two portions of the volume scattering phase function to be 
c			multiplied by rho and tau, respectively
c
			call volscatt(tts,tto,psi,ttl,chi_s,chi_o,frho,ftau)
c
c			Extinction coefficients
c
			ksli=chi_s/cts
			koli=chi_o/cto
c
c			Area scattering coefficient fractions
c
			sobli=frho*pi/ctscto
			sofli=ftau*pi/ctscto
c
			bfli=ctl*ctl
c
			ks=ks+ksli*lidf(ili)
			ko=ko+koli*lidf(ili)
			bf=bf+bfli*lidf(ili)
			sob=sob+sobli*lidf(ili)
			sof=sof+sofli*lidf(ili)
c
		end do
c
c		Geometric factors to be used later in combination with rho and tau
c
		sdb=0.5*(ks+bf)
		sdf=0.5*(ks-bf)
		dob=0.5*(ko+bf)
		dof=0.5*(ko-bf)
		ddb=0.5*(1.+bf)
		ddf=0.5*(1.-bf)
c
	end if
c
c     LAIs in two layers
c
	lai1=(1-fbu)*lai
	lai2=fbu*lai
c 
	if (do_hotspot) then
c   
c		Treatment of the hot spot effect for 2 layers	
c
	   tss=exp(-ks*lai)
	   ck=exp(-ks*lai1)
c
		alf=1D6
		if (hot.gt.0.) alf=(dso/hot)*2./(ks+ko)
         if (alf.gt.200.) alf=200.     !inserted H. Bach 1/3/04
c
		if (alf.eq.0.) then
c
c			The pure hotspot
c
			tsstoo=tss
			s1=(1-ck)/(ks*lai)
			s2=(ck-tss)/(ks*lai)
c
		else
c       
c			Outside the hotspot
c
			fhot=lai*sqrt(ko*ks)
c    
c			Integrate 2 layers by exponential simpson method in 20 steps
c			the steps are arranged according to equal partitioning
c			of the derivative of the joint probability function
c
			x1=0.
			y1=0.
			f1=1.
			ca=exp(alf*(fbu-1.))
			fint=(1.-ca)*.05
			s1=0.
c		
			do istep=1,20
c
				if (istep.lt.20) then 
    					x2=-log(1.-istep*fint)/alf
				else
					x2=1.-fbu
				end if
				y2=-(ko+ks)*lai*x2+fhot*(1.-exp(-alf*x2))/alf
				f2=exp(y2)
				s1=s1+(f2-f1)*(x2-x1)/(y2-y1)
				x1=x2
				y1=y2
				f1=f2
c
			end do
c
			fint=(ca-exp(-alf))*.05
			s2=0.
c
			do istep=1,20
c
				if (istep.lt.20) then
					x2=-log(ca-istep*fint)/alf
				else
					x2=1.
				end if
				y2=-(ko+ks)*lai*x2+fhot*(1.-exp(-alf*x2))/alf
				f2=exp(y2)
				s2=s2+(f2-f1)*(x2-x1)/(y2-y1)
 				x1=x2
				y1=y2
				f1=f2
c
			end do
c
			tsstoo=f1
c
		end if
c
	end if	
c
c     Calculate reflectances and transmittances
c
c     Bottom layer
c
	tss=exp(-ks*lai2)
	too=exp(-ko*lai2)
c
	sb=sdb*rho2+sdf*tau2
	sf=sdf*rho2+sdb*tau2
c
	vb=dob*rho2+dof*tau2
	vf=dof*rho2+dob*tau2
c
	w2=sob*rho2+sof*tau2
c
      sigb=ddb*rho2+ddf*tau2
	sigf=ddf*rho2+ddb*tau2
      att=1.-sigf
c
	m2=(att+sigb)*(att-sigb)
	if (m2.lt.0) m2=0
	m=sqrt(m2)
c
	if (m.gt.0.01) then
c
c		Normal case
c
		e1=exp(-m*lai2)
		e2=e1*e1
		rinf=(att-m)/sigb
		rinf2=rinf*rinf
		re=rinf*e1
		denom=1.-rinf2*e2
c
		J1ks=Jfunc1(ks,m,lai2)
		J2ks=Jfunc2(ks,m,lai2)
		J1ko=Jfunc1(ko,m,lai2)
		J2ko=Jfunc2(ko,m,lai2)
c
		Ps=(sf+sb*rinf)*J1ks
		Qs=(sf*rinf+sb)*J2ks
		Pv=(vf+vb*rinf)*J1ko
		Qv=(vf*rinf+vb)*J2ko
c
		tdd=(1.-rinf2)*e1/denom
		rdd=rinf*(1.-e2)/denom
		tsd=(Ps-re*Qs)/denom
		rsd=(Qs-re*Ps)/denom
		tdo=(Pv-re*Qv)/denom
		rdo=(Qv-re*Pv)/denom
c
		z=Jfunc2(ks,ko,lai2)
		g1=(z-J1ks*too)/(ko+m)
		g2=(z-J1ko*tss)/(ks+m)
c
		Tv1=(vf*rinf+vb)*g1
		Tv2=(vf+vb*rinf)*g2
c		
		T1=Tv1*(sf+sb*rinf)
		T2=Tv2*(sf*rinf+sb)
		T3=(rdo*Qs+tdo*Ps)*rinf
c
c		Multiple scattering contribution to bidirectional canopy reflectance
c	
		rsod=(T1+T2-T3)/(1.-rinf2)
c
	else
c
c		Near or complete conservative scattering
c
		J3=Jfunc3(m,lai2)
		amsig=att-sigb
		apsig=att+sigb
		rtp=(1-amsig*J3)/(1+amsig*J3)
		rtm=(-1+apsig*J3)/(1+apsig*J3)
		rdd=.5*(rtp+rtm)
		tdd=.5*(rtp-rtm)
c
		dns=ks*ks-m*m
		dno=ko*ko-m*m
		cks=(sb*(ks-att)-sf*sigb)/dns
		cko=(vb*(ko-att)-vf*sigb)/dno
		dks=(-sf*(ks+att)-sb*sigb)/dns
		dko=(-vf*(ko+att)-vb*sigb)/dno
		ho=(sf*cko+sb*dko)/(ko+ks)				
c
		rsd=cks*(1-tss*tdd)-dks*rdd
		rdo=cko*(1-too*tdd)-dko*rdd
		tsd=dks*(tss-tdd)-cks*tss*rdd
		tdo=dko*(too-tdd)-cko*too*rdd
c
c		Multiple scattering contribution to bidirectional canopy reflectance
c	
		rsod=ho*(1-tss*too)-cko*tsd*too-dko*rsd
c
	end if
c
c     Set background properties equal to those of the bottom layer on a black soil
c
      rddb=rdd
      rsdb=rsd
      rdob=rdo
      rsodb=rsod
	tddb=tdd
	tsdb=tsd
	tdob=tdo
	toob=too
	tssb=tss
c
c     Top layer
c
	tss=exp(-ks*lai1)
	too=exp(-ko*lai1)
c
	sb=sdb*rho1+sdf*tau1
	sf=sdf*rho1+sdb*tau1
c
	vb=dob*rho1+dof*tau1
	vf=dof*rho1+dob*tau1
c
	w1=sob*rho1+sof*tau1
c
      sigb=ddb*rho1+ddf*tau1
	sigf=ddf*rho1+ddb*tau1
      att=1.-sigf
c
	m2=(att+sigb)*(att-sigb)
	if (m2.lt.0) m2=0
	m=sqrt(m2)
c
	if (m.gt.0.01) then
c
c		Normal case
c
		e1=exp(-m*lai1)
		e2=e1*e1
		rinf=(att-m)/sigb
		rinf2=rinf*rinf
		re=rinf*e1
		denom=1.-rinf2*e2
c
		J1ks=Jfunc1(ks,m,lai1)
		J2ks=Jfunc2(ks,m,lai1)
		J1ko=Jfunc1(ko,m,lai1)
		J2ko=Jfunc2(ko,m,lai1)
c
		Ps=(sf+sb*rinf)*J1ks
		Qs=(sf*rinf+sb)*J2ks
		Pv=(vf+vb*rinf)*J1ko
		Qv=(vf*rinf+vb)*J2ko
c
		tdd=(1.-rinf2)*e1/denom
		rdd=rinf*(1.-e2)/denom
		tsd=(Ps-re*Qs)/denom
		rsd=(Qs-re*Ps)/denom
		tdo=(Pv-re*Qv)/denom
		rdo=(Qv-re*Pv)/denom
c
		z=Jfunc2(ks,ko,lai1)
		g1=(z-J1ks*too)/(ko+m)
		g2=(z-J1ko*tss)/(ks+m)
c
		Tv1=(vf*rinf+vb)*g1
		Tv2=(vf+vb*rinf)*g2
c		
		T1=Tv1*(sf+sb*rinf)
		T2=Tv2*(sf*rinf+sb)
		T3=(rdo*Qs+tdo*Ps)*rinf
c
c		Multiple scattering contribution to bidirectional canopy reflectance
c	
		rsod=(T1+T2-T3)/(1.-rinf2)
c
	else
c
c		Near or complete conservative scattering
c
		J3=Jfunc3(m,lai1)
		amsig=att-sigb
		apsig=att+sigb
		rtp=(1-amsig*J3)/(1+amsig*J3)
		rtm=(-1+apsig*J3)/(1+apsig*J3)
		rdd=.5*(rtp+rtm)
		tdd=.5*(rtp-rtm)
c
		dns=ks*ks-m*m
		dno=ko*ko-m*m
		cks=(sb*(ks-att)-sf*sigb)/dns
		cko=(vb*(ko-att)-vf*sigb)/dno
		dks=(-sf*(ks+att)-sb*sigb)/dns
		dko=(-vf*(ko+att)-vb*sigb)/dno
		ho=(sf*cko+sb*dko)/(ko+ks)				
c
		rsd=cks*(1-tss*tdd)-dks*rdd
		rdo=cko*(1-too*tdd)-dko*rdd
		tsd=dks*(tss-tdd)-cks*tss*rdd
		tdo=dko*(too-tdd)-cko*too*rdd
c
c		Multiple scattering contribution to bidirectional canopy reflectance
c	
		rsod=ho*(1-tss*too)-cko*tsd*too-dko*rsd
c
	end if
c
c     Combine with bottom layer reflectances and transmittances (adding method)
c
      rn=1.-rdd*rddb
      tup=(tss*rsdb+tsd*rddb)/rn
      tdn=(tsd+tss*rsdb*rdd)/rn
      rsdt=rsd+tup*tdd
      rdot=rdo+tdd*(rddb*tdo+rdob*too)/rn
      rsodt=rsod+(tss*rsodb+tdn*rdob)*too+tup*tdo
c
	rsost=(w1*s1+w2*s2)*lai
c	    
      rsot=rsost+rsodt
c
c	Diffuse reflectances at the top and the bottom are now different 
c
	rddt_t=rdd+tdd*rddb*tdd/rn
	rddt_b=rddb+tddb*rdd*tddb/rn
c
c	Transmittances of the combined canopy layers
c
	tsst=tss*tssb
	toot=too*toob
	tsdt=tss*tsdb+tdn*tddb
	tdot=tdob*too+tddb*(tdo+rdd*rdob*too)/rn
	tddt=tdd*tddb/rn
c
c	Apply clumping effects to vegetation layer
c
	rddcb=Cv*rddt_b
	rddct=Cv*rddt_t
	tddc=1-Cv+Cv*tddt
	rsdc=Cs*rsdt
	tsdc=Cs*tsdt
	rdoc=Co*rdot
	tdoc=Co*tdot
	tssc=1-Cs+Cs*tsst
	tooc=1-Co+Co*toot

c	New weight function Fcdc for crown contribution (W. Verhoef, 22-05-08)
	
	rsoc=Fcdc*rsot
	tssooc=Fcd*tsstoo+Fcs*toot+Fod*tsst+Fos
c
c	Canopy absorptance for black background (W. Verhoef, 02-03-04)
c
	alfas=1.-tssc-tsdc-rsdc
	alfad=1.-tddc-rddct
c
c	Add the soil background 
c
	rn=1-rddcb*rddsoil
      tup=(tssc*rsdsoil+tsdc*rddsoil)/rn
      tdn=(tsdc+tssc*rsdsoil*rddcb)/rn
c
	rddt=rddct+tddc*rddsoil*tddc/rn
      rsdt=rsdc+tup*tddc
      rdot=rdoc+tddc*(rddsoil*tdoc+rdosoil*tooc)/rn
      rsot=rsoc+tssooc*rsosoil+tdn*rdosoil*tooc+tup*tdoc
c
c	Effect of soil background on canopy absorptances (W. Verhoef, 02-03-04)
c
	alfast=alfas+tup*alfad
	alfadt=alfad*(1.+tddc*rddsoil/rn)
c
c	Before returning, save current input parameters as old ones
c
	lai_old=lai
	LIDFa_old=LIDFa
	LIDFb_old=LIDFb
	fb_old=fb
	hot_old=hot
	Cv_old=Cv
	zeta_old=zeta
	tts_old=tts
	tto_old=tto
	psi_old=psi
c
	ifirst=0
c
      return
	end


	real function Jfunc1(k,l,t)
c
c	J1 function with avoidance of singularity problem
c	
	real k,l,t,del
c
	del=(k-l)*t
	if (abs(del)>1e-3) then
		Jfunc1=(exp(-l*t)-exp(-k*t))/(k-l)
	else
		Jfunc1=0.5*t*(exp(-k*t)+exp(-l*t))*(1.-del*del/12.)
	end if	
c
	return
	end


	real function Jfunc2(k,l,t)
c
c	J2 function
c
	real k,l,t
c
	Jfunc2=(1.-exp(-(k+l)*t))/(k+l)
c
	return
	end


	real function Jfunc3(m,t)
c
c	J3 function for treating (near) conservative scattering
c
	real m,t,del
c
	del=m*t
c
	if (del.gt.1e-3) then
c
		e=exp(-del)
		Jfunc3=(1-e)/(m*(1+e))
c
	else
c
		Jfunc3=0.5*t*(1.-del*del/12.)
c
	end if
c
	return
	end
