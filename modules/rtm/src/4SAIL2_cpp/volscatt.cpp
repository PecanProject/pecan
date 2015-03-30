//	Compute volume scattering functions and interception coefficients
//	for given solar zenith, viewing zenith, azimuth and leaf inclination angle.
//
//	chi_s and chi_o are the interception functions, 
//	frho and ftau are the functions to be multiplied by leaf reflectance rho and
//	leaf transmittance tau, respectively, in order to obtain the volume scattering
//	function.
//
//	Wout Verhoef, april 2001, for CROMA

void volscatt(tts,tto,psi,ttl,chi_s,chi_o,frho,ftau){
    rd = M_PI / 180;

    costs = cos(rd*tts);
    costo = cos(rd*tto);
    sints = sin(rd*tts);
    sinto = sin(rd*tto);
    cospsi = cos(rd*psi);

    psir = rd*psi;

    costl = cos(rd*ttl);
    sintl = sin(rd*ttl);
    cs = costl*costs;
    co = costl*costo;
    ss = sintl*sints;
    so = sintl*sinto;

    cosbts = 5;
    if (abs(ss) > 1e-6) cosbts = -cs/ss;
    cosbto = 5;
    if (abs(so) > 1e-6) cosbto = -co/so;

    if (abs(cosbts) < 1.d0) {
        bts = acos(cosbts);
        ds = ss;
    } else {
        bts = pi;
        ds = cs;
    }

      chi_s = 2./pi*((bts-pi*.5)*cs+sin(bts)*ss);

      if (abs(cosbto) < 1){
		bto = acos(cosbto);
		doo = so;
      } else {
		if (tto < 90.){
			bto = M_PI;
			doo = co;
                } else {
			bto = 0;
			doo = -co;
                }		
      }
      chi_o = 2./pi*((bto-pi*.5)*co+sin(bto)*so);

      btran1 = abs(bts-bto);
      btran2 = pi-abs(bts+bto-pi);

      if (psir.le.btran1) {
		bt1 = psir;
		bt2 = btran1;
		bt3 = btran2;
      } else {
   		bt1 = btran1;
		if (psir.le.btran2) {
			bt2 = psir;
			bt3 = btran2;
                } else {
			bt2 = btran2;
			bt3 = psir;
                }
      }

      t1 = 2.*cs*co+ss*so*cospsi;
	t2 = 0;
        if (bt2 > 0.) t2 = sin(bt2)*(2.*ds*doo+ss*so*cos(bt1)*cos(bt3));
        denom = 2.*pi*pi;
        frho = ((pi-bt2)*t1+t2)/denom;
        ftau = (-bt2*t1+t2)/denom;
        if (frho < 0) frho = 0;
        if (ftau < 0) ftau = 0;
	  
      return;
}

