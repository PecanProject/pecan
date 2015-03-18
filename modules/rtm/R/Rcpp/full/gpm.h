// NAME: gpm
// TITLE: Generalized plate model
// DESCRIPTION: Returns reflectance of a leaf with "N" layers.
double gpm(double tao1,
        double tao2,
        double rho1, 
        double rho2, 
        double x, 
        double y, 
        double theta, 
        double N)
{
    double rhoa, taoa, rho90, tao90;
    double d90, a90, b90, nmR, nmT, dmRT;
    double RNa;
    // Reflectance and transmittance of first layer (N=1)
    rhoa = rho1 + (tao1 * tao2 * rho2 * theta*theta) / (1 - rho2*rho2 * theta*theta);
    taoa = tao1 * tao2 * theta / (1 - rho2*rho2 * theta*theta);
    rho90 = (rhoa - y) / x;
    tao90 = taoa / x;
    // Reflectance and transmittance of N layers (Stokes coefficients)
    d90 = sqrt((tao90*tao90 - rho90*rho90 - 1.0)*(tao90*tao90 - rho90*rho90 - 1.0) - 4.0*rho90*rho90);
    a90 = (1.0 + rho90*rho90 - tao90*tao90 + d90) / (2.0*rho90);
    b90 = (1.0 - rho90*rho90 + tao90*tao90 + d90) / (2.0*tao90);
    nmR = taoa * tao90 * (pow(b90,(N-1.0)) - pow(b90,(1.0-N)));
    //nmT = taoa * (a90 - 1/a90);
    dmRT = a90*pow(b90, (N-1.0)) - pow(b90, (1.0-N))/a90 - rho90 * (pow(b90, (N-1.0)) - pow(b90,(1.0-N)));
    RNa = rhoa + nmR / dmRT;
    //TNa = nmT / dmRT;
    return RNa;
}
