subroutine FOURSAIL(rho, tau,             & !! Leaf spectra
        LIDFa,LIDFb,TypeLIDF, & !! LIDF parameters and function
        lai,q,                & !! LAI and hot spot
        tts,tto,psi,          & !! Instrument geometry
        rsoil,                & !! Soil reflectance
        nw,                   & !! Number of wavelengths

        !! Outputs
        rddt,                 & !! Bi-hemispherical reflectance
            rsdt,                 & !! Directional-hemispherical reflectance for solar incident flux
            rdot,                 & !! Hemispherical-directional reflectance in viewing direction
            rsot)                   !! Bi-directional reflectance factor

        implicit none
        real*8,intent(in),dimension(nw) :: rho,tau,rsoil
        REAL*8,INTENT(in) :: LIDFa,LIDFb,lai,q,tts,tto,psi
        INTEGER,INTENT(in) :: TypeLIDF
        integer,intent(in) :: nw

        common /angle/ pi,rd
        real*8 :: pi,rd

        real*8,intent(out),dimension(nw) :: rddt,rsdt,rdot,rsot

        INTEGER*4 :: na
        parameter(na = 13)
        real*8 :: cts,cto,ctscto,dso,lidf(na),litab(na)
        real*8 :: ks,ko,sob,sof,sdb,sdf,dob,dof,ddb,ddf,tss,too
        real*8 :: tsstoo,sumint
        real*8,dimension(nw) :: sigb,att,m,sb,sf,vb,vf,w
        real*8,dimension(nw) :: rdd,tdd,tsd,rsd,tdo,rdo,rsod

        data litab/5.,15.,25.,35.,45.,55.,65.,75.,81.,83.,85.,87.,89./

        pi = 4 * atan(1.)
        rd = pi / 180.

        !! FLAG 1 - Geometry: Compute geometric quantities
        CALL sail_sensgeom(tts,tto,psi, &
            cts,cto,ctscto,dso)

        !! FLAG 2 - Leaf angle distribution function
        CALL LIDF_fun(TypeLIDF, na, LIDFa, LIDFb, &
            lidf)

        !! FLAG 3 (if 1 or 2): Angular distance -- compensation of shadow length
        CALL SUITS(na,litab,lidf,tts,tto,cts,cto,psi,ctscto, &
            ks,ko,sob,sof,sdb,sdf,dob,dof,ddb,ddf)

        !! FLAG 4 (if 3 or leaf refl) -- Geometric leaf reflectance
        !!      Input rho and tau (from PROSPECT)
        !!      Correct using SUITS factors
        CALL RTgeom(nw,rho,tau,ddb,ddf,sdb,sdf,dob,dof,sob,sof, &
            sigb,att,m,sb,sf,vb,vf,w)

        !! FLAG 5 (if 4 or LAI or soil) -- Canopy reflectance and transmittance
        !!      Input LAI and leaf rho and tau
        !!      Also give geometric factors
        CALL ReflTrans(nw,rho,tau,lai,att,m,sigb,ks,ko,sf,sb,vf,vb, &
            rdd,tdd,tsd,rsd,tdo,rdo,tss,too,rsod)

        !! FLAG 6 (if 3 or LAI or hot) -- Hot spot effect
        CALL HotSpot(lai,q,tss,ks,ko,dso, &
            tsstoo,sumint)

        !! Bidirectional reflectance
        CALL sail_BDRF(nw,w,lai,sumint,tsstoo,rsoil, &
                rdd,tdd,tsd,rsd,tdo,rdo,tss,too,rsod, &
                rddt,rsdt,rdot,rsot)

        return
    end subroutine FOURSAIL
