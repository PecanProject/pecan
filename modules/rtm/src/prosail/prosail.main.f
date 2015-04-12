!! PRO4SAIL model
!!      Author: Alexey Shiklomanov

SUBROUTINE PRO4SAIL( &
        !! Inputs
        N,Cab,Car,Cbrown,Cw,Cm,     &   !! PROSPECT parameters 
        LIDFa,LIDFb,TypeLIDF,       &   !! LIDF parameters and function
        lai,q,                      &   !! LAI and hot spot
        tts,tto,psi,                &   !! Sun-sensor geometry
        psoil,                      &   !! Soil moisture

        !! Outputs
        rddt,   &   !! Bi-hemispherical reflectance
        rsdt,   &   !! Directional-hemispherical reflectance for solar incident flux
        rdot,   &   !! Hemispherical-directional reflectance in viewing direction
        rsot)       !! Bi-directional reflectance factor

        use MOD_dataSpec_P5B
        IMPLICIT NONE
        integer,intent(in) :: TypeLIDF
        real*8,intent(in) :: N,Cab,Car,Cbrown,Cw,Cm
        real*8,intent(in) :: LIDFa,LIDFb,lai,q,tts,tto,psi,psoil
        real*8,intent(out),dimension(nw) :: rddt,rsdt,rdot,rsot

        real*8 :: LRT(nw,2),rho(nw),tau(nw),rsoil(nw)

        rsoil = psoil * Rsoil1 + (1-psoil)*Rsoil2

        !! If LAI is 0, return soil reflectance
        if (lai <= 0) then
            rddt = rsoil;
            rsdt = rsoil;
            rdot = rsoil;
            rsot = rsoil;
            return
        endif

        CALL PROSPECT_5B(N,Cab,Car,Cbrown,Cw,Cm,LRT)
        rho = LRT(:,1)
        tau = LRT(:,2)

        CALL FOURSAIL(rho,tau,LIDFa,LIDFb,TypeLIDF,lai,q,tts,tto,psi,rsoil,nw, &
            rddt, rsdt, rdot, rsot)

        return
    end subroutine pro4sail
