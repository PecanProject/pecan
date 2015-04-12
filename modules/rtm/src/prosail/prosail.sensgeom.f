subroutine sail_sensgeom(tts,tto,psi, &
        cts,cto,ctscto,dso)

    implicit none
    REAL*8, INTENT(in) :: tts,tto,psi
    REAL*8, INTENT(out) :: cts,cto,ctscto,dso

    real*8 :: pi,rd
    common /angle/ pi,rd
    REAL*8 :: tants,tanto,cospsi

        cts    = COS(rd*tts)
        cto    = COS(rd*tto)
        ctscto = cts*cto
        tants  = TAN(rd*tts)
        tanto  = TAN(rd*tto)
        cospsi = COS(rd*psi)
        dso    = SQRT(tants*tants+tanto*tanto-2.*tants*tanto*cospsi)
        
        return
end subroutine sail_sensgeom
