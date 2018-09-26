subroutine pro5b2s( &
        !!! Inputs !!!
        N, Cab, Car, Cbrown, Cw, Cm, &      !! PROSPECT 5 parameters
        solar_zenith, L, soil_moisture, &       ! Two stream parameters
        !!! Outputs !!!
        alpha_c, alpha_i, Tc, Ti, Ac, Ai)       ! Two stream Outputs

    use mod_dataspec_wavelength 
    implicit none

    real*8, intent(in) :: N, Cab, Car, Cbrown, Cw, Cm
    real*8, intent(in) :: solar_zenith, L, soil_moisture

    real*8, intent(out), dimension(nw) :: alpha_c, alpha_i, Tc, Ti, Ac, Ai

    real*8, dimension(nw,2) :: LRT

    call PROSPECT_5B(N, Cab, Car, Cbrown, Cw, Cm, LRT)
    call two_stream(LRT(:,1), LRT(:,2), solar_zenith, L, soil_moisture, &
        alpha_c, alpha_i, Tc, Ti, Ac, Ai)

end subroutine pro5b2s
