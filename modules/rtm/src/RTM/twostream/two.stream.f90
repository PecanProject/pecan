subroutine two_stream(rl, tl, solar_zenith, L, soil_moisture, &     ! Inputs
        alpha_c, alpha_i, Tc, Ti, Ac, Ai)       ! Outputs

    use mod_dataspec_wavelength
    use mod_hapke_soil
    use mod_biggamma

    implicit none
    real*8, intent(in), dimension(nw) :: rl, tl
    real*8, intent(in) :: L, solar_zenith, soil_moisture

    real*8, intent(out), dimension(nw) :: alpha_c, alpha_i, Tc, Ti, Ac, Ai

    !! TODO: More intelligent treatment of zeta and zetastar
    !! TODO: Scale leaf optics by forward/backward scattering efficiency (depends on leaf orientation function)
    !! TODO: Implement alternative G functions (as callable functions of mu)

    !! Fixed parameters
    real*8, parameter :: mui = 0.5d0 / 0.705d0      ! Isotropic cosine constant
    real*8, parameter :: G = 0.5d0                ! Structure factor for spherically (randomly) distributed leaf angles
    real*8, parameter :: zeta = 0.67d0          ! Structure factor for collimated radiation (see Pinty 2006)
    real*8, parameter :: zetastar_factor = 1.0d0    ! Structure factor scaling term for isotropic radiation (see Pinty 2006)

    !! Derived quantities
    real*8 :: mu0, KL, zetastar
    real*8 :: tau_c, tau_i, exptaui             ! Optical depths for collimated (c) and isotropic (i) radiation
    real*8, dimension(nw, 4) :: gammas_c, gammas_i      ! gamma terms for Meador & Weaver (1980) solutions
    real*8, dimension(nw) :: w0, rho_soil               ! Canopy albedo, soil reflectance
    real*8, dimension(nw) :: RcBB, TcBB, RiBB, TiBB     ! Black background reflectance and transmittance
    real*8 :: TcBC, TiBC                                ! Black canopy transmittances
    real*8, dimension(nw) :: Rstar                      ! Infinite series reflectance term
    real*8, dimension(nw) :: TcT, TiT                   ! Total one-way transmittances
    real*8, dimension(nw) :: rhoxTiT

    mu0 = cos(solar_zenith)
    zetastar = zeta * zetastar_factor

    w0 = rl + tl
    KL = L * G / mu0
    tau_c = KL * zeta
    tau_i = KL * zetastar

    !! Calculate soil reflectance from soil moisture (simple Hapke model)
    rho_soil = hapke_soil(soil_moisture)

    !! Gamma coefficients
    call pinty(nw, rl, tl, mu0, gammas_c)
    call pinty(nw, rl, tl, mui, gammas_i)

    !! Black background solutions (from Meador & Weaver 1980, plane-parallel atmospheres)
    call meador_plane_parallel(nw, w0, mu0, tau_c, &
        gammas_c(:,1), gammas_c(:,2), gammas_c(:,3), gammas_c(:,4), &
        RcBB, TcBB)
    call meador_plane_parallel(nw, w0, mui, tau_i, &
        gammas_i(:,1), gammas_i(:,2), gammas_i(:,3), gammas_i(:,4), &
        RiBB, TiBB)

    !! Black canopy transmittances
    TcBC = exp(-tau_c / mu0)
    exptaui = exp(tau_i)
    TiBC = (1.0d0 - tau_i + tau_i * tau_i * BigGamma(tau_i)) / exptaui

    !! Infinite series reflectance term
    Rstar = 1.0d0 / (1.0d0 - RiBB * rho_soil)

    !! Total one-way transmittance
    TcT = TcBB + TcBC
    TiT = TiBB + TiBC

    !! Results: Transmitance to background layer
    Tc = TcT * Rstar
    Ti = TiT * Rstar

    !! Results: Canopy reflectance
    !! Full formulae for reference 
    !! alpha_c = RcBB + TcT * rho_soil * TiT * Rstar
    !! alpha_i = RiBB + TiT * rho_soil * TiT * Rstar
    rhoxTiT = rho_soil * TiT
    alpha_c = RcBB + Tc * rhoxTiT
    alpha_i = RiBB + Ti * rhoxTit

    !! Results: Canopy absorption
    !! Full formulae for reference
    !! Ac = 1 - (alpha_c + Tc) + rho_soil * TiT
    !! Ai = 1 - (alpha_i + Ti) + rho_soil * TiT
    Ac = 1 - (alpha_c + Tc) + rhoxTiT
    Ai = 1 - (alpha_i + Ti) + rhoxTiT

end subroutine two_stream

