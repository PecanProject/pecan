!!!!!
! This subroutine implements the simple Hapke soil reflectance model, 
! which averages reflectance spectra of fully wet soil and fully dry 
! soil weighted by a soil moisture coefficient between 0 and 1.
!!!!!

module mod_hapke_soil
contains
function hapke_soil(soil_moisture) result(rho_soil)
    ! Simple soil reflectance model as a function of soil moisture
    use mod_dataspec_wavelength
    use mod_dataspec_soil
    implicit none

    real*8, intent(in) :: soil_moisture
    real*8, dimension(nw) :: rho_soil

    rho_soil = soil_moisture * Rsoil1 + (1 - soil_moisture) * Rsoil2
end function hapke_soil
end module mod_hapke_soil

