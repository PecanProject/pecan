module resources

use parameters_site
use parameters_plant
implicit none

real :: DTRINT,PARAV,PARINT,TRANRF
real :: RWA, WFPS

Contains

Subroutine Light(DAYL,DTR,LAI,PAR)
  real :: DAYL,DTR,LAI,PAR
  if (DAYL > 0) then
    PARAV = PAR * (1E6/(24*3600)) / DAYL
  else
    PARAV = 0.
  end if
  PARINT = PAR * (1 - exp(-1.0*K*LAI))
  DTRINT = DTR * (1 - exp(-0.75*K*LAI))
end Subroutine Light
  
Subroutine EVAPTRTRF(Fdepth,PEVAP,PTRAN,ROOTD,WAL, EVAP,TRAN)
  real :: Fdepth, PEVAP, PTRAN, ROOTD, WAL, EVAP, TRAN
  real :: AVAILF, FR, WAAD, WCL, WCCR
  if (Fdepth < ROOTD) then
    WCL = WAL*0.001 / (ROOTD-Fdepth)
  else
    WCL = 0
  end if                                                        ! (m3 m-3)
  RWA  = max(0., min(1., (WCL - WCAD) / (WCFC  - WCAD) ) )      ! % (-)
  WFPS = max(0., min(1., (WCL - WCAD) / (WCST  - WCAD) ) )      ! % (-)
  WAAD = 1000. * WCAD * (ROOTD-Fdepth)                          ! (mm)
  EVAP = PEVAP * RWA                                            ! (mm d-1)
  WCCR = WCWP + max( 0.01, PTRAN/(PTRAN+TRANCO) * (WCFC-WCWP) ) ! (m3 m-3)
  if (WCL > WCCR) then
    FR = max(0., min(1., (WCST-WCL)/(WCST-WCWET) ))              
  else
    FR = max(0., min(1., (WCL-WCWP)/(WCCR-WCWP)  ))              
  end if                                                        ! (mm mm-1)
  TRAN = PTRAN * FR                                             ! (mm d-1)
  if (EVAP+TRAN > 0.) then
    AVAILF = min( 1., ((WAL-WAAD)/DELT) / (EVAP+TRAN) )         
  else
    AVAILF = 0                                                
  end if                                                        ! (mm mm-1)
  EVAP = EVAP * AVAILF                                          ! (mm d-1)
  TRAN = TRAN * AVAILF                                          ! (mm d-1)
  if (PTRAN > 0.) then
    TRANRF = TRAN / PTRAN                                       ! (-)
  else
    TRANRF = 1                                                  ! (-)
  end if
end Subroutine EVAPTRTRF

Subroutine ROOTDG(Fdepth,ROOTD,WAL, EXPLOR,RROOTD)
  real :: Fdepth,ROOTD,WAL
  real :: EXPLOR,RROOTD
  real :: WCL  
  if (Fdepth < ROOTD) then
    WCL = WAL*0.001 / (ROOTD-Fdepth)
  else
    WCL = 0
  end if                                                        ! (m3 m-3)
  if ( (ROOTD<ROOTDM) .and. (WCL>WCWP) ) then
     RROOTD = min( RRDMAX, (ROOTDM-ROOTD)/DELT )
  else
     RROOTD = 0.
  end if
  EXPLOR = 1000. * RROOTD * WCFC
end Subroutine ROOTDG

end module resources
