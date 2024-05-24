module soil

use parameters_site
use parameters_plant
implicit none

real :: FO2, fPerm, Tsurf, WCL

real :: DRAIN, RUNOFF
real :: dCLITT, rCLITT, rCSOMF, Rsoil
real :: dCLITTrsoil, dCLITTsomf, dCSOMF, dCSOMFrsoil, dCSOMFsoms, dCSOMS
real :: Nemission, NemissionN2O, NemissionNO, Nfixation, Nleaching
real :: NLITTnmin, NLITTsomf, Nmineralisation
real :: dNLITT, dNSOMF, dNSOMS, NSOMFnmin, NSOMFsoms, rNLITT, rNSOMF
real :: fTsoil

Contains

Subroutine SoilWaterContent(Fdepth,ROOTD,WAL)
  real :: Fdepth,ROOTD,WAL
  if (Fdepth < ROOTD) then
    WCL = WAL*0.001 / (ROOTD-Fdepth)
  else
    WCL = 0
  end if
end Subroutine SoilWaterContent

Subroutine Physics(DAVTMP,Fdepth,ROOTD,Sdepth,WAS, Frate)
  real :: DAVTMP,Fdepth,ROOTD,Sdepth,WAS
  real :: Frate
  if (Fdepth > 0.) then
    Tsurf = DAVTMP / (1. + 10. * (Sdepth / Fdepth) )
    fPerm = 0.
  else
    Tsurf = DAVTMP * exp(-gamma*Sdepth)
    fPerm = 1.
  end if  
  call Frozensoil(Fdepth,ROOTD,WAS, Frate)
end Subroutine Physics
  
   Subroutine FrozenSoil(Fdepth,ROOTD,WAS, Frate)
     real :: Fdepth,ROOTD,WAS
     real :: Frate
     real :: alpha, PFrate, WCeff
     ! Determining the amount of solid water that contributes in transportation of heat to surface 'WCeff'
     if (Fdepth > ROOTD) then
       WCeff = WCFC
     else if (Fdepth > 0.) then
       WCeff = (0.001*WAS) / Fdepth
     else
       WCeff = WCL
     end if
     ! Calculating potential frost rate 'PFrate'
     if (((Fdepth == 0.).and.(Tsurf>0.)).or.(WCeff == 0.)) then ! No soil frost present AND no frost starting
       PFrate = 0.
     else
       alpha  = LAMBDAsoil / ( RHOwater * WCeff * LatentHeat )
       PFrate = Sqrt( max(0.,Fdepth**2 - 2.*alpha*Tsurf) ) - Fdepth
     end if
     if ((PFrate >= 0.).and.(Fdepth > 0.).and.(Fdepth < ROOTD)) then
       Frate = PFrate * (0.001*WAS/Fdepth) / WCFC ! Soil frost increasing
     else if ((PFrate+Fdepth/DELT) < 0.) then
       Frate = -Fdepth / DELT                     ! Remaining soil frost thaws away
     else
       Frate = PFrate
     end if
   end Subroutine FrozenSoil

Subroutine FRDRUNIR(EVAP,Fdepth,Frate,INFIL,poolDRAIN,ROOTD,TRAN,WAL,WAS, &
                                               FREEZEL,IRRIG,THAWS)
  real :: EVAP,Fdepth,Frate,INFIL,poolDRAIN,ROOTD,TRAN,WAL,WAS
  real :: FREEZEL,IRRIG,THAWS
  real :: INFILTOT,WAFC,WAST
  WAFC   = 1000. * WCFC * max(0.,(ROOTD-Fdepth))                      ! (mm)
  WAST   = 1000. * WCST * max(0.,(ROOTD-Fdepth))                      ! (mm)
  INFILTOT = INFIL + poolDrain
  if (Fdepth < ROOTD) then
    FREEZEL = max(0., min( WAL/DELT + (INFILTOT - EVAP - TRAN), &
                         (Frate/(ROOTD-Fdepth))*WAL))                 ! (mm d-1)
  else
    FREEZEL = 0.                                                      ! (mm d-1)
  end if
  if ((Fdepth > 0.) .and. (Fdepth <= ROOTD)) then
    THAWS   = max(0.,min( WAS/DELT, -Frate*WAS/Fdepth ))              ! (mm d-1)
  else
    THAWS   = 0.                                                      ! (mm d-1)
  end if
  DRAIN  = max(0.,min( DRATE, (WAL-WAFC)/DELT + &
         (INFILTOT - EVAP - TRAN - FREEZEL + THAWS) ))                ! (mm d-1)
  RUNOFF = max(0.,            (WAL-WAST)/DELT + &
         (INFILTOT - EVAP - TRAN - FREEZEL + THAWS - DRAIN) )         ! (mm d-1)
  IRRIG  = IRRIGF *  (        (WAFC-WAL)/DELT - &
         (INFILTOT - EVAP - TRAN - FREEZEL + THAWS - DRAIN - RUNOFF)) ! (mm d-1)
end Subroutine FRDRUNIR

Subroutine O2status(O2,ROOTD)
  real :: O2,ROOTD
  FO2 = O2 / (ROOTD * FGAS * 1000./22.4)
end Subroutine O2status
  
Subroutine O2fluxes(O2,PERMgas,ROOTD,RplantAer, O2IN,O2OUT)
  real :: O2,PERMgas,ROOTD,RplantAer
  real :: O2IN,O2OUT
  real :: O2MX
  O2OUT = RplantAer * KRTOTAER * 1./12. * 1.
  O2MX  = FO2MX * ROOTD * FGAS * 1000./22.4
  O2IN  = PERMgas * ( (O2MX-O2) + O2OUT*DELT )  
end Subroutine O2fluxes

Subroutine N_fert(year,doy,DAYS_FERT,NFERTV, Nfert, input_soluble_c, input_compost_c, input_org_n)
  integer, intent(in)      :: year,doy
  integer, intent(in), dimension(:,:) :: DAYS_FERT
  real, intent(in), dimension(:,:) :: NFERTV
  real, intent(out)        :: Nfert
  real, intent(out)        :: input_soluble_c
  real, intent(out)        :: input_compost_c
  real, intent(out)        :: input_org_n

  integer :: i
  
  Nfert = 0.0
  input_soluble_c = 0.0
  input_compost_c = 0.0
  input_org_n = 0.0
  
  do i=1,size(days_fert, 1)    
     if ( (year==DAYS_FERT (i,1)) .and. (doy==DAYS_FERT (i,2)) ) then
        Nfert   = NFERTV (i, 1)
        input_org_n = NFERTV(i, 2)
        input_soluble_c = NFERTV(i, 3)
        input_compost_c = NFERTV(i, 4)
     end if
  end do

end Subroutine N_fert

Subroutine N_dep(year,doy,DAYS_NDEP,NDEPV, Ndep)
  integer                  :: year,doy,j
  integer,dimension(300,2) :: DAYS_NDEP
  real   ,dimension(300  ) :: NDEPV
  integer                  :: idep
  real                     :: NDEPV_interval,t
  real   ,dimension(300)   :: tNdep
  real                     :: Ndep
  t     = year           + (doy           -0.5)/366
  tNdep = DAYS_NDEP(:,1) + (DAYS_NDEP(:,2)-0.5)/366
  do j = 2,300
   if ( (tNdep(j-1)<t) .and. (tNdep(j)>=t) ) idep = j-1
  end do
  NDEPV_interval = NDEPV(idep+1) - NDEPV(idep)
  Ndep           = NDEPV(idep) + NDEPV_interval * (t            -tNdep(idep)) / &
                                                  (tNdep(idep+1)-tNdep(idep))
end Subroutine N_dep

Subroutine CNsoil(ROOTD,RWA,WFPS,WAL,GCR,CLITT,CSOMF,NLITT,NSOMF,NSOMS,NMIN,CSOMS)
  real :: CLITT, CSOMF, CSOMS, fN2O, GCR, NLITT, NMIN, NSOMF, NSOMS
  real :: ROOTD, RWA, WAL, WFPS
  ! Soil temperature effect
  fTsoil = exp((Tsurf-10.)*(2.*TMAXF-Tsurf-10.)/(2.*TSIGMAF**2.)) 
  ! C Litter
  rCLITT      = ((CLITT*RUNOFF) / ROOTD) * RRUNBULK * 0.001
  dCLITT      =  (CLITT*fTsoil) / TCLITT
  dCLITTsomf  = FLITTSOMF * dCLITT
  dCLITTrsoil = dCLITT - dCLITTsomf
  ! C SOM fast
  rCSOMF      = ((CSOMF*RUNOFF) / ROOTD) * RRUNBULK * 0.001
  dCSOMF      =  (CSOMF*fTsoil) / TCSOMF
  dCSOMFsoms  = FSOMFSOMS * dCSOMF
  dCSOMFrsoil = dCSOMF - dCSOMFSOMS
  ! C SOM slow
  dCSOMS      = (CSOMS*fTsoil) / TCSOMS
  ! Respiration
  Rsoil       = dCLITTrsoil + dCSOMFrsoil + dCSOMS
  ! N Litter
  rNLITT      = ((NLITT*RUNOFF) / ROOTD) * RRUNBULK * 0.001
  dNLITT      =  (NLITT*dCLITT) / CLITT
  NLITTsomf   = dNLITT * FLITTSOMF
  NLITTnmin   = dNLITT - NLITTsomf
  ! N SOM fast
  rNSOMF      = ((NSOMF*RUNOFF) / ROOTD) * RRUNBULK * 0.001
  dNSOMF      =  (NSOMF*dCSOMF) / CSOMF
  NSOMFsoms   = dNSOMF * FSOMFSOMS
  NSOMFnmin   = dNSOMF - NSOMFsoms
  ! N SOM slow
  dNSOMS      = (NSOMS*dCSOMS) / CSOMS
  ! N mineralisation, fixation, leaching, emission
  Nmineralisation = NLITTnmin + NSOMFnmin + dNSOMS
  Nfixation       = gCR * KNFIX
  ! Nleaching       = (NMIN*RNLEACH*DRAIN) / WAL
  if ((WAL > 0.) .and. (NMIN > 0.)) then
    Nleaching       = (NMIN*RNLEACH*DRAIN) / WAL
  else
    Nleaching       = 0.
  end if
  Nemission       =  NMIN * KNEMIT * RWA
  fN2O            = 1. / (1. + exp(-RFN2O*(WFPS-WFPS50N2O)))
  NemissionN2O    = Nemission *     fN2O
  NemissionNO     = Nemission * (1.-fN2O) 
end Subroutine CNsoil

subroutine CNSoil_stub(ROOTD, RWA, WFPS, WAL, GCR, cstate_yasso, nstate_yasso, NMIN, runoff_cstate)
  use yasso, only : partition_nitr, statesize_yasso
  real, intent(in) :: ROOTD
  real, intent(in) :: RWA
  real, intent(in) :: WFPS
  real, intent(in) :: WAL
  real, intent(in) :: GCR
  real, intent(in) :: cstate_yasso(statesize_yasso)
  real, intent(in) :: nstate_yasso
  real, intent(in) :: NMIN
  real, intent(out) :: runoff_cstate(statesize_yasso)
  
  real :: c_awen
  real :: nitr_awen, nitr_h
  real :: fN2O
  
  c_awen = sum(cstate_yasso(1:4))
  call partition_nitr(cstate_yasso, nstate_yasso, nitr_awen, nitr_h)

  runoff_cstate(1:4) = ((cstate_yasso(1:4)*RUNOFF) / ROOTD) * RRUNBULK * 0.001
  runoff_cstate(5)   = 0.0
  rCSOMF      = sum(runoff_cstate)
  !rCSOMF      = ((c_awen*RUNOFF) / ROOTD) * RRUNBULK * 0.001
  rNSOMF      = ((nitr_awen*RUNOFF) / ROOTD) * RRUNBULK * 0.001
  rCLITT = 0.0
  rNLITT = 0.0
  
  ! N fixation, leaching, emission
  Nfixation       = gCR * KNFIX
  ! Nleaching       = (NMIN*RNLEACH*DRAIN) / WAL
  if ((WAL > 0.) .and. (NMIN > 0.)) then
    Nleaching       = (NMIN*RNLEACH*DRAIN) / WAL
  else
    Nleaching       = 0.
  end if
  Nemission       =  NMIN * KNEMIT * RWA
  fN2O            = 1. / (1. + exp(-RFN2O*(WFPS-WFPS50N2O)))
  NemissionN2O    = Nemission *     fN2O
  NemissionNO     = Nemission * (1.-fN2O) 
end subroutine CNSoil_stub

subroutine adjust_nmin_fluxes(use_yasso, NMIN, nupt_max, yasso_ntend, nupt_max_adj, nmin_immob_yasso)
  logical, intent(in) :: use_yasso
  real, intent(in) :: NMIN ! current mineral N pool
  real, intent(in) :: nupt_max ! plant N need
  real, intent(in) :: yasso_ntend ! rate of change of the YASSO N pool
  real, intent(out) :: nupt_max_adj ! plant uptake capacity after taking immobilisation into account
  real, intent(out), optional :: nmin_immob_yasso ! nitrogen mineralisation (> 0) or immobilisation (< 0) when use_yasso = .true.
  
  real :: nmin_immob_pot
  real :: reduction

  if (.not. use_yasso) then
     nupt_max_adj = nupt_max
     return
  end if
  
  if (.not. present(nmin_immob_yasso)) then
     error stop 'nmin_immob_yasso must be present use use_yasso == .true.'
  end if
  
  nmin_immob_pot = yasso_ntend
  if (nmin_immob_pot < 0.0) then
     ! net N mineralisation, no constraint on plant uptake
     nmin_immob_yasso = -nmin_immob_pot
     nupt_max_adj = nupt_max
  else
     ! net immobilisation
     if (nupt_max + nmin_immob_pot < NMIN) then
        ! plant and microbial N demand fully satisfied
        nupt_max_adj = nupt_max
        nmin_immob_yasso = -nmin_immob_pot
     else
        ! scale down proportionally
        reduction = NMIN / (nupt_max + nmin_immob_pot)
        nupt_max_adj = nupt_max * reduction
        nmin_immob_yasso = -nmin_immob_pot * reduction
     end if
  end if
  
end subroutine adjust_nmin_fluxes

end module soil
