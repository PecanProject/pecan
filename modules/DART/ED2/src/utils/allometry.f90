!==========================================================================================!
!==========================================================================================!
!    This module is a library with several allometric relationships.                       !
!------------------------------------------------------------------------------------------!
module allometry

   contains
   !=======================================================================================!
   !=======================================================================================!
   real function h2dbh(h,ipft)

      use pft_coms    , only : is_tropical & ! intent(in)
                             , b1Ht        & ! intent(in), lookup table
                             , b2Ht        & ! intent(in), lookup table
                             , hgt_ref     ! ! intent(in)
      use ed_misc_coms, only : iallom      ! ! intent(in)

      implicit none
      !----- Arguments --------------------------------------------------------------------!
      real   , intent(in) :: h
      integer, intent(in) :: ipft
      !------------------------------------------------------------------------------------!


      !------------------------------------------------------------------------------------!
      !      Select which type of model we are running.                                    !
      !------------------------------------------------------------------------------------!
      !----- Size- and age-structure (typical ED model). ----------------------------------!
      if (is_tropical(ipft)) then
         select case (iallom)
         case (0,1)
            !----- Default ED-2.1 allometry. ----------------------------------------------!
            h2dbh = exp((log(h)-b1Ht(ipft))/b2Ht(ipft))
         case (2)
            !----- Poorter et al. (2006) allometry. ---------------------------------------!
            h2dbh =  ( log(hgt_ref(ipft) / ( hgt_ref(ipft) - h ) ) / b1Ht(ipft) )          &
                  ** ( 1.0 / b2Ht(ipft) )
         end select
      else ! Temperate
         h2dbh = log(1.0-(h-hgt_ref(ipft))/b1Ht(ipft))/b2Ht(ipft)
      end if
      !------------------------------------------------------------------------------------!

      return
   end function h2dbh
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   real function dbh2h(ipft, dbh)
      use pft_coms    , only : is_tropical & ! intent(in)
                             , dbh_crit    & ! intent(in)
                             , b1Ht        & ! intent(in)
                             , b2Ht        & ! intent(in)
                             , hgt_ref     & ! intent(in)
                             , hgt_max     ! ! intent(in)
      use ed_misc_coms, only : iallom      & ! intent(in)
                             , ibigleaf    ! ! intent(in)
      implicit none
      !----- Arguments --------------------------------------------------------------------!
      integer , intent(in) :: ipft
      real    , intent(in) :: dbh
      !----- Local variables --------------------------------------------------------------!
      real                :: mdbh
      !------------------------------------------------------------------------------------!


      !------------------------------------------------------------------------------------!
      !      Select which type of model we are running.                                    !
      !------------------------------------------------------------------------------------!
      select case (ibigleaf)
      case (0)
         !----- Size- and age-structure (typical ED model). -------------------------------!
         if (is_tropical(ipft)) then
            mdbh = min(dbh,dbh_crit(ipft))
            select case (iallom)
            case (0,1)
               !----- Default ED-2.1 allometry. -------------------------------------------!
               dbh2h = exp (b1Ht(ipft) + b2Ht(ipft) * log(mdbh) )
            case (2)
               !----- Poorter et al. (2006) allometry. ------------------------------------!
               dbh2h = hgt_ref(ipft) * (1. - exp (-b1Ht(ipft) * mdbh ** b2Ht(ipft) ) )
            end select
         else !----- Temperate PFT allometry. ---------------------------------------------!
            dbh2h = hgt_ref(ipft) + b1Ht(ipft) * (1.0 - exp(b2Ht(ipft) * dbh))
         end if

      case (1)
         !---------------------------------------------------------------------------------!
         !     Big-leaf version of ED. DBH is not really meaningful, but in the big-leaf   !
         ! model the typical allometry doesn't really make sense so we impose maximum      !
         ! height.                                                                         !
         !---------------------------------------------------------------------------------!
         dbh2h = hgt_max(ipft)
         !---------------------------------------------------------------------------------!
      end select
      !------------------------------------------------------------------------------------!

      return
   end function dbh2h
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   real function dbh2bd(dbh,ipft)

      use pft_coms    , only : C2B         & ! intent(in)
                             , b1Bs_small  & ! intent(in), lookup table
                             , b2Bs_small  & ! intent(in), lookup table
                             , b1Bs_large  & ! intent(in), lookup table
                             , b2Bs_large  & ! intent(in), lookup table
                             , is_grass    & ! intent(in)
                             , dbh_crit    ! ! intent(in), lookup table
      use ed_misc_coms, only : igrass      ! ! intent(in)
      implicit none
      !----- Arguments --------------------------------------------------------------------!
      real   , intent(in) :: dbh
      integer, intent(in) :: ipft
      !------------------------------------------------------------------------------------!

      if (is_grass(ipft).and. igrass==1) then
         dbh2bd = 0.0
      else if (dbh <= dbh_crit(ipft)) then
         dbh2bd = b1Bs_small(ipft) / C2B * dbh ** b2Bs_small(ipft)
      else
         dbh2bd = b1Bs_large(ipft) / C2B * dbh ** b2Bs_large(ipft)
      end if

      return
   end function dbh2bd
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !     This subroutine computes the diameter at the breast height given the structural   !
   ! (dead) biomass and the PFT type.  In addition to these inputs, it is also required to !
   ! give a first guess for DBH, which can be the previous value, for example.   This is   !
   ! only used for the new allometry, because we must solve an iterative root-finding      !
   ! method.                                                                               !
   !---------------------------------------------------------------------------------------!
   real function bd2dbh(ipft, bdead)
      use pft_coms    , only : b1Bs_small  & ! intent(in), lookup table
                             , b2Bs_small  & ! intent(in), lookup table
                             , b1Bs_large  & ! intent(in), lookup table
                             , b2Bs_large  & ! intent(in), lookup table
                             , bdead_crit  & ! intent(in), lookup table
                             , C2B         ! ! intent(in)
      implicit none
      !----- Arguments --------------------------------------------------------------------!
      integer, intent(in) :: ipft      ! PFT type                            [         ---]
      real   , intent(in) :: bdead     ! Structural (dead) biomass           [   kgC/plant]
      !------------------------------------------------------------------------------------!



      !------------------------------------------------------------------------------------!
      !    Decide which coefficients to use based on the critical bdead.                   !
      !------------------------------------------------------------------------------------!
      if (bdead <= bdead_crit(ipft)) then
         bd2dbh = (bdead / b1Bs_small(ipft) * C2B)**(1.0/b2Bs_small(ipft))
      else
         bd2dbh = (bdead / b1Bs_large(ipft) * C2B)**(1.0/b2Bs_large(ipft))
      end if
      !------------------------------------------------------------------------------------!

      return
   end function bd2dbh
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !      This function computes the maximum leaf biomass [kgC/m2] given the size (either  !
   ! height or DBH).  Trees would always use DBH as the starting point, but grasses may    !
   ! use DBH (old style) or height (new style).  This replaces dbh2bl and h2bl with a      !
   ! single generic function that should be used by all plants.                            !
   !---------------------------------------------------------------------------------------!
   real function size2bl(dbh,hite,ipft)
      use pft_coms    , only : dbh_crit    & ! intent(in), lookup table
                             , C2B         & ! intent(in), lookup table
                             , b1Bl        & ! intent(in), lookup table
                             , b2Bl        & ! intent(in), lookup table
                             , hgt_max     & ! intent(in), lookup table
                             , is_grass    ! ! intent(in)
      use ed_misc_coms, only : igrass      ! ! intent(in)


      implicit none
      !----- Arguments --------------------------------------------------------------------!
      real   , intent(in) :: dbh
      real   , intent(in) :: hite
      integer, intent(in) :: ipft
      !----- Local variables --------------------------------------------------------------!
      real                :: mdbh
      !------------------------------------------------------------------------------------!
      
      if (is_grass(ipft) .and. igrass == 1) then 
         !---- Use height for new grasses. ------------------------------------------------!
         mdbh   = min(h2dbh(hite,ipft),dbh_crit(ipft))
      else
         !---- Use dbh for trees. ---------------------------------------------------------!
         mdbh   = min(dbh,dbh_crit(ipft))
      end if
      
      size2bl = b1Bl(ipft) / C2B * mdbh ** b2Bl(ipft)
      
      return
   end function size2bl
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !      This function determines an effective DBH for grasses given their leaf biomass.  !
   ! DBH has no real meaning for grasses with the new allometry.                           !
   !---------------------------------------------------------------------------------------!
   real function bl2dbh(bleaf,ipft)
      use pft_coms,     only : is_tropical & ! intent(in), lookup table
                             , rho         & ! intent(in), lookup table
                             , dbh_crit    & ! intent(in), lookup table
                             , hgt_max     & ! intent(in), lookup table
                             , is_grass    & ! intent(in)
                             , C2B         & ! intent(in)
                             , b1Bl        & ! intent(in), lookup table
                             , b2Bl        ! ! intent(in), lookup table
      use ed_misc_coms, only : igrass      ! ! intent(in)

      implicit none
      !----- Arguments --------------------------------------------------------------------!
      real   , intent(in) :: bleaf
      integer, intent(in) :: ipft
      !----- Local variables --------------------------------------------------------------!
      real                :: mdbh
      !------------------------------------------------------------------------------------!

      mdbh = (bleaf * C2B / b1Bl(ipft) ) ** (1./b2Bl(ipft))

      if (is_grass(ipft) .and. igrass==1) then  
          ! For grasses, limit maximum effective dbh by maximum height
          bl2dbh = min(mdbh, h2dbh(hgt_max(ipft),ipft))
      else
          bl2dbh = min(mdbh, dbh_crit(ipft))
      end if

      return
    end function bl2dbh
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !      This function determines the height for grasses given their leaf biomass.        !
   !---------------------------------------------------------------------------------------!
   real function bl2h(bleaf,ipft)
      use pft_coms, only:  hgt_max     ! ! intent(in), lookup table
      
      implicit none
      !----- Arguments --------------------------------------------------------------------!
      real   , intent(in) :: bleaf
      integer, intent(in) :: ipft
      !------------------------------------------------------------------------------------!
      
      !----- Use existing allometric equations to convert leaves to height. ---------------!
      bl2h = min(hgt_max(ipft),dbh2h(ipft,bl2dbh(bleaf,ipft)))

      return
   end function bl2h
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !    Canopy Area allometry from Dietze and Clark (2008).                                !
   !---------------------------------------------------------------------------------------!
   real function dbh2ca(dbh,hite,sla,ipft)
      use ed_misc_coms, only : iallom      ! ! intent(in)
      use pft_coms    , only : dbh_crit    & ! intent(in)
                             , hgt_max     & ! intent(in)
                             , is_tropical & ! intent(in)
                             , is_grass    & ! intent(in)
                             , b1Ca        & ! intent(in)
                             , b2Ca        ! ! intent(in)
      use ed_misc_coms, only : igrass      ! ! intent(in)
      implicit none
      !----- Arguments --------------------------------------------------------------------!
      real   , intent(in) :: dbh
      real   , intent(in) :: hite
      real   , intent(in) :: sla
      integer, intent(in) :: ipft
      !----- Internal variables -----------------------------------------------------------!
      real                :: loclai ! The maximum local LAI for a given DBH
      real                :: dmbh   ! The maximum 
      !------------------------------------------------------------------------------------!
      if (dbh < tiny(1.0)) then
         loclai = 0.0
         dbh2ca = 0.0
      else

         !----- make this function generic to size, not just dbh. -------------------------!
         loclai = sla * size2bl(dbh,hite,ipft) 
         select case (iallom)
         case (0)
            !----- No upper bound in the allometry. ---------------------------------------!
            dbh2ca = b1Ca(ipft) * dbh ** b2Ca(ipft)

         case default
            !----- Impose a maximum crown area. -------------------------------------------!
            if (is_grass(ipft) .and. igrass==1) then
                 dbh2ca = b1Ca(ipft) * min(dbh,h2dbh(hgt_max(ipft),ipft) ) ** b2Ca(ipft)
            else
                 dbh2ca = b1Ca(ipft) * min(dbh,dbh_crit(ipft)            ) ** b2Ca(ipft)
            end if
         end select
      end if

      !----- Local LAI / Crown area should never be less than one. ------------------------!
      dbh2ca = min (loclai, dbh2ca)

      return
   end function dbh2ca
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !    This function computes the standing volume of a tree.                              !
   !---------------------------------------------------------------------------------------!
   real function dbh2vol(hgt,dbh,ipft)
      use pft_coms    , only : b1Vol    & ! intent(in)
                             , b2Vol    ! ! intent(in)
      implicit none
      !----- Arguments --------------------------------------------------------------------!
      real   , intent(in) :: hgt
      real   , intent(in) :: dbh
      integer, intent(in) :: ipft
      !------------------------------------------------------------------------------------!


      dbh2vol = b1Vol(ipft) * hgt * dbh ** b2Vol(ipft)

      return
   end function dbh2vol
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   integer function dbh2krdepth(hite,dbh,ipft,lsl)
      use ed_misc_coms, only : iallom   ! ! intent(in)
      use grid_coms   , only : nzg      ! ! intent(in)
      use soil_coms   , only : slz      ! ! intent(in)
      use pft_coms    , only : b1Rd     & ! intent(in)
                             , b2Rd     ! ! intent(in)
      implicit none 
      !----- Arguments --------------------------------------------------------------------!
      real   , intent(in) :: hite
      real   , intent(in) :: dbh
      integer, intent(in) :: ipft
      integer, intent(in) :: lsl
      !----- Local variables --------------------------------------------------------------!
      real                :: volume
      real                :: root_depth
      integer             :: k
      !------------------------------------------------------------------------------------!


      !----- Grasses get a fixed rooting depth of 70 cm. ----------------------------------!
      select case (iallom)
      case (0)
         !---------------------------------------------------------------------------------!
         !    Original ED-2.1 (I don't know the source for this equation, though).         !
         !---------------------------------------------------------------------------------!
         volume     = dbh2vol(hite,dbh,ipft) 
         root_depth = b1Rd(ipft)  * volume ** b2Rd(ipft)

      case (1,2)
         !---------------------------------------------------------------------------------!
         !    This is just a test allometry, that imposes root depth to be 0.5 m for       !
         ! plants that are 0.15-m tall, and 5.0 m for plants that are 35-m tall.           !
         !---------------------------------------------------------------------------------!
         root_depth = b1Rd(ipft) * hite ** b2Rd(ipft)
      end select


      !------------------------------------------------------------------------------------!
      !     Root depth is the maximum root depth if the soil is that deep.  Find what is   !
      ! the deepest soil layer this root can go.                                           !
      !------------------------------------------------------------------------------------!
      dbh2krdepth = nzg
      do k=nzg,lsl+1,-1
         if (root_depth < slz(k)) dbh2krdepth = k-1
      end do
      !------------------------------------------------------------------------------------!

      return
   end function dbh2krdepth
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !    This function finds the trunk height.  Currently this is based on the following    !
   ! reference, which is for a site in Bolivia:                                            !
   !                                                                                       !
   ! Poorter L., L. Bongers, F. Bongers, 2006: Architecture of 54 moist-forest tree        !
   !     species: traits, trade-offs, and functional groups. Ecology, 87, 1289-1301.       !
   !---------------------------------------------------------------------------------------!
   real function h2crownbh(height,ipft)
      use pft_coms, only : b1Cl & ! intent(in)
                         , b2Cl ! ! intent(in)
      implicit none
      !----- Arguments --------------------------------------------------------------------!
      real   , intent(in) :: height
      integer, intent(in) :: ipft
      !----- Local variables. -------------------------------------------------------------!
      real                :: crown_length
      !------------------------------------------------------------------------------------!
      
      crown_length = b1Cl(ipft) * height ** b2Cl(ipft)
      h2crownbh    = max(0.05,height - crown_length)

      return
   end function h2crownbh
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !     This subroutine finds the total above ground biomass (wood + leaves)              !
   !---------------------------------------------------------------------------------------!
   real function ed_biomass(bdead, bleaf, bsapwooda, ipft)
      use pft_coms, only:  agf_bs ! ! intent(in)
      
      implicit none
      !----- Arguments --------------------------------------------------------------------!
      real    , intent(in) :: bdead
      real    , intent(in) :: bleaf
      real    , intent(in) :: bsapwooda
      integer , intent(in) :: ipft
      !----- Local variables --------------------------------------------------------------!
      real                 :: bwood
      !------------------------------------------------------------------------------------!

      bwood      = bsapwooda + (bdead * agf_bs(ipft))
      ed_biomass = bleaf + bwood

      return
   end function ed_biomass
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !     This subroutine estimates the tree area indices, namely leaf, branch(plus twigs), !
   ! and stem.  For the leaf area index (LAI), we use the specific leaf area (SLA), a      !
   ! constant.  The wood area index WAI is found using the model proposed by Järvelä       !
   ! (2004) to find the specific projected area.                                           !
   !                                                                                       !
   !    Ahrends, B., C. Penne, O. Panferov, 2010: Impact of target diameter harvesting on  !
   !        spatial and temporal pattern of drought risk in forest ecosystems under        !
   !        climate change conditions.  The Open Geography Journal, 3, 91-102  (they       !
   !        didn't develop the allometry, but the original reference is in German...)      !
   !---------------------------------------------------------------------------------------!
   subroutine area_indices(nplant,bleaf,bdead,balive,dbh,hite,pft,sla,lai,wai,crown_area   &
                          ,bsapwooda)
      use pft_coms    , only : is_tropical     & ! intent(in)
                             , is_grass        & ! intent(in)
                             , agf_bs          & ! intent(in)
                             , rho             & ! intent(in)
                             , C2B             & ! intent(in)
                             , dbh_crit        & ! intent(in)
                             , b1WAI           & ! intent(in)
                             , b2WAI           ! ! intent(in)
      use consts_coms , only : onethird        & ! intent(in)
                             , pi1             ! ! intent(in)
      use rk4_coms    , only : ibranch_thermo  ! ! intent(in)
      use ed_misc_coms, only : igrass          ! ! intent(in)
      use canopy_radiation_coms, only : clumping_factor  !! intent(in)
      implicit none
      !----- Arguments --------------------------------------------------------------------!
      integer , intent(in)  :: pft        ! Plant functional type            [         ---]
      real    , intent(in)  :: nplant     ! Number of plants                 [    plant/m²]
      real    , intent(in)  :: bleaf      ! Specific leaf biomass            [   kgC/plant]
      real    , intent(in)  :: bdead      ! Specific structural              [   kgC/plant]
      real    , intent(in)  :: balive     ! Specific live tissue biomass     [   kgC/plant]
      real    , intent(in)  :: bsapwooda  ! Specific sapwood biomass above grnd[   kgC/plant]
      real    , intent(in)  :: dbh        ! Diameter at breast height        [          cm]
      real    , intent(in)  :: hite       ! Plant height                     [           m]
      real    , intent(in)  :: sla        ! Specific leaf area               [m²leaf/plant]
      real    , intent(out) :: lai        ! Leaf area index                  [   m²leaf/m²]
      real    , intent(out) :: wai        ! Wood area index                  [   m²wood/m²]
      real    , intent(out) :: crown_area ! Crown area                       [  m²crown/m²]
      !----- Local variables --------------------------------------------------------------!
      real                  :: bwood      ! Wood biomass                     [   kgC/plant]
      real                  :: swa        ! Specific wood area               [    m²/plant]
      real                  :: bdiamet    ! Diameter of current branch       [           m]
      real                  :: blength    ! Length of each branch            [           m]
      real                  :: nbranch    ! Number of branches               [        ----]
      real                  :: bdmin      ! Minimum diameter                 [           m]
      !----- External functions -----------------------------------------------------------!
      real    , external    :: errorfun ! Error function.
      !------------------------------------------------------------------------------------!
      
      !----- First, we compute the LAI ----------------------------------------------------!
!      lai = clumping_factor(pft)*bleaf * nplant * sla
      lai = bleaf * nplant * sla

      !----- Find the crown area. ---------------------------------------------------------!
      crown_area = min(1.0, nplant * dbh2ca(dbh,hite,sla,pft))

      !------------------------------------------------------------------------------------!
      !     Here we check whether we need to compute the branch, stem, and effective       !
      ! branch area indices.  These are only needed when branch thermodynamics is used,    !
      ! otherwise, simply assign zeroes to them.                                           !
      !------------------------------------------------------------------------------------!
      select case (ibranch_thermo)
      case (0) 
         !----- Ignore branches and trunk. ------------------------------------------------!
         wai  = 0.
         !---------------------------------------------------------------------------------!

      case (1,2)
         !---------------------------------------------------------------------------------!
         !    Solve branches using the equations from Ahrends et al. (2010).               !
         !---------------------------------------------------------------------------------!
         wai = nplant * b1WAI(pft) * min(dbh,dbh_crit(pft)) ** b2WAI(pft)
         !---------------------------------------------------------------------------------!

      end select
      !------------------------------------------------------------------------------------!


      !------------------------------------------------------------------------------------!

      return
   end subroutine area_indices
   !=======================================================================================!
   !=======================================================================================!
end module allometry
!==========================================================================================!
!==========================================================================================!


