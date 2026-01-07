!==========================================================================================!
!==========================================================================================!
!     Module phenology_startup.                                                            !
!
!     This module contains 
!------------------------------------------------------------------------------------------!
module phenology_startup

   !=======================================================================================!
   !=======================================================================================!


   contains



   !=======================================================================================!
   !=======================================================================================!
   subroutine phenology_init
    
      use phenology_coms, only : iphen_scheme  ! ! intent(in)
      use ed_misc_coms  , only : runtype       ! ! intent(in)

      implicit none
      !----- Local parameters. ------------------------------------------------------------!
      logical,parameter :: bypass=.true.

      !----- Initialize the Botta et al. scheme. ------------------------------------------!
      select case (iphen_scheme)
      case (1)

         !---------------------------------------------------------------------------------!
         !     Initialize from satellite.  This subroutine gives ALL SITES the             !
         ! phenological parameters prescribed in the input file.                           !
         !---------------------------------------------------------------------------------!

         call read_prescribed_phenology

      case default

         !---------------------------------------------------------------------------------!
         !    Initialize thermal sums here only if no thermal sums information is avail-   !
         ! able from the restart file, or if this is a run with bare ground initial-       !
         ! ization.                                                                        !
         !---------------------------------------------------------------------------------!
         if (runtype /= 'HISTORY') then
            write (unit=*,fmt='(a)') ' - Reading thermal sums.'
            call read_thermal_sums
         end if
      end select


      return
   end subroutine phenology_init
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !    This subroutine initialises the thermal sums.  Two properties must be set at the   !
   ! beginning of the simulation:                                                          !
   !  + Chill days  (chd): number of days with average temperatures below 278.15 K.        !
   !  + Degree days (dgd): sum of daily average temperatures above 278.15 K.               !
   !---------------------------------------------------------------------------------------!
   subroutine read_thermal_sums
      use ed_max_dims   , only : str_len          ! ! intent(in)
      use ed_state_vars , only : edtype           & ! structure
                               , polygontype      & ! structure
                               , sitetype         & ! structure
                               , edgrid_g         ! ! structure
      use grid_coms     , only : ngrids           ! ! intent(in)
      use ed_misc_coms  , only : iyeara           & ! intent(in)
                               , imontha          & ! intent(in)
                               , idatea           & ! intent(in)
                               , itimea           & ! intent(in)
                               , thsums_database  & ! intent(in)
                               , max_thsums_dist  ! ! intent(in)
      implicit none
      !----- Local variables. -------------------------------------------------------------!
      type(edtype)                            , pointer     :: cgrid
      type(polygontype)                       , pointer     :: cpoly
      type(sitetype)                          , pointer     :: csite
      character(len=str_len)                                :: fyearname
      character(len=str_len)                                :: favgname
      integer                                               :: npoints
      integer                                               :: ierr
      integer                                               :: i
      integer                                               :: j
      integer                                               :: itype
      integer                                               :: igr
      integer                                               :: ipy
      integer                                               :: isi
      logical                                               :: yearthere
      logical                                               :: avgthere
      logical                                               :: iisclose
      real                  , dimension(12)                 :: ndmon
      real                  , dimension(2,12)               :: var_out
      real                  , dimension(2,12)               :: var_current_year 
      real                  , dimension(2,12)               :: var_past_year    
      real                  , dimension(:)    , allocatable :: flat
      real                  , dimension(:)    , allocatable :: flon
      real                  , dimension(:)    , allocatable :: fdist
      real                  , dimension(:,:,:), allocatable :: varc
      real                  , dimension(:,:,:), allocatable :: varp
      real                                                  :: tlat
      real                                                  :: tlon
      real                                                  :: partial_month_fraction
      !----- External functions. ----------------------------------------------------------!
      logical                                 , external    :: isleap
      real                                    , external    :: dist_gc
      !----- Local constants. -------------------------------------------------------------!
      character(len=3)      , dimension(2)    , parameter   :: ftype=(/'chd','dgd'/)
      !------------------------------------------------------------------------------------!



      !------------------------------------------------------------------------------------!
      !    Initialise the number of days for each month.                                   !
      !           JAN  FEB  MAR  APR  MAY  JUN  JUL  AUG  SEP  OCT  NOV  DEC               !
      !------------------------------------------------------------------------------------!
      ndmon = (/  31., 28., 31., 30., 31., 30., 31., 31., 30., 31., 30., 31. /)
      !----- Change number of days in February in case it is a leap year. -----------------!
      if (isleap(iyeara)) ndmon(2) = 29.
      !------------------------------------------------------------------------------------!



      !------------------------------------------------------------------------------------!
      !      How far are we into the current month?  Use this to weight the current and    !
      ! past year data for this month.                                                     !
      !------------------------------------------------------------------------------------!
      partial_month_fraction = (real(idatea-1) + real(itimea) * 0.01 / 24.0)               &
                             / ndmon(imontha)
      !------------------------------------------------------------------------------------!



      !------------------------------------------------------------------------------------!
      !     The two types are chilling days (chd) and degree days (dgd).                   !
      !------------------------------------------------------------------------------------!
      typeloop: do itype = 1, 2

         !----- First we try to use a file corresponding to the current year. -------------!
         write(fyearname,'(5a,i4.4,a)') trim(thsums_database),ftype(itype),'/temp.'        &
                                       ,ftype(itype),'.y',iyeara, '.dat'
         inquire (file=trim(fyearname),exist=yearthere)
         !---------------------------------------------------------------------------------!



         !----- If not, look for the average data. ----------------------------------------!
         write(favgname,'(2a)') trim(thsums_database)//ftype(itype)                        &
                               ,'/temp.'//ftype(itype)//'.avg.dat'
         inquire(file=trim(favgname),exist=avgthere)
         !---------------------------------------------------------------------------------!



         !---------------------------------------------------------------------------------!
         !     If a thermal sums file for the previous year is available, we use it to     !
         ! initialise the thermal sums.  The average files will be used only when the year !
         ! file is unavailable.  In case none of them exists, we stop the simulation.      !
         !---------------------------------------------------------------------------------!
         if (yearthere) then
            open(unit=12,file=trim(fyearname),form='formatted',status='old',action='read')
         elseif (avgthere) then
            open(unit=12,file=trim(favgname) ,form='formatted',status='old',action='read')
         else
            write (unit=*,fmt='(a)') '----------------------------------------------------'
            write (unit=*,fmt='(a)') '  Variable: '//ftype(itype)//'.'
            write (unit=*,fmt='(a)') '  Failing reading thermal sums for current year!!!'
            write (unit=*,fmt='(a)') '  You provided the following thermal sums path:'
            write (unit=*,fmt='(a)') '  THSUMS_DATABASE = '//trim(thsums_database)
            write (unit=*,fmt='(a)') '  You must provide a file with one of the following:'
            write (unit=*,fmt='(a)') '  1. This year thermal sums, named:'
            write (unit=*,fmt='(a)') '    '//trim(fyearname)
            write (unit=*,fmt='(a)') '  2. Average thermal sums, named:'
            write (unit=*,fmt='(a)') '    '//trim(favgname)
            write (unit=*,fmt='(a)') '----------------------------------------------------'
            call fatal_error (' Thermal sums file ('//ftype(itype)//') not found!!!'       &
                             ,'read_thermal_sums','phenology_startup.f90')
         end if
         !---------------------------------------------------------------------------------!



         !---------------------------------------------------------------------------------!
         !    If this is the first time, count number of points.                           !
         !---------------------------------------------------------------------------------!
         if (itype == 1) then
            npoints = 0
            count_points: do
               read(unit=12,fmt=*,iostat=ierr) tlat
               if (ierr /= 0) exit count_points
               npoints = npoints + 1
            end do count_points
            rewind(12)
            !------------------------------------------------------------------------------!


            !----- Allocate arrays. -------------------------------------------------------!
            allocate(flat(npoints))
            allocate(flon(npoints))
            allocate(fdist(npoints))
            allocate(varc(2,npoints,12))
            allocate(varp(2,npoints,12))
            varc(:,:,:) = 0.0
            varp(:,:,:) = 0.0
            !------------------------------------------------------------------------------!

         end if
         !---------------------------------------------------------------------------------!



         !----- Now we read in the file. --------------------------------------------------!
         do i=1,npoints
            read (unit=12,fmt=*) flat(i),flon(i),(varc(itype,i,j),j=1,12)
         end do
         close (unit=12,status='keep')
         !---------------------------------------------------------------------------------!



         !----- First we try to use a file corresponding to the previous year. ------------!
         write(fyearname,'(5a,i4.4,a)') trim(thsums_database),ftype(itype),'/temp.'        &
                                       ,ftype(itype),'.y',iyeara-1, '.dat'
         inquire (file=trim(fyearname),exist=yearthere)
         !---------------------------------------------------------------------------------!


         !----- If not, look for the average data. ----------------------------------------!
         write(favgname,'(2a)') trim(thsums_database)//ftype(itype)                        &
                               ,'/temp.'//ftype(itype)//'.avg.dat'
         inquire(file=trim(favgname),exist=avgthere)
         !---------------------------------------------------------------------------------!



         !---------------------------------------------------------------------------------!
         !     If a thermal sums file for the previous year is available, we use it to     !
         ! initialise the thermal sums.  The average files will be used only when the year !
         ! file is unavailable.  In case none of them exists, we stop the simulation.      !
         !---------------------------------------------------------------------------------!
         if (yearthere) then
            open(unit=12,file=trim(fyearname),form='formatted',status='old',action='read')
         elseif (avgthere) then
            open(unit=12,file=trim(favgname),form='formatted',status='old',action='read')
         else
            write (unit=*,fmt='(a)') '----------------------------------------------------'
            write (unit=*,fmt='(a)') '  Variable: '//ftype(itype)//'.'
            write (unit=*,fmt='(a)') '  Failing reading thermal sums for previous year!!!'
            write (unit=*,fmt='(a)') '  You provided the following thermal sums path:'
            write (unit=*,fmt='(a)') '  THSUMS_DATABASE = '//trim(thsums_database)
            write (unit=*,fmt='(a)') '  You must provide a file with one of the following:'
            write (unit=*,fmt='(a)') '  1. This year thermal sums, named:'
            write (unit=*,fmt='(a)') '    '//trim(fyearname)
            write (unit=*,fmt='(a)') '  2. Average thermal sums, named:'
            write (unit=*,fmt='(a)') '    '//trim(favgname)
            write (unit=*,fmt='(a)') '----------------------------------------------------'
            call fatal_error (' Thermal sums file ('//ftype(itype)//') not found!!!'       &
                             ,'read_thermal_sums','phenology_startup.f90')
         end if

         !----- Read file. ----------------------------------------------------------------!
         do i = 1, npoints
            read(unit=12,fmt=*)tlat,tlon,(varp(itype,i,j),j=1,12)
         end do
         close(unit=12,status='keep')
         !---------------------------------------------------------------------------------!

      end do typeloop
      !------------------------------------------------------------------------------------!





      !------------------------------------------------------------------------------------!
      !     Big loop over all grids.                                                       !
      !------------------------------------------------------------------------------------!
      do igr=1,ngrids
         cgrid => edgrid_g(igr)

         do ipy=1,cgrid%npolygons
            cpoly => cgrid%polygon(ipy)

            do i=1,npoints
               fdist(i) = dist_gc(cgrid%lon(ipy),flon(i),cgrid%lat(ipy),flat(i))
            end do

            !----- "i" becomes the closest grid point. ------------------------------------!
            i = minloc(fdist,dim=1)

            do isi = 1,cpoly%nsites
               csite => cpoly%site(isi)

               !----- Find the right index. -----------------------------------------------!
               iisclose     = fdist(i) < max_thsums_dist
               var_out(:,:) = 0.0

               !----- Fill this site's info. ----------------------------------------------!
               if (iisclose) then
                  do itype = 1,2
                     var_current_year(itype,1:12) = varc(itype,i,1:12)
                     var_past_year(itype,1:12)    = varp(itype,i,1:12)

                     !----- Fill contribution from current month. -------------------------!
                     var_out(itype,imontha) = partial_month_fraction                       &
                                            * var_current_year(itype,imontha)

                     !----- Fill contribution from previous months in this year. ----------!
                     var_out(itype,1:(imontha-1)) = var_current_year(itype,1:(imontha-1))

                     !----- Fill contribution from previous year. -------------------------!
                     var_out(itype,imontha) = (1.0 - partial_month_fraction)               &
                                            * var_past_year(itype,imontha)

                     !----- Fill remaining info from past year. ---------------------------!
                     var_out(itype,(imontha+1):12) = var_past_year(itype,(imontha+1):12)
                  end do
               end if

               !----- Fill the patch-level degree and chill days. -------------------------!
               call fill_thermal_sums(csite, cgrid%lat(ipy), imontha, var_out)
            end do
         end do
      end do

      deallocate(flat )
      deallocate(flon )
      deallocate(fdist)
      deallocate(varc )
      deallocate(varp )

      return
   end subroutine read_thermal_sums
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   subroutine fill_thermal_sums(csite,lat,imontha,therm_sums)
      use ed_state_vars, only : sitetype
      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      type(sitetype)       , target     :: csite
      real                 , intent(in) :: lat
      integer              , intent(in) :: imontha
      real, dimension(2,12), intent(in) :: therm_sums
      !----- Local variables. -------------------------------------------------------------!
      real                              :: dgd
      real                              :: chd
      integer                           :: ipa
      !------------------------------------------------------------------------------------!


      !------------------------------------------------------------------------------------!
      !     Decide how to find the thermal sums depending on the hemisphere.               !
      !------------------------------------------------------------------------------------!
      if (lat >= 0.0) then
         !----- Northern Hemisphere. ------------------------------------------------------!
         if (imontha <= 8) then
            dgd = sum(therm_sums(2,1:imontha))
         else
            dgd = 0.0
         end if
         
         if (imontha >= 11) then
            chd = sum(therm_sums(1,imontha:12))
         elseif (imontha <= 6) then
            chd = sum(therm_sums(1,11:12)) + sum(therm_sums(1,1:imontha))
         else
            chd = 0.0
         end if
         
      else

         !----- Southern Hemisphere. ------------------------------------------------------!
         
         if (imontha <= 2) then
            dgd = sum(therm_sums(2,7:12)) + sum(therm_sums(2,1:imontha))
         elseif (imontha >= 7) then
            dgd = sum(therm_sums(2,7:imontha))
         else
            dgd = 0.0
         end if

         if (imontha >= 5) then
            chd = sum(therm_sums(1,5:imontha))
         else
            chd = 0.0
         end if
      end if

      !----- Loop over patches. -----------------------------------------------------------!
      do ipa = 1,csite%npatches
         csite%sum_chd(ipa) = chd
         csite%sum_dgd(ipa) = dgd
      end do

      return
   end subroutine fill_thermal_sums
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !     This subroutine reads the prescribed phenology for all polygons.                  !
   !---------------------------------------------------------------------------------------!
   subroutine read_prescribed_phenology

      use ed_state_vars , only : edgrid_g              & ! structure
                               , edtype                & ! structure
                               , polygontype           & ! structure
                               , sitetype              ! ! structure
      use ed_misc_coms  , only : imontha               & ! intent(in)
                               , idatea                & ! intent(in)
                               , iyeara                ! ! intent(in)
      use grid_coms     , only : ngrids                ! ! intent(in)
      use ed_misc_coms  , only : runtype               ! ! intent(in)
      use phenology_coms, only : prescribed_phen       & ! structure
                               , phenpath              & ! intent(in)
                               , max_phenology_dist    ! ! intent(in)
      use ed_max_dims   , only : str_len               & ! intent(in)
                               , maxfiles              & ! intent(in)
                               , maxlist               ! ! intent(in)
      use phenology_aux , only : prescribed_leaf_state ! ! subroutine
      implicit none
      !----- Local variables. -------------------------------------------------------------!
      type(edtype)                              , pointer   :: cgrid
      type(polygontype)                         , pointer   :: cpoly
      type(prescribed_phen)                                 :: phen_temp
      character(len=str_len), dimension(maxlist)            :: full_list
      character(len=str_len), dimension(maxlist)            :: phen_list
      real                  , dimension(maxlist)            :: phen_lon
      real                  , dimension(maxlist)            :: phen_lat
      real                  , dimension(maxlist)            :: phen_dist
      character(len=str_len)                                :: phen_file
      integer                                               :: igr
      integer                                               :: isi
      integer                                               :: ipy
      integer                                               :: iyr
      integer                                               :: doy
      integer                                               :: ncl
      integer                                               :: nf
      integer                                               :: nflist
      integer                                               :: nfphen
      integer                                               :: yearsa
      integer                                               :: yearsz
      !----- Local constants. ----------------------------------------------------------------!
      character(len=12)                         , parameter :: fffmt='(a,1x,f12.5)'
      character(len=13)                         , parameter :: esfmt='(a,1x,es12.5)'
      !----- External functions. ----------------------------------------------------------!
      integer                                   , external  :: julday  
      real                                      , external  :: dist_gc 
      !------------------------------------------------------------------------------------!

      !------ List all files that has the phenology prefix. -------------------------------!

      call ed_filelist(full_list,phenpath,nflist)
      call ed1_fileinfo('.txt',nflist,full_list,nfphen,phen_list,phen_lon,phen_lat)
      !------------------------------------------------------------------------------------!

      gridloop: do igr = 1,ngrids
         cgrid => edgrid_g(igr)


         polyloop: do ipy = 1,cgrid%npolygons
            cpoly => cgrid%polygon(ipy)

            !------------------------------------------------------------------------------!
            !     Compute the distance between the current polygon and the phenology       !
            ! files.  Pick up the closest file.                                            !
            !------------------------------------------------------------------------------!
            do nf = 1, nfphen
               phen_dist(nf) = dist_gc(cgrid%lon(ipy),phen_lon(nf)                         &
                                      ,cgrid%lat(ipy),phen_lat(nf))
            end do
            ncl = minloc(phen_dist(1:nfphen),dim=1)
            phen_file=phen_list(ncl)

            !----- Check whether the closest phenology file is enough close... ------------!
            if (phen_dist(ncl) > max_phenology_dist) then
               write (unit=*,fmt='(a)') '-------------------------------------------------'
               write (unit=*,fmt='(a)') ' The closest phenology point is too far away...'
               write (unit=*,fmt=*)     ' '
               write (unit=*,fmt=fffmt) ' Polygon longitude:          ',cgrid%lon(ipy)
               write (unit=*,fmt=fffmt) ' Polygon latitude:           ',cgrid%lat(ipy)
               write (unit=*,fmt=fffmt) ' Closest phenology longitude:',phen_lon(ncl)
               write (unit=*,fmt=fffmt) ' Closest phenology latitude: ',phen_lat(ncl)
               write (unit=*,fmt=esfmt) ' Distance:                   ',phen_dist(ncl)
               write (unit=*,fmt=esfmt) ' Maximum accepted distance:  ',max_phenology_dist
               write (unit=*,fmt='(a)') '-------------------------------------------------'
               write (unit=*,fmt=*)     ' '
               call fatal_error('No valid phenology file was found!!!'                     &
                               ,'read_prescribed_phenology','phenology_init.f90')
            end if

            !----- Open phenology file. ---------------------------------------------------!
            open(unit=12,file=trim(phen_file),form='formatted',status='old',action='read')
            
            !----- Read the number of years, and allocate the temporary array. ------------!
            read(unit=12,fmt=*) phen_temp%nyears
            allocate(phen_temp%years  (phen_temp%nyears))
            allocate(phen_temp%flush_a(phen_temp%nyears))
            allocate(phen_temp%flush_b(phen_temp%nyears))
            allocate(phen_temp%color_a(phen_temp%nyears))
            allocate(phen_temp%color_b(phen_temp%nyears))

            !----- Read the remaining lines. ----------------------------------------------!
            do iyr = 1,phen_temp%nyears
               read(unit=12,fmt=*)  phen_temp%years(iyr)  , phen_temp%flush_a(iyr)         &
                                  , phen_temp%flush_b(iyr), phen_temp%color_a(iyr)         &
                                  , phen_temp%color_b(iyr)
            end do
            close (unit=12,status='keep')


            !----- Write phenology to each site. ------------------------------------------!
            siteloop: do isi = 1,cpoly%nsites
               
               !----- Allocate memory for all years having data. --------------------------!
               cpoly%phen_pars(isi)%nyears = phen_temp%nyears
               allocate(cpoly%phen_pars(isi)%years(cpoly%phen_pars(isi)%nyears))
               allocate(cpoly%phen_pars(isi)%flush_a(cpoly%phen_pars(isi)%nyears))
               allocate(cpoly%phen_pars(isi)%flush_b(cpoly%phen_pars(isi)%nyears))
               allocate(cpoly%phen_pars(isi)%color_a(cpoly%phen_pars(isi)%nyears))
               allocate(cpoly%phen_pars(isi)%color_b(cpoly%phen_pars(isi)%nyears))
                     
               do iyr = 1,phen_temp%nyears
                  cpoly%phen_pars(isi)%years(iyr)   = phen_temp%years(iyr)
                  cpoly%phen_pars(isi)%flush_a(iyr) = phen_temp%flush_a(iyr)
                  cpoly%phen_pars(isi)%flush_b(iyr) = phen_temp%flush_b(iyr)
                  cpoly%phen_pars(isi)%color_a(iyr) = phen_temp%color_a(iyr)
                  cpoly%phen_pars(isi)%color_b(iyr) = phen_temp%color_b(iyr)
               enddo
               
               !----- Initialize green_leaf_factor and leaf_aging_factor. -----------------!
               doy = julday(imontha,idatea,iyeara)

               if(runtype /= 'HISTORY') then                 

                  print *, cpoly%a_par(isi)
                  print *, 
                  print *, 
                  print *, 
                  print *, 
                  print *, 
                  print *, 
                  print *, 
                  print *, 

                  yearsa = phen_temp%years(1)
                  yearsz = phen_temp%years(phen_temp%nyears)

                  if(iyeara > yearsz) then 
                     if(doy .lt. 212) then
                        cpoly%a_par(isi) = phen_temp%flush_a(mod(iyeara-phen_temp%years(1),phen_temp%nyears)+1)
                        cpoly%b_par(isi) = phen_temp%flush_b(mod(iyeara-phen_temp%years(1),phen_temp%nyears)+1)
                     else
                        cpoly%a_par(isi) = phen_temp%color_a(mod(iyeara-phen_temp%years(1),phen_temp%nyears)+1)
                        cpoly%b_par(isi) = phen_temp%color_b(mod(iyeara-phen_temp%years(1),phen_temp%nyears)+1)
                     end if

                     cpoly%a_fall(isi) = phen_temp%color_a(mod(iyeara-phen_temp%years(1),phen_temp%nyears)+1)
                  elseif(iyeara < yearsa) then 
                     if(doy .lt. 212) then
                        cpoly%a_par(isi) = phen_temp%flush_a(phen_temp%nyears - &
                             mod(phen_temp%years(phen_temp%nyears)-iyeara,phen_temp%nyears))
                        cpoly%b_par(isi) = phen_temp%flush_b(phen_temp%nyears - &
                             mod(phen_temp%years(phen_temp%nyears)-iyeara,phen_temp%nyears))
                     else
                        cpoly%a_par(isi) = phen_temp%color_a(phen_temp%nyears - &
                             mod(phen_temp%years(phen_temp%nyears)-iyeara,phen_temp%nyears))
                        cpoly%b_par(isi) = phen_temp%color_b(phen_temp%nyears - &
                             mod(phen_temp%years(phen_temp%nyears)-iyeara,phen_temp%nyears))
                     end if

                     cpoly%a_fall(isi) = phen_temp%color_a(phen_temp%nyears - &
                             mod(phen_temp%years(phen_temp%nyears)-iyeara,phen_temp%nyears))
                  else
                     if(doy .lt. 212) then
                        cpoly%a_par(isi) = phen_temp%flush_a(iyeara - phen_temp%years(1) + 1)
                        cpoly%b_par(isi) = phen_temp%flush_b(iyeara - phen_temp%years(1) + 1)
                     else
                        cpoly%a_par(isi) = phen_temp%color_a(iyeara - phen_temp%years(1) + 1)
                        cpoly%b_par(isi) = phen_temp%color_b(iyeara - phen_temp%years(1) + 1)
                     end if

                     cpoly%a_fall(isi) = phen_temp%color_a(iyeara - phen_temp%years(1) + 1)
                  end if

                  call prescribed_leaf_state(cgrid%lat(ipy), imontha, iyeara, doy                &
                                      ,cpoly%green_leaf_factor(:,isi)                      &
                                      ,cpoly%leaf_aging_factor(:,isi),cpoly%phen_pars(isi) &
                                      ,cpoly%a_par(isi),cpoly%b_par(isi),cpoly%a_fall(isi))
               end if
            end do siteloop

            deallocate(phen_temp%years)
            deallocate(phen_temp%flush_a)
            deallocate(phen_temp%flush_b)
            deallocate(phen_temp%color_a)
            deallocate(phen_temp%color_b)

         end do polyloop
      end do gridloop

      return
   end subroutine read_prescribed_phenology
   !=======================================================================================!
   !=======================================================================================!

end module phenology_startup
!==========================================================================================!
!==========================================================================================!
