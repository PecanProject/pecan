!==========================================================================================!
!==========================================================================================!
!     This subroutine will set a full history start for the simulation.  In a full history !
! start (runtype = 'HISTORY'), we assumes that you are continuing with the exact same      !
! configuration as a given model simulation that wrote the file in which you are using.    !
! In this case, each node starts with a list of polygons, and searches the HDF history     !
! file for these polygons.  It expects to find at least one polygon in the file within 250 !
! meters of proximity (although it should be exactly at the same place).  If it does not   !
! find this it stops.                                                                      !
!     Assuming that it finds the polygons, the subroutine then fills each of these         !
! polygons with data, and traverses the data hierarchical tree that roots from each        !
! polygon, initializing the model to the exact same state as the end of the previous run.  !
! A search based restart does not expect to find exact matches, and may not use all of the !
! polygons in the file.  Nonetheless, it will traverse the tree from these polygons and    !
! populate the model states with what is found in the files tree.                          !
!------------------------------------------------------------------------------------------!
subroutine init_full_history_restart()
   use ed_max_dims      , only : n_pft                 & ! intent(in)
                               , str_len               ! ! intent(in)
   use ed_misc_coms     , only : sfilin                & ! intent(in)
                               , current_time          & ! intent(in)
                               , max_poihist_dist      ! ! intent(in)
   use ed_state_vars    , only : polygontype           & ! structure
                               , sitetype              & ! structure
                               , patchtype             & ! structure
                               , edtype                & ! structure
                               , edgrid_g              & ! structure
                               , allocate_sitetype     & ! subroutine
                               , allocate_patchtype    & ! subroutine
                               , allocate_polygontype  ! ! subroutine
   use soil_coms        , only : alloc_soilgrid        ! ! subroutine
   use grid_coms        , only : ngrids                ! ! intent(in)
   use phenology_startup, only : phenology_init        ! ! subroutine
   use ed_node_coms     , only : mynum                 & ! intent(in)
                               , nmachs                & ! intent(in)
                               , nnodetot              & ! intent(in)
                               , mchnum                & ! intent(in)
                               , machs                 ! ! intent(in)
   use hdf5
   use hdf5_coms        , only : file_id               & ! intent(inout)
                               , dset_id               & ! intent(inout)
                               , dspace_id             & ! intent(inout)
                               , plist_id              & ! intent(inout)
                               , globdims              & ! intent(inout)
                               , chnkdims              & ! intent(inout)
                               , chnkoffs              ! ! intent(inout)
   implicit none
   !------ Standard common block. ---------------------------------------------------------!
   include 'mpif.h'
   !------ Local variables. ---------------------------------------------------------------!
   type(edtype)                        , pointer     :: cgrid
   type(polygontype)                   , pointer     :: cpoly
   type(sitetype)                      , pointer     :: csite
   type(patchtype)                     , pointer     :: cpatch
   character(len=3)                                  :: cgr
   character(len=str_len)                            :: hnamel
   integer               , dimension(:), allocatable :: pysi_n
   integer               , dimension(:), allocatable :: pysi_id
   integer               , dimension(:), allocatable :: sipa_n
   integer               , dimension(:), allocatable :: sipa_id
   integer               , dimension(:), allocatable :: paco_n
   integer               , dimension(:), allocatable :: paco_id
   integer                                           :: ngr
   integer                                           :: ifpy
   integer                                           :: ipft
   integer                                           :: ipy
   integer                                           :: isi
   integer                                           :: ipa
   integer                                           :: ico
   integer                                           :: py_index
   integer                                           :: si_index
   integer                                           :: pa_index
   integer                                           :: hdferr
   logical                                           :: exists
   real                  , dimension(:), allocatable :: file_lats
   real                  , dimension(:), allocatable :: file_lons
   real                                              :: mindist
   real                                              :: polydist
   real(kind=8)                                      :: dbletime
   !------ Local constants. ---------------------------------------------------------------!
   character(len=1)                    , parameter   :: vnam = 'S'
   !------ External functions. ------------------------------------------------------------!
   real                                , external    :: dist_gc
   !---------------------------------------------------------------------------------------!

   write (unit=*,fmt='(a)') '-----------------------------------------------------'
   write (unit=*,fmt='(a)') '  Loading Full State (HISTORY)'


   !----- Open the HDF environment. -------------------------------------------------------!
   call h5open_f(hdferr)
   !---------------------------------------------------------------------------------------!



   !---------------------------------------------------------------------------------------!
   !     Turn off automatic error printing.  This is done because there may be datasets    !
   ! that are not in the file, but it is OK.  If data that should be there are missing,    !
   ! ED2 error reporting will detect it.  If something that can't be found is missing, the !
   ! following call can be bypassed.  Note that automatic error reporting is turned back   !
   ! on in the end.                                                                        !
   !---------------------------------------------------------------------------------------!
   call h5eset_auto_f(0,hdferr)


   gridloop: do ngr=1,ngrids
      cgrid => edgrid_g(ngr)

      !------------------------------------------------------------------------------------!
      !     Make the history file name.                                                    !
      !------------------------------------------------------------------------------------!
      write(cgr,'(a1,i2.2)') 'g',ngr

      dbletime = dble(current_time%time)

      call makefnam(hnamel,sfilin(1),dbletime,current_time%year,current_time%month         &
                   ,current_time%date,0,vnam,cgr,'h5 ')
      inquire(file=trim(hnamel),exist=exists)

      if (.not.exists) then
         !----- History couldn't be found.  Stop the run. ---------------------------------!
         call fatal_error ('File '//trim(hnamel)//' not found.'                            &
                          ,'init_full_history_restart','ed_init_full_history.F90')
      else
         call h5fopen_f(hnamel, H5F_ACC_RDONLY_F, file_id, hdferr)
         if (hdferr < 0) then
            write(unit=*,fmt='(a,1x,i8)') 'Error opening HDF5 file - error - ',hdferr
            write(unit=*,fmt='(a,1x,a)' ) '- Filename: ',trim(hnamel)
            call fatal_error('Error opening HDF5 file - error - '//trim(hnamel)            &
                            ,'init_full_history_restart','ed_init_full_history.F90')
         end if
      end if


      !------------------------------------------------------------------------------------!
      !     Retrieve global vector sizes.                                                  !
      !------------------------------------------------------------------------------------!
      globdims    = 0_8
      chnkdims    = 0_8
      chnkoffs    = 0_8
      globdims(1) = 1_8
      
      call h5dopen_f(file_id,'NPOLYGONS_GLOBAL', dset_id, hdferr)
      call h5dget_space_f(dset_id, dspace_id, hdferr)
      call h5dread_f(dset_id, H5T_NATIVE_INTEGER,cgrid%npolygons_global,globdims, hdferr)
      call h5sclose_f(dspace_id, hdferr)
      call h5dclose_f(dset_id, hdferr)
      
      call h5dopen_f(file_id,'NSITES_GLOBAL', dset_id, hdferr)
      call h5dget_space_f(dset_id, dspace_id, hdferr)
      call h5dread_f(dset_id, H5T_NATIVE_INTEGER,cgrid%nsites_global,globdims, hdferr)
      call h5sclose_f(dspace_id, hdferr)
      call h5dclose_f(dset_id, hdferr)
      
      call h5dopen_f(file_id,'NPATCHES_GLOBAL', dset_id, hdferr)
      call h5dget_space_f(dset_id, dspace_id, hdferr)
      call h5dread_f(dset_id, H5T_NATIVE_INTEGER,cgrid%npatches_global,globdims, hdferr)
      call h5sclose_f(dspace_id, hdferr)
      call h5dclose_f(dset_id, hdferr)
      
      call h5dopen_f(file_id,'NCOHORTS_GLOBAL', dset_id, hdferr)
      call h5dget_space_f(dset_id, dspace_id, hdferr)
      call h5dread_f(dset_id, H5T_NATIVE_INTEGER,cgrid%ncohorts_global,globdims, hdferr)
      call h5sclose_f(dspace_id, hdferr)
      call h5dclose_f(dset_id, hdferr)
      !------------------------------------------------------------------------------------!



      !------------------------------------------------------------------------------------!
      !     Retrieve the mapping of the data tree.                                         !
      !------------------------------------------------------------------------------------!
      globdims    = 0_8
      globdims(1) = int(cgrid%npolygons_global,8)
      
      allocate(pysi_n(cgrid%npolygons_global))
      allocate(pysi_id(cgrid%npolygons_global))
      
      call h5dopen_f(file_id,'PYSI_N', dset_id, hdferr)
      call h5dget_space_f(dset_id, dspace_id, hdferr)
      call h5dread_f(dset_id, H5T_NATIVE_INTEGER,pysi_n,globdims, hdferr)
      call h5sclose_f(dspace_id, hdferr)
      call h5dclose_f(dset_id, hdferr)
      
      call h5dopen_f(file_id,'PYSI_ID', dset_id, hdferr)
      call h5dget_space_f(dset_id, dspace_id, hdferr)
      call h5dread_f(dset_id, H5T_NATIVE_INTEGER,pysi_id,globdims, hdferr)
      call h5sclose_f(dspace_id, hdferr)
      call h5dclose_f(dset_id, hdferr)
      
      globdims(1) = int(cgrid%nsites_global,8)
      
      allocate(sipa_n(cgrid%nsites_global))
      allocate(sipa_id(cgrid%nsites_global))
      
      call h5dopen_f(file_id,'SIPA_N', dset_id, hdferr)
      call h5dget_space_f(dset_id, dspace_id, hdferr)
      call h5dread_f(dset_id, H5T_NATIVE_INTEGER,sipa_n,globdims, hdferr)
      call h5sclose_f(dspace_id, hdferr)
      call h5dclose_f(dset_id, hdferr)
      
      call h5dopen_f(file_id,'SIPA_ID', dset_id, hdferr)
      call h5dget_space_f(dset_id, dspace_id, hdferr)
      call h5dread_f(dset_id, H5T_NATIVE_INTEGER,sipa_id,globdims, hdferr)
      call h5sclose_f(dspace_id, hdferr)
      call h5dclose_f(dset_id, hdferr)
      
      globdims(1) = int(cgrid%npatches_global,8)
      
      allocate(paco_n(cgrid%npatches_global))
      allocate(paco_id(cgrid%npatches_global))
      
      call h5dopen_f(file_id,'PACO_N', dset_id, hdferr)
      call h5dget_space_f(dset_id, dspace_id, hdferr)
      call h5dread_f(dset_id, H5T_NATIVE_INTEGER,paco_n,globdims, hdferr)
      call h5sclose_f(dspace_id, hdferr)
      call h5dclose_f(dset_id, hdferr)
      
      call h5dopen_f(file_id,'PACO_ID', dset_id, hdferr)
      call h5dget_space_f(dset_id, dspace_id, hdferr)
      call h5dread_f(dset_id, H5T_NATIVE_INTEGER,paco_id,globdims, hdferr)
      call h5sclose_f(dspace_id, hdferr)
      call h5dclose_f(dset_id, hdferr)
      !------------------------------------------------------------------------------------!
      
      
      !------------------------------------------------------------------------------------!
      !      Retrieve the polygon coordinates data.                                        !
      !------------------------------------------------------------------------------------!
      globdims(1) = int(cgrid%npolygons_global,8)
      allocate(file_lats(cgrid%npolygons_global))
      allocate(file_lons(cgrid%npolygons_global))
      
      call h5dopen_f(file_id,'LATITUDE', dset_id, hdferr)
      call h5dget_space_f(dset_id, dspace_id, hdferr)
      call h5dread_f(dset_id, H5T_NATIVE_REAL,file_lats,globdims, hdferr)
      call h5sclose_f(dspace_id, hdferr)
      call h5dclose_f(dset_id, hdferr)

      call h5dopen_f(file_id,'LONGITUDE', dset_id, hdferr)
      call h5dget_space_f(dset_id, dspace_id, hdferr)
      call h5dread_f(dset_id, H5T_NATIVE_REAL,file_lons,globdims, hdferr)
      call h5sclose_f(dspace_id, hdferr)
      call h5dclose_f(dset_id, hdferr)

      !------------------------------------------------------------------------------------!
      !     Loop the polygons in the model state and match them with those in the file.    !
      ! After the match, we must walk through the data from that polygon and initialize.   !
      ! We check the distance between the expected coordinates and the retrieved ones, and !
      ! they ought to be less than 250 metres apart, otherwise we can't use the polygon.   !
      !------------------------------------------------------------------------------------!
      polyloop: do ipy = 1,cgrid%npolygons
         cpoly => cgrid%polygon(ipy)

         py_index   = 0
         mindist    = huge(1.)
         do ifpy = 1,cgrid%npolygons_global
            polydist = dist_gc(file_lons(ifpy),cgrid%lon(ipy)                              &
                              ,file_lats(ifpy),cgrid%lat(ipy))
            if (polydist < mindist) then
               mindist    = polydist
               py_index   = ifpy
            end if
         end do

         !---------------------------------------------------------------------------------!
         !     Check whether the closest polygon is close.                                 !
         !---------------------------------------------------------------------------------!
         if (mindist > max_poihist_dist) then
            write (unit=*,fmt='(a)'          ) '------------------------------------------'
            write (unit=*,fmt='(a)'          ) ' None of the polygons in the history file'
            write (unit=*,fmt='(a)'          ) '    is enough close!  The model will stop!'
            write (unit=*,fmt='(a)'          ) ' ED Polygon:'
            write (unit=*,fmt='(a,1x,i8)'    ) ' - Polygon   :',ipy
            write (unit=*,fmt='(a,1x,es12.5)') ' - Longitude :',cgrid%lon(ipy)
            write (unit=*,fmt='(a,1x,es12.5)') ' - Latitude  :',cgrid%lat(ipy)
            write (unit=*,fmt='(a)'          ) ' History file''s closest polygon:'
            write (unit=*,fmt='(a,1x,i8)'    ) ' - Polygon   :',py_index
            write (unit=*,fmt='(a,1x,es12.5)') ' - Longitude :',file_lons(py_index)
            write (unit=*,fmt='(a,1x,es12.5)') ' - Latitude  :',file_lats(py_index)
            write (unit=*,fmt='(a)'          ) '------------------------------------------'

            call fatal_error('Mismatch between polygon and dataset'                        &
                            ,'init_full_history_restart','ed_init_full_history.F90')
         end if
         !---------------------------------------------------------------------------------!

         !---------------------------------------------------------------------------------!
         !      Get all necessary polygon variables associated with this index for the     !
         ! current polygon.                                                                !
         !---------------------------------------------------------------------------------!
         call fill_history_grid(cgrid,ipy,py_index)

         if (pysi_n(py_index) > 0) then
            !----- Allocate the polygontype structure (site level). -----------------------!
            call allocate_polygontype(cpoly,pysi_n(py_index))
            !------------------------------------------------------------------------------!



            !------------------------------------------------------------------------------!
            !     Get all necessary site variables associated with this index for the      !
            ! current polygon.                                                             !
            !------------------------------------------------------------------------------!
            call fill_history_polygon(cpoly,pysi_id(py_index),cgrid%nsites_global)
            
            siteloop: do isi = 1,cpoly%nsites
               csite => cpoly%site(isi)
               
               !------ Calculate the index of this site's data in the HDF. ----------------!
               si_index = pysi_id(py_index) + isi - 1

               if (sipa_n(si_index) > 0) then
                  !----- Allocate the sitetype structure (patch level). -------------------!
                  call allocate_sitetype(csite,sipa_n(si_index))

                  !------------------------------------------------------------------------!
                  !     Get all necessary site variables associated with this index for    !
                  ! the current site.                                                      !
                  !------------------------------------------------------------------------!
                  call fill_history_site(csite,sipa_id(si_index),cgrid%npatches_global)

                  patchloop: do ipa = 1,csite%npatches
                     cpatch => csite%patch(ipa)
                     pa_index = sipa_id(si_index) + ipa - 1

                     if (paco_n(pa_index) > 0) then
                        !----- Allocate the patchtype structure (cohort level). -----------!
                        call allocate_patchtype(cpatch,paco_n(pa_index))
                        
                        !------------------------------------------------------------------!
                        !     Get all necessary site variables associated with this index  !
                        ! for the current patch.                                           !
                        !------------------------------------------------------------------!
                        call fill_history_patch(cpatch,paco_id(pa_index)                   &
                                               ,cgrid%ncohorts_global                      &
                                               ,cpoly%green_leaf_factor(:,isi))
                     else
                        cpatch%ncohorts = 0
                     endif
                  end do patchloop
               else
                  write (unit=*,fmt='(a)'          ) '------------------------------------'
                  write (unit=*,fmt='(a)'          ) ' Found a site with no patches.'
                  write (unit=*,fmt='(a)'          ) ' This is not allowed.'
                  write (unit=*,fmt='(a)'          ) ' ED Polygon and Site:'
                  write (unit=*,fmt='(a,1x,i8)'    ) ' - Polygon   :',ipy
                  write (unit=*,fmt='(a,1x,i8)'    ) ' - Site      :',isi
                  write (unit=*,fmt='(a,1x,es12.5)') ' - Longitude :',cgrid%lon(ipy)
                  write (unit=*,fmt='(a,1x,es12.5)') ' - Latitude  :',cgrid%lat(ipy)
                  write (unit=*,fmt='(a)'          ) '------------------------------------'
                  call fatal_error('Attempted to load an empty site.'                      &
                                  ,'init_full_history_restart','ed_init_full_history.F90')
               end if

            end do siteloop
            
         else
            write (unit=*,fmt='(a)'          ) '------------------------------------'
            write (unit=*,fmt='(a)'          ) ' Found a polygon with no sites.'
            write (unit=*,fmt='(a)'          ) ' This is not allowed.'
            write (unit=*,fmt='(a)'          ) ' ED Polygon:'
            write (unit=*,fmt='(a,1x,i8)'    ) ' - Polygon   :',ipy
            write (unit=*,fmt='(a,1x,es12.5)') ' - Longitude :',cgrid%lon(ipy)
            write (unit=*,fmt='(a,1x,es12.5)') ' - Latitude  :',cgrid%lat(ipy)
            write (unit=*,fmt='(a)'          ) '------------------------------------'
            call fatal_error('Attempted to load an empty polygon.'                         &
                            ,'init_full_history_restart','ed_init_full_history.F90')
         end if
      end do polyloop

      call h5fclose_f(file_id, hdferr)
      if (hdferr /= 0) then
          print*,hdferr
          call fatal_error('Could not close the HDF file'                                  &
                          ,'init_full_history_restart','ed_init_full_history.F90')
          
      end if

      deallocate(file_lats)
      deallocate(file_lons)
      deallocate(paco_n)
      deallocate(paco_id)
      deallocate(sipa_n)
      deallocate(sipa_id)
      deallocate(pysi_n)
      deallocate(pysi_id)

   end do gridloop


   !---------------------------------------------------------------------------------------!
   !     Turn automatic error reporting back on.  This is probably unnecessary, because    !
   ! the environment is about to be flushed.                                               !
   !---------------------------------------------------------------------------------------!
   call h5eset_auto_f(1,hdferr)
   !---------------------------------------------------------------------------------------!


   !----- Close the HDF environment. ------------------------------------------------------!
   call h5close_f(hdferr)
   !---------------------------------------------------------------------------------------!


   !----- Load the anthropogenic disturbance (or set them all to zero). -------------------!
   write(unit=*,fmt='(a,i2.2)') ' Checking anthropogenic disturbance.  Node: ',mynum
   call landuse_init()
   !---------------------------------------------------------------------------------------!


   !----- Load phenology in case it is prescribed (or set them with defaults). ------------!
   write(unit=*,fmt='(a,i2.2)') ' Checking prescribed phenology.  Node: ',mynum
   call phenology_init()

   return
end subroutine init_full_history_restart
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
subroutine fill_history_grid(cgrid,ipy,py_index)

  use ed_state_vars,only: edtype,polygontype
  use grid_coms,only : nzg
  use ed_max_dims,only : n_pft,n_dbh,n_age,n_dist_types
  use hdf5
  use hdf5_coms,only:file_id,dset_id,dspace_id,plist_id, &
       globdims,chnkdims,chnkoffs,cnt,stride, &
       memdims,memoffs,memsize,datatype_id
  use ed_misc_coms, only: ndcycle
  implicit none

  
#if USE_INTERF
  interface
     subroutine hdf_getslab_r(buff,varn,dsetrank,iparallel,required,foundvar)
       use hdf5_coms,only:memsize
       real,dimension(memsize(1),memsize(2),memsize(3),memsize(4)) :: buff
       integer :: dsetrank,iparallel
       character(len=*),intent(in) :: varn
       logical,intent(in) :: required
       logical,intent(out) :: foundvar
     end subroutine hdf_getslab_r
     subroutine hdf_getslab_d(buff,varn,dsetrank,iparallel,required,foundvar)
       use hdf5_coms,only:memsize
       real(kind=8),dimension(memsize(1),memsize(2),memsize(3),memsize(4)) :: buff
       integer :: dsetrank,iparallel
       character(len=*),intent(in) :: varn
       logical,intent(in) :: required
       logical,intent(out) :: foundvar
     end subroutine hdf_getslab_d
     subroutine hdf_getslab_i(buff,varn,dsetrank,iparallel,required,foundvar)
       use hdf5_coms,only:memsize
       integer,dimension(memsize(1),memsize(2),memsize(3),memsize(4)) :: buff
       integer :: dsetrank,iparallel
       character(len=*),intent(in) :: varn
       logical,intent(in) :: required
       logical,intent(out) :: foundvar
     end subroutine hdf_getslab_i
  end interface
#endif

  type(edtype),target ::       cgrid

  integer,intent(in)  :: ipy,py_index
  integer             :: iparallel
  integer             :: dsetrank
  integer(SIZE_T)     :: sz
  integer             :: hdferr
  logical             :: foundvar

  iparallel = 0
 

  globdims = 0_8
  chnkdims = 0_8
  chnkoffs = 0_8
  memoffs  = 0_8
  memdims  = 0_8
  memsize  = 1_8

  dsetrank = 1

  ! These are the dimensions in the filespace
  ! itself. Global is the size of the dataset,
  ! chnkoffs is the offset of the chunk we
  ! are going to read.  Chnkdims is the size
  ! of the slab that is to be read.
  
  globdims(1) = int(cgrid%npolygons_global,8)
  chnkdims(1) = 1_8
  chnkoffs(1) = int(py_index - 1,8)

  ! These are the dimensions for the memory space
  ! this should essentially be the same dimensioning
  ! as the buffer that we are filling. This routine
  ! is just filling a scalar point in a vector
  ! of polygons.

  memdims(1)  = 1_8
  memoffs(1)  = 0_8
  memsize(1)  = 1_8


  call hdf_getslab_d(cgrid%walltime_py(ipy:ipy),'WALLTIME_PY ',dsetrank,iparallel,.false.,foundvar)

  call hdf_getslab_i(cgrid%lsl(ipy:ipy),'LSL ',dsetrank,iparallel,.true.,foundvar)

  call hdf_getslab_i(cgrid%ncol_soil(ipy:ipy),'NCOL_SOIL ',dsetrank,iparallel,.false.,foundvar)

  call hdf_getslab_r(cgrid%wbar(ipy:ipy),'WBAR ',dsetrank,iparallel,.true.,foundvar)
  
  call hdf_getslab_r(cgrid%Te(ipy:ipy),'TE ',dsetrank,iparallel,.true.,foundvar)

  call hdf_getslab_r(cgrid%zbar(ipy:ipy),'ZBAR ',dsetrank,iparallel,.true.,foundvar)

!!  call hdf_getslab_r(cgrid%tau(ipy:ipy),'TAU ',dsetrank,iparallel,.true.,foundvar)

  call hdf_getslab_r(cgrid%sheat(ipy:ipy),'SHEAT ',dsetrank,iparallel,.true.,foundvar)

  call hdf_getslab_r(cgrid%baseflow(ipy:ipy),'BASEFLOW ',dsetrank,iparallel,.true.,foundvar)

  call hdf_getslab_i(cgrid%load_adjacency(ipy:ipy),'LOAD_ADJACENCY ',dsetrank,iparallel,.true.,foundvar)

  call hdf_getslab_r(cgrid%swliq(ipy:ipy),'SWLIQ ',dsetrank,iparallel,.true.,foundvar)

  call hdf_getslab_r(cgrid%cosz(ipy:ipy),'COSZ ',dsetrank,iparallel,.true.,foundvar)


  call hdf_getslab_r(cgrid%lai    (ipy:ipy),'LAI     ',dsetrank,iparallel,.true.,foundvar)
  call hdf_getslab_r(cgrid%wai    (ipy:ipy),'WAI     ',dsetrank,iparallel,.true.,foundvar)
  call hdf_getslab_r(cgrid%avg_lma(ipy:ipy),'AVG_LMA ',dsetrank,iparallel,.false.,foundvar)
 
  call hdf_getslab_r(cgrid%runoff(ipy:ipy),'RUNOFF ',dsetrank,iparallel,.true.,foundvar)

  call hdf_getslab_r(cgrid%cbudget_initialstorage (ipy:ipy),'CBUDGET_INITIALSTORAGE '      &
                    ,dsetrank,iparallel,.true.,foundvar)
  call hdf_getslab_r(cgrid%cbudget_nep            (ipy:ipy),'CBUDGET_NEP '                 &
                    ,dsetrank,iparallel,.true.,foundvar)
  call hdf_getslab_r(cgrid%nbudget_initialstorage (ipy:ipy),'NBUDGET_INITIALSTORAGE '      &
                    ,dsetrank,iparallel,.true.,foundvar)


  call hdf_getslab_r(cgrid%total_agb                (ipy:ipy),'TOTAL_AGB '                 &
                    ,dsetrank,iparallel,.true.,foundvar)
  call hdf_getslab_r(cgrid%total_agb_growth         (ipy:ipy),'TOTAL_AGB_GROWTH '          &
                    ,dsetrank,iparallel,.true.,foundvar)
  call hdf_getslab_r(cgrid%total_agb_mort           (ipy:ipy),'TOTAL_AGB_MORT '            &
                    ,dsetrank,iparallel,.true.,foundvar)
  call hdf_getslab_r(cgrid%total_agb_recruit        (ipy:ipy),'TOTAL_AGB_RECRUIT '         &
                    ,dsetrank,iparallel,.true.,foundvar)

  call hdf_getslab_r(cgrid%total_basal_area         (ipy:ipy),'TOTAL_BASAL_AREA '          &
                    ,dsetrank,iparallel,.true.,foundvar)
  call hdf_getslab_r(cgrid%total_basal_area_growth  (ipy:ipy),'TOTAL_BASAL_AREA_GROWTH '   &
                    ,dsetrank,iparallel,.true.,foundvar)
  call hdf_getslab_r(cgrid%total_basal_area_mort    (ipy:ipy),'TOTAL_BASAL_AREA_MORT '     &
                    ,dsetrank,iparallel,.true.,foundvar)
  call hdf_getslab_r(cgrid%total_basal_area_recruit (ipy:ipy),'TOTAL_BASAL_AREA_RECRUIT '  &
                    ,dsetrank,iparallel,.true.,foundvar)
 
  ! All daily and monthly variables need to be retrieved if you are loading there...
  
  if (associated(cgrid%dmean_pcpg           ))                                             &
     call hdf_getslab_r(cgrid%dmean_pcpg           (ipy:ipy) ,'DMEAN_PCPG            '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_runoff         ))                                             &
     call hdf_getslab_r(cgrid%dmean_runoff         (ipy:ipy) ,'DMEAN_RUNOFF          '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_drainage       ))                                             &
     call hdf_getslab_r(cgrid%dmean_drainage       (ipy:ipy) ,'DMEAN_DRAINAGE        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_gpp            ))                                             &
     call hdf_getslab_r(cgrid%dmean_gpp            (ipy:ipy) ,'DMEAN_GPP             '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_nppleaf        ))                                             &
     call hdf_getslab_r(cgrid%dmean_nppleaf        (ipy:ipy) ,'DMEAN_NPPLEAF         '     &
                       ,dsetrank,iparallel,.false.,foundvar)
                       
  if (associated(cgrid%dmean_nppfroot       ))                                             &
     call hdf_getslab_r(cgrid%dmean_nppfroot       (ipy:ipy) ,'DMEAN_NPPFROOT        '     &
                       ,dsetrank,iparallel,.false.,foundvar)
                       
  if (associated(cgrid%dmean_nppsapwood     ))                                             &
     call hdf_getslab_r(cgrid%dmean_nppsapwood     (ipy:ipy) ,'DMEAN_NPPSAPWOOD      '     &
                       ,dsetrank,iparallel,.false.,foundvar)
                       
  if (associated(cgrid%dmean_nppcroot       ))                                             &
     call hdf_getslab_r(cgrid%dmean_nppcroot       (ipy:ipy) ,'DMEAN_NPPCROOT        '     &
                       ,dsetrank,iparallel,.false.,foundvar)
                       
  if (associated(cgrid%dmean_nppseeds       ))                                             &
     call hdf_getslab_r(cgrid%dmean_nppseeds       (ipy:ipy) ,'DMEAN_NPPSEEDS        '     &
                       ,dsetrank,iparallel,.false.,foundvar)
                       
  if (associated(cgrid%dmean_nppwood        ))                                             &
     call hdf_getslab_r(cgrid%dmean_nppwood        (ipy:ipy) ,'DMEAN_NPPWOOD         '     &
                       ,dsetrank,iparallel,.false.,foundvar)
                       
  if (associated(cgrid%dmean_nppdaily       ))                                             &
     call hdf_getslab_r(cgrid%dmean_nppdaily       (ipy:ipy) ,'DMEAN_NPPDAILY        '     &
                       ,dsetrank,iparallel,.false.,foundvar)
                       
  if (associated(cgrid%dmean_evap           ))                                             &
     call hdf_getslab_r(cgrid%dmean_evap           (ipy:ipy) ,'DMEAN_EVAP            '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_transp         ))                                             &
     call hdf_getslab_r(cgrid%dmean_transp         (ipy:ipy) ,'DMEAN_TRANSP          '     &
                       ,dsetrank,iparallel,.false.,foundvar)                                        

  if (associated(cgrid%dmean_sensible_lc    ))                                             &
     call hdf_getslab_r(cgrid%dmean_sensible_lc    (ipy:ipy) ,'DMEAN_SENSIBLE_LC     '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_sensible_wc    ))                                             &
     call hdf_getslab_r(cgrid%dmean_sensible_wc    (ipy:ipy) ,'DMEAN_SENSIBLE_WC     '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_sensible_gc    ))                                             &
     call hdf_getslab_r(cgrid%dmean_sensible_gc    (ipy:ipy) ,'DMEAN_SENSIBLE_GC     '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_sensible_ac    ))                                             &
     call hdf_getslab_r(cgrid%dmean_sensible_ac    (ipy:ipy) ,'DMEAN_SENSIBLE_AC     '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_vapor_lc       ))                                             &
     call hdf_getslab_r(cgrid%dmean_vapor_lc       (ipy:ipy) ,'DMEAN_VAPOR_LC        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_vapor_wc       ))                                             &
     call hdf_getslab_r(cgrid%dmean_vapor_wc       (ipy:ipy) ,'DMEAN_VAPOR_WC        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_vapor_gc       ))                                             &
     call hdf_getslab_r(cgrid%dmean_vapor_gc       (ipy:ipy) ,'DMEAN_VAPOR_GC        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_vapor_ac       ))                                             &
     call hdf_getslab_r(cgrid%dmean_vapor_ac       (ipy:ipy) ,'DMEAN_VAPOR_AC        '     &
                       ,dsetrank,iparallel,.false.,foundvar)                                        

  if (associated(cgrid%dmean_ustar          ))                                             &
     call hdf_getslab_r(cgrid%dmean_ustar          (ipy:ipy) ,'DMEAN_USTAR           '     &
                       ,dsetrank,iparallel,.false.,foundvar)                                        

  if (associated(cgrid%dmean_tstar          ))                                             &
     call hdf_getslab_r(cgrid%dmean_tstar          (ipy:ipy) ,'DMEAN_TSTAR           '     &
                       ,dsetrank,iparallel,.false.,foundvar)                                        

  if (associated(cgrid%dmean_qstar          ))                                             &
     call hdf_getslab_r(cgrid%dmean_qstar          (ipy:ipy) ,'DMEAN_QSTAR           '     &
                       ,dsetrank,iparallel,.false.,foundvar)                                        

  if (associated(cgrid%dmean_cstar          ))                                             &
     call hdf_getslab_r(cgrid%dmean_cstar          (ipy:ipy) ,'DMEAN_CSTAR           '     &
                       ,dsetrank,iparallel,.false.,foundvar)                                        

  if (associated(cgrid%dmean_nep            ))                                             &
     call hdf_getslab_r(cgrid%dmean_nep            (ipy:ipy) ,'DMEAN_NEP             '     &
                       ,dsetrank,iparallel,.false.,foundvar)                                        

  if (associated(cgrid%dmean_carbon_ac      ))                                             &
     call hdf_getslab_r(cgrid%dmean_carbon_ac      (ipy:ipy) ,'DMEAN_CARBON_AC       '     &
                       ,dsetrank,iparallel,.false.,foundvar)                                        

  if (associated(cgrid%dmean_carbon_st      ))                                             &
     call hdf_getslab_r(cgrid%dmean_carbon_st      (ipy:ipy) ,'DMEAN_CARBON_ST       '     &
                       ,dsetrank,iparallel,.false.,foundvar)                                        

  if (associated(cgrid%dmean_plresp         ))                                             &
     call hdf_getslab_r(cgrid%dmean_plresp         (ipy:ipy) ,'DMEAN_PLRESP          '     &
                       ,dsetrank,iparallel,.false.,foundvar)                                        

  if (associated(cgrid%dmean_rh             ))                                             &
     call hdf_getslab_r(cgrid%dmean_rh             (ipy:ipy) ,'DMEAN_RH              '     &
                       ,dsetrank,iparallel,.false.,foundvar)                                        

  if (associated(cgrid%dmean_cwd_rh         ))                                             &
     call hdf_getslab_r(cgrid%dmean_cwd_rh         (ipy:ipy) ,'DMEAN_CWD_RH          '     &
                       ,dsetrank,iparallel,.false.,foundvar)                                        

  if (associated(cgrid%dmean_leaf_resp      ))                                             &
     call hdf_getslab_r(cgrid%dmean_leaf_resp      (ipy:ipy) ,'DMEAN_LEAF_RESP       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_root_resp      ))                                             &
     call hdf_getslab_r(cgrid%dmean_root_resp      (ipy:ipy) ,'DMEAN_ROOT_RESP       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_growth_resp    ))                                             &
     call hdf_getslab_r(cgrid%dmean_growth_resp    (ipy:ipy) ,'DMEAN_GROWTH_RESP     '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_storage_resp   ))                                             &
     call hdf_getslab_r(cgrid%dmean_storage_resp   (ipy:ipy) ,'DMEAN_STORAGE_RESP    '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_vleaf_resp     ))                                             &
     call hdf_getslab_r(cgrid%dmean_vleaf_resp     (ipy:ipy) ,'DMEAN_VLEAF_RESP      '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_fs_open        ))                                             &
     call hdf_getslab_r(cgrid%dmean_fs_open        (ipy:ipy) ,'DMEAN_FS_OPEN         '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_fsw            ))                                             &
     call hdf_getslab_r(cgrid%dmean_fsw            (ipy:ipy) ,'DMEAN_FSW             '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_fsn            ))                                             &
     call hdf_getslab_r(cgrid%dmean_fsn            (ipy:ipy) ,'DMEAN_FSN             '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_can_temp       ))                                             &
     call hdf_getslab_r(cgrid%dmean_can_temp       (ipy:ipy) ,'DMEAN_CAN_TEMP        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_can_shv        ))                                             &
     call hdf_getslab_r(cgrid%dmean_can_shv        (ipy:ipy) ,'DMEAN_CAN_SHV         '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_can_prss       ))                                             &
     call hdf_getslab_r(cgrid%dmean_can_prss       (ipy:ipy) ,'DMEAN_CAN_PRSS        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_can_theta      ))                                             &
     call hdf_getslab_r(cgrid%dmean_can_theta      (ipy:ipy) ,'DMEAN_CAN_THETA       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_can_theiv      ))                                             &
     call hdf_getslab_r(cgrid%dmean_can_theiv      (ipy:ipy) ,'DMEAN_CAN_THEIV       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_can_vpdef      ))                                             &
     call hdf_getslab_r(cgrid%dmean_can_vpdef      (ipy:ipy) ,'DMEAN_CAN_VPDEF       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_can_co2        ))                                             &
     call hdf_getslab_r(cgrid%dmean_can_co2        (ipy:ipy) ,'DMEAN_CAN_CO2         '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_can_rhos       ))                                             &
     call hdf_getslab_r(cgrid%dmean_can_rhos       (ipy:ipy) ,'DMEAN_CAN_RHOS        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_gnd_temp       ))                                             &
     call hdf_getslab_r(cgrid%dmean_gnd_temp       (ipy:ipy) ,'DMEAN_GND_TEMP        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_gnd_shv        ))                                             &
     call hdf_getslab_r(cgrid%dmean_gnd_shv        (ipy:ipy) ,'DMEAN_GND_SHV         '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_leaf_energy    ))                                             &
     call hdf_getslab_r(cgrid%dmean_leaf_energy    (ipy:ipy) ,'DMEAN_LEAF_ENERGY     '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_leaf_water     ))                                             &
     call hdf_getslab_r(cgrid%dmean_leaf_water     (ipy:ipy) ,'DMEAN_LEAF_WATER      '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_leaf_hcap      ))                                             &
     call hdf_getslab_r(cgrid%dmean_leaf_hcap      (ipy:ipy) ,'DMEAN_LEAF_HCAP       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_leaf_temp      ))                                             &
     call hdf_getslab_r(cgrid%dmean_leaf_temp      (ipy:ipy) ,'DMEAN_LEAF_TEMP       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_leaf_vpdef     ))                                             &
     call hdf_getslab_r(cgrid%dmean_leaf_vpdef     (ipy:ipy) ,'DMEAN_LEAF_VPDEF      '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_wood_energy    ))                                             &
     call hdf_getslab_r(cgrid%dmean_wood_energy    (ipy:ipy) ,'DMEAN_WOOD_ENERGY     '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_wood_water     ))                                             &
     call hdf_getslab_r(cgrid%dmean_wood_water     (ipy:ipy) ,'DMEAN_WOOD_WATER      '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_wood_hcap      ))                                             &
     call hdf_getslab_r(cgrid%dmean_wood_hcap      (ipy:ipy) ,'DMEAN_WOOD_HCAP       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_wood_temp      ))                                             &
     call hdf_getslab_r(cgrid%dmean_wood_temp      (ipy:ipy) ,'DMEAN_WOOD_TEMP       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_atm_temp       ))                                             &
     call hdf_getslab_r(cgrid%dmean_atm_temp       (ipy:ipy) ,'DMEAN_ATM_TEMP        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_atm_vpdef      ))                                             &
     call hdf_getslab_r(cgrid%dmean_atm_vpdef      (ipy:ipy) ,'DMEAN_ATM_VPDEF       '     &
                       ,dsetrank,iparallel,.false.,foundvar)
  
  if (associated(cgrid%dmean_rshort       ))                                               &
       call hdf_getslab_r(cgrid%dmean_rshort       (ipy:ipy) ,'DMEAN_RSHORT        '       &
       ,dsetrank,iparallel,.false.,foundvar)
  
  if (associated(cgrid%dmean_rshort_diff  ))                                               &
       call hdf_getslab_r(cgrid%dmean_rshort_diff  (ipy:ipy) ,'DMEAN_RSHORT_DIFF   '       &
       ,dsetrank,iparallel,.false.,foundvar)
  
  if (associated(cgrid%dmean_rlong       ))                                                &
       call hdf_getslab_r(cgrid%dmean_rlong       (ipy:ipy) ,'DMEAN_RLONG        '         &
       ,dsetrank,iparallel,.false.,foundvar)
  
  if (associated(cgrid%dmean_rlongup     ))                                                &
       call hdf_getslab_r(cgrid%dmean_rlongup     (ipy:ipy) ,'DMEAN_RLONGUP      '         &
       ,dsetrank,iparallel,.false.,foundvar)
  
  if (associated(cgrid%dmean_parup       ))                                                &
       call hdf_getslab_r(cgrid%dmean_parup       (ipy:ipy) ,'DMEAN_PARUP        '         &
       ,dsetrank,iparallel,.false.,foundvar)
  
  if (associated(cgrid%dmean_nirup       ))                                                &
       call hdf_getslab_r(cgrid%dmean_nirup       (ipy:ipy) ,'DMEAN_NIRUP        '         &
       ,dsetrank,iparallel,.false.,foundvar)
  
  if (associated(cgrid%dmean_rshortup    ))                                                &
       call hdf_getslab_r(cgrid%dmean_rshortup    (ipy:ipy) ,'DMEAN_RSHORTUP     '         &
       ,dsetrank,iparallel,.false.,foundvar)
  
  if (associated(cgrid%dmean_rnet        ))                                                &
       call hdf_getslab_r(cgrid%dmean_rnet        (ipy:ipy) ,'DMEAN_RNET         '         &
       ,dsetrank,iparallel,.false.,foundvar)
  
  if (associated(cgrid%dmean_rshort_gnd  ))                                                &
       call hdf_getslab_r(cgrid%dmean_rshort_gnd  (ipy:ipy) ,'DMEAN_RSHORT_GND     '       &
       ,dsetrank,iparallel,.false.,foundvar)
  
  if (associated(cgrid%dmean_rlong_gnd   ))                                                &
       call hdf_getslab_r(cgrid%dmean_rlong_gnd   (ipy:ipy) ,'DMEAN_RLONG_GND    '         &
       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_atm_shv        ))                                             &
     call hdf_getslab_r(cgrid%dmean_atm_shv        (ipy:ipy) ,'DMEAN_ATM_SHV         '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_atm_co2        ))                                             &
     call hdf_getslab_r(cgrid%dmean_atm_co2        (ipy:ipy) ,'DMEAN_ATM_CO2         '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_atm_prss       ))                                             &
     call hdf_getslab_r(cgrid%dmean_atm_prss       (ipy:ipy) ,'DMEAN_ATM_PRSS        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_atm_vels       ))                                             &
     call hdf_getslab_r(cgrid%dmean_atm_vels       (ipy:ipy) ,'DMEAN_ATM_VELS        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_co2_residual   ))                                             &
     call hdf_getslab_r(cgrid%dmean_co2_residual   (ipy:ipy) ,'DMEAN_CO2_RESIDUAL    '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_energy_residual))                                             &
     call hdf_getslab_r(cgrid%dmean_energy_residual(ipy:ipy) ,'DMEAN_ENERGY_RESIDUAL '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%dmean_water_residual ))                                             &
     call hdf_getslab_r(cgrid%dmean_water_residual (ipy:ipy) ,'DMEAN_WATER_RESIDUAL  '     &
                       ,dsetrank,iparallel,.false.,foundvar)


  if (associated(cgrid%mmean_co2_residual   ))                                             &
     call hdf_getslab_r(cgrid%mmean_co2_residual   (ipy:ipy) ,'MMEAN_CO2_RESIDUAL    '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_energy_residual))                                             &
     call hdf_getslab_r(cgrid%mmean_energy_residual(ipy:ipy) ,'MMEAN_ENERGY_RESIDUAL '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_water_residual ))                                             &
     call hdf_getslab_r(cgrid%mmean_water_residual (ipy:ipy) ,'MMEAN_WATER_RESIDUAL  '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_gpp            ))                                             &
     call hdf_getslab_r(cgrid%mmean_gpp            (ipy:ipy) ,'MMEAN_GPP             '     &
                       ,dsetrank,iparallel,.false.,foundvar)
                       
  if (associated(cgrid%mmean_nppleaf        ))                                             &
     call hdf_getslab_r(cgrid%mmean_nppleaf        (ipy:ipy) ,'MMEAN_NPPLEAF         '     &
                       ,dsetrank,iparallel,.false.,foundvar)
                       
  if (associated(cgrid%mmean_nppfroot       ))                                             &
     call hdf_getslab_r(cgrid%mmean_nppfroot       (ipy:ipy) ,'MMEAN_NPPFROOT        '     &
                       ,dsetrank,iparallel,.false.,foundvar)
                       
  if (associated(cgrid%mmean_nppsapwood     ))                                             &
     call hdf_getslab_r(cgrid%mmean_nppsapwood     (ipy:ipy) ,'MMEAN_NPPSAPWOOD      '     &
                       ,dsetrank,iparallel,.false.,foundvar)
                       
  if (associated(cgrid%mmean_nppcroot       ))                                             &
     call hdf_getslab_r(cgrid%mmean_nppcroot       (ipy:ipy) ,'MMEAN_NPPCROOT        '     &
                       ,dsetrank,iparallel,.false.,foundvar)
                       
  if (associated(cgrid%mmean_nppseeds       ))                                             &
     call hdf_getslab_r(cgrid%mmean_nppseeds       (ipy:ipy) ,'MMEAN_NPPSEEDS        '     &
                       ,dsetrank,iparallel,.false.,foundvar)
                       
  if (associated(cgrid%mmean_nppwood        ))                                             &
     call hdf_getslab_r(cgrid%mmean_nppwood        (ipy:ipy) ,'MMEAN_NPPWOOD         '     &
                       ,dsetrank,iparallel,.false.,foundvar)
                       
  if (associated(cgrid%mmean_nppdaily       ))                                             &
     call hdf_getslab_r(cgrid%mmean_nppdaily       (ipy:ipy) ,'MMEAN_NPPDAILY        '     &
                       ,dsetrank,iparallel,.false.,foundvar)
                       
  if (associated(cgrid%mmean_evap           ))                                             &
     call hdf_getslab_r(cgrid%mmean_evap           (ipy:ipy) ,'MMEAN_EVAP            '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_transp         ))                                             &
     call hdf_getslab_r(cgrid%mmean_transp         (ipy:ipy) ,'MMEAN_TRANSP          '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_sensible_lc    ))                                             &
     call hdf_getslab_r(cgrid%mmean_sensible_lc    (ipy:ipy) ,'MMEAN_SENSIBLE_LC     '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_sensible_wc    ))                                             &
     call hdf_getslab_r(cgrid%mmean_sensible_wc    (ipy:ipy) ,'MMEAN_SENSIBLE_WC     '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_sensible_gc    ))                                             &
     call hdf_getslab_r(cgrid%mmean_sensible_gc    (ipy:ipy) ,'MMEAN_SENSIBLE_GC     '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_sensible_ac    ))                                             &
     call hdf_getslab_r(cgrid%mmean_sensible_ac    (ipy:ipy) ,'MMEAN_SENSIBLE_AC     '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_vapor_lc       ))                                             &
     call hdf_getslab_r(cgrid%mmean_vapor_lc       (ipy:ipy) ,'MMEAN_VAPOR_LC        '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_vapor_wc       ))                                             &
     call hdf_getslab_r(cgrid%mmean_vapor_wc       (ipy:ipy) ,'MMEAN_VAPOR_WC        '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_vapor_gc       ))                                             &
     call hdf_getslab_r(cgrid%mmean_vapor_gc       (ipy:ipy) ,'MMEAN_VAPOR_GC        '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_vapor_ac       ))                                             &
     call hdf_getslab_r(cgrid%mmean_vapor_ac       (ipy:ipy) ,'MMEAN_VAPOR_AC     '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_nep            ))                                             &
     call hdf_getslab_r(cgrid%mmean_nep            (ipy:ipy) ,'MMEAN_NEP             '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_ustar          ))                                             &
     call hdf_getslab_r(cgrid%mmean_ustar          (ipy:ipy) ,'MMEAN_USTAR           '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_tstar          ))                                             &
     call hdf_getslab_r(cgrid%mmean_tstar          (ipy:ipy) ,'MMEAN_TSTAR           '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_qstar          ))                                             &
     call hdf_getslab_r(cgrid%mmean_qstar          (ipy:ipy) ,'MMEAN_QSTAR           '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_cstar          ))                                             &
     call hdf_getslab_r(cgrid%mmean_cstar          (ipy:ipy) ,'MMEAN_CSTAR           '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_carbon_ac      ))                                             &
     call hdf_getslab_r(cgrid%mmean_carbon_ac      (ipy:ipy) ,'MMEAN_CARBON_AC       '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_carbon_st      ))                                             &
     call hdf_getslab_r(cgrid%mmean_carbon_st      (ipy:ipy) ,'MMEAN_CARBON_ST       '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_plresp         ))                                             &
     call hdf_getslab_r(cgrid%mmean_plresp         (ipy:ipy) ,'MMEAN_PLRESP          '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_rh             ))                                             &
     call hdf_getslab_r(cgrid%mmean_rh             (ipy:ipy) ,'MMEAN_RH              '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_cwd_rh         ))                                             &
     call hdf_getslab_r(cgrid%mmean_cwd_rh         (ipy:ipy) ,'MMEAN_CWD_RH          '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_leaf_resp      ))                                             &
     call hdf_getslab_r(cgrid%mmean_leaf_resp      (ipy:ipy) ,'MMEAN_LEAF_RESP       '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_root_resp      ))                                             &
     call hdf_getslab_r(cgrid%mmean_root_resp      (ipy:ipy) ,'MMEAN_ROOT_RESP       '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_growth_resp    ))                                             &
     call hdf_getslab_r(cgrid%mmean_growth_resp    (ipy:ipy) ,'MMEAN_GROWTH_RESP     '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_storage_resp   ))                                             &
     call hdf_getslab_r(cgrid%mmean_storage_resp   (ipy:ipy) ,'MMEAN_STORAGE_RESP    '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_vleaf_resp     ))                                             &
     call hdf_getslab_r(cgrid%mmean_vleaf_resp     (ipy:ipy) ,'MMEAN_VLEAF_RESP      '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_can_temp       ))                                             &
     call hdf_getslab_r(cgrid%mmean_can_temp       (ipy:ipy) ,'MMEAN_CAN_TEMP        '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_can_shv        ))                                             &
     call hdf_getslab_r(cgrid%mmean_can_shv        (ipy:ipy) ,'MMEAN_CAN_SHV         '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_can_co2        ))                                             &
     call hdf_getslab_r(cgrid%mmean_can_co2        (ipy:ipy) ,'MMEAN_CAN_CO2         '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_can_rhos       ))                                             &
     call hdf_getslab_r(cgrid%mmean_can_rhos       (ipy:ipy) ,'MMEAN_CAN_RHOS        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmean_can_prss       ))                                             &
     call hdf_getslab_r(cgrid%mmean_can_prss       (ipy:ipy) ,'MMEAN_CAN_PRSS        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmean_can_theta      ))                                             &
     call hdf_getslab_r(cgrid%mmean_can_theta      (ipy:ipy) ,'MMEAN_CAN_THETA       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmean_can_theiv      ))                                             &
     call hdf_getslab_r(cgrid%mmean_can_theiv      (ipy:ipy) ,'MMEAN_CAN_THEIV       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmean_can_vpdef      ))                                             &
     call hdf_getslab_r(cgrid%mmean_can_vpdef      (ipy:ipy) ,'MMEAN_CAN_VPDEF       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmean_gnd_temp       ))                                             &
     call hdf_getslab_r(cgrid%mmean_gnd_temp       (ipy:ipy) ,'MMEAN_GND_TEMP        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmean_gnd_shv        ))                                             &
     call hdf_getslab_r(cgrid%mmean_gnd_shv        (ipy:ipy) ,'MMEAN_GND_SHV         '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmean_leaf_energy     ))                                            &
     call hdf_getslab_r(cgrid%mmean_leaf_energy    (ipy:ipy) ,'MMEAN_LEAF_ENERGY     '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_leaf_water      ))                                            &
     call hdf_getslab_r(cgrid%mmean_leaf_water     (ipy:ipy) ,'MMEAN_LEAF_WATER      '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_leaf_temp       ))                                            &
     call hdf_getslab_r(cgrid%mmean_leaf_temp      (ipy:ipy) ,'MMEAN_LEAF_TEMP       '     &
                       ,dsetrank,iparallel,.false.,foundvar)
  
  if (associated(cgrid%mmean_leaf_vpdef      ))                                            &
     call hdf_getslab_r(cgrid%mmean_leaf_vpdef     (ipy:ipy) ,'MMEAN_LEAF_VPDEF      '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmean_leaf_hcap       ))                                            &
     call hdf_getslab_r(cgrid%mmean_leaf_hcap      (ipy:ipy) ,'MMEAN_LEAF_HCAP       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmean_wood_energy     ))                                            &
     call hdf_getslab_r(cgrid%mmean_wood_energy    (ipy:ipy) ,'MMEAN_WOOD_ENERGY     '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_wood_water      ))                                            &
     call hdf_getslab_r(cgrid%mmean_wood_water     (ipy:ipy) ,'MMEAN_WOOD_WATER      '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_wood_temp       ))                                            &
     call hdf_getslab_r(cgrid%mmean_wood_temp      (ipy:ipy) ,'MMEAN_WOOD_TEMP       '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_wood_hcap       ))                                            &
     call hdf_getslab_r(cgrid%mmean_wood_hcap      (ipy:ipy) ,'MMEAN_WOOD_HCAP       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmean_rshort       ))                                               &
       call hdf_getslab_r(cgrid%mmean_rshort       (ipy:ipy) ,'MMEAN_RSHORT        '       &
       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmean_rshort_diff  ))                                               &
       call hdf_getslab_r(cgrid%mmean_rshort_diff  (ipy:ipy) ,'MMEAN_RSHORT_DIFF   '       &
       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmean_rlong       ))                                                &
       call hdf_getslab_r(cgrid%mmean_rlong       (ipy:ipy) ,'MMEAN_RLONG        '         &
       ,dsetrank,iparallel,.false.,foundvar)
  
  if (associated(cgrid%mmean_rlongup     ))                                                &
       call hdf_getslab_r(cgrid%mmean_rlongup     (ipy:ipy) ,'MMEAN_RLONGUP      '         &
       ,dsetrank,iparallel,.false.,foundvar)
  
  if (associated(cgrid%mmean_parup       ))                                                &
       call hdf_getslab_r(cgrid%mmean_parup       (ipy:ipy) ,'MMEAN_PARUP        '         &
       ,dsetrank,iparallel,.false.,foundvar)
  
  if (associated(cgrid%mmean_nirup       ))                                                &
       call hdf_getslab_r(cgrid%mmean_nirup       (ipy:ipy) ,'MMEAN_NIRUP        '         &
       ,dsetrank,iparallel,.false.,foundvar)
  
  if (associated(cgrid%mmean_rshortup    ))                                                &
       call hdf_getslab_r(cgrid%mmean_rshortup    (ipy:ipy) ,'MMEAN_RSHORTUP     '         &
       ,dsetrank,iparallel,.false.,foundvar)
  
  if (associated(cgrid%mmean_rnet        ))                                                &
       call hdf_getslab_r(cgrid%mmean_rnet        (ipy:ipy) ,'MMEAN_RNET         '         &
       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmean_rshort_gnd   ))                                               &
       call hdf_getslab_r(cgrid%mmean_rshort_gnd   (ipy:ipy) ,'MMEAN_RSHORT_GND    '       &
       ,dsetrank,iparallel,.false.,foundvar)
  
  if (associated(cgrid%mmean_rlong_gnd   ))                                                &
       call hdf_getslab_r(cgrid%mmean_rlong_gnd   (ipy:ipy) ,'MMEAN_RLONG_GND    '         &
       ,dsetrank,iparallel,.false.,foundvar)
  
  if (associated(cgrid%mmean_atm_temp       ))                                             &
     call hdf_getslab_r(cgrid%mmean_atm_temp       (ipy:ipy) ,'MMEAN_ATM_TEMP        '     &
                       ,dsetrank,iparallel,.false.,foundvar)
  
  if (associated(cgrid%mmean_atm_vpdef      ))                                             &
     call hdf_getslab_r(cgrid%mmean_atm_vpdef      (ipy:ipy) ,'MMEAN_ATM_VPDEF       '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_atm_shv        ))                                             &
     call hdf_getslab_r(cgrid%mmean_atm_shv        (ipy:ipy) ,'MMEAN_ATM_SHV         '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_atm_co2        ))                                             &
     call hdf_getslab_r(cgrid%mmean_atm_co2        (ipy:ipy) ,'MMEAN_ATM_CO2         '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_atm_prss       ))                                             &
     call hdf_getslab_r(cgrid%mmean_atm_prss       (ipy:ipy) ,'MMEAN_ATM_PRSS        '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_atm_vels       ))                                             &
     call hdf_getslab_r(cgrid%mmean_atm_vels       (ipy:ipy) ,'MMEAN_ATM_VELS        '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_pcpg           ))                                             &
     call hdf_getslab_r(cgrid%mmean_pcpg           (ipy:ipy) ,'MMEAN_PCPG            '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_runoff         ))                                             &
     call hdf_getslab_r(cgrid%mmean_runoff         (ipy:ipy) ,'MMEAN_RUNOFF          '     &
                       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%mmean_drainage       ))                                             &
     call hdf_getslab_r(cgrid%mmean_drainage       (ipy:ipy) ,'MMEAN_DRAINAGE        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmean_fs_open        ))                                             &
     call hdf_getslab_r(cgrid%mmean_fs_open        (ipy:ipy) ,'MMEAN_FS_OPEN         '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmean_fsw            ))                                             &
     call hdf_getslab_r(cgrid%mmean_fsw            (ipy:ipy) ,'MMEAN_FSW             '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmean_fsn            ))                                             &
     call hdf_getslab_r(cgrid%mmean_fsn            (ipy:ipy) ,'MMEAN_FSN             '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmsqu_gpp            ))                                             &
     call hdf_getslab_r(cgrid%mmsqu_gpp            (ipy:ipy) ,'MMSQU_GPP             '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmsqu_leaf_resp       ))                                            &
     call hdf_getslab_r(cgrid%mmsqu_leaf_resp      (ipy:ipy) ,'MMSQU_LEAF_RESP       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmsqu_root_resp       ))                                            &
     call hdf_getslab_r(cgrid%mmsqu_root_resp      (ipy:ipy) ,'MMSQU_ROOT_RESP       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmsqu_plresp       ))                                               &
     call hdf_getslab_r(cgrid%mmsqu_plresp      (ipy:ipy) ,'MMSQU_PLRESP             '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmsqu_carbon_ac    ))                                               &
     call hdf_getslab_r(cgrid%mmsqu_carbon_ac   (ipy:ipy) ,'MMSQU_CARBON_AC          '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmsqu_carbon_st    ))                                               &
     call hdf_getslab_r(cgrid%mmsqu_carbon_st   (ipy:ipy) ,'MMSQU_CARBON_ST          '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmsqu_nep          ))                                               &
     call hdf_getslab_r(cgrid%mmsqu_nep         (ipy:ipy) ,'MMSQU_NEP                '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmsqu_rh           ))                                               &
     call hdf_getslab_r(cgrid%mmsqu_rh          (ipy:ipy) ,'MMSQU_RH                 '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmsqu_sensible_ac  ))                                               &
     call hdf_getslab_r(cgrid%mmsqu_sensible_ac (ipy:ipy) ,'MMSQU_SENSIBLE_AC        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmsqu_sensible_lc  ))                                               &
     call hdf_getslab_r(cgrid%mmsqu_sensible_lc (ipy:ipy) ,'MMSQU_SENSIBLE_LC        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmsqu_sensible_wc  ))                                               &
     call hdf_getslab_r(cgrid%mmsqu_sensible_wc (ipy:ipy) ,'MMSQU_SENSIBLE_WC        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmsqu_sensible_gc  ))                                               &
     call hdf_getslab_r(cgrid%mmsqu_sensible_gc (ipy:ipy) ,'MMSQU_SENSIBLE_GC        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmsqu_evap         ))                                               &
     call hdf_getslab_r(cgrid%mmsqu_evap        (ipy:ipy) ,'MMSQU_EVAP               '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmsqu_transp       ))                                               &
     call hdf_getslab_r(cgrid%mmsqu_transp      (ipy:ipy) ,'MMSQU_TRANSP             '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmsqu_vapor_ac     ))                                               &
     call hdf_getslab_r(cgrid%mmsqu_vapor_ac    (ipy:ipy) ,'MMSQU_VAPOR_AC           '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmsqu_vapor_lc     ))                                               &
     call hdf_getslab_r(cgrid%mmsqu_vapor_lc    (ipy:ipy) ,'MMSQU_VAPOR_LC           '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmsqu_vapor_wc     ))                                               &
     call hdf_getslab_r(cgrid%mmsqu_vapor_wc    (ipy:ipy) ,'MMSQU_VAPOR_WC           '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmsqu_vapor_gc     ))                                               &
     call hdf_getslab_r(cgrid%mmsqu_vapor_gc    (ipy:ipy) ,'MMSQU_VAPOR_GC           '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmsqu_ustar        ))                                               &
     call hdf_getslab_r(cgrid%mmsqu_ustar       (ipy:ipy) ,'MMSQU_USTAR              '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmsqu_rlongup      ))                                               &
     call hdf_getslab_r(cgrid%mmsqu_rlongup     (ipy:ipy) ,'MMSQU_RLONGUP            '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmsqu_parup        ))                                               &
     call hdf_getslab_r(cgrid%mmsqu_parup       (ipy:ipy) ,'MMSQU_PARUP              '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmsqu_nirup        ))                                               &
     call hdf_getslab_r(cgrid%mmsqu_nirup       (ipy:ipy) ,'MMSQU_NIRUP              '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmsqu_rshortup     ))                                               &
     call hdf_getslab_r(cgrid%mmsqu_rshortup    (ipy:ipy) ,'MMSQU_RSHORTUP           '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmsqu_rnet         ))                                               &
     call hdf_getslab_r(cgrid%mmsqu_rnet        (ipy:ipy) ,'MMSQU_RNET               '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%mmsqu_albedo       ))                                               &
     call hdf_getslab_r(cgrid%mmsqu_albedo      (ipy:ipy) ,'MMSQU_ALBEDO             '     &
                       ,dsetrank,iparallel,.false.,foundvar)

   ! Variables with 2 dimensions (nzg,npolygons)
   dsetrank    = 2
   globdims(1) = int(nzg,8)
   chnkdims(1) = int(nzg,8)
   memdims(1)  = int(nzg,8)
   memsize(1)  = int(nzg,8)
   chnkoffs(1) = 0_8
   memoffs(1)  = 0_8

   globdims(2)  = int(cgrid%npolygons_global,8)
   chnkdims(2)  = 1_8
   chnkoffs(2)  = int(py_index - 1,8)
   memdims(2)   = 1_8
   memsize(2)   = 1_8
   memoffs(2)   = 0_8

   call hdf_getslab_i(cgrid%ntext_soil(:,ipy)          ,'NTEXT_SOIL '       ,&
        dsetrank,iparallel,.false.,foundvar)

   if(associated(cgrid%dmean_soil_temp)) &
      call hdf_getslab_r(cgrid%dmean_soil_temp(:,ipy)  ,'DMEAN_SOIL_TEMP '  ,&
      dsetrank,iparallel,.false.,foundvar)

   if(associated(cgrid%dmean_soil_water)) &
      call hdf_getslab_r(cgrid%dmean_soil_water(:,ipy) ,'DMEAN_SOIL_WATER ' ,&
      dsetrank,iparallel,.false.,foundvar)

   if(associated(cgrid%dmean_soil_mstpot)) &
      call hdf_getslab_r(cgrid%dmean_soil_mstpot(:,ipy),'DMEAN_SOIL_MSTPOT ' ,&
      dsetrank,iparallel,.false.,foundvar)
      
   if(associated(cgrid%dmean_transloss)) &
      call hdf_getslab_r(cgrid%dmean_transloss(:,ipy) ,'DMEAN_TRANSLOSS ' ,&
      dsetrank,iparallel,.false.,foundvar)

   if(associated(cgrid%mmean_soil_temp)) &
      call hdf_getslab_r(cgrid%mmean_soil_temp(:,ipy)  ,'MMEAN_SOIL_TEMP '  ,&
      dsetrank,iparallel,.false.,foundvar)

   if(associated(cgrid%mmean_soil_water)) &
      call hdf_getslab_r(cgrid%mmean_soil_water(:,ipy) ,'MMEAN_SOIL_WATER ' ,&
      dsetrank,iparallel,.false.,foundvar)

   if(associated(cgrid%mmean_soil_mstpot)) &
      call hdf_getslab_r(cgrid%mmean_soil_mstpot(:,ipy),'MMEAN_SOIL_MSTPOT ' ,&
      dsetrank,iparallel,.false.,foundvar)

   if(associated(cgrid%mmean_transloss)) &
      call hdf_getslab_r(cgrid%mmean_transloss(:,ipy) ,'MMEAN_TRANSLOSS ' ,&
      dsetrank,iparallel,.false.,foundvar)

   ! Variables with 2 dimensions (ndcycle,npolygons)
   dsetrank    = 2
   globdims(1) = int(ndcycle,8)
   chnkdims(1) = int(ndcycle,8)
   memdims(1)  = int(ndcycle,8)
   memsize(1)  = int(ndcycle,8)
   chnkoffs(1) = 0_8
   memoffs(1)  = 0_8

   globdims(2)  = int(cgrid%npolygons_global,8)
   chnkdims(2)  = 1_8
   chnkoffs(2)  = int(py_index - 1,8)
   memdims(2)   = 1_8
   memsize(2)   = 1_8
   memoffs(2)   = 0_8
 
  if (associated(cgrid%qmean_pcpg           ))                                             &
     call hdf_getslab_r(cgrid%qmean_pcpg           (:,ipy)   ,'QMEAN_PCPG            '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_runoff         ))                                             &
     call hdf_getslab_r(cgrid%qmean_runoff         (:,ipy)   ,'QMEAN_RUNOFF          '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_drainage       ))                                             &
     call hdf_getslab_r(cgrid%qmean_drainage       (:,ipy)   ,'QMEAN_DRAINAGE        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_gpp            ))                                             &
     call hdf_getslab_r(cgrid%qmean_gpp            (:,ipy)   ,'QMEAN_GPP             '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_evap           ))                                             &
     call hdf_getslab_r(cgrid%qmean_evap           (:,ipy)   ,'QMEAN_EVAP            '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_transp         ))                                             &
     call hdf_getslab_r(cgrid%qmean_transp         (:,ipy)   ,'QMEAN_TRANSP          '     &
                       ,dsetrank,iparallel,.false.,foundvar)                                        

  if (associated(cgrid%qmean_sensible_lc    ))                                             &
     call hdf_getslab_r(cgrid%qmean_sensible_lc    (:,ipy)   ,'QMEAN_SENSIBLE_LC     '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_sensible_wc    ))                                             &
     call hdf_getslab_r(cgrid%qmean_sensible_wc    (:,ipy)   ,'QMEAN_SENSIBLE_WC     '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_sensible_gc    ))                                             &
     call hdf_getslab_r(cgrid%qmean_sensible_gc    (:,ipy)   ,'QMEAN_SENSIBLE_GC     '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_sensible_ac    ))                                             &
     call hdf_getslab_r(cgrid%qmean_sensible_ac    (:,ipy)   ,'QMEAN_SENSIBLE_AC     '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_vapor_lc       ))                                             &
     call hdf_getslab_r(cgrid%qmean_vapor_lc       (:,ipy)   ,'QMEAN_VAPOR_LC        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_vapor_wc       ))                                             &
     call hdf_getslab_r(cgrid%qmean_vapor_wc       (:,ipy)   ,'QMEAN_VAPOR_WC        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_vapor_gc       ))                                             &
     call hdf_getslab_r(cgrid%qmean_vapor_gc       (:,ipy)   ,'QMEAN_VAPOR_GC        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_vapor_ac       ))                                             &
     call hdf_getslab_r(cgrid%qmean_vapor_ac       (:,ipy)   ,'QMEAN_VAPOR_AC        '     &
                       ,dsetrank,iparallel,.false.,foundvar)                                        

  if (associated(cgrid%qmean_ustar          ))                                             &
     call hdf_getslab_r(cgrid%qmean_ustar          (:,ipy)   ,'QMEAN_USTAR           '     &
                       ,dsetrank,iparallel,.false.,foundvar)                                        

  if (associated(cgrid%qmean_tstar          ))                                             &
     call hdf_getslab_r(cgrid%qmean_tstar          (:,ipy)   ,'QMEAN_TSTAR           '     &
                       ,dsetrank,iparallel,.false.,foundvar)                                        

  if (associated(cgrid%qmean_qstar          ))                                             &
     call hdf_getslab_r(cgrid%qmean_qstar          (:,ipy)   ,'QMEAN_QSTAR           '     &
                       ,dsetrank,iparallel,.false.,foundvar)                                        

  if (associated(cgrid%qmean_cstar          ))                                             &
     call hdf_getslab_r(cgrid%qmean_cstar          (:,ipy)   ,'QMEAN_CSTAR           '     &
                       ,dsetrank,iparallel,.false.,foundvar)                                        

  if (associated(cgrid%qmean_nep            ))                                             &
     call hdf_getslab_r(cgrid%qmean_nep            (:,ipy)   ,'QMEAN_NEP             '     &
                       ,dsetrank,iparallel,.false.,foundvar)                                        

  if (associated(cgrid%qmean_carbon_ac      ))                                             &
     call hdf_getslab_r(cgrid%qmean_carbon_ac      (:,ipy)   ,'QMEAN_CARBON_AC       '     &
                       ,dsetrank,iparallel,.false.,foundvar)                                        

  if (associated(cgrid%qmean_carbon_st      ))                                             &
     call hdf_getslab_r(cgrid%qmean_carbon_st      (:,ipy)   ,'QMEAN_CARBON_ST       '     &
                       ,dsetrank,iparallel,.false.,foundvar)                                        

  if (associated(cgrid%qmean_plresp         ))                                             &
     call hdf_getslab_r(cgrid%qmean_plresp         (:,ipy)   ,'QMEAN_PLRESP          '     &
                       ,dsetrank,iparallel,.false.,foundvar)                                        

  if (associated(cgrid%qmean_rh             ))                                             &
     call hdf_getslab_r(cgrid%qmean_rh             (:,ipy)   ,'QMEAN_RH              '     &
                       ,dsetrank,iparallel,.false.,foundvar)                                        

  if (associated(cgrid%qmean_cwd_rh         ))                                             &
     call hdf_getslab_r(cgrid%qmean_cwd_rh         (:,ipy)   ,'QMEAN_CWD_RH          '     &
                       ,dsetrank,iparallel,.false.,foundvar)                                        

  if (associated(cgrid%qmean_leaf_resp      ))                                             &
     call hdf_getslab_r(cgrid%qmean_leaf_resp      (:,ipy)   ,'QMEAN_LEAF_RESP       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_root_resp      ))                                             &
     call hdf_getslab_r(cgrid%qmean_root_resp      (:,ipy)   ,'QMEAN_ROOT_RESP       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_fs_open        ))                                             &
     call hdf_getslab_r(cgrid%qmean_fs_open        (:,ipy)   ,'QMEAN_FS_OPEN         '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_fsw            ))                                             &
     call hdf_getslab_r(cgrid%qmean_fsw            (:,ipy)   ,'QMEAN_FSW             '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_fsn            ))                                             &
     call hdf_getslab_r(cgrid%qmean_fsn            (:,ipy)   ,'QMEAN_FSN             '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_can_temp       ))                                             &
     call hdf_getslab_r(cgrid%qmean_can_temp       (:,ipy)   ,'QMEAN_CAN_TEMP        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_can_shv        ))                                             &
     call hdf_getslab_r(cgrid%qmean_can_shv        (:,ipy)   ,'QMEAN_CAN_SHV         '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_can_prss       ))                                             &
     call hdf_getslab_r(cgrid%qmean_can_prss       (:,ipy)   ,'QMEAN_CAN_PRSS        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_can_theta      ))                                             &
     call hdf_getslab_r(cgrid%qmean_can_theta      (:,ipy)   ,'QMEAN_CAN_THETA       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_can_theiv      ))                                             &
     call hdf_getslab_r(cgrid%qmean_can_theiv      (:,ipy)   ,'QMEAN_CAN_THEIV       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_can_vpdef      ))                                             &
     call hdf_getslab_r(cgrid%qmean_can_vpdef      (:,ipy)   ,'QMEAN_CAN_VPDEF       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_can_co2        ))                                             &
     call hdf_getslab_r(cgrid%qmean_can_co2        (:,ipy)   ,'QMEAN_CAN_CO2         '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_can_rhos       ))                                             &
     call hdf_getslab_r(cgrid%qmean_can_rhos       (:,ipy)   ,'QMEAN_CAN_RHOS        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_gnd_temp       ))                                             &
     call hdf_getslab_r(cgrid%qmean_gnd_temp       (:,ipy)   ,'QMEAN_GND_TEMP        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_gnd_shv        ))                                             &
     call hdf_getslab_r(cgrid%qmean_gnd_shv        (:,ipy)   ,'QMEAN_GND_SHV         '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_leaf_energy     ))                                            &
     call hdf_getslab_r(cgrid%qmean_leaf_energy    (:,ipy)   ,'QMEAN_LEAF_ENERGY     '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_leaf_water      ))                                            &
     call hdf_getslab_r(cgrid%qmean_leaf_water     (:,ipy)   ,'QMEAN_LEAF_WATER      '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_leaf_hcap       ))                                            &
     call hdf_getslab_r(cgrid%qmean_leaf_hcap      (:,ipy)   ,'QMEAN_LEAF_HCAP       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_leaf_temp       ))                                            &
     call hdf_getslab_r(cgrid%qmean_leaf_temp      (:,ipy)   ,'QMEAN_LEAF_TEMP       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_leaf_vpdef      ))                                            &
     call hdf_getslab_r(cgrid%qmean_leaf_vpdef     (:,ipy)   ,'QMEAN_LEAF_VPDEF      '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_wood_energy     ))                                            &
     call hdf_getslab_r(cgrid%qmean_wood_energy    (:,ipy)   ,'QMEAN_WOOD_ENERGY     '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_wood_water      ))                                            &
     call hdf_getslab_r(cgrid%qmean_wood_water     (:,ipy)   ,'QMEAN_WOOD_WATER      '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_wood_hcap       ))                                            &
     call hdf_getslab_r(cgrid%qmean_wood_hcap      (:,ipy)   ,'QMEAN_WOOD_HCAP       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_wood_temp       ))                                            &
     call hdf_getslab_r(cgrid%qmean_wood_temp      (:,ipy)   ,'QMEAN_WOOD_TEMP       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_atm_temp       ))                                             &
     call hdf_getslab_r(cgrid%qmean_atm_temp       (:,ipy)   ,'QMEAN_ATM_TEMP        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_atm_vpdef      ))                                             &
     call hdf_getslab_r(cgrid%qmean_atm_vpdef      (:,ipy)   ,'QMEAN_ATM_VPDEF       '     &
                       ,dsetrank,iparallel,.false.,foundvar)
  
  if (associated(cgrid%qmean_rshort       ))                                               &
     call hdf_getslab_r(cgrid%qmean_rshort       (:,ipy)   ,'QMEAN_RSHORT        '         &
       ,dsetrank,iparallel,.false.,foundvar)
  
  if (associated(cgrid%qmean_rshort_diff  ))                                               &
     call hdf_getslab_r(cgrid%qmean_rshort_diff  (:,ipy)   ,'QMEAN_RSHORT_DIFF   '         &
       ,dsetrank,iparallel,.false.,foundvar)
  
  if (associated(cgrid%qmean_rlong       ))                                                &
     call hdf_getslab_r(cgrid%qmean_rlong       (:,ipy)   ,'QMEAN_RLONG        '           &
       ,dsetrank,iparallel,.false.,foundvar)
  
  if (associated(cgrid%qmean_rlongup     ))                                                &
     call hdf_getslab_r(cgrid%qmean_rlongup     (:,ipy)   ,'QMEAN_RLONGUP      '           &
       ,dsetrank,iparallel,.false.,foundvar)
  
  if (associated(cgrid%qmean_parup       ))                                                &
     call hdf_getslab_r(cgrid%qmean_parup       (:,ipy)   ,'QMEAN_PARUP        '           &
       ,dsetrank,iparallel,.false.,foundvar)
  
  if (associated(cgrid%qmean_nirup       ))                                                &
     call hdf_getslab_r(cgrid%qmean_nirup       (:,ipy)   ,'QMEAN_NIRUP        '           &
       ,dsetrank,iparallel,.false.,foundvar)
   
  if (associated(cgrid%qmean_rshortup    ))                                                &
     call hdf_getslab_r(cgrid%qmean_rshortup    (:,ipy)   ,'QMEAN_RSHORTUP     '           &
       ,dsetrank,iparallel,.false.,foundvar)
   
  if (associated(cgrid%qmean_rnet        ))                                                &
     call hdf_getslab_r(cgrid%qmean_rnet        (:,ipy)   ,'QMEAN_RNET         '           &
       ,dsetrank,iparallel,.false.,foundvar)
 
  if (associated(cgrid%qmean_rshort_gnd   ))                                               &
     call hdf_getslab_r(cgrid%qmean_rshort_gnd   (:,ipy)   ,'QMEAN_RSHORT_GND    '         &
       ,dsetrank,iparallel,.false.,foundvar)
  
  if (associated(cgrid%qmean_rlong_gnd   ))                                                &
     call hdf_getslab_r(cgrid%qmean_rlong_gnd   (:,ipy)   ,'QMEAN_RLONG_GND    '           &
       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_atm_shv        ))                                             &
     call hdf_getslab_r(cgrid%qmean_atm_shv        (:,ipy)   ,'QMEAN_ATM_SHV         '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_atm_co2        ))                                             &
     call hdf_getslab_r(cgrid%qmean_atm_co2        (:,ipy)   ,'QMEAN_ATM_CO2         '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_atm_prss       ))                                             &
     call hdf_getslab_r(cgrid%qmean_atm_prss       (:,ipy)   ,'QMEAN_ATM_PRSS        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmean_atm_vels       ))                                             &
     call hdf_getslab_r(cgrid%qmean_atm_vels       (:,ipy)   ,'QMEAN_ATM_VELS        '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmsqu_gpp       ))                                                  &
     call hdf_getslab_r(cgrid%qmsqu_gpp            (:,ipy)   ,'QMSQU_GPP             '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmsqu_leaf_resp ))                                                  &
     call hdf_getslab_r(cgrid%qmsqu_leaf_resp      (:,ipy)   ,'QMSQU_LEAF_RESP       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmsqu_root_resp ))                                                  &
     call hdf_getslab_r(cgrid%qmsqu_root_resp      (:,ipy)   ,'QMSQU_ROOT_RESP       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmsqu_plresp ))                                                     &
     call hdf_getslab_r(cgrid%qmsqu_plresp         (:,ipy)   ,'QMSQU_PLRESP          '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmsqu_carbon_ac))                                                   &
     call hdf_getslab_r(cgrid%qmsqu_carbon_ac      (:,ipy)   ,'QMSQU_CARBON_AC       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmsqu_carbon_st))                                                   &
     call hdf_getslab_r(cgrid%qmsqu_carbon_st      (:,ipy)   ,'QMSQU_CARBON_ST       '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmsqu_nep       ))                                                  &
     call hdf_getslab_r(cgrid%qmsqu_nep            (:,ipy)   ,'QMSQU_NEP             '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmsqu_rh        ))                                                  &
     call hdf_getslab_r(cgrid%qmsqu_rh             (:,ipy)   ,'QMSQU_RH              '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmsqu_sensible_ac))                                                 &
     call hdf_getslab_r(cgrid%qmsqu_sensible_ac    (:,ipy)   ,'QMSQU_SENSIBLE_AC     '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmsqu_sensible_lc))                                                 &
     call hdf_getslab_r(cgrid%qmsqu_sensible_lc    (:,ipy)   ,'QMSQU_SENSIBLE_LC     '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmsqu_sensible_wc))                                                 &
     call hdf_getslab_r(cgrid%qmsqu_sensible_wc    (:,ipy)   ,'QMSQU_SENSIBLE_WC     '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmsqu_sensible_gc))                                                 &
     call hdf_getslab_r(cgrid%qmsqu_sensible_gc    (:,ipy)   ,'QMSQU_SENSIBLE_GC     '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmsqu_evap      ))                                                  &
     call hdf_getslab_r(cgrid%qmsqu_evap           (:,ipy)   ,'QMSQU_EVAP            '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmsqu_transp    ))                                                  &
     call hdf_getslab_r(cgrid%qmsqu_transp         (:,ipy)   ,'QMSQU_TRANSP          '     &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmsqu_vapor_ac))                                                    &
     call hdf_getslab_r(cgrid%qmsqu_vapor_ac       (:,ipy)   ,'QMSQU_VAPOR_AC     '        &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmsqu_vapor_lc))                                                    &
     call hdf_getslab_r(cgrid%qmsqu_vapor_lc       (:,ipy)   ,'QMSQU_VAPOR_LC     '        &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmsqu_vapor_wc))                                                    &
     call hdf_getslab_r(cgrid%qmsqu_vapor_wc       (:,ipy)   ,'QMSQU_VAPOR_WC     '        &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmsqu_vapor_gc))                                                    &
     call hdf_getslab_r(cgrid%qmsqu_vapor_gc       (:,ipy)   ,'QMSQU_VAPOR_GC     '        &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmsqu_ustar        ))                                               &
     call hdf_getslab_r(cgrid%qmsqu_ustar          (:,ipy)   ,'QMSQU_USTAR              '  &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmsqu_rlongup      ))                                               &
     call hdf_getslab_r(cgrid%qmsqu_rlongup        (:,ipy)   ,'QMSQU_RLONGUP            '  &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmsqu_parup        ))                                               &
     call hdf_getslab_r(cgrid%qmsqu_parup          (:,ipy)   ,'QMSQU_PARUP              '  &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmsqu_nirup        ))                                               &
     call hdf_getslab_r(cgrid%qmsqu_nirup          (:,ipy)   ,'QMSQU_NIRUP              '  &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmsqu_rshortup     ))                                               &
     call hdf_getslab_r(cgrid%qmsqu_rshortup       (:,ipy)   ,'QMSQU_RSHORTUP           '  &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmsqu_rnet         ))                                               &
     call hdf_getslab_r(cgrid%qmsqu_rnet           (:,ipy)   ,'QMSQU_RNET               '  &
                       ,dsetrank,iparallel,.false.,foundvar)

  if (associated(cgrid%qmsqu_albedo       ))                                               &
     call hdf_getslab_r(cgrid%qmsqu_albedo         (:,ipy)   ,'QMSQU_ALBEDO             '  &
                       ,dsetrank,iparallel,.false.,foundvar)


   ! Variables with 2 dimensions (n_pft,npolygons)
   dsetrank    = 2
   globdims(1) = int(n_pft,8)
   chnkdims(1) = int(n_pft,8)
   memdims(1)  = int(n_pft,8)
   memsize(1)  = int(n_pft,8)
   chnkoffs(1) = 0_8
   memoffs(1)  = 0_8

   globdims(2)  = int(cgrid%npolygons_global,8)
   chnkdims(2)  = 1_8
   chnkoffs(2)  = int(py_index - 1,8)
   memdims(2)   = 1_8
   memsize(2)   = 1_8
   memoffs(2)   = 0_8

   if(associated(cgrid%bseeds_pft)) call hdf_getslab_r(cgrid%bseeds_pft(:,ipy) ,'BSEEDS_PFT '    , &
        dsetrank,iparallel,.false.,foundvar)
   if(associated(cgrid%lai_pft)) call hdf_getslab_r(cgrid%lai_pft(:,ipy) ,'LAI_PFT '       , &
        dsetrank,iparallel,.false.,foundvar)
   if(associated(cgrid%wai_pft)) call hdf_getslab_r(cgrid%wai_pft(:,ipy) ,'WAI_PFT '       , &
        dsetrank,iparallel,.false.,foundvar)
   if(associated(cgrid%mmean_lai_pft)) call hdf_getslab_r(cgrid%mmean_lai_pft(:,ipy) ,'MMEAN_LAI_PFT ' , &
        dsetrank,iparallel,.false.,foundvar)
   if(associated(cgrid%mmean_wai_pft)) call hdf_getslab_r(cgrid%mmean_wai_pft(:,ipy) ,'MMEAN_WAI_PFT ' , &
        dsetrank,iparallel,.false.,foundvar)
   if(associated(cgrid%agb_pft)) call hdf_getslab_r(cgrid%agb_pft(:,ipy) ,'AGB_PFT '       , &
        dsetrank,iparallel,.false.,foundvar)
   if(associated(cgrid%ba_pft)) call hdf_getslab_r(cgrid%ba_pft(:,ipy) ,'BA_PFT '        ,   &
        dsetrank,iparallel,.false.,foundvar)


   ! Variables with 2 dimensions (n_dbh,npolygons)
   dsetrank    = 2
   globdims(1) = int(n_dbh,8)
   chnkdims(1) = int(n_dbh,8)
   memdims(1)  = int(n_dbh,8)
   memsize(1)  = int(n_dbh,8)
   chnkoffs(1) = 0_8
   memoffs(1)  = 0_8

   globdims(2)  = int(cgrid%npolygons_global,8)
   chnkdims(2)  = 1_8
   chnkoffs(2)  = int(py_index - 1,8)
   memdims(2)   = 1_8
   memsize(2)   = 1_8
   memoffs(2)   = 0_8

   if(associated(cgrid%dmean_gpp_dbh)) call hdf_getslab_r(cgrid%dmean_gpp_dbh(:,ipy) , &
        'DMEAN_GPP_DBH ' ,dsetrank,iparallel,.false.,foundvar)
   if(associated(cgrid%mmean_gpp_dbh)) call hdf_getslab_r(cgrid%mmean_gpp_dbh(:,ipy) , &
        'MMEAN_GPP_DBH ' ,dsetrank,iparallel,.false.,foundvar)

   ! Variables with 2 dimensions (13,npolygons)
   dsetrank    = 2
   globdims(1) = int(13,8)
   chnkdims(1) = int(13,8)
   memdims(1)  = int(13,8)
   memsize(1)  = int(13,8)
   chnkoffs(1) = 0_8
   memoffs(1)  = 0_8

   globdims(2)  = int(cgrid%npolygons_global,8)
   chnkdims(2)  = 1_8
   chnkoffs(2)  = int(py_index - 1,8)
   memdims(2)   = 1_8
   memsize(2)   = 1_8
   memoffs(2)   = 0_8

   if(associated(cgrid%workload)) call hdf_getslab_r(cgrid%workload(:,ipy) , &
        'WORKLOAD ' ,dsetrank,iparallel,.true.,foundvar)


   ! Variables with three dimensions(n_dist_types,n_dist_types,npolygons)
   dsetrank    = 3
   globdims(1) = int(n_dist_types,8)
   chnkdims(1) = int(n_dist_types,8)
   memdims(1)  = int(n_dist_types,8)
   memsize(1)  = int(n_dist_types,8)
   chnkoffs(1) = 0_8
   memoffs(1)  = 0_8

   globdims(2) = int(n_dist_types,8)
   chnkdims(2) = int(n_dist_types,8)
   memdims(2)  = int(n_dist_types,8)
   memsize(2)  = int(n_dist_types,8)
   chnkoffs(2) = 0_8
   memoffs(2)  = 0_8

   globdims(3)  = int(cgrid%npolygons_global,8)
   chnkdims(3)  = 1_8
   chnkoffs(3)  = int(py_index - 1,8)
   memdims(3)   = 1_8
   memsize(3)   = 1_8
   memoffs(3)   = 0_8
   if (associated(cgrid%disturbance_rates))                                                &
       call hdf_getslab_r(cgrid%disturbance_rates(:,:,ipy),'DISTURBANCE_RATES '            &
                         ,dsetrank,iparallel,.false.,foundvar)


   ! Variables with three dimensions(nzg,ndcycle,npolygons)
   dsetrank    = 3
   globdims(1) = int(nzg,8)
   chnkdims(1) = int(nzg,8)
   memdims(1)  = int(nzg,8)
   memsize(1)  = int(nzg,8)
   chnkoffs(1) = 0_8
   memoffs(1)  = 0_8

   globdims(2) = int(ndcycle,8)
   chnkdims(2) = int(ndcycle,8)
   memdims(2)  = int(ndcycle,8)
   memsize(2)  = int(ndcycle,8)
   chnkoffs(2) = 0_8
   memoffs(2)  = 0_8

   globdims(3)  = int(cgrid%npolygons_global,8)
   chnkdims(3)  = 1_8
   chnkoffs(3)  = int(py_index - 1,8)
   memdims(3)   = 1_8
   memsize(3)   = 1_8
   memoffs(3)   = 0_8

   if(associated(cgrid%qmean_soil_temp)) &
      call hdf_getslab_r(cgrid%qmean_soil_temp(:,:,ipy)  ,'QMEAN_SOIL_TEMP '  ,&
      dsetrank,iparallel,.false.,foundvar)

   if(associated(cgrid%qmean_soil_water)) &
      call hdf_getslab_r(cgrid%qmean_soil_water(:,:,ipy) ,'QMEAN_SOIL_WATER ' ,&
      dsetrank,iparallel,.false.,foundvar)

   if(associated(cgrid%qmean_soil_mstpot)) &
      call hdf_getslab_r(cgrid%qmean_soil_mstpot(:,:,ipy),'QMEAN_SOIL_MSTPOT' ,&
      dsetrank,iparallel,.false.,foundvar)


   dsetrank    = 3
   globdims(1) = int(n_pft,8)
   chnkdims(1) = int(n_pft,8)
   memdims(1)  = int(n_pft,8)
   memsize(1)  = int(n_pft,8)
   chnkoffs(1) = 0
   memoffs(1)  = 0
   globdims(2) = int(n_dbh,8)
   chnkdims(2) = int(n_dbh,8)
   memdims(2)  = int(n_dbh,8)
   memsize(2)  = int(n_dbh,8)
   chnkoffs(2) = 0
   memoffs(2)  = 0

   globdims(3)  = int(cgrid%npolygons_global,8)
   chnkdims(3)  = 1_8
   chnkoffs(3)  = int(py_index - 1,8)
   memdims(3)   = 1_8
   memsize(3)   = 1_8
   memoffs(3)   = 0_8

   call hdf_getslab_r(cgrid%basal_area(:,:,ipy),'BASAL_AREA ',dsetrank,iparallel,.false.,foundvar)
   call hdf_getslab_r(cgrid%agb       (:,:,ipy),'AGB '       ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cgrid%pldens    (:,:,ipy),'PLDENS '    ,dsetrank,iparallel,.false.,foundvar)
   call hdf_getslab_r(cgrid%bseeds    (:,:,ipy),'BSEEDS '    ,dsetrank,iparallel,.false.,foundvar)


   return
 end subroutine fill_history_grid
 !=========================================================================================!
 !=========================================================================================!






 !==========================================================================================!
 !==========================================================================================!
 subroutine fill_history_polygon(cpoly,pysi_index,nsites_global)

   use ed_state_vars,only: polygontype
   use hdf5
   use hdf5_coms,only:file_id,dset_id,dspace_id,plist_id, &
        globdims,chnkdims,chnkoffs,cnt,stride, &
        memdims,memoffs,memsize

   use grid_coms,only : nzg
   use ed_max_dims,only : n_pft,n_dbh,n_dist_types

   implicit none

#if USE_INTERF
  interface
     subroutine hdf_getslab_r(buff,varn,dsetrank,iparallel,required,foundvar)
       use hdf5_coms,only:memsize
       real,dimension(memsize(1),memsize(2),memsize(3),memsize(4)) :: buff
       integer :: dsetrank,iparallel
       character(len=*),intent(in) :: varn
       logical,intent(in) :: required
       logical,intent(out) :: foundvar
     end subroutine hdf_getslab_r
     subroutine hdf_getslab_d(buff,varn,dsetrank,iparallel,required,foundvar)
       use hdf5_coms,only:memsize
       real(kind=8),dimension(memsize(1),memsize(2),memsize(3),memsize(4)) :: buff
       integer :: dsetrank,iparallel
       character(len=*),intent(in) :: varn
       logical,intent(in) :: required
       logical,intent(out) :: foundvar
     end subroutine hdf_getslab_d
     subroutine hdf_getslab_i(buff,varn,dsetrank,iparallel,required,foundvar)
       use hdf5_coms,only:memsize
       integer,dimension(memsize(1),memsize(2),memsize(3),memsize(4)) :: buff
       integer :: dsetrank,iparallel
       character(len=*),intent(in) :: varn
       logical,intent(in) :: required
       logical,intent(out) :: foundvar
     end subroutine hdf_getslab_i
  end interface
#endif

   type(polygontype),target :: cpoly
   integer,intent(in) :: pysi_index
   integer,intent(in) :: nsites_global
   integer :: iparallel
   integer :: dsetrank
   logical :: foundvar

   iparallel = 0

   dsetrank = 1_8
   globdims = 0_8
   chnkdims = 0_8
   chnkoffs = 0_8
   memoffs  = 0_8
   memdims  = 0_8
   memsize  = 1_8

   globdims(1) = int(nsites_global,8)
   chnkdims(1) = int(cpoly%nsites,8)
   chnkoffs(1) = int(pysi_index - 1,8)
   memdims(1)  = int(cpoly%nsites,8)
   memsize(1)  = int(cpoly%nsites,8)
   memoffs(1)  = 0_8

   call hdf_getslab_i(cpoly%patch_count,'PATCH_COUNT ',dsetrank,iparallel,.true.,foundvar)  
   call hdf_getslab_i(cpoly%sitenum,'SITENUM ',dsetrank,iparallel,.true.,foundvar)

   call hdf_getslab_i(cpoly%lsl,'LSL_SI ',dsetrank,iparallel,.true.,foundvar)   
   call hdf_getslab_i(cpoly%ncol_soil,'NCOL_SOIL_SI ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%area,'AREA_SI ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%patch_area,'PATCH_AREA ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%elevation,'ELEVATION ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%slope,'SLOPE ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%aspect,'ASPECT ',dsetrank,iparallel,.true.,foundvar)

   call hdf_getslab_r(cpoly%a_par,'PHEN_PAR_A',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%b_par,'PHEN_PAR_B',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%a_fall,'PHEN_PAR_FALL',dsetrank,iparallel,.true.,foundvar)

   call hdf_getslab_i(cpoly%num_landuse_years,'NUM_LANDUSE_YEARS ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%TCI,'TCI ',dsetrank,iparallel,.true.,foundvar)      
   call hdf_getslab_r(cpoly%pptweight,'pptweight ',dsetrank,iparallel,.true.,foundvar)      
   call hdf_getslab_i(cpoly%hydro_next,'HYDRO_NEXT ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_i(cpoly%hydro_prev,'HYDRO_PREV ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%moist_W,'MOIST_W ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%moist_f,'MOIST_F ',dsetrank,iparallel,.true.,foundvar)  
   call hdf_getslab_r(cpoly%moist_tau,'MOIST_TAU ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%moist_zi,'MOIST_ZI ',dsetrank,iparallel,.true.,foundvar) 
   call hdf_getslab_r(cpoly%baseflow,'BASEFLOW_SI ',dsetrank,iparallel,.true.,foundvar) 
!   call hdf_getslab_i(cpoly%metplex_beg_month,'METPLEX_BEG_MONTH ',dsetrank,iparallel,.true.,foundvar)
!   call hdf_getslab_i(cpoly%metplex_beg_year,'METPLEX_BEG_YEAR ',dsetrank,iparallel,.true.,foundvar)
!   call hdf_getslab_i(cpoly%metplex_end_year,'METPLEX_END_YEAR ',dsetrank,iparallel,.true.,foundvar)

   call hdf_getslab_r(cpoly%min_monthly_temp,'MIN_MONTHLY_TEMP ',dsetrank,iparallel,.true.,foundvar)
!   call hdf_getslab_r(cpoly%removed_biomass,'REMOVED_BIOMASS ',dsetrank,iparallel,.true.,foundvar) 
!   call hdf_getslab_r(cpoly%harvested_biomass,'HARVESTED_BIOMASS ', &
!        dsetrank,iparallel,.true.,foundvar) 
   call hdf_getslab_r(cpoly%rad_avg,'RAD_AVG ',dsetrank,iparallel,.true.,foundvar)
   
   call hdf_getslab_i(cpoly%plantation,'PLANTATION_SI ',dsetrank,iparallel,.true.,foundvar) 
   call hdf_getslab_i(cpoly%agri_stocking_pft,'AGRI_STOCKING_PFT ', &
        dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%agri_stocking_density,'AGRI_STOCKING_DENSITY ', &
        dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_i(cpoly%plantation_stocking_pft,'PLANTATION_STOCKING_PFT ', &
        dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%plantation_stocking_density,'PLANTATION_STOCKING_DENSITY ', &
        dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%primary_harvest_memory,'PRIMARY_HARVEST_MEMORY ', &
        dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%secondary_harvest_memory,'SECONDARY_HARVEST_MEMORY ', &
        dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%fire_disturbance_rate,'FIRE_DISTURBANCE_RATE ', &
        dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%ignition_rate,'IGNITION_RATE ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%nat_disturbance_rate,'NAT_DISTURBANCE_RATE ', &
        dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_i(cpoly%nat_dist_type,'NAT_DIST_TYPE ',dsetrank,iparallel,.true.,foundvar)


   if (associated(cpoly%dmean_co2_residual))                                               &
      call hdf_getslab_r(cpoly%dmean_co2_residual   , 'DMEAN_CO2_RESIDUAL_SI '             &
                        ,dsetrank,iparallel,.false.,foundvar)

   if (associated(cpoly%dmean_water_residual))                                             &
      call hdf_getslab_r(cpoly%dmean_water_residual , 'DMEAN_WATER_RESIDUAL_SI '           &
                        ,dsetrank,iparallel,.false.,foundvar)

   if (associated(cpoly%dmean_energy_residual))                                            &
      call hdf_getslab_r(cpoly%dmean_energy_residual, 'DMEAN_ENERGY_RESIDUAL_SI '          &
                        ,dsetrank,iparallel,.false.,foundvar)

   if (associated(cpoly%mmean_co2_residual))                                               &
      call hdf_getslab_r(cpoly%mmean_co2_residual   , 'MMEAN_CO2_RESIDUAL_SI '             &
                        ,dsetrank,iparallel,.false.,foundvar)

   if (associated(cpoly%mmean_water_residual))                                             &
      call hdf_getslab_r(cpoly%mmean_water_residual , 'MMEAN_WATER_RESIDUAL_SI '           &
                        ,dsetrank,iparallel,.false.,foundvar)

   if (associated(cpoly%mmean_energy_residual))                                            &
      call hdf_getslab_r(cpoly%mmean_energy_residual, 'MMEAN_ENERGY_RESIDUAL_SI '          &
                        ,dsetrank,iparallel,.false.,foundvar)

   dsetrank    = 2_8
   globdims(1) = int(n_pft,8)
   chnkdims(1) = int(n_pft,8)
   memdims(1)  = int(n_pft,8)
   memsize(1)  = int(n_pft,8)
   chnkoffs(1) = 0_8
   memoffs(1)  = 0_8
   globdims(2)  = int(nsites_global,8)
   chnkdims(2)  = int(cpoly%nsites,8)
   chnkoffs(2)  = int(pysi_index - 1,8)
   memdims(2)   = int(cpoly%nsites,8)
   memsize(2)   = int(cpoly%nsites,8)
   memoffs(2)   = 0_8

   if (associated(cpoly%lai_pft)) call hdf_getslab_r(cpoly%lai_pft,'LAI_PFT_SI ', &
        dsetrank,iparallel,.false.,foundvar)
   if (associated(cpoly%wai_pft)) call hdf_getslab_r(cpoly%wai_pft,'WAI_PFT_SI ', &
        dsetrank,iparallel,.false.,foundvar)
   call hdf_getslab_r(cpoly%green_leaf_factor,'GREEN_LEAF_FACTOR ', &
        dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%leaf_aging_factor,'LEAF_AGING_FACTOR ', &
        dsetrank,iparallel,.true.,foundvar)

   dsetrank    = 2_8
   globdims(1) = int(nzg,8)
   chnkdims(1) = int(nzg,8)
   memdims(1)  = int(nzg,8)
   memsize(1)  = int(nzg,8)
   chnkoffs(1) = 0_8
   memoffs(1)  = 0_8
   globdims(2)  = int(nsites_global,8)
   chnkdims(2)  = int(cpoly%nsites,8)
   chnkoffs(2)  = int(pysi_index - 1,8)
   memdims(2)   = int(cpoly%nsites,8)
   memsize(2)   = int(cpoly%nsites,8)
   memoffs(2)   = 0_8

   call hdf_getslab_i(cpoly%ntext_soil,'NTEXT_SOIL_SI ',dsetrank,iparallel,.true.,foundvar)

   dsetrank    = 2_8
   globdims(1) = 12_8
   chnkdims(1) = 12_8
   memdims(1)  = 12_8
   memsize(1)  = 12_8
   chnkoffs(1) = 0_8
   memoffs(1)  = 0_8
   globdims(2)  = int(nsites_global,8)
   chnkdims(2)  = int(cpoly%nsites,8)
   chnkoffs(2)  = int(pysi_index - 1,8)
   memdims(2)   = int(cpoly%nsites,8)
   memsize(2)   = int(cpoly%nsites,8)
   memoffs(2)   = 0_8

   call hdf_getslab_r(cpoly%lambda_fire     ,'LAMBDA_FIRE '                                &
                     ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%avg_monthly_pcpg,'AVG_MONTHLY_PCPG '                           &
                     ,dsetrank,iparallel,.true.,foundvar)

   dsetrank    = 3_8
   globdims(1:2) = int(n_dist_types,8)
   chnkdims(1:2) = int(n_dist_types,8)
   memdims(1:2)  = int(n_dist_types,8)
   memsize(1:2)  = int(n_dist_types,8)
   chnkoffs(1:2) = 0
   memoffs(1:2)  = 0
   globdims(3)  = int(nsites_global,8)
   chnkdims(3)  = int(cpoly%nsites,8)
   chnkoffs(3)  = int(pysi_index - 1,8)
   memdims(3)   = int(cpoly%nsites,8)
   memsize(3)   = int(cpoly%nsites,8)
   memoffs(3)   = 0

   call hdf_getslab_r(cpoly%disturbance_memory,'DISTURBANCE_MEMORY ', &
        dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%disturbance_rates,'DISTURBANCE_RATES_SI ', &
        dsetrank,iparallel,.true.,foundvar)

   dsetrank    = 3
   globdims(3) = int(nsites_global,8)
   chnkdims(3) = int(cpoly%nsites,8)
   chnkoffs(3) = int(pysi_index - 1,8)
   memdims(3)  = int(cpoly%nsites,8)
   memsize(3)  = int(cpoly%nsites,8)
   memoffs(3)  = 0
   globdims(2) = int(n_dbh,8)
   chnkdims(2) = int(n_dbh,8)
   memdims(2)  = int(n_dbh,8)
   memsize(2)  = int(n_dbh,8)
   chnkoffs(2) = 0
   memoffs(2)  = 0
   globdims(1) = int(n_pft,8)
   chnkdims(1) = int(n_pft,8)
   memdims(1)  = int(n_pft,8)
   memsize(1)  = int(n_pft,8)
   chnkoffs(1) = 0
   memoffs(1)  = 0

   call hdf_getslab_r(cpoly%basal_area,'BASAL_AREA_SI ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%agb,'AGB_SI ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%bseeds,'BSEEDS_SI ',dsetrank,iparallel,.false.,foundvar)
   call hdf_getslab_r(cpoly%pldens,'PLDENS_SI ',dsetrank,iparallel,.false.,foundvar)
   call hdf_getslab_r(cpoly%basal_area_growth,'BASAL_AREA_GROWTH ', &
        dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%agb_growth,'AGB_GROWTH ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%basal_area_mort,'BASAL_AREA_MORT ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%basal_area_cut,'BASAL_AREA_CUT ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%agb_mort,'AGB_MORT ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(cpoly%agb_cut,'AGB_CUT ',dsetrank,iparallel,.true.,foundvar)

   return
 end subroutine fill_history_polygon
 !==========================================================================================!
 !==========================================================================================!






 !==========================================================================================!
 !==========================================================================================!
 subroutine fill_history_site(csite,sipa_index,npatches_global)

   use ed_state_vars,only: sitetype
   use grid_coms,only : nzg,nzs
   use ed_max_dims,only : n_pft,n_dbh
   use hdf5_coms,only:file_id,dset_id,dspace_id,plist_id, &
        globdims,chnkdims,chnkoffs,cnt,stride, &
        memdims,memoffs,memsize,datatype_id,setsize
   use fusion_fission_coms, only: ff_nhgt
   use ed_misc_coms, only : ndcycle
   use hdf5

   implicit none

#if USE_INTERF
  interface
     subroutine hdf_getslab_r(buff,varn,dsetrank,iparallel,required,foundvar)
       use hdf5_coms,only:memsize
       real,dimension(memsize(1),memsize(2),memsize(3),memsize(4)) :: buff
       integer :: dsetrank,iparallel
       character(len=*),intent(in) :: varn
       logical,intent(in) :: required
       logical,intent(out) :: foundvar
     end subroutine hdf_getslab_r
     subroutine hdf_getslab_d(buff,varn,dsetrank,iparallel,required,foundvar)
       use hdf5_coms,only:memsize
       real(kind=8),dimension(memsize(1),memsize(2),memsize(3),memsize(4)) :: buff
       integer :: dsetrank,iparallel
       character(len=*),intent(in) :: varn
       logical,intent(in) :: required
       logical,intent(out) :: foundvar
     end subroutine hdf_getslab_d
     subroutine hdf_getslab_i(buff,varn,dsetrank,iparallel,required,foundvar)
       use hdf5_coms,only:memsize
       integer,dimension(memsize(1),memsize(2),memsize(3),memsize(4)) :: buff
       integer :: dsetrank,iparallel
       character(len=*),intent(in) :: varn
       logical,intent(in) :: required
       logical,intent(out) :: foundvar
     end subroutine hdf_getslab_i
  end interface
#endif

   type(sitetype),target :: csite
   integer,intent(in) :: sipa_index
   integer,intent(in) :: npatches_global
   integer :: iparallel
   integer :: dsetrank
   integer :: hdferr
   integer :: ipa,ipft
   real(kind=8),allocatable, dimension(:,:) ::  buff
   logical :: foundvar

   iparallel = 0

   dsetrank = 1
   globdims = 0
   chnkdims = 0
   chnkoffs = 0
   memoffs  = 0
   memdims  = 0
   memsize  = 1

   ! These are the dimensions in the filespace
   ! itself. Global is the size of the dataset,
   ! chnkoffs is the offset of the chunk we
   ! are going to read.  Chnkdims is the size
   ! of the slab that is to be read.

   globdims(1) = int(npatches_global,8)
   chnkdims(1) = int(csite%npatches,8)
   chnkoffs(1) = int(sipa_index - 1,8)

   memdims(1)  = int(csite%npatches,8)
   memsize(1)  = int(csite%npatches,8)
   memoffs(1)  = 0

   call hdf_getslab_i(csite%dist_type,'DIST_TYPE ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%age,'AGE ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%area,'AREA ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%fast_soil_C,'FAST_SOIL_C ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%slow_soil_C,'SLOW_SOIL_C ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%structural_soil_C,'STRUCTURAL_SOIL_C ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%structural_soil_L,'STRUCTURAL_SOIL_L ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%mineralized_soil_N,'MINERALIZED_SOIL_N ', &
        dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%fast_soil_N,'FAST_SOIL_N ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%sum_dgd,'SUM_DGD ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%sum_chd,'SUM_CHD ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_i(csite%plantation,'PLANTATION ',dsetrank,iparallel,.true.,foundvar)
   !  call hdf_getslab_i(csite%cohort_count,'COHORT_COUNT ',dsetrank,iparallel)
   call hdf_getslab_r(csite%can_theiv,'CAN_THEIV ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%can_vpdef,'CAN_VPDEF ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%can_prss,'CAN_PRSS ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%can_theta,'CAN_THETA ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%can_temp,'CAN_TEMP ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%can_temp_pv,'CAN_TEMP_PV ',dsetrank,iparallel,.false.,foundvar)
   call hdf_getslab_r(csite%can_shv,'CAN_SHV ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%can_co2,'CAN_CO2 ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%can_rhos,'CAN_RHOS ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%can_depth,'CAN_DEPTH ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%ggbare,'GGBARE ',dsetrank,iparallel,.false.,foundvar)
   call hdf_getslab_r(csite%ggveg,'GGVEG ',dsetrank,iparallel,.false.,foundvar)
   call hdf_getslab_r(csite%ggnet,'GGNET ',dsetrank,iparallel,.false.,foundvar)
   call hdf_getslab_r(csite%ggsoil,'GGSOIL ',dsetrank,iparallel,.false.,foundvar)
   call hdf_getslab_r(csite%opencan_frac,'OPENCAN_FRAC ',dsetrank,iparallel,.false.,foundvar)
   !  call hdf_getslab_i(csite%pname,'PNAME ',dsetrank,iparallel)
   call hdf_getslab_r(csite%lai,'LAI_PA ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%wai,'WAI_PA ',dsetrank,iparallel,.false.,foundvar)
   call hdf_getslab_i(csite%nlev_sfcwater,'NLEV_SFCWATER ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%ground_shv ,'GROUND_SHV ',dsetrank,iparallel,.false.,foundvar)
   call hdf_getslab_r(csite%ground_ssh ,'GROUND_SSH ',dsetrank,iparallel,.false.,foundvar)
   call hdf_getslab_r(csite%ground_temp,'GROUND_TEMP ',dsetrank,iparallel,.false.,foundvar)
   call hdf_getslab_r(csite%ground_fliq,'GROUND_FLIQ ',dsetrank,iparallel,.false.,foundvar)
   call hdf_getslab_r(csite%rough,'ROUGH ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%avg_daily_temp,'AVG_DAILY_TEMP ',dsetrank,iparallel,.true.,foundvar)  
   call hdf_getslab_r(csite%avg_monthly_gndwater,'AVG_MONTHLY_GNDWATER ',dsetrank,iparallel,.true.,foundvar)  
   call hdf_getslab_r(csite%avg_monthly_waterdef,'AVG_MONTHLY_WATERDEF ',dsetrank,iparallel,.true.,foundvar)  
   call hdf_getslab_r(csite%mean_rh,'MEAN_RH ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%mean_cwd_rh,'MEAN_CWD_RH ',dsetrank,iparallel,.true.,foundvar)

   if (associated(csite%dmean_rh       )) &
        call hdf_getslab_r(csite%dmean_rh,'DMEAN_RH_PA ',dsetrank,iparallel,.false.,foundvar)
   if (associated(csite%mmean_rh       )) &
        call hdf_getslab_r(csite%mmean_rh,'MMEAN_RH_PA ',dsetrank,iparallel,.false.,foundvar)
   if (associated(csite%dmean_cwd_rh   )) &
        call hdf_getslab_r(csite%dmean_cwd_rh,'DMEAN_CWD_RH_PA ',dsetrank,iparallel,.false.,foundvar)
   if (associated(csite%mmean_cwd_rh   )) &
        call hdf_getslab_r(csite%mmean_cwd_rh,'MMEAN_CWD_RH_PA ',dsetrank,iparallel,.false.,foundvar)

   if (associated(csite%dmean_albedo       )) &
        call hdf_getslab_r(csite%dmean_albedo,'DMEAN_ALBEDO_PA ',dsetrank,iparallel,.false.,foundvar)
   if (associated(csite%dmean_albedo_beam  )) &
        call hdf_getslab_r(csite%dmean_albedo_beam,'DMEAN_ALBEDO_BEAM_PA ',dsetrank,iparallel,.false.,foundvar)
   if (associated(csite%dmean_albedo_diffuse  )) &
        call hdf_getslab_r(csite%dmean_albedo_diffuse,'DMEAN_ALBEDO_DIFFUSE_PA ',dsetrank,iparallel,.false.,foundvar)

   if (associated(csite%mmean_albedo       )) &
        call hdf_getslab_r(csite%mmean_albedo,'MMEAN_ALBEDO_PA ',dsetrank,iparallel,.false.,foundvar)
   if (associated(csite%mmean_albedo_beam  )) &
        call hdf_getslab_r(csite%mmean_albedo_beam,'MMEAN_ALBEDO_BEAM_PA ',dsetrank,iparallel,.false.,foundvar)
   if (associated(csite%mmean_albedo_diffuse  )) &
        call hdf_getslab_r(csite%mmean_albedo_diffuse,'MMEAN_ALBEDO_DIFFUSE_PA ',dsetrank,iparallel,.false.,foundvar)

   call hdf_getslab_r(csite%mean_nep,'MEAN_NEP ',dsetrank,iparallel,.true.,foundvar)

   call hdf_getslab_r(csite%wbudget_loss2atm,'WBUDGET_LOSS2ATM ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%wbudget_denseffect,'WBUDGET_DENSEFFECT ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%wbudget_precipgain,'WBUDGET_PRECIPGAIN ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%wbudget_loss2runoff,'WBUDGET_LOSS2RUNOFF ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%wbudget_initialstorage,'WBUDGET_INITIALSTORAGE ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%ebudget_loss2atm,'EBUDGET_LOSS2ATM ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%ebudget_denseffect,'EBUDGET_DENSEFFECT ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%ebudget_prsseffect,'EBUDGET_PRSSEFFECT ',dsetrank,iparallel,.false.,foundvar)
   call hdf_getslab_r(csite%ebudget_loss2runoff,'EBUDGET_LOSS2RUNOFF ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%ebudget_netrad,'EBUDGET_NETRAD ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%ebudget_precipgain,'EBUDGET_PRECIPGAIN ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%ebudget_initialstorage,'EBUDGET_INITIALSTORAGE ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%co2budget_initialstorage,'CO2BUDGET_INITIALSTORAGE ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%co2budget_loss2atm,'CO2BUDGET_LOSS2ATM ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%co2budget_denseffect,'CO2BUDGET_DENSEFFECT ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%co2budget_gpp,'CO2BUDGET_GPP ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%co2budget_plresp,'CO2BUDGET_PLRESP ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%co2budget_rh,'CO2BUDGET_RH ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%today_A_decomp,'TODAY_A_DECOMP ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%today_Af_decomp,'TODAY_AF_DECOMP ',dsetrank,iparallel,.true.,foundvar)
   if (associated(csite%dmean_A_decomp       )) &
        call hdf_getslab_r(csite%dmean_A_decomp,'DMEAN_A_DECOMP ',dsetrank,iparallel,.false.,foundvar)
   if (associated(csite%dmean_Af_decomp       )) &
        call hdf_getslab_r(csite%dmean_Af_decomp,'DMEAN_AF_DECOMP ',dsetrank,iparallel,.false.,foundvar)
   if (associated(csite%mmean_A_decomp       )) &
        call hdf_getslab_r(csite%mmean_A_decomp,'MMEAN_A_DECOMP ',dsetrank,iparallel,.false.,foundvar)
   if (associated(csite%mmean_Af_decomp       )) &
   call hdf_getslab_r(csite%mmean_Af_decomp,'MMEAN_AF_DECOMP ',dsetrank,iparallel,.false.,foundvar)
   call hdf_getslab_r(csite%veg_rough,'VEG_ROUGH ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%veg_height ,'VEG_HEIGHT ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%veg_displace ,'VEG_DISPLACE ',dsetrank,iparallel,.false.,foundvar)
   call hdf_getslab_r(csite%fsc_in,'FSC_IN ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%ssc_in,'SSC_IN ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%ssl_in,'SSL_IN ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%fsn_in,'FSN_IN ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%total_plant_nitrogen_uptake,'TOTAL_PLANT_NITROGEN_UPTAKE ',dsetrank,iparallel,.true.,foundvar)
   
   call hdf_getslab_r(csite%rshort_g,'RSHORT_G ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%rshort_g_beam,'RSHORT_G_BEAM ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%rshort_g_diffuse,'RSHORT_G_DIFFUSE ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%par_b,'PAR_B ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%par_b_beam,'PAR_B_BEAM ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%par_b_diffuse,'PAR_B_DIFFUSE ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%nir_b,'NIR_B ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%nir_b_beam,'NIR_B_BEAM ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%nir_b_diffuse,'NIR_B_DIFFUSE ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%rlong_g,'RLONG_G ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%rlong_g_surf,'RLONG_G_SURF ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%rlong_g_incid,'RLONG_G_INCID ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%rlong_s,'RLONG_S ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%rlong_s_surf,'RLONG_S_SURF ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%rlong_s_incid,'RLONG_S_INCID ',dsetrank,iparallel,.true.,foundvar)
   
   call hdf_getslab_r(csite%albedo,'ALBEDO ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%albedo_beam,'ALBEDO_BEAM ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%albedo_diffuse,'ALBEDO_DIFFUSE ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%rlong_albedo,'RLONG_ALBEDO ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%rlongup,'RLONGUP ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%parup,'PARUP ',dsetrank,iparallel,.false.,foundvar)
   call hdf_getslab_r(csite%nirup,'NIRUP ',dsetrank,iparallel,.false.,foundvar)
   call hdf_getslab_r(csite%rshortup,'RSHORTUP ',dsetrank,iparallel,.false.,foundvar)
   call hdf_getslab_r(csite%rnet,'RNET ',dsetrank,iparallel,.false.,foundvar)

   call hdf_getslab_r(csite%total_sfcw_depth,'TOTAL_SFCW_DEPTH ',dsetrank,iparallel,.false.,foundvar)
   call hdf_getslab_r(csite%snowfac,'SNOWFAC ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%A_decomp,'A_DECOMP ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%f_decomp,'F_DECOMP ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%rh,'RH ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%cwd_rh,'CWD_RH ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%plant_ag_biomass,'PLANT_AG_BIOMASS ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%mean_wflux,'MEAN_WFLUX ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%mean_latflux,'MEAN_LATFLUX ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%mean_hflux,'MEAN_HFLUX ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%mean_runoff,'MEAN_RUNOFF ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%mean_qrunoff,'MEAN_QRUNOFF ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%htry,'HTRY ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%hprev,'HPREV ',dsetrank,iparallel,.false.,foundvar)

   if(csite%hprev(1) < 1.0d-10)then
      csite%hprev=csite%htry
   end if
   

   if (associated(csite%dmean_rk4step)) &
        call hdf_getslab_r(csite%dmean_rk4step,'DMEAN_RK4STEP ',dsetrank,iparallel,.false.,foundvar)
   if (associated(csite%mmean_rk4step)) &
        call hdf_getslab_r(csite%mmean_rk4step,'MMEAN_RK4STEP ',dsetrank,iparallel,.false.,foundvar)

   call hdf_getslab_r(csite%mineralized_N_loss ,'NMIN_LOSS  ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%mineralized_N_input,'NMIN_INPUT ',dsetrank,iparallel,.true.,foundvar)
   
   call hdf_getslab_r(csite%ustar    ,'USTAR '    ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%tstar    ,'TSTAR '    ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%qstar    ,'QSTAR '    ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%qstar    ,'CSTAR '    ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%zeta     ,'ZETA '     ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%ribulk   ,'RIBULK '   ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%upwp     ,'UPWP '     ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%tpwp     ,'TPWP '     ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%qpwp     ,'QPWP '     ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%cpwp     ,'CPWP '     ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%wpwp     ,'WPWP '     ,dsetrank,iparallel,.true.,foundvar)


   call hdf_getslab_r(csite%par_l_max        ,'PAR_L_MAX '                                 &
                     ,dsetrank,iparallel,.false.,foundvar)
   call hdf_getslab_r(csite%par_l_beam_max   ,'PAR_L_BEAM_MAX '                            &
                     ,dsetrank,iparallel,.false.,foundvar)
   call hdf_getslab_r(csite%par_l_diffuse_max,'PAR_L_DIFFUSE_MAX '                         &
                     ,dsetrank,iparallel,.false.,foundvar)


   call hdf_getslab_r(csite%co2budget_initialstorage  ,'CO2BUDGET_INITIALSTORAGE '         &
                     ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%co2budget_residual        ,'CO2BUDGET_RESIDUAL '               &
                     ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%co2budget_loss2atm        ,'CO2BUDGET_LOSS2ATM '               &
                     ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%co2budget_denseffect      ,'CO2BUDGET_DENSEFFECT '             &
                     ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%co2budget_gpp             ,'CO2BUDGET_GPP        '             &
                     ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%co2budget_plresp          ,'CO2BUDGET_PLRESP     '             &
                     ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%co2budget_rh              ,'CO2BUDGET_RH         '             &
                     ,dsetrank,iparallel,.true.,foundvar)

   call hdf_getslab_r(csite%ebudget_initialstorage    ,'EBUDGET_INITIALSTORAGE '           &
                     ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%ebudget_residual          ,'EBUDGET_RESIDUAL '                 &
                     ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%ebudget_loss2atm          ,'EBUDGET_LOSS2ATM '                 &
                     ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%ebudget_denseffect        ,'EBUDGET_DENSEFFECT '               &
                     ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%ebudget_prsseffect        ,'EBUDGET_PRSSEFFECT '               &
                     ,dsetrank,iparallel,.false.,foundvar)
   call hdf_getslab_r(csite%ebudget_loss2runoff       ,'EBUDGET_LOSS2RUNOFF '              &
                     ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%ebudget_loss2drainage     ,'EBUDGET_LOSS2DRAINAGE '            &
                     ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%ebudget_netrad            ,'EBUDGET_NETRAD     '               &
                     ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%ebudget_precipgain        ,'EBUDGET_PRECIPGAIN '               &
                     ,dsetrank,iparallel,.true.,foundvar)

   call hdf_getslab_r(csite%wbudget_initialstorage    ,'WBUDGET_INITIALSTORAGE '           &
                     ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%wbudget_residual          ,'WBUDGET_RESIDUAL '                 &
                     ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%wbudget_loss2atm          ,'WBUDGET_LOSS2ATM '                 &
                     ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%wbudget_denseffect        ,'WBUDGET_DENSEFFECT '               &
                     ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%wbudget_loss2runoff       ,'WBUDGET_LOSS2RUNOFF '              &
                     ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%wbudget_loss2drainage     ,'WBUDGET_LOSS2DRAINAGE '            &
                     ,dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%wbudget_precipgain        ,'WBUDGET_PRECIPGAIN '               &
                     ,dsetrank,iparallel,.true.,foundvar)

   if (associated(csite%dmean_co2_residual))                                               &
      call hdf_getslab_r(csite%dmean_co2_residual   , 'DMEAN_CO2_RESIDUAL_PA '             &
                        ,dsetrank,iparallel,.false.,foundvar)

   if (associated(csite%dmean_water_residual))                                             &
      call hdf_getslab_r(csite%dmean_water_residual , 'DMEAN_WATER_RESIDUAL_PA '           &
                        ,dsetrank,iparallel,.false.,foundvar)

   if (associated(csite%dmean_energy_residual))                                            &
      call hdf_getslab_r(csite%dmean_energy_residual, 'DMEAN_ENERGY_RESIDUAL_PA '          &
                        ,dsetrank,iparallel,.false.,foundvar)

   if (associated(csite%mmean_co2_residual))                                               &
      call hdf_getslab_r(csite%mmean_co2_residual   , 'MMEAN_CO2_RESIDUAL_PA '             &
                        ,dsetrank,iparallel,.false.,foundvar)

   if (associated(csite%mmean_water_residual))                                             &
      call hdf_getslab_r(csite%mmean_water_residual , 'MMEAN_WATER_RESIDUAL_PA '           &
                        ,dsetrank,iparallel,.false.,foundvar)

   if (associated(csite%mmean_energy_residual))                                            &
      call hdf_getslab_r(csite%mmean_energy_residual, 'MMEAN_ENERGY_RESIDUAL_PA '          &
                        ,dsetrank,iparallel,.false.,foundvar)





   dsetrank    = 2
   globdims(1) = int(ndcycle,8)
   chnkdims(1) = int(ndcycle,8)
   memdims(1)  = int(ndcycle,8)
   memsize(1)  = int(ndcycle,8)
   chnkoffs(1) = 0
   memoffs(1)  = 0
   globdims(2) = int(npatches_global,8)
   chnkdims(2) = int(csite%npatches,8)
   chnkoffs(2) = int(sipa_index - 1,8)
   memdims(2)  = int(csite%npatches,8)
   memsize(2)  = int(csite%npatches,8)
   memoffs(2)  = 0
   
   if (associated(csite%qmean_rh))                                                         &
      call hdf_getslab_r(csite%qmean_rh    ,'QMEAN_RH_PA '                                 &
                        ,dsetrank,iparallel,.true.,foundvar)
   if (associated(csite%qmean_cwd_rh))                                                     &
      call hdf_getslab_r(csite%qmean_cwd_rh,'QMEAN_CWD_RH_PA '                             &
                        ,dsetrank,iparallel,.true.,foundvar)

   if (associated(csite%qmean_albedo       ))                                              &
        call hdf_getslab_r(csite%qmean_albedo,'QMEAN_ALBEDO_PA '                           &
                          ,dsetrank,iparallel,.false.,foundvar)
   if (associated(csite%qmean_albedo_beam  ))                                              &
        call hdf_getslab_r(csite%qmean_albedo_beam,'QMEAN_ALBEDO_BEAM_PA '                 &
                          ,dsetrank,iparallel,.false.,foundvar)
   if (associated(csite%qmean_albedo_diffuse  ))                                           &
        call hdf_getslab_r(csite%qmean_albedo_diffuse,'QMEAN_ALBEDO_DIFFUSE_PA '           &
                          ,dsetrank,iparallel,.false.,foundvar)
   
   dsetrank    = 2
   globdims(1) = int(nzs,8)
   chnkdims(1) = int(nzs,8)
   memdims(1)  = int(nzs,8)
   memsize(1)  = int(nzs,8)
   chnkoffs(1) = 0
   memoffs(1)  = 0
   globdims(2) = int(npatches_global,8)
   chnkdims(2) = int(csite%npatches,8)
   chnkoffs(2) = int(sipa_index - 1,8)
   memdims(2)  = int(csite%npatches,8)
   memsize(2)  = int(csite%npatches,8)
   memoffs(2)  = 0
   
   call hdf_getslab_r(csite%sfcwater_mass,'SFCWATER_MASS ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%sfcwater_energy,'SFCWATER_ENERGY ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%sfcwater_depth,'SFCWATER_DEPTH ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%rshort_s,'RSHORT_S ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%rshort_s_beam,'RSHORT_S_BEAM ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%rshort_s_diffuse,'RSHORT_S_DIFFUSE ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%sfcwater_tempk,'SFCWATER_TEMPK ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%sfcwater_fracliq,'SFCWATER_FRACLIQ ',dsetrank,iparallel,.true.,foundvar)
   
   dsetrank    = 2
   globdims(1) = int(nzg,8)
   chnkdims(1) = int(nzg,8)
   memdims(1)  = int(nzg,8)
   memsize(1)  = int(nzg,8)
   chnkoffs(1) = 0
   memoffs(1)  = 0
   globdims(2) = int(npatches_global,8)
   chnkdims(2) = int(csite%npatches,8)
   chnkoffs(2) = int(sipa_index - 1,8)
   memdims(2)  = int(csite%npatches,8)
   memsize(2)  = int(csite%npatches,8)
   memoffs(2)  = 0
   call hdf_getslab_r(csite%soil_energy,'SOIL_ENERGY_PA ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%soil_water,'SOIL_WATER_PA ',dsetrank,iparallel,.true.,foundvar)
   
   call hdf_getslab_r(csite%soil_tempk,'SOIL_TEMPK_PA ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%soil_fracliq,'SOIL_FRACLIQ_PA ',dsetrank,iparallel,.true.,foundvar)
   call hdf_getslab_r(csite%rootdense,'PATCH_ROOT_DENSITY ',dsetrank,iparallel,.false.,foundvar)

   !-----------------------------------------------------------------------------------!
   !  Soil water is double precision, although it may not be DP in the dataset
   !  The following lines make provisions for this by testing the dataset.
   
!   call h5dopen_f(file_id,'SOIL_WATER_PA ', dset_id, hdferr)
!   if (hdferr /= 0 ) then
!      call fatal_error('Dataset did not have soil water?' &
!           ,'fill_history_site','ed_init_full_history.F90')
!   endif
   ! ---------------------------------------------------------------------------------!
   ! THESE LINES ARE USEFULL FOR DETERMINING DATA SIZE OF ANY GIVEN OBJECT IN A SET   !
   ! ---------------------------------------------------------------------------------!
   
   !call h5dget_type_f(dset_id,datatype_id,hdferr)
   !call h5tget_size_f(datatype_id,setsize,hdferr)
   !call h5dclose_f(dset_id  , hdferr)
 
! =============================================================================================
! KEEP THIS CODE AS A TEMPLATE IN CASE WE NEED TO DO SOMETHING LIKE THIS IN THE FUTURE
!  HELPFUL IF WE CHANGE DATA TYPES
! ---------------------------------------------------------------------------------------------
!   if (setsize==4_8) then  !Old precision
!      call hdf_getslab_r(csite%soil_water,'SOIL_WATER_PA ',dsetrank,iparallel,.true.,foundvar)
!   else if (setsize==8_8) then ! Newer precision
!      allocate(buff(nzg,csite%npatches))
!      write (unit=*,fmt='(a)') '-------------------------------------------------------------------'
!      write (unit=*,fmt='(a)') '  Loading 8-byte precision soil water and converting to 4-byte'
!      write (unit=*,fmt='(a)') '-------------------------------------------------------------------'
!      call hdf_getslab_d(buff,'SOIL_WATER_PA ',dsetrank,iparallel,.true.,foundvar)
!      csite%soil_water(1:nzg,1:csite%npatches) = sngl(buff(1:nzg,1:csite%npatches))
!      deallocate(buff)
!  else
!     call fatal_error('Soil water dataset is not real nor double?'                         &
!                     ,'fill_history_site','ed_init_full_history.F90')
!  end if

  !--------------------------------------------------------------------------------------------  

  dsetrank    = 2
  globdims(1) = int(n_pft,8)
  chnkdims(1) = int(n_pft,8)
  memdims(1)  = int(n_pft,8)
  memsize(1)  = int(n_pft,8)
  chnkoffs(1) = 0_8
  memoffs(1)  = 0_8
  globdims(2) = int(npatches_global,8)
  chnkdims(2) = int(csite%npatches,8)
  chnkoffs(2) = int(sipa_index - 1,8)
  memdims(2)  = int(csite%npatches,8)
  memsize(2)  = int(csite%npatches,8)
  memoffs(2)  = 0_8
  
  call hdf_getslab_r(csite%A_o_max,'A_O_MAX ',dsetrank,iparallel,.true.,foundvar) 
  call hdf_getslab_r(csite%A_c_max,'A_C_MAX ',dsetrank,iparallel,.true.,foundvar) 
  call hdf_getslab_r(csite%repro,'REPRO_PA ',dsetrank,iparallel,.true.,foundvar)


  dsetrank    = 2
  globdims(1) = int(n_dbh,8)
  chnkdims(1) = int(n_dbh,8)
  memdims(1)  = int(n_dbh,8)
  memsize(1)  = int(n_dbh,8)
  chnkoffs(1) = 0_8
  memoffs(1)  = 0_8
  globdims(2) = int(npatches_global,8)
  chnkdims(2) = int(csite%npatches,8)
  chnkoffs(2) = int(sipa_index - 1,8)
  memdims(2)  = int(csite%npatches,8)
  memsize(2)  = int(csite%npatches,8)
  memoffs(2)  = 0_8
  call hdf_getslab_r(csite%co2budget_gpp_dbh,'CO2BUDGET_GPP_DBH ',dsetrank,iparallel,.true.,foundvar)

  dsetrank    = 3
  globdims(3) = int(npatches_global,8)
  chnkdims(3) = int(csite%npatches,8)
  chnkoffs(3) = int(sipa_index - 1,8)

  memdims(3)  = int(csite%npatches,8)
  memsize(3)  = int(csite%npatches,8)
  memoffs(3)  = 0_8
  
  globdims(2) = int(ff_nhgt,8)
  chnkdims(2) = int(ff_nhgt,8)
  memdims(2)  = int(ff_nhgt,8)
  memsize(2)  = int(ff_nhgt,8)
  chnkoffs(2) = 0_8
  memoffs(2)  = 0_8

  globdims(1) = int(n_pft,8)
  chnkdims(1) = int(n_pft,8)
  memdims(1)  = int(n_pft,8)
  memsize(1)  = int(n_pft,8)
  chnkoffs(1) = 0_8
  memoffs(1)  = 0_8

  call hdf_getslab_r(csite%cumlai_profile,'CUMLAI_PROFILE ',dsetrank,iparallel,.false.,foundvar)

  return
end subroutine fill_history_site

!==========================================================================================!
!==========================================================================================!

subroutine fill_history_patch(cpatch,paco_index,ncohorts_global,green_leaf_factor)
  
  use ed_state_vars,only: patchtype
  use hdf5_coms,only:file_id,dset_id,dspace_id,plist_id, &
       filespace,memspace, &
       globdims,chnkdims,chnkoffs,cnt,stride, &
       memdims,memoffs,memsize
  use ed_max_dims,only: n_pft, n_mort
  use allometry, only : dbh2ca
  use ed_misc_coms, only : ndcycle
  implicit none

#if USE_INTERF
  interface
     subroutine hdf_getslab_r(buff,varn,dsetrank,iparallel,required,foundvar)
       use hdf5_coms,only:memsize
       real,dimension(memsize(1),memsize(2),memsize(3),memsize(4)) :: buff
       integer :: dsetrank,iparallel
       character(len=*),intent(in) :: varn
       logical,intent(in) :: required
       logical,intent(out) :: foundvar
     end subroutine hdf_getslab_r
     subroutine hdf_getslab_d(buff,varn,dsetrank,iparallel,required,foundvar)
       use hdf5_coms,only:memsize
       real(kind=8),dimension(memsize(1),memsize(2),memsize(3),memsize(4)) :: buff
       integer :: dsetrank,iparallel
       character(len=*),intent(in) :: varn
       logical,intent(in) :: required
       logical,intent(out) :: foundvar
     end subroutine hdf_getslab_d
     subroutine hdf_getslab_i(buff,varn,dsetrank,iparallel,required,foundvar)
       use hdf5_coms,only:memsize
       integer,dimension(memsize(1),memsize(2),memsize(3),memsize(4)) :: buff
       integer :: dsetrank,iparallel
       character(len=*),intent(in) :: varn
       logical,intent(in) :: required
       logical,intent(out) :: foundvar
     end subroutine hdf_getslab_i
  end interface
#endif

  type(patchtype),target :: cpatch
  integer,intent(in) :: paco_index
  integer,intent(in) :: ncohorts_global
  real, dimension(n_pft), intent(in) :: green_leaf_factor
  integer :: iparallel,dsetrank
   logical :: foundvar
  
  ! Needed for reconstructing veg_energy if using an old restart
  ! ------------------------------------------------------------
  real :: plai
  integer :: ico
  ! ------------------------------------------------------------

  iparallel = 0
  
  dsetrank = 1
  globdims = 0_8
  chnkdims = 0_8
  chnkoffs = 0_8
  memoffs  = 0_8
  memdims  = 0_8
  memsize  = 1_8
  
  globdims(1) = int(ncohorts_global,8)
  chnkdims(1) = int(cpatch%ncohorts,8)
  chnkoffs(1) = int(paco_index - 1,8)

  memdims(1)  = int(cpatch%ncohorts,8)
  memsize(1)  = int(cpatch%ncohorts,8)
  memoffs(1)  = 0_8

  if (cpatch%ncohorts > 0) then

     call hdf_getslab_i(cpatch%pft,'PFT ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%nplant,'NPLANT ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%hite,'HITE ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%dbh,'DBH ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%agb,'AGB_CO ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%basarea,'BA_CO',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%dagb_dt,'DAGB_DT ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%dlnagb_dt,'DLNAGB_DT ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%dba_dt,'DBA_DT',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%dlnba_dt,'DLNBA_DT',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%ddbh_dt,'DDBH_DT',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%dlndbh_dt,'DLNDBH_DT',dsetrank,iparallel,.true.,foundvar)

     call hdf_getslab_r(cpatch%bdead,'BDEAD ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%bleaf,'BLEAF ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_i(cpatch%phenology_status,'PHENOLOGY_STATUS ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_i(cpatch%recruit_dbh,'RECRUIT_DBH ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_i(cpatch%census_status,'CENSUS_STATUS ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%balive,'BALIVE ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%broot,'BROOT  ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%bsapwooda,'BSAPWOODA ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%bsapwoodb,'BSAPWOODB ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%lai,'LAI_CO ',dsetrank,iparallel,.true.,foundvar)
     
     call hdf_getslab_r(cpatch%llspan,'LLSPAN ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%turnover_amp,'TURNOVER_AMP ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%vm_bar,'VM_BAR ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%sla,'SLA ',dsetrank,iparallel,.true.,foundvar)

     call hdf_getslab_r(cpatch%bstorage,'BSTORAGE ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%cbr_bar,'CBR_BAR ',dsetrank,iparallel,.true.,foundvar)
     
     call hdf_getslab_r(cpatch%wai,'WAI_CO ',dsetrank,iparallel,.true.,foundvar)
     
     call hdf_getslab_r(cpatch%crown_area,'CROWN_AREA_CO ',dsetrank,iparallel,.false.,foundvar)
     if (all(cpatch%crown_area == 0.)) then
         do ico= 1,cpatch%ncohorts
              cpatch%crown_area(ico) = min(1.0, cpatch%nplant(ico) * dbh2ca(cpatch%dbh(ico)  &
                               ,cpatch%hite(ico),cpatch%sla(ico),cpatch%pft(ico)))
         end do
     end if

     call hdf_getslab_r(cpatch%leaf_energy,'LEAF_ENERGY ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%leaf_hcap,'LEAF_HCAP ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%leaf_temp,'LEAF_TEMP ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%leaf_vpdef,'LEAF_VPDEF ',dsetrank,iparallel,.false.,foundvar)
     call hdf_getslab_r(cpatch%leaf_temp_pv,'LEAF_TEMP_PV ',dsetrank,iparallel,.false.,foundvar)
     call hdf_getslab_r(cpatch%leaf_water,'LEAF_WATER ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%leaf_fliq,'LEAF_FLIQ ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%wood_energy,'WOOD_ENERGY ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%wood_hcap,'WOOD_HCAP ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%wood_temp,'WOOD_TEMP ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%wood_temp_pv,'WOOD_TEMP_PV ',dsetrank,iparallel,.false.,foundvar)
     call hdf_getslab_r(cpatch%wood_water,'WOOD_WATER ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%wood_fliq,'WOOD_FLIQ ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%veg_wind,'VEG_WIND ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%lsfc_co2_open,'LSFC_CO2_OPEN ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%lsfc_co2_closed,'LSFC_CO2_CLOSED ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%lsfc_shv_open,'LSFC_SHV_OPEN ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%lsfc_shv_closed,'LSFC_SHV_CLOSED ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%lint_shv,'LINT_SHV ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%lint_co2_open,'LINT_CO2_OPEN ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%lint_co2_closed,'LINT_CO2_CLOSED ',dsetrank,iparallel,.true.,foundvar)
 
     call hdf_getslab_r(cpatch%mean_gpp,'MEAN_GPP ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%mean_leaf_resp,'MEAN_LEAF_RESP ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%mean_root_resp,'MEAN_ROOT_RESP ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%today_leaf_resp,'TODAY_LEAF_RESP ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%today_root_resp,'TODAY_ROOT_RESP ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%today_gpp,'TODAY_GPP ',dsetrank,iparallel,.true.,foundvar)
     
     if (associated(cpatch%today_nppleaf            ))                                          &
         call hdf_getslab_r(cpatch%today_nppleaf ,'TODAY_NPPLEAF ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%today_nppfroot           ))                                          &
         call hdf_getslab_r(cpatch%today_nppfroot,'TODAY_NPPFROOT ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%today_nppsapwood         ))                                          &
         call hdf_getslab_r(cpatch%today_nppsapwood ,'TODAY_NPPSAPWOOD ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%today_nppcroot           ))                                          &
         call hdf_getslab_r(cpatch%today_nppcroot,'TODAY_NPPCROOT ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%today_nppseeds           ))                                          &
         call hdf_getslab_r(cpatch%today_nppseeds,'TODAY_NPPSEEDS ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%today_nppwood            ))                                          &
         call hdf_getslab_r(cpatch%today_nppwood ,'TODAY_NPPWOOD ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%today_nppdaily           ))                                          &
         call hdf_getslab_r(cpatch%today_nppdaily,'TODAY_NPPDAILY ',dsetrank,iparallel,.false.,foundvar)
     
     call hdf_getslab_r(cpatch%today_gpp_pot,'TODAY_GPP_POT ',dsetrank,iparallel,.true.,foundvar)

     !-------------------------------------------------------------------------------------!
     !     Check whether this is a new or old history file.  Old history files don't have  !
     ! the light and moisture distinction.                                                 !
     !-------------------------------------------------------------------------------------!
     call hdf_getslab_r(cpatch%today_gpp_lightmax,'TODAY_GPP_MAX ',dsetrank,iparallel      &
                       ,.false.,foundvar)
     if (.not. foundvar) then
        !---- New history, load the new variables. ----------------------------------------!
        call hdf_getslab_r(cpatch%today_gpp_lightmax,'TODAY_GPP_LIGHTMAX ',dsetrank        &
                          ,iparallel,.true.,foundvar)
        call hdf_getslab_r(cpatch%today_gpp_moistmax,'TODAY_GPP_MOISTMAX ',dsetrank        &
                          ,iparallel,.true.,foundvar)
        !----------------------------------------------------------------------------------!
     else
        !---- Old history, make up something for the moisture maximum. --------------------!
        cpatch%today_gpp_moistmax(:) = cpatch%today_gpp(:)
        !----------------------------------------------------------------------------------!
     end if
     !-------------------------------------------------------------------------------------!


     call hdf_getslab_r(cpatch%growth_respiration,'GROWTH_RESPIRATION ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%storage_respiration,'STORAGE_RESPIRATION ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%vleaf_respiration,'VLEAF_RESPIRATION ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%fsn,'FSN ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%monthly_dndt,'MONTHLY_DNDT ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%monthly_dlnndt,'MONTHLY_DLNNDT ',dsetrank,iparallel,.true.,foundvar)

     if (associated(cpatch%mmean_gpp       )) &
          call hdf_getslab_r(cpatch%mmean_gpp,'MMEAN_GPP_CO ',dsetrank,iparallel,.false.,foundvar)
          
     if (associated(cpatch%mmean_nppleaf            ))                                          &
         call hdf_getslab_r(cpatch%mmean_nppleaf ,'MMEAN_NPPLEAF_CO ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%mmean_nppfroot           ))                                          &
         call hdf_getslab_r(cpatch%mmean_nppfroot,'MMEAN_NPPFROOT_CO ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%mmean_nppsapwood         ))                                          &
         call hdf_getslab_r(cpatch%mmean_nppsapwood ,'MMEAN_NPPSAPWOOD_CO ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%mmean_nppcroot           ))                                          &
         call hdf_getslab_r(cpatch%mmean_nppcroot,'MMEAN_NPPCROOT_CO ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%mmean_nppseeds           ))                                          &
         call hdf_getslab_r(cpatch%mmean_nppseeds,'MMEAN_NPPSEEDS_CO ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%mmean_nppwood            ))                                          &
         call hdf_getslab_r(cpatch%mmean_nppwood ,'MMEAN_NPPWOOD_CO ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%mmean_nppdaily           ))                                          &
         call hdf_getslab_r(cpatch%mmean_nppdaily,'MMEAN_NPPDAILY_CO ',dsetrank,iparallel,.false.,foundvar)
          
     if (associated(cpatch%mmean_leaf_resp       )) &
          call hdf_getslab_r(cpatch%mmean_leaf_resp,'MMEAN_LEAF_RESP_CO ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%mmean_root_resp       )) &
          call hdf_getslab_r(cpatch%mmean_root_resp,'MMEAN_ROOT_RESP_CO ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%mmean_growth_resp       )) &
          call hdf_getslab_r(cpatch%mmean_growth_resp,'MMEAN_GROWTH_RESP_CO ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%mmean_storage_resp       )) &
          call hdf_getslab_r(cpatch%mmean_storage_resp,'MMEAN_STORAGE_RESP_CO ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%mmean_vleaf_resp       )) &
          call hdf_getslab_r(cpatch%mmean_vleaf_resp,'MMEAN_VLEAF_RESP_CO ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%dmean_leaf_resp       )) &
          call hdf_getslab_r(cpatch%dmean_leaf_resp,'DMEAN_LEAF_RESP_CO ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%dmean_root_resp       )) &
          call hdf_getslab_r(cpatch%dmean_root_resp,'DMEAN_ROOT_RESP_CO ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%dmean_gpp       )) &
          call hdf_getslab_r(cpatch%dmean_gpp,'DMEAN_GPP_CO ',dsetrank,iparallel,.false.,foundvar)
          
     if (associated(cpatch%dmean_nppleaf            ))                                          &
         call hdf_getslab_r(cpatch%dmean_nppleaf ,'DMEAN_NPPLEAF_CO ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%dmean_nppfroot           ))                                          &
         call hdf_getslab_r(cpatch%dmean_nppfroot,'DMEAN_NPPFROOT_CO ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%dmean_nppsapwood         ))                                          &
         call hdf_getslab_r(cpatch%dmean_nppsapwood ,'DMEAN_NPPSAPWOOD_CO ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%dmean_nppcroot           ))                                          &
         call hdf_getslab_r(cpatch%dmean_nppcroot,'DMEAN_NPPCROOT_CO ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%dmean_nppseeds           ))                                          &
         call hdf_getslab_r(cpatch%dmean_nppseeds,'DMEAN_NPPSEEDS_CO ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%dmean_nppwood            ))                                          &
         call hdf_getslab_r(cpatch%dmean_nppwood ,'DMEAN_NPPWOOD_CO ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%dmean_nppdaily           ))                                          &
         call hdf_getslab_r(cpatch%dmean_nppdaily,'DMEAN_NPPDAILY_CO ',dsetrank,iparallel,.false.,foundvar)
          
     if (associated(cpatch%dmean_fs_open       )) &
     call hdf_getslab_r(cpatch%dmean_fs_open,'DMEAN_FS_OPEN_CO ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%mmean_fs_open       )) &
     call hdf_getslab_r(cpatch%mmean_fs_open,'MMEAN_FS_OPEN_CO ',dsetrank,iparallel,.false.,foundvar) 
     if (associated(cpatch%dmean_fsw       )) &
     call hdf_getslab_r(cpatch%dmean_fsw,'DMEAN_FSW_CO ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%mmean_fsw       )) &
     call hdf_getslab_r(cpatch%mmean_fsw,'MMEAN_FSW_CO ',dsetrank,iparallel,.false.,foundvar) 
     if (associated(cpatch%dmean_fsn       )) &
     call hdf_getslab_r(cpatch%dmean_fsn,'DMEAN_FSN_CO ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%mmean_fsn       )) &
     call hdf_getslab_r(cpatch%mmean_fsn,'MMEAN_FSN_CO ',dsetrank,iparallel,.false.,foundvar) 
     if (associated(cpatch%dmean_psi_open   )) &
     call hdf_getslab_r(cpatch%dmean_psi_open,'DMEAN_PSI_OPEN_CO ',dsetrank,iparallel,.false.,foundvar) 
     if (associated(cpatch%dmean_psi_closed )) &
     call hdf_getslab_r(cpatch%dmean_psi_closed,'DMEAN_PSI_CLOSED_CO ',dsetrank,iparallel,.false.,foundvar) 
     if (associated(cpatch%mmean_psi_open   )) &
     call hdf_getslab_r(cpatch%mmean_psi_open,'MMEAN_PSI_OPEN_CO ',dsetrank,iparallel,.false.,foundvar) 
     if (associated(cpatch%mmean_psi_closed )) &
     call hdf_getslab_r(cpatch%mmean_psi_closed,'MMEAN_PSI_CLOSED_CO ',dsetrank,iparallel,.false.,foundvar) 
     if (associated(cpatch%dmean_water_supply )) &
     call hdf_getslab_r(cpatch%dmean_water_supply,'DMEAN_WATER_SUPPLY_CO ',dsetrank,iparallel,.false.,foundvar) 
     if (associated(cpatch%mmean_water_supply )) &
     call hdf_getslab_r(cpatch%mmean_water_supply,'MMEAN_WATER_SUPPLY_CO ',dsetrank,iparallel,.false.,foundvar) 
     if (associated(cpatch%mmean_leaf_maintenance )) &
     call hdf_getslab_r(cpatch%mmean_leaf_maintenance,'MMEAN_LEAF_MAINTENANCE ',dsetrank,iparallel,.false.,foundvar)   
     if (associated(cpatch%mmean_root_maintenance )) &
     call hdf_getslab_r(cpatch%mmean_root_maintenance,'MMEAN_ROOT_MAINTENANCE ',dsetrank,iparallel,.false.,foundvar)   
     if (associated(cpatch%mmean_leaf_drop       )) &
     call hdf_getslab_r(cpatch%mmean_leaf_drop,'MMEAN_LEAF_DROP_CO ',dsetrank,iparallel,.false.,foundvar)   
     if (associated(cpatch%mmean_cb       )) &
     call hdf_getslab_r(cpatch%mmean_cb,'MMEAN_CB ',dsetrank,iparallel,.false.,foundvar)   
     if (associated(cpatch%dmean_light_level       )) &
     call hdf_getslab_r(cpatch%dmean_light_level,'DMEAN_LIGHT_LEVEL ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%mmean_light_level       )) &
     call hdf_getslab_r(cpatch%mmean_light_level,'MMEAN_LIGHT_LEVEL ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%mmean_light_level       )) &
     call hdf_getslab_r(cpatch%dmean_light_level_beam,'DMEAN_LIGHT_LEVEL_BEAM ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%mmean_light_level_beam       )) &
     call hdf_getslab_r(cpatch%mmean_light_level_beam,'MMEAN_LIGHT_LEVEL_BEAM ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%dmean_light_level_diff       )) &
     call hdf_getslab_r(cpatch%dmean_light_level_diff,'DMEAN_LIGHT_LEVEL_DIFF ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%mmean_light_level_diff       )) &
     call hdf_getslab_r(cpatch%mmean_light_level_diff,'MMEAN_LIGHT_LEVEL_DIFF ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%dmean_par_l       )) &
     call hdf_getslab_r(cpatch%dmean_par_l,'DMEAN_PAR_L ',dsetrank,iparallel,.false.,foundvar)

     if (associated(cpatch%dmean_par_l_beam       )) &
     call hdf_getslab_r(cpatch%dmean_par_l_beam,'DMEAN_PAR_L_BEAM ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%dmean_par_l_diff       )) &
     call hdf_getslab_r(cpatch%dmean_par_l_diff,'DMEAN_PAR_L_DIFF ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%mmean_par_l       )) &
     call hdf_getslab_r(cpatch%mmean_par_l,'MMEAN_PAR_L ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%mmean_par_l_beam       )) &
     call hdf_getslab_r(cpatch%mmean_par_l_beam,'MMEAN_PAR_L_BEAM ',dsetrank,iparallel,.false.,foundvar)
     if (associated(cpatch%mmean_par_l_diff       )) &
     call hdf_getslab_r(cpatch%mmean_par_l_diff,'MMEAN_PAR_L_DIFF ',dsetrank,iparallel,.false.,foundvar)
     call hdf_getslab_r(cpatch%Psi_open,'PSI_OPEN ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_i(cpatch%krdepth,'KRDEPTH ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_i(cpatch%first_census,'FIRST_CENSUS ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_i(cpatch%new_recruit_flag,'NEW_RECRUIT_FLAG ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%light_level,'LIGHT_LEVEL ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%light_level_beam,'LIGHT_LEVEL_BEAM ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%light_level_diff,'LIGHT_LEVEL_DIFF ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%par_l,'PAR_L ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%par_l_beam,'PAR_L_BEAM ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%par_l_diffuse,'PAR_L_DIFFUSE ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%rshort_l,'RSHORT_L ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%rshort_l_beam,'RSHORT_L_BEAM ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%rshort_l_diffuse,'RSHORT_L_DIFFUSE ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%rlong_l,'RLONG_L ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%rlong_l_surf,'RLONG_L_SURF ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%rlong_l_incid,'RLONG_L_INCID ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%rshort_w,'RSHORT_W ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%rshort_w_beam,'RSHORT_W_BEAM ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%rshort_w_diffuse,'RSHORT_W_DIFFUSE ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%rlong_w,'RLONG_W ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%rlong_w_surf,'RLONG_W_SURF ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%rlong_w_incid,'RLONG_W_INCID ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%leaf_gbh,'LEAF_GBH ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%leaf_gbw,'LEAF_GBW ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%wood_gbh,'WOOD_GBH ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%wood_gbw,'WOOD_GBW ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%A_open,'A_OPEN ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%A_closed,'A_CLOSED ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%Psi_closed,'PSI_CLOSED ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%gsw_open,'GSW_OPEN ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%gsw_closed,'GSW_CLOSED ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%fsw,'FSW ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%fs_open,'FS_OPEN ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%water_supply,'WATER_SUPPLY ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%stomatal_conductance,'STOMATAL_CONDUCTANCE ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%leaf_maintenance,'LEAF_MAINTENANCE ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%root_maintenance,'ROOT_MAINTENANCE ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%leaf_drop,'LEAF_DROP ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%bseeds,'BSEEDS_CO ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%leaf_respiration,'LEAF_RESPIRATION ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%root_respiration,'ROOT_RESPIRATION ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%gpp,'GPP ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%paw_avg,'PAW_AVG ',dsetrank,iparallel,.true.,foundvar)
     call hdf_getslab_r(cpatch%elongf,'ELONGF ',dsetrank,iparallel,.false.,foundvar)
     
     !----- 13-month dimension (12 previous months + current month). ----------------------!
     dsetrank    = 2
     globdims(1) = 13_8
     chnkdims(1) = 13_8
     chnkoffs(1) = 0_8
     memdims(1)  = 13_8
     memsize(1)  = 13_8
     memoffs(2)  = 0_8
     
     globdims(2) = int(ncohorts_global,8)
     chnkdims(2) = int(cpatch%ncohorts,8)
     chnkoffs(2) = int(paco_index - 1,8)
     
     memdims(2)  = int(cpatch%ncohorts,8)
     memsize(2)  = int(cpatch%ncohorts,8)
     memoffs(2)  = 0_8
     
     call hdf_getslab_r(cpatch%cb,'CB ',dsetrank,iparallel,.true.,foundvar)
     !-------------------------------------------------------------------------------------!
     !      Maximum Carbon balance has changed, so check whether this is an old history.   !
     ! In case it is, we copy cb_max to lightmax, and assume cb_moistmax to be 1.0.        !
     ! Otherwise, we read in both maxima.                                                  !
     !-------------------------------------------------------------------------------------!
     call hdf_getslab_r(cpatch%cb_lightmax,'CB_MAX ',dsetrank,iparallel,.false.,foundvar)
     if (.not. foundvar) then
        !----- New file, both light and moisture are present.  Read them instead. ---------!
        call hdf_getslab_r(cpatch%cb_lightmax,'CB_LIGHTMAX ',dsetrank,iparallel            &
                          ,.true.,foundvar)
        call hdf_getslab_r(cpatch%cb_moistmax,'CB_MOISTMAX ',dsetrank,iparallel            &
                          ,.true.,foundvar)
        !----------------------------------------------------------------------------------!
     else
        !----------------------------------------------------------------------------------!
        !      Old file, make up some initial condition to moisture.  The only reason why  !
        ! the 13th element is not set to 0 is because history files may be from the middle !
        ! of the month, which could mess things up.                                        !
        !----------------------------------------------------------------------------------!
        cpatch%cb_moistmax(1:12,:) = 1.0
        cpatch%cb_moistmax(  13,:) = cpatch%cb(13,:)
        !----------------------------------------------------------------------------------!
     end if
     !-------------------------------------------------------------------------------------!


     !----- 2-D, dimensioned by the number of mortality rates. ----------------------------!
     dsetrank    = 2
     globdims(1) = int(n_mort,8)
     chnkdims(1) = int(n_mort,8)
     chnkoffs(1) = 0_8
     memdims(1)  = int(n_mort,8)
     memsize(1)  = int(n_mort,8)
     memoffs(2)  = 0_8
     
     globdims(2) = int(ncohorts_global,8)
     chnkdims(2) = int(cpatch%ncohorts,8)
     chnkoffs(2) = int(paco_index - 1,8)
     
     memdims(2)  = int(cpatch%ncohorts,8)
     memsize(2)  = int(cpatch%ncohorts,8)
     memoffs(2)  = 0_8

     call hdf_getslab_r(cpatch%mort_rate,'MORT_RATE_CO ',dsetrank,iparallel,.true.,foundvar)

     if (associated(cpatch%mmean_mort_rate))                                               &
        call hdf_getslab_r(cpatch%mmean_mort_rate,'MMEAN_MORT_RATE_CO '                    &
                          ,dsetrank,iparallel,.false.,foundvar)

     !----- 2-D, dimensioned by the number of diurnal cycle times. ------------------------!
     dsetrank    = 2
     globdims(1) = int(ndcycle,8)
     chnkdims(1) = int(ndcycle,8)
     chnkoffs(1) = 0_8
     memdims(1)  = int(ndcycle,8)
     memsize(1)  = int(ndcycle,8)
     memoffs(2)  = 0_8

     globdims(2) = int(ncohorts_global,8)
     chnkdims(2) = int(cpatch%ncohorts,8)
     chnkoffs(2) = int(paco_index - 1,8)
     memdims(2)  = int(cpatch%ncohorts,8)
     memsize(2)  = int(cpatch%ncohorts,8)
     memoffs(2)  = 0_8

     if (associated(cpatch%qmean_gpp))                                                     &
        call hdf_getslab_r(cpatch%qmean_gpp         ,'QMEAN_GPP_CO '                       &
                          ,dsetrank,iparallel,.true.,foundvar)

     if (associated(cpatch%qmean_leaf_resp))                                               &
        call hdf_getslab_r(cpatch%qmean_leaf_resp   ,'QMEAN_LEAF_RESP_CO '                 &
                          ,dsetrank,iparallel,.true.,foundvar)

     if (associated(cpatch%qmean_root_resp))                                               &
        call hdf_getslab_r(cpatch%qmean_root_resp   ,'QMEAN_ROOT_RESP_CO '                 &
                          ,dsetrank,iparallel,.true.,foundvar)

     if (associated(cpatch%qmean_par_l))                                                   &
        call hdf_getslab_r(cpatch%qmean_par_l       ,'QMEAN_PAR_L '                        &
                          ,dsetrank,iparallel,.true.,foundvar)

     if (associated(cpatch%qmean_par_l_beam))                                              &
        call hdf_getslab_r(cpatch%qmean_par_l_beam  ,'QMEAN_PAR_L_BEAM '                   &
                          ,dsetrank,iparallel,.true.,foundvar)

     if (associated(cpatch%qmean_par_l_diff))                                              &
        call hdf_getslab_r(cpatch%qmean_par_l_diff  ,'QMEAN_PAR_L_DIFF '                   &
                          ,dsetrank,iparallel,.true.,foundvar)

     if (associated(cpatch%qmean_fs_open))                                                 &
        call hdf_getslab_r(cpatch%qmean_fs_open     ,'QMEAN_FS_OPEN_CO '                   &
                          ,dsetrank,iparallel,.true.,foundvar)

     if (associated(cpatch%qmean_fsn))                                                     &
        call hdf_getslab_r(cpatch%qmean_fsn         ,'QMEAN_FSN_CO '                       &
                          ,dsetrank,iparallel,.true.,foundvar)

     if (associated(cpatch%qmean_fsw))                                                     &
        call hdf_getslab_r(cpatch%qmean_fsw         ,'QMEAN_FSW_CO '                       &
                          ,dsetrank,iparallel,.true.,foundvar)

     if (associated(cpatch%qmean_psi_open))                                                &
        call hdf_getslab_r(cpatch%qmean_psi_open    ,'QMEAN_PSI_OPEN_CO '                  &
                          ,dsetrank,iparallel,.true.,foundvar)

     if (associated(cpatch%qmean_psi_closed))                                              &
        call hdf_getslab_r(cpatch%qmean_psi_closed  ,'QMEAN_PSI_CLOSED_CO '                &
                          ,dsetrank,iparallel,.true.,foundvar)

     if (associated(cpatch%qmean_water_supply))                                            &
        call hdf_getslab_r(cpatch%qmean_water_supply,'QMEAN_WATER_SUPPLY_CO '              &
                          ,dsetrank,iparallel,.true.,foundvar)

endif

  return
end subroutine fill_history_patch
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
!      This routine reads in real variables from HDF5 files.                               !
! BUFF      -- Where the dataset should be loaded to                                       !
! VARN      -- Variable name as in the HDF5 file                                           !
! DSETRANK  -- Number of dimensions.                                                       !
! IPARALLEL -- Should fortran read data in parallel?  (0 = no, 1 = yes)                    !
! REQUIRED  -- Is this variable required? (.true. = yes, .false. = no)                     !
!              If a variable is missing and required is set to true, then the model will   !
!              crash.  If it is missing but required is false, then we initialise the      !
!              variable with zeroes and print a big banner to the standard output to       !
!              warn the user.                                                              !
! FOUNDVAR  -- Output flag that tells whether the variable was found or not.               !
!------------------------------------------------------------------------------------------!
subroutine hdf_getslab_r(buff,varn,dsetrank,iparallel,required,foundvar)
   
   use hdf5
   use hdf5_coms, only : file_id      & ! intent(inout)
                       , dset_id      & ! intent(inout)
                       , dspace_id    & ! intent(inout)
                       , plist_id     & ! intent(inout)
                       , filespace    & ! intent(inout)
                       , memspace     & ! intent(inout)
                       , globdims     & ! intent(inout)
                       , chnkdims     & ! intent(inout)
                       , chnkoffs     & ! intent(inout)
                       , cnt          & ! intent(inout)
                       , stride       & ! intent(inout)
                       , memdims      & ! intent(inout)
                       , memoffs      & ! intent(inout)
                       , memsize        ! intent(inout)

   use ed_misc_coms,only: suppress_h5_warnings

   implicit none
   !----- Arguments. ----------------------------------------------------------------------!
   real(kind=4)    , dimension(memsize(1),memsize(2),memsize(3),memsize(4))                &
                                                              , intent(inout) :: buff
   character(len=*)                                           , intent(in)    :: varn
   integer                                                    , intent(in)    :: dsetrank
   integer                                                    , intent(in)    :: iparallel
   logical                                                    , intent(in)    :: required
   logical                                                    , intent(out)   :: foundvar
   !----- Local variables. ----------------------------------------------------------------!
   integer                                                                    :: hdferr
   !---------------------------------------------------------------------------------------!



   !---------------------------------------------------------------------------------------!
   !     Try to open dataset, and save the success/failure flag.                           !
   !---------------------------------------------------------------------------------------!
   call h5dopen_f(file_id,trim(varn), dset_id, hdferr)
   foundvar = hdferr == 0
   !---------------------------------------------------------------------------------------!


   !---------------------------------------------------------------------------------------!
   !    Check whether the dataset was opened before continuing.                            !
   !---------------------------------------------------------------------------------------!
   if ((.not. foundvar) .and. required ) then
      !------------------------------------------------------------------------------------!
      !    Variable is required but wasn't found; stop the run.                            !
      !------------------------------------------------------------------------------------!
      write(unit=*,fmt=*) 'File_ID = ',file_id
      write(unit=*,fmt=*) 'Dset_ID = ',dset_id
      call fatal_error('Could not get the dataset for '//trim(varn)//'!!!' &
           ,'hdf_getslab_r','ed_init_full_history.F90')
      !------------------------------------------------------------------------------------!

   else if ((.not. foundvar) .and. (.not.required) ) then
      !------------------------------------------------------------------------------------!
      !    Variable wasn't found but it wasn't required either; initialise buffer with     !
      ! zeroes, and warn the user that we are doing this.                                  !
      !------------------------------------------------------------------------------------!

      if(.not.suppress_h5_warnings)then
      write(unit=*,fmt=*) 'File_ID = ',file_id
      write(unit=*,fmt=*) 'Dset_ID = ',dset_id
      write (unit=*,fmt='(a)') '----------------------------------------------------------'
      write (unit=*,fmt='(a)') '   WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!  '
      write (unit=*,fmt='(a)') '                                                          '
      write (unit=*,fmt='(a)') ' + Variable '//trim(varn)//' not found in your history.'
      write (unit=*,fmt='(a)') ' + Initializing this variable with zero. '
      write (unit=*,fmt='(a)') ' + his may cause some of your diagnostic output related'
      write (unit=*,fmt='(a)') '   to this variable to be incorrect the current period.'
      write (unit=*,fmt='(a)') ''
      write (unit=*,fmt='(a)') '   This variable has been specified as:'
      write (unit=*,fmt='(a)') '   NOT ABSOUTELY NECESSARY TO RESTART THE PROGNOSTIC STATE'
      write (unit=*,fmt='(a)') '----------------------------------------------------------'
      write (unit=*,fmt='(a)') ''
      end if
      
      buff(:,:,:,:) = 0.
      return
      !------------------------------------------------------------------------------------!

   else
      !------------------------------------------------------------------------------------!
      !     Found the variable, read in the data, checking that every step was success-    !
      ! fully done before moving to the next.                                              !
      !------------------------------------------------------------------------------------!
      call h5dget_space_f(dset_id,filespace,hdferr)
      if (hdferr /= 0) then
         call fatal_error('Could not get the hyperslabs filespace for '//trim(varn)//'!'   &
              ,'hdf_getslab_r','ed_init_full_history.F90')
      end if
      
      call h5sselect_hyperslab_f(filespace,H5S_SELECT_SET_F,chnkoffs, &
           chnkdims,hdferr)
      if (hdferr /= 0) then
         call fatal_error('Couldn''t assign the hyperslab filespace for '//trim(varn)//'!' &
              ,'hdf_getslab_r','ed_init_full_history.F90')
      end if
      
      call h5screate_simple_f(dsetrank,memsize,memspace,hdferr)
      if (hdferr /= 0) then
         write(unit=*,fmt=*) 'Chnkdims = ',chnkdims
         write(unit=*,fmt=*) 'Dsetrank = ',dsetrank
         call fatal_error('Couldn''t create the hyperslab memspace for '//trim(varn)//'!'  &
                         ,'hdf_getslab_r','ed_init_full_history.F90')
      end if
      
      call h5sselect_hyperslab_f(memspace,H5S_SELECT_SET_F,memoffs, &
           memdims,hdferr)
      if (hdferr /= 0) then
         call fatal_error('Couldn''t assign the hyperslab filespace for '//trim(varn)//'!' &
              ,'hdf_getslab_r','ed_init_full_history.F90')
      end if
      
      if (iparallel == 1) then
         
         call h5dread_f(dset_id, H5T_NATIVE_REAL,buff,globdims, hdferr, &
              mem_space_id = memspace, file_space_id = filespace, &
              xfer_prp = plist_id)
         
         if (hdferr /= 0) then
            call fatal_error('Couldn''t read in hyperslab dataset for '//trim(varn)//'!'   &
                 ,'hdf_getslab_r','ed_init_full_history.F90')
         end if

      else

         call h5dread_f(dset_id, H5T_NATIVE_REAL,buff,globdims, hdferr, &
              mem_space_id = memspace, file_space_id = filespace )

         if (hdferr /= 0) then
            call fatal_error('Couldn''t read in hyperslab dataset for '//trim(varn)//'!'   &
                 ,'hdf_getslab_r','ed_init_full_history.F90')
         end if

      end if
      
      !  write(unit=*,fmt='(a)') 'History start: Loading '//trim(varn)//'...'
      
      call h5sclose_f(filespace, hdferr)
      call h5sclose_f(memspace , hdferr)
      call h5dclose_f(dset_id  , hdferr)
      
   end if
   return
end subroutine hdf_getslab_r
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
!      This routine reads in double precision variables from HDF5 files.                   !
! BUFF      -- Where the dataset should be loaded to                                       !
! VARN      -- Variable name as in the HDF5 file                                           !
! DSETRANK  -- Number of dimensions.                                                       !
! IPARALLEL -- Should fortran read data in parallel?  (0 = no, 1 = yes)                    !
! REQUIRED  -- Is this variable required? (.true. = yes, .false. = no)                     !
!              If a variable is missing and required is set to true, then the model will   !
!              crash.  If it is missing but required is false, then we initialise the      !
!              variable with zeroes and print a big banner to the standard output to       !
!              warn the user.                                                              !
! FOUNDVAR  -- Output flag that tells whether the variable was found or not.               !
!------------------------------------------------------------------------------------------!
subroutine hdf_getslab_d(buff,varn,dsetrank,iparallel,required,foundvar)
   
   use hdf5
   use hdf5_coms, only : file_id      & ! intent(inout)
                       , dset_id      & ! intent(inout)
                       , dspace_id    & ! intent(inout)
                       , plist_id     & ! intent(inout)
                       , filespace    & ! intent(inout)
                       , memspace     & ! intent(inout)
                       , globdims     & ! intent(inout)
                       , chnkdims     & ! intent(inout)
                       , chnkoffs     & ! intent(inout)
                       , cnt          & ! intent(inout)
                       , stride       & ! intent(inout)
                       , memdims      & ! intent(inout)
                       , memoffs      & ! intent(inout)
                       , memsize      ! ! intent(inout)

   use ed_misc_coms,only: suppress_h5_warnings
   
   implicit none
   !----- Arguments. ----------------------------------------------------------------------!
   real(kind=8)    , dimension(memsize(1),memsize(2),memsize(3),memsize(4))                &
                                                              , intent(inout) :: buff
   character(len=*)                                           , intent(in)    :: varn
   integer                                                    , intent(in)    :: dsetrank
   integer                                                    , intent(in)    :: iparallel
   logical                                                    , intent(in)    :: required
   logical                                                    , intent(out)   :: foundvar
   !----- Local variables. ----------------------------------------------------------------!
   integer                                                                    :: hdferr
   !---------------------------------------------------------------------------------------!



   !---------------------------------------------------------------------------------------!
   !     Try to open dataset, and save the success/failure flag.                           !
   !---------------------------------------------------------------------------------------!
   call h5dopen_f(file_id,trim(varn), dset_id, hdferr)
   foundvar = hdferr == 0
   !---------------------------------------------------------------------------------------!


   !---------------------------------------------------------------------------------------!
   !    Check whether the dataset was opened before continuing.                            !
   !---------------------------------------------------------------------------------------!
   if ((.not. foundvar) .and. required ) then
      !------------------------------------------------------------------------------------!
      !    Variable is required but wasn't found; stop the run.                            !
      !------------------------------------------------------------------------------------!
      write(unit=*,fmt=*) 'File_ID = ',file_id
      write(unit=*,fmt=*) 'Dset_ID = ',dset_id
      call fatal_error('Could not get the dataset for '//trim(varn)//'!!!' &
           ,'hdf_getslab_d','ed_init_full_history.F90')
      !------------------------------------------------------------------------------------!

   else if ((.not. foundvar) .and. (.not.required) ) then
      !------------------------------------------------------------------------------------!
      !    Variable wasn't found but it wasn't required either; initialise buffer with     !
      ! zeroes, and warn the user that we are doing this.                                  !
      !------------------------------------------------------------------------------------!
      if(.not.suppress_h5_warnings)then
      write(unit=*,fmt=*) 'File_ID = ',file_id
      write(unit=*,fmt=*) 'Dset_ID = ',dset_id
      write (unit=*,fmt='(a)') '----------------------------------------------------------'
      write (unit=*,fmt='(a)') '   WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!  '
      write (unit=*,fmt='(a)') '                                                          '
      write (unit=*,fmt='(a)') ' + Variable '//trim(varn)//' not found in your history.'
      write (unit=*,fmt='(a)') ' + Initializing this variable with zero. '
      write (unit=*,fmt='(a)') ' + his may cause some of your diagnostic output related'
      write (unit=*,fmt='(a)') '   to this variable to be incorrect the current period.'
      write (unit=*,fmt='(a)') ''
      write (unit=*,fmt='(a)') '   This variable has been specified as:'
      write (unit=*,fmt='(a)') '   NOT ABSOUTELY NECESSARY TO RESTART THE PROGNOSTIC STATE'
      write (unit=*,fmt='(a)') '----------------------------------------------------------'
      write (unit=*,fmt='(a)') ''
      end if
      buff(:,:,:,:) = 0.d0
      return
      !------------------------------------------------------------------------------------!

   else
      !------------------------------------------------------------------------------------!
      !     Found the variable, read in the data, checking that every step was success-    !
      ! fully done before moving to the next.                                              !
      !------------------------------------------------------------------------------------!
      call h5dget_space_f(dset_id,filespace,hdferr)
      if (hdferr /= 0) then
         call fatal_error('Could not get the hyperslabs filespace for '//trim(varn)//'!'   &
              ,'hdf_getslab_d','ed_init_full_history.F90')
      end if
      
      call h5sselect_hyperslab_f(filespace,H5S_SELECT_SET_F,chnkoffs, &
           chnkdims,hdferr)
      if (hdferr /= 0) then
         call fatal_error('Couldn''t assign the hyperslab filespace for '//trim(varn)//'!' &
              ,'hdf_getslab_d','ed_init_full_history.F90')
      end if
      
      call h5screate_simple_f(dsetrank,memsize,memspace,hdferr)
      if (hdferr /= 0) then
         write(unit=*,fmt=*) 'Chnkdims = ',chnkdims
         write(unit=*,fmt=*) 'Dsetrank = ',dsetrank
         call fatal_error('Couldn''t create the hyperslab memspace for '//trim(varn)//'!'  &
                         ,'hdf_getslab_d','ed_init_full_history.F90')
      end if
      
      call h5sselect_hyperslab_f(memspace,H5S_SELECT_SET_F,memoffs, &
           memdims,hdferr)
      if (hdferr /= 0) then
         call fatal_error('Couldn''t assign the hyperslab filespace for '//trim(varn)//'!' &
              ,'hdf_getslab_d','ed_init_full_history.F90')
      end if
      
      if (iparallel == 1) then
         
         call h5dread_f(dset_id, H5T_NATIVE_DOUBLE,buff,globdims, hdferr, &
              mem_space_id = memspace, file_space_id = filespace, &
              xfer_prp = plist_id)
         
         if (hdferr /= 0) then
            call fatal_error('Couldn''t read in hyperslab dataset for '//trim(varn)//'!'   &
                 ,'hdf_getslab_d','ed_init_full_history.F90')
         end if

      else

         call h5dread_f(dset_id, H5T_NATIVE_DOUBLE,buff,globdims, hdferr, &
              mem_space_id = memspace, file_space_id = filespace )

         if (hdferr /= 0) then
            call fatal_error('Couldn''t read in hyperslab dataset for '//trim(varn)//'!'   &
                 ,'hdf_getslab_d','ed_init_full_history.F90')
         end if

      end if
      
      !  write(unit=*,fmt='(a)') 'History start: Loading '//trim(varn)//'...'
      
      call h5sclose_f(filespace, hdferr)
      call h5sclose_f(memspace , hdferr)
      call h5dclose_f(dset_id  , hdferr)
      
   end if
   return
end subroutine hdf_getslab_d
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
!      This routine reads in integer variables from HDF5 files.                            !
! BUFF      -- Where the dataset should be loaded to                                       !
! VARN      -- Variable name as in the HDF5 file                                           !
! DSETRANK  -- Number of dimensions.                                                       !
! IPARALLEL -- Should fortran read data in parallel?  (0 = no, 1 = yes)                    !
! REQUIRED  -- Is this variable required? (.true. = yes, .false. = no)                     !
!              If a variable is missing and required is set to true, then the model will   !
!              crash.  If it is missing but required is false, then we initialise the      !
!              variable with zeroes and print a big banner to the standard output to       !
!              warn the user.                                                              !
! FOUNDVAR  -- Output flag that tells whether the variable was found or not.               !
!------------------------------------------------------------------------------------------!
subroutine hdf_getslab_i(buff,varn,dsetrank,iparallel,required,foundvar)
   
   use hdf5
   use hdf5_coms, only : file_id      & ! intent(inout)
                       , dset_id      & ! intent(inout)
                       , dspace_id    & ! intent(inout)
                       , plist_id     & ! intent(inout)
                       , filespace    & ! intent(inout)
                       , memspace     & ! intent(inout)
                       , globdims     & ! intent(inout)
                       , chnkdims     & ! intent(inout)
                       , chnkoffs     & ! intent(inout)
                       , cnt          & ! intent(inout)
                       , stride       & ! intent(inout)
                       , memdims      & ! intent(inout)
                       , memoffs      & ! intent(inout)
                       , memsize      ! ! intent(inout)

   use ed_misc_coms,only: suppress_h5_warnings
   implicit none
   !----- Arguments. ----------------------------------------------------------------------!
   integer         , dimension(memsize(1),memsize(2),memsize(3),memsize(4))                &
                                                              , intent(inout) :: buff
   character(len=*)                                           , intent(in)    :: varn
   integer                                                    , intent(in)    :: dsetrank
   integer                                                    , intent(in)    :: iparallel
   logical                                                    , intent(in)    :: required
   logical                                                    , intent(out)   :: foundvar
   !----- Local variables. ----------------------------------------------------------------!
   integer                                                                    :: hdferr
   !---------------------------------------------------------------------------------------!



   !---------------------------------------------------------------------------------------!
   !     Try to open dataset, and save the success/failure flag.                           !
   !---------------------------------------------------------------------------------------!
   call h5dopen_f(file_id,trim(varn), dset_id, hdferr)
   foundvar = hdferr == 0
   !---------------------------------------------------------------------------------------!


   !---------------------------------------------------------------------------------------!
   !    Check whether the dataset was opened before continuing.                            !
   !---------------------------------------------------------------------------------------!
   if ((.not. foundvar) .and. required ) then
      !------------------------------------------------------------------------------------!
      !    Variable is required but wasn't found; stop the run.                            !
      !------------------------------------------------------------------------------------!
      write(unit=*,fmt=*) 'File_ID = ',file_id
      write(unit=*,fmt=*) 'Dset_ID = ',dset_id
      call fatal_error('Could not get the dataset for '//trim(varn)//'!!!' &
           ,'hdf_getslab_i','ed_init_full_history.F90')
      !------------------------------------------------------------------------------------!

   else if ((.not. foundvar) .and. (.not.required) ) then
      !------------------------------------------------------------------------------------!
      !    Variable wasn't found but it wasn't required either; initialise buffer with     !
      ! zeroes, and warn the user that we are doing this.                                  !
      !------------------------------------------------------------------------------------!
      if(.not.suppress_h5_warnings)then
      write(unit=*,fmt=*) 'File_ID = ',file_id
      write(unit=*,fmt=*) 'Dset_ID = ',dset_id
      write (unit=*,fmt='(a)') '----------------------------------------------------------'
      write (unit=*,fmt='(a)') '   WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!  '
      write (unit=*,fmt='(a)') '                                                          '
      write (unit=*,fmt='(a)') ' + Variable '//trim(varn)//' not found in your history.'
      write (unit=*,fmt='(a)') ' + Initializing this variable with zero. '
      write (unit=*,fmt='(a)') ' + his may cause some of your diagnostic output related'
      write (unit=*,fmt='(a)') '   to this variable to be incorrect the current period.'
      write (unit=*,fmt='(a)') ''
      write (unit=*,fmt='(a)') '   This variable has been specified as:'
      write (unit=*,fmt='(a)') '   NOT ABSOUTELY NECESSARY TO RESTART THE PROGNOSTIC STATE'
      write (unit=*,fmt='(a)') '----------------------------------------------------------'
      write (unit=*,fmt='(a)') ''
      end if
      buff(:,:,:,:) = 0
      return
      !------------------------------------------------------------------------------------!

   else
      !------------------------------------------------------------------------------------!
      !     Found the variable, read in the data, checking that every step was success-    !
      ! fully done before moving to the next.                                              !
      !------------------------------------------------------------------------------------!
      call h5dget_space_f(dset_id,filespace,hdferr)
      if (hdferr /= 0) then
         call fatal_error('Could not get the hyperslabs filespace for '//trim(varn)//'!'   &
              ,'hdf_getslab_i','ed_init_full_history.F90')
      end if
      
      call h5sselect_hyperslab_f(filespace,H5S_SELECT_SET_F,chnkoffs, &
           chnkdims,hdferr)
      if (hdferr /= 0) then
         call fatal_error('Couldn''t assign the hyperslab filespace for '//trim(varn)//'!' &
              ,'hdf_getslab_i','ed_init_full_history.F90')
      end if
      
      call h5screate_simple_f(dsetrank,memsize,memspace,hdferr)
      if (hdferr /= 0) then
         write(unit=*,fmt=*) 'Chnkdims = ',chnkdims
         write(unit=*,fmt=*) 'Dsetrank = ',dsetrank
         call fatal_error('Couldn''t create the hyperslab memspace for '//trim(varn)//'!'  &
                         ,'hdf_getslab_i','ed_init_full_history.F90')
      end if
      
      call h5sselect_hyperslab_f(memspace,H5S_SELECT_SET_F,memoffs, &
           memdims,hdferr)
      if (hdferr /= 0) then
         call fatal_error('Couldn''t assign the hyperslab filespace for '//trim(varn)//'!' &
              ,'hdf_getslab_i','ed_init_full_history.F90')
      end if
      
      if (iparallel == 1) then
         
         call h5dread_f(dset_id, H5T_NATIVE_INTEGER,buff,globdims, hdferr, &
              mem_space_id = memspace, file_space_id = filespace, &
              xfer_prp = plist_id)
         
         if (hdferr /= 0) then
            call fatal_error('Couldn''t read in hyperslab dataset for '//trim(varn)//'!'   &
                 ,'hdf_getslab_i','ed_init_full_history.F90')
         end if

      else

         call h5dread_f(dset_id, H5T_NATIVE_INTEGER,buff,globdims, hdferr, &
              mem_space_id = memspace, file_space_id = filespace )

         if (hdferr /= 0) then
            call fatal_error('Couldn''t read in hyperslab dataset for '//trim(varn)//'!'   &
                 ,'hdf_getslab_i','ed_init_full_history.F90')
         end if

      end if
      
      !  write(unit=*,fmt='(a)') 'History start: Loading '//trim(varn)//'...'
      
      call h5sclose_f(filespace, hdferr)
      call h5sclose_f(memspace , hdferr)
      call h5dclose_f(dset_id  , hdferr)
      
   end if
   return
end subroutine hdf_getslab_i
!==========================================================================================!
!==========================================================================================!
