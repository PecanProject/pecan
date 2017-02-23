! DART software - Copyright 2004 - 2011 UCAR. This open source software is
! provided by UCAR, "as is", without charge, subject to all terms of use at
! http://www.image.ucar.edu/DAReS/DART/DART_download

module model_mod

! <next few lines under version control, do not edit>
! $URL: https://proxy.subversion.ucar.edu/DAReS/DART/releases/Kodiak/models/template/model_mod.f90 $
! $Id: model_mod.f90 4933 2011-06-01 17:55:44Z thoar $
! $Revision: 4933 $
! $Date: 2011-06-01 13:55:44 -0400 (Wed, 01 Jun 2011) $

! This is a template showing the interfaces required for a model to be compliant
! with the DART data assimilation infrastructure. The public interfaces listed
! must all be supported with the argument lists as indicated. Many of the interfaces
! are not required for minimal implementation (see the discussion of each
! interface and look for NULL INTERFACE). 

! Modules that are absolutely required for use are listed
use        types_mod, only : r8, MISSING_R8
use time_manager_mod, only : time_type, set_time
use     location_mod, only : location_type,      get_close_maxdist_init, &
                             get_close_obs_init, get_close_obs, set_location, &
                             set_location_missing
use    utilities_mod, only : register_module, error_handler, nc_check, &
                             E_ERR, E_MSG
                             ! nmlfileunit, do_output, do_nml_file, do_nml_term,  &
                             ! find_namelist_in_file, check_namelist_read
use   random_nr_mod,  only : random_seq_type
use   random_seq_mod, only : init_random_seq, random_uniform
use   obs_kind_mod,   only : KIND_DBH, KIND_STEM_DENSITY

implicit none
private

public :: get_model_size,         &
          adv_1step,              &
          get_state_meta_data,    &
          model_interpolate,      &
          get_model_time_step,    &
          end_model,              &
          static_init_model,      &
          init_time,              &
          init_conditions,        &
          nc_write_model_atts,    &
          nc_write_model_vars,    &
          pert_model_state,       &
          get_close_maxdist_init, &
          get_close_obs_init,     &
          get_close_obs,          &
          ens_mean_for_model


! version controlled file description for error handling, do not edit
character(len=128), parameter :: &
   source   = "$URL: https://proxy.subversion.ucar.edu/DAReS/DART/releases/Kodiak/models/template/model_mod.f90 $", &
   revision = "$Revision: 4933 $", &
   revdate  = "$Date: 2011-06-01 13:55:44 -0400 (Wed, 01 Jun 2011) $"

! EXAMPLE: define model parameters here
integer, parameter               :: model_size = 130
type(time_type)                  :: time_step
type(location_type), allocatable :: state_loc(:)

! EXAMPLE: perhaps a namelist here 
logical  :: output_state_vector = .true.
integer  :: time_step_days      = 0
integer  :: time_step_seconds   = 5
namelist /model_nml/ output_state_vector, time_step_days, time_step_seconds

contains

!==================================================================



subroutine static_init_model()
!------------------------------------------------------------------
!
! Called to do one time initialization of the model. As examples,
! might define information about the model size or model timestep.
! In models that require pre-computed static data, for instance
! spherical harmonic weights, these would also be computed here.
! Can be a NULL INTERFACE for the simplest models.

 real(r8) :: x_loc
 integer  :: i
!integer  :: iunit, io

! Print module information to log file and stdout.
call register_module(source, revision, revdate)

! This is where you would read a namelist, for example.
!call find_namelist_in_file("input.nml", "model_nml", iunit)
!read(iunit, nml = model_nml, iostat = io)
!call check_namelist_read(iunit, io, "model_nml")


! Record the namelist values used for the run ...
!if (do_nml_file()) write(nmlfileunit, nml=model_nml)
!if (do_nml_term()) write(     *     , nml=model_nml)

! Create storage for locations
allocate(state_loc(model_size))

! Define the locations of the model state variables
! naturally, this can be done VERY differently for more complicated models.
! set_location() is different for 1D vs. 3D models, not surprisingly.
do i = 1, model_size/2
!   x_loc = (i - 1.0_r8) / model_size
   x_loc = i/(model_size/2)
   ! must do one of these:
   state_loc(i) =  set_location(x_loc)
   state_loc(model_size/2 + i) =  set_location(x_loc)
   !state_loc(i) =  set_location(x_loc,y_loc,v_loc,v_type)
end do

! The time_step in terms of a time type must also be initialized.
time_step = set_time(time_step_seconds, time_step_days)

end subroutine static_init_model




subroutine init_conditions(x)
!------------------------------------------------------------------
! subroutine init_conditions(x)
!
! Returns a model state vector, x, that is some sort of appropriate
! initial condition for starting up a long integration of the model.
! At present, this is only used if the namelist parameter 
! start_from_restart is set to .false. in the program perfect_model_obs.
! If this option is not to be used in perfect_model_obs, or if no 
! synthetic data experiments using perfect_model_obs are planned, 
! this can be a NULL INTERFACE.

real(r8), intent(out) :: x(:)

x = 1.0_R8

end subroutine init_conditions



subroutine adv_1step(x, time)
!------------------------------------------------------------------
! subroutine adv_1step(x, time)
!
! Does a single timestep advance of the model. The input value of
! the vector x is the starting condition and x is updated to reflect
! the changed state after a timestep. The time argument is intent
! in and is used for models that need to know the date/time to 
! compute a timestep, for instance for radiation computations.
! This interface is only called if the namelist parameter
! async is set to 0 in perfect_model_obs of filter or if the 
! program integrate_model is to be used to advance the model
! state as a separate executable. If one of these options
! is not going to be used (the model will only be advanced as
! a separate model-specific executable), this can be a 
! NULL INTERFACE.

real(r8),        intent(inout) :: x(:)
type(time_type), intent(in)    :: time

call random_perturbation(x)

end subroutine adv_1step


!----------------------------------------------------

subroutine random_perturbation(x)

real(r8), intent(inout) :: x(:)

type(random_seq_type) :: s
real(r8) :: p, r

call init_random_seq(s)
p = random_uniform(s)

r = 1 + (p - 0.5)

x = r*x

end subroutine random_perturbation

function get_model_size()
!------------------------------------------------------------------
!
! Returns the size of the model as an integer. Required for all
! applications.

integer :: get_model_size

get_model_size = model_size

end function get_model_size



subroutine init_time(time)
!------------------------------------------------------------------
!
! Companion interface to init_conditions. Returns a time that is somehow 
! appropriate for starting up a long integration of the model.
! At present, this is only used if the namelist parameter 
! start_from_restart is set to .false. in the program perfect_model_obs.
! If this option is not to be used in perfect_model_obs, or if no 
! synthetic data experiments using perfect_model_obs are planned, 
! this can be a NULL INTERFACE.

type(time_type), intent(out) :: time

! for now, just set to 0
time = set_time(0,0)

end subroutine init_time



subroutine model_interpolate(x, location, itype, obs_val, istatus)
!------------------------------------------------------------------
!
! Given a state vector, a location, and a model state variable type,
! interpolates the state variable field to that location and returns
! the value in obs_val. The istatus variable should be returned as
! 0 unless there is some problem in computing the interpolation in
! which case an alternate value should be returned. The itype variable
! is a model specific integer that specifies the type of field (for
! instance temperature, zonal wind component, etc.). In low order
! models that have no notion of types of variables, this argument can
! be ignored. For applications in which only perfect model experiments
! with identity observations (i.e. only the value of a particular
! state variable is observed), this can be a NULL INTERFACE.

real(r8),            intent(in) :: x(:)
type(location_type), intent(in) :: location
integer,             intent(in) :: itype
real(r8),           intent(out) :: obs_val
integer,            intent(out) :: istatus

! This should be the result of the interpolation of a
! given kind (itype) of variable at the given location.
obs_val = x(1)

! The return code for successful return should be 0. 
! Any positive number is an error.
! Negative values are reserved for use by the DART framework.
! Using distinct positive values for different types of errors can be
! useful in diagnosing problems.
istatus = 0

end subroutine model_interpolate



function get_model_time_step()
!------------------------------------------------------------------
!
! Returns the the time step of the model; the smallest increment
! in time that the model is capable of advancing the state in a given
! implementation. This interface is required for all applications.

type(time_type) :: get_model_time_step

get_model_time_step = time_step

end function get_model_time_step



subroutine get_state_meta_data(index_in, location, var_type)
!------------------------------------------------------------------
!
! Given an integer index into the state vector structure, returns the
! associated location. A second intent(out) optional argument kind
! can be returned if the model has more than one type of field (for
! instance temperature and zonal wind component). This interface is
! required for all filter applications as it is required for computing
! the distance between observations and state variables.

integer,             intent(in)            :: index_in
type(location_type), intent(out)           :: location
integer,             intent(out), optional :: var_type

! these should be set to the actual location and obs kind
location = state_loc(index_in)
! if (present(var_type)) var_type = 1  
if(index_in .lt. 384) var_type = KIND_DBH
if(index_in .gt. 384) var_type = KIND_STEM_DENSITY

end subroutine get_state_meta_data



subroutine end_model()
!------------------------------------------------------------------
!
! Does any shutdown and clean-up needed for model. Can be a NULL
! INTERFACE if the model has no need to clean up storage, etc.

! good style ... perhaps you could deallocate stuff (from static_init_model?).
! deallocate(state_loc)

end subroutine end_model



function nc_write_model_atts( ncFileID ) result (ierr)
!------------------------------------------------------------------
! TJH 24 Oct 2006 -- Writes the model-specific attributes to a netCDF file.
!     This includes coordinate variables and some metadata, but NOT
!     the model state vector. We do have to allocate SPACE for the model
!     state vector, but that variable gets filled as the model advances.
!
! As it stands, this routine will work for ANY model, with no modification.
!
! The simplest possible netCDF file would contain a 3D field
! containing the state of 'all' the ensemble members. This requires
! three coordinate variables -- one for each of the dimensions 
! [model_size, ensemble_member, time]. A little metadata is useful, 
! so we can also create some 'global' attributes. 
! This is what is implemented here.
!
! Once the simplest case is working, this routine (and nc_write_model_vars)
! can be extended to create a more logical partitioning of the state vector,
! fundamentally creating a netCDF file with variables that are easily 
! plotted. The bgrid model_mod is perhaps a good one to view, keeping
! in mind it is complicated by the fact it has two coordinate systems. 
! There are stubs in this template, but they are only stubs.
!
! TJH 29 Jul 2003 -- for the moment, all errors are fatal, so the
! return code is always '0 == normal', since the fatal errors stop execution.
!
! assim_model_mod:init_diag_output uses information from the location_mod
!     to define the location dimension and variable ID. All we need to do
!     is query, verify, and fill ...
!
! Typical sequence for adding new dimensions,variables,attributes:
! NF90_OPEN             ! open existing netCDF dataset
!    NF90_redef         ! put into define mode 
!    NF90_def_dim       ! define additional dimensions (if any)
!    NF90_def_var       ! define variables: from name, type, and dims
!    NF90_put_att       ! assign attribute values
! NF90_ENDDEF           ! end definitions: leave define mode
!    NF90_put_var       ! provide values for variable
! NF90_CLOSE            ! close: save updated netCDF dataset

use typeSizes
use netcdf

integer, intent(in)  :: ncFileID      ! netCDF file identifier
integer              :: ierr          ! return value of function

integer :: nDimensions, nVariables, nAttributes, unlimitedDimID

integer :: StateVarDimID   ! netCDF pointer to state variable dimension (model size)
integer :: MemberDimID     ! netCDF pointer to dimension of ensemble    (ens_size)
integer :: TimeDimID       ! netCDF pointer to time dimension           (unlimited)

integer :: StateVarVarID   ! netCDF pointer to state variable coordinate array
integer :: StateVarID      ! netCDF pointer to 3D [state,copy,time] array

character(len=129)    :: errstring

! we are going to need these to record the creation date in the netCDF file.
! This is entirely optional, but nice.

character(len=8)      :: crdate      ! needed by F90 DATE_AND_TIME intrinsic
character(len=10)     :: crtime      ! needed by F90 DATE_AND_TIME intrinsic
character(len=5)      :: crzone      ! needed by F90 DATE_AND_TIME intrinsic
integer, dimension(8) :: values      ! needed by F90 DATE_AND_TIME intrinsic
character(len=NF90_MAX_NAME) :: str1

integer :: i

!-------------------------------------------------------------------------------
! make sure ncFileID refers to an open netCDF file, 
! and then put into define mode.
!-------------------------------------------------------------------------------

ierr = -1 ! assume things go poorly

call nc_check(nf90_inquire(ncFileID,nDimensions,nVariables,nAttributes,unlimitedDimID), &
                     "nc_write_model_atts", "inquire")
call nc_check(nf90_redef(ncFileID), "nc_write_model_atts", "redef")

!-------------------------------------------------------------------------------
! We need the dimension ID for the number of copies/ensemble members, and
! we might as well check to make sure that Time is the Unlimited dimension. 
! Our job is create the 'model size' dimension.
!-------------------------------------------------------------------------------

call nc_check(nf90_inq_dimid(ncid=ncFileID, name="copy", dimid=MemberDimID), &
                            "nc_write_model_atts", "inq_dimid copy")
call nc_check(nf90_inq_dimid(ncid=ncFileID, name="time", dimid= TimeDimID), &
                            "nc_write_model_atts", "inq_dimid time")

if ( TimeDimID /= unlimitedDimId ) then
   write(errstring,*)"Time Dimension ID ",TimeDimID, &
                     " should equal Unlimited Dimension ID",unlimitedDimID
   call error_handler(E_ERR,"nc_write_model_atts", errstring, source, revision, revdate)
endif

!-------------------------------------------------------------------------------
! Define the model size / state variable dimension / whatever ...
!-------------------------------------------------------------------------------
call nc_check(nf90_def_dim(ncid=ncFileID, name="StateVariable",  &
                           len=model_size, dimid=StateVarDimID), &
                           "nc_write_model_atts", "def_dim state")

!-------------------------------------------------------------------------------
! Write Global Attributes 
!-------------------------------------------------------------------------------

call DATE_AND_TIME(crdate,crtime,crzone,values)
write(str1,'(''YYYY MM DD HH MM SS = '',i4,5(1x,i2.2))') &
                  values(1), values(2), values(3), values(5), values(6), values(7)

call nc_check(nf90_put_att(ncFileID, NF90_GLOBAL, "creation_date" ,str1), &
                          "nc_write_model_atts", "put_att creation_date")
call nc_check(nf90_put_att(ncFileID, NF90_GLOBAL, "model_source"  ,source), &
                          "nc_write_model_atts", "put_att model_source")
call nc_check(nf90_put_att(ncFileID, NF90_GLOBAL, "model_revision",revision), &
                          "nc_write_model_atts", "put_att model_revision")
call nc_check(nf90_put_att(ncFileID, NF90_GLOBAL, "model_revdate" ,revdate), &
                          "nc_write_model_atts", "put_att model_revdate")
call nc_check(nf90_put_att(ncFileID, NF90_GLOBAL, "model","template"), &
                          "nc_write_model_atts", "put_att model")

!-------------------------------------------------------------------------------
! Here is the extensible part. The simplest scenario is to output the state vector,
! parsing the state vector into model-specific parts is complicated, and you need
! to know the geometry, the output variables (PS,U,V,T,Q,...) etc. We're skipping
! complicated part.
!-------------------------------------------------------------------------------

if ( output_state_vector ) then

   !----------------------------------------------------------------------------
   ! Create a variable for the state vector
   !----------------------------------------------------------------------------

  ! Define the state vector coordinate variable and some attributes.
   call nc_check(nf90_def_var(ncid=ncFileID,name="StateVariable", xtype=NF90_INT, &
                              dimids=StateVarDimID, varid=StateVarVarID), &
                             "nc_write_model_atts", "def_var StateVariable")
   call nc_check(nf90_put_att(ncFileID, StateVarVarID,"long_name","State Variable ID"), &
                             "nc_write_model_atts", "put_att StateVariable long_name")
   call nc_check(nf90_put_att(ncFileID, StateVarVarID, "units",     "indexical"), &
                             "nc_write_model_atts", "put_att StateVariable units")
   call nc_check(nf90_put_att(ncFileID, StateVarVarID, "valid_range", (/ 1, model_size /)), &
                             "nc_write_model_atts", "put_att StateVariable valid_range")

   ! Define the actual (3D) state vector, which gets filled as time goes on ... 
   call nc_check(nf90_def_var(ncid=ncFileID, name="state", xtype=NF90_REAL, &
                 dimids = (/ StateVarDimID, MemberDimID, unlimitedDimID /), &
                 varid=StateVarID), "nc_write_model_atts", "def_var state")
   call nc_check(nf90_put_att(ncFileID, StateVarID, "long_name", "model state or fcopy"), &
                             "nc_write_model_atts", "put_att state long_name")

   ! Leave define mode so we can fill the coordinate variable.
   call nc_check(nf90_enddef(ncfileID),"nc_write_model_atts", "state_vector enddef")

   ! Fill the state variable coordinate variable
   call nc_check(nf90_put_var(ncFileID, StateVarVarID, (/ (i,i=1,model_size) /)), &
                                    "nc_write_model_atts", "put_var state")

else

   !----------------------------------------------------------------------------
   ! We need to process the prognostic variables.
   !----------------------------------------------------------------------------

   ! This block is a stub for something more complicated.
   ! Usually, the control for the execution of this block is a namelist variable.
   ! Take a peek at the bgrid model_mod.f90 for a (rather complicated) example.

   call nc_check(nf90_enddef(ncfileID), "nc_write_model_atts", "prognostic enddef")

endif

!-------------------------------------------------------------------------------
! Flush the buffer and leave netCDF file open
!-------------------------------------------------------------------------------
call nc_check(nf90_sync(ncFileID),"nc_write_model_atts", "sync")

ierr = 0 ! If we got here, things went well.

end function nc_write_model_atts



function nc_write_model_vars( ncFileID, statevec, copyindex, timeindex ) result (ierr)         
!------------------------------------------------------------------
! TJH 24 Oct 2006 -- Writes the model variables to a netCDF file.
!
! TJH 29 Jul 2003 -- for the moment, all errors are fatal, so the
! return code is always '0 == normal', since the fatal errors stop execution.
!
! For the lorenz_96 model, each state variable is at a separate location.
! that's all the model-specific attributes I can think of ...
!
! assim_model_mod:init_diag_output uses information from the location_mod
!     to define the location dimension and variable ID. All we need to do
!     is query, verify, and fill ...
!
! Typical sequence for adding new dimensions,variables,attributes:
! NF90_OPEN             ! open existing netCDF dataset
!    NF90_redef         ! put into define mode
!    NF90_def_dim       ! define additional dimensions (if any)
!    NF90_def_var       ! define variables: from name, type, and dims
!    NF90_put_att       ! assign attribute values
! NF90_ENDDEF           ! end definitions: leave define mode
!    NF90_put_var       ! provide values for variable
! NF90_CLOSE            ! close: save updated netCDF dataset

use typeSizes
use netcdf

integer,                intent(in) :: ncFileID      ! netCDF file identifier
real(r8), dimension(:), intent(in) :: statevec
integer,                intent(in) :: copyindex
integer,                intent(in) :: timeindex
integer                            :: ierr          ! return value of function

integer :: nDimensions, nVariables, nAttributes, unlimitedDimID

integer :: StateVarID

!-------------------------------------------------------------------------------
! make sure ncFileID refers to an open netCDF file, 
!-------------------------------------------------------------------------------

ierr = -1 ! assume things go poorly

call nc_check(nf90_inquire(ncFileID,nDimensions,nVariables,nAttributes,unlimitedDimID), &
                          "nc_write_model_vars", "inquire")

if ( output_state_vector ) then

   call nc_check(nf90_inq_varid(ncFileID, "state", StateVarID), &
                               "nc_write_model_vars", "inq_varid state" )
   call nc_check(nf90_put_var(ncFileID, StateVarID, statevec,  &
                              start=(/ 1, copyindex, timeindex /)), &
                             "nc_write_model_vars", "put_var state")                   

else

   !----------------------------------------------------------------------------
   ! We need to process the prognostic variables.
   !----------------------------------------------------------------------------

   ! This block is a stub for something more complicated.
   ! Usually, the control for the execution of this block is a namelist variable.
   ! Take a peek at the bgrid model_mod.f90 for a (rather complicated) example.
   !
   ! Generally, it is necessary to take the statevec and decompose it into 
   ! the separate prognostic variables. In this (commented out) example,
   ! global_Var is a user-defined type that has components like:
   ! global_Var%ps, global_Var%t, ... etc. Each of those can then be passed
   ! directly to the netcdf put_var routine. This may cause a huge storage
   ! hit, so large models may want to avoid the duplication if possible.

   ! call vector_to_prog_var(statevec, get_model_size(), global_Var)

   ! the 'start' array is crucial. In the following example, 'ps' is a 2D
   ! array, and the netCDF variable "ps" is a 4D array [lat,lon,copy,time]

   ! call nc_check(nf90_inq_varid(ncFileID, "ps", psVarID), &
   !                             "nc_write_model_vars",  "inq_varid ps")
   ! call nc_check(nf90_put_var( ncFileID, psVarID, global_Var%ps, &
   !                             start=(/ 1, 1, copyindex, timeindex /)), &
   !                            "nc_write_model_vars", "put_var ps")

endif

!-------------------------------------------------------------------------------
! Flush the buffer and leave netCDF file open
!-------------------------------------------------------------------------------

call nc_check(nf90_sync(ncFileID), "nc_write_model_vars", "sync")

ierr = 0 ! If we got here, things went well.

end function nc_write_model_vars



subroutine pert_model_state(state, pert_state, interf_provided)
!------------------------------------------------------------------
!
! Perturbs a model state for generating initial ensembles.
! The perturbed state is returned in pert_state.
! A model may choose to provide a NULL INTERFACE by returning
! .false. for the interf_provided argument. This indicates to
! the filter that if it needs to generate perturbed states, it
! may do so by adding an O(0.1) magnitude perturbation to each
! model state variable independently. The interf_provided argument
! should be returned as .true. if the model wants to do its own
! perturbing of states.  The returned pert_state should in any
! case be valid, since it will be read by filter even if 
! interf_provided is .false.

real(r8), intent(in)  :: state(:)
real(r8), intent(out) :: pert_state(:)
logical,  intent(out) :: interf_provided

pert_state      = state
interf_provided = .false.

end subroutine pert_model_state




subroutine ens_mean_for_model(ens_mean)
!------------------------------------------------------------------
! Not used in low-order models

real(r8), intent(in) :: ens_mean(:)

end subroutine ens_mean_for_model



!===================================================================
! End of model_mod
!===================================================================
end module model_mod
