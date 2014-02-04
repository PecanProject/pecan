! This program reads the output values from the model run and writes them to a ensmeble member file for DART to use
! IMPORTANT: Compile this with gfortran, as the compilar affects how it reads and writes binary data.

Program R2F
  implicit none

  integer, parameter :: r8 = SELECTED_REAL_KIND(12)

  integer :: ii, month, date
  integer, dimension(13) :: c_days
  integer :: day, time, day2, time2
  real(r8) :: r, s, x, y, z,f

! Here the program reads the end time for the model. The ensemble member file needs to start with that.

  open(10, file='temp_input.dat', status='old', form='unformatted')
  read(10) time, day
  close(10)

! Here the program reads the state vector values as written out by the readValue.R

  open(11, file="Routput.dat", status='old')
  read(11,*) x, y, z, f
  close(11)

  print *, x, y, z

! Here the ensemble member data file is written out.

  open(20, file='temp_output.dat', status="unknown", form='unformatted')
  write(20) time, day
  write(20) x, y, z, f
  close(20)

end Program R2F
