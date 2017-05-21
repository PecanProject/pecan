! This program reads the output values from the model run and writes them to a ensmeble member file for DART to use
! IMPORTANT: Compile this with gfortran, as the compilar affects how it reads and writes binary data.

Program R2F
  implicit none

  integer, parameter :: r8 = SELECTED_REAL_KIND(12)

  integer :: end_year, time
  real(r8) :: r, s, x(65), y(65)

! Here the program reads the end time for the model. The ensemble member file needs to start with that.

  open(10, file='temp_input.dat', status='old', form='unformatted')
  read(10) end_year, time
  close(10)

! Here the program reads the state vector values as written out by the readValue.R

  open(11, file="Routput.dat", status='old')
  read(11,*) x
  read(11,*) y
  close(11)

! Here the ensemble member data file is written out.

  open(20, file='temp_output.dat', status="unknown", form='unformatted')
  write(20) end_year, time
  write(20) x, y
  close(20)

end Program R2F
