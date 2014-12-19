! This program reads the binary DART state vector and writes out the data in ASCII format necessary for the R scripts.
! It also determines both the beginning and ending date from the days included in DART state vector and from these names
! both the initial and final history files.

Program F2R
  implicit none

  integer, parameter :: r8 = SELECTED_REAL_KIND(12)

  integer :: ii, init_year, end_year, time, time2
  real(r8) :: r, s, x(65), y(65)

! Here the state vector and time values are read from the DART ensemble member file.

  open(10, file='temp_input.dat', status='old', form='unformatted')
  read(10) end_year, time
  read(10) init_year, time2
  read(10) x, y
  close(10)
! Here bot the initial and final dates are calculated from the days given by DART

! Here the history file names are saved for the advance_model.csh to read

  open(21,file="file_name.txt",status="unknown")
  write(21,'(A7,I4.4,A20)') 'duke-S-',init_year,'-09-01-000000-g01.h5'
  close(21)

  open(21,file="end_file.txt",status="unknown")
  write(21,'(A7,I4.4,A20)') 'duke-S-',end_year,'-09-01-000000-g01.h5'
  close(21)

  open(22, file='end_date',status='unknown')
  write(22,*) end_year
  close(22)

  open(23, file='start_date',status='unknown')
  write(23,*) init_year
  close(23)


! Here the dates are saved for createInput.R program.

  open(11,file="4Rdate.dat",status='unknown')
  write(11,*) time, end_year
  write(11,*) time2, init_year
  write(11,*) 
  close(11)

! Here the state vector values are saved for adjValue.R program.

  open(12,file="4Rvalues.dat", status='unknown')
  write(12,'(65E18.10)') x
  write(12,'(65E18.10)') y
  close(12)


end Program F2R
