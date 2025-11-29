! This program reads the binary DART state vector and writes out the data in ASCII format necessary for the R scripts.
! It also determines both the beginning and ending date from the days included in DART state vector and from these names
! both the initial and final history files.

Program F2R
  implicit none

  integer, parameter :: r8 = SELECTED_REAL_KIND(12)

  integer :: ii, month, date, year, end_month, end_date, end_year, n
  integer, dimension(13) :: c_days
  integer :: day, time, day2, time2
  real(r8) :: r, s, x, y, z, f

! Here the state vector and time values are read from the DART ensemble member file.

  open(10, file='temp_input.dat', status='old', form='unformatted')
  read(10) time, day
  read(10) time2, day2
  read(10) x, y, z, f
  close(10)

  open(11, file='sim_year', status='old')
  read(11,*) year
  close(11)


! Here bot the initial and final dates are calculated from the days given by DART

  n = 1

!  do while (day2 > n*365)
!     year = year + n
!     n = n+1
!  end do

  day2 = day2 - (n-1)*365
  
  c_days= (/0,31,59,90,120,151,181,212,243,273,304,334,365/)

  do ii = 1, 12
     if(day2 > c_days(ii)) then
        if(day2 <= c_days(ii+1)) then
           month = ii
        end if
     end if
  end do

  date = day2 - c_days(month)

  n = 1

!  do while (day > n*365)
!     end_year = 2002 + n
!     n = n+1
!  end do
  end_year = year

  day = day - (n-1)*365

  do ii = 1, 12
     if(day > c_days(ii)) then
        if(day <= c_days(ii+1)) then
           end_month = ii
        end if
     end if
  end do

  end_date = day - c_days(end_month)

! Here the history file names are saved for the advance_model.csh to read

  open(21,file="file_name.txt",status="unknown")
  write(21,'(A9,I4.4,A1,I2.2,A1,I2.2,A14)') 'wcreek-S-',year,'-',month,'-',date,'-000000-g01.h5'
  close(21)

  open(21,file="end_file.txt",status="unknown")
  write(21,'(A9,I4.4,A1,I2.2,A1,I2.2,A14)') 'wcreek-S-',end_year,'-',end_month,'-',end_date,'-000000-g01.h5'
  close(21)

  open(22, file='end_date',status='unknown')
  write(22,*) day
  close(22)

! Here the dates are saved for createInput.R program.

  open(11,file="4Rdate.dat",status='unknown')
  write(11,*) time, day
  write(11,*) time2, day2
  write(11,*) 
  close(11)

! Here the state vector values are saved for adjValue.R program.

  open(12,file="4Rvalues.dat", status='unknown')
  write(12,*) x, y, z, f
  close(12)


  print *, x, y, z

end Program F2R
