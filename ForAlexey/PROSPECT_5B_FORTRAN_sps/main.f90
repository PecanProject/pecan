! ********************************************************************************
! main.f90
! ********************************************************************************
!
! Jean-Baptiste FERET
! 
! Department of Global Ecology / Carnegie Institution for Sciences
! 260 Panama Street
! Stanford, CA 94305, USA
! E-mail: jbferet@stanford.edu
!
! Stéphane JACQUEMOUD
!
! Université Paris Diderot / Institut de Physique du Globe de Paris
! 35 rue Hélène Brion
! 75013 Paris, France
! E-mail: jacquemoud@ipgp.fr
!
! http://teledetection.ipgp.fr/prosail/
!
! Adapted by Shawn P. Serbin (10.25.2013)
!
! ********************************************************************************
! version 5.02 (26 July 2011)
! ********************************************************************************

program main

use dataSpec_P5B
implicit none

integer*4 :: ii
real*8 :: N,Cab,Car,Cbrown,Cw,Cm
real*8, allocatable, save :: RT(:,:)
CHARACTER *100 BUFFER

!
allocate (RT(nw,2))

! N      = 1.2		! structure coefficient
! Cab    = 30.		! chlorophyll content (µg.cm-2) 
! Car    = 10.		! carotenoid content (µg.cm-2)
! Cbrown = 0.0		! brown pigment content (arbitrary units)
! Cw     = 0.015	! EWT (cm)
! Cm     = 0.009	! LMA (g.cm-2)

!open(11,file='leaf_parameter.txt')
!read(11,*) N,Cab,Car,Cbrown,Cw,Cm
!close(11)

! Get command line arguments
CALL GETARG(1,BUFFER)
READ(BUFFER,*) N                ! PROSPECT N parameter
CALL GETARG(2,BUFFER)
READ(BUFFER,*) Cab              ! PROSPECT chlorophyll parameter
CALL GETARG(3,BUFFER)
READ(BUFFER,*) Car              ! PROSPECT carotenoid parameter
CALL GETARG(4,BUFFER)
READ(BUFFER,*) Cbrown           ! PROSPECT brown pigment parameter
CALL GETARG(5,BUFFER)
READ(BUFFER,*) Cw               ! PROSPECT water parameter
CALL GETARG(6,BUFFER)
READ(BUFFER,*) Cm               ! PROSPECT dry matter (i.e LMA) parameter

! Call PROSPECT 5B model
call prospect_5B(N,Cab,Car,Cbrown,Cw,Cm,RT)

open(12,file='PROSPECT_5B_leaf_spectrum.txt')
write(12,'(i4,f7.4,f7.4)') (lambda(ii),RT(ii,1),RT(ii,2), ii=1,nw)
close(12)

stop
end
