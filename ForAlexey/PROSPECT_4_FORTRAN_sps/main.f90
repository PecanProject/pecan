! ********************************************************************************
! main.f90 --> PROSPECT-4
! ********************************************************************************
!
! 
! Shawn P. Serbin
! 
! Department of Forest and Wildlife Ecology
! 226 Russell Labs
! 1630 Linden Drive
! Madison, WI, 53706
! E-mail: serbin@wisc.edu
!
!
! Code adapted from:
!
! Jean-Baptiste FERET
!
! Department of Global Ecology / Carnegie Institution for Sciences
! 260 Panama Street
! Stanford, CA 94305, USA
! E-mail: jbferet@stanford.edu
!
! StÈphane JACQUEMOUD
!
! UniversitÈ Paris Diderot / Institut de Physique du Globe de Paris
! 35 rue HÈlËne Brion
! 75013 Paris, France
! E-mail: jacquemoud@ipgp.fr
!
! http://teledetection.ipgp.fr/prosail/
!
! ********************************************************************************
! version 5.02 (26 July 2011)
! ********************************************************************************

program main

use dataSpec_P4
implicit none

integer*4 :: ii
real*8 :: N,Cab,Cw,Cm
real*8, allocatable, save :: RT(:,:)
CHARACTER *100 BUFFER
!
allocate (RT(nw,2))

! N      = 1.2		! structure coefficient
! Cab    = 30.		! chlorophyll content (µg.cm-2)
! Cw     = 0.015	! EWT (cm)
! Cm     = 0.009	! LMA (g.cm-2)
!


! Get command line arguments
CALL GETARG(1,BUFFER)
READ(BUFFER,*) N                ! PROSPECT N parameter
CALL GETARG(2,BUFFER)
READ(BUFFER,*) Cab              ! PROSPECT chlorophyll parameter
CALL GETARG(3,BUFFER)
READ(BUFFER,*) Cw               ! PROSPECT water parameter
CALL GETARG(4,BUFFER)
READ(BUFFER,*) Cm               ! PROSPECT dry matter (i.e LMA) parameter

! Call PROSPECT 4 model
call prospect_4(N,Cab,Cw,Cm,RT)

! Generate output
open(12,file='PROSPECT_4_leaf_spectrum.txt')
write(12,'(i4,f7.4,f7.4)') (lambda(ii),RT(ii,1),RT(ii,2), ii=1,nw)
close(12)

stop
end