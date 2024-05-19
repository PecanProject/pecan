module set_params_mod
contains
  
  Subroutine set_params(pa)

    use parameters_site
    use parameters_plant
    use yasso, only: nc_h_max
    implicit none
    real :: pa(:) ! The length of pa() should be at least as high as the number of parameters

    real :: cn_h_min

    if (size(pa) < 160) then
       call rexit('parameter vector too small')
    end if

    ! Initial constants
    LOG10CLVI  = pa(1)
    LOG10CRESI = pa(2)
    LOG10CRTI  = pa(3)
    CSTI	   = pa(4)
    LOG10LAII  = pa(5)	   
    PHENI	   = pa(6) 
    TILTOTI	   = pa(7) 
    FRTILGI	   = pa(8)
    LT50I      = pa(9)

    ! Process parameters 
    ! CLAIV     = pa(10)	   
    COCRESMX  =	pa(11)
    CSTAVM	  = pa(12)
    DAYLB	  =	pa(13)  
    DAYLP	  =	pa(14)
    DLMXGE	  = pa(15)
    FSLAMIN   = pa(16)	 
    FSMAX     = pa(17)  
    HAGERE    =	pa(18)	  
    K         =	pa(19)  
    LAICR	  = pa(20)	 
    LAIEFT    = pa(21)	   
    LAITIL	  =	pa(22) 
    LFWIDG	  =	pa(23)
    LFWIDV	  = pa(24) 
    NELLVM	  = pa(25)	 
    PHENCR    = pa(26)	  
    PHY	      =	pa(27)	  
    RDRSCO	  =	pa(28)  
    RDRSMX	  = pa(29)	 
    RDRTEM    = pa(30) 
    RGENMX	  =	pa(31)  
    ROOTDM	  =	pa(32)	  
    RRDMAX	  = pa(33)
    RUBISC    = pa(34)	   
    SHAPE	  =	pa(35)	  
    SIMAX1T	  =	pa(36)
    SLAMAX    = pa(37)
    TBASE     = pa(38)	   
    TCRES     = pa(39)	  
    TOPTGE	  =	pa(40)	  
    TRANCO	  = pa(41)	 
    YG        = pa(42)

    LAT       = pa(43)
    WCI       = pa(44)
    FWCAD     = pa(45)
    FWCWP     = pa(46)
    FWCFC     = pa(47)
    FWCWET    = pa(48)
    WCST      = pa(49)
    WpoolMax  = pa(50)

    Dparam	     = pa(51)			
    FGAS	     = pa(52)			
    FO2MX	     = pa(53)			
    gamma	     = pa(54)			
    Hparam	     = pa(55)			
    KRDRANAER    = pa(56)			
    KRESPHARD    = pa(57)			
    KRSR3H	     = pa(58)			
    KRTOTAER     = pa(59)			
    KSNOW	     = pa(60)			
    LAMBDAsoil   = pa(61)			
    LDT50A	     = pa(62)			
    LDT50B	     = pa(63)			
    LT50MN	     = pa(64)			
    LT50MX	     = pa(65)			
    RATEDMX	     = pa(66)			
    reHardRedDay = pa(67)			
    RHOnewSnow	 = pa(68)			
    RHOpack	     = pa(69)			
    SWret	     = pa(70)			
    SWrf	     = pa(71)			
    THARDMX	     = pa(72)			
    TmeltFreeze	 = pa(73)			
    TrainSnow	 = pa(74)			
    TsurfDiff	 = pa(75)
    KLUETILG	 = pa(76)
    FRTILGG1I	 = pa(77)
    DAYLG1G2     = pa(78)
    RGRTG1G2     = pa(79)
    RDRTMIN      = pa(80)
    TVERN        = pa(81)

    CLITT0    = pa( 82) ! (g C m-2)    Initial C in litter
    CSOM0     = pa( 83) ! (g C m-2)    Initial C in OM
    CNLITT0   = pa( 84) ! (g C g-1 N)  Initial C/N ratio of litter
    CNSOMF0   = pa( 85) ! (g C g-1 N)  Initial C/N ratio of fast-decomposing OM
    CNSOMS0   = pa( 86) ! (g C g-1 N)  Initial C/N ratio of slowly decomposing OM
    FCSOMF0   = pa( 87) ! (-)          Initial C in fast-decomposing OM as a fraction of total OM
    FLITTSOMF = pa( 88) ! (-)          Fraction of decomposing litter that becomes OM
    FSOMFSOMS = pa( 89) ! (-)          Fraction of decomposing 'fast' OM that becomes slowly decomposing OM
    RNLEACH   = pa( 90) ! (-)          Mineral N concentration of drainage water as a ratio of that in soil water
    KNEMIT    = pa( 91) ! (d-1)        Max. relative emission rate of soil mineral N
    NMIN0     = pa( 92) ! (g N m-2)    Initial mineral N
    TCLITT    = pa( 93) ! (d)          Residence time of litter
    TCSOMF    = pa( 94) ! (d)          Residence time of fast-decomposing OM
    TCSOMS    = pa( 95) ! (d)          Residence time of slowly decomposing OM
    TMAXF     = pa( 96) ! (degC)       Temperature at which soil decomposition (fTsoil) is max.
    TSIGMAF   = pa( 97) ! (degC)       Tolerance of soil decomposition for suboptimal temperature
    RFN2O     = pa( 98) ! (-)          Sensitivity of the N2O/NO emission ratio to extreme values of water-filled pore space
    WFPS50N2O = pa( 99) ! (-)          Water-filled pore space at which the N2O and NO emission rates are equal

    ! Parameters for N-processes
    NCSHMAX   = pa(100) ! (g N g-1 C)
    NCR       = pa(101) ! (g N g-1 C)

    ! Senescence of roots and stubble
    RDRROOT   = pa(102)
    RDRSTUB   = pa(103)

    ! Parameter for sensitivity analysis of fertilisation
    NFERTMULT = pa(104) ! Multiplication factor for changing fertlisation (default = 1)

    ! Additional parameters for N-processes
    FNCGSHMIN = pa(105)
    TCNSHMOB  = pa(106)
    TCNUPT    = pa(107)

    F_DIGEST_WALL_FMIN = pa(108)
    F_DIGEST_WALL_MAX  = pa(109)
    F_WALL_LV_FMIN     = pa(110)
    F_WALL_LV_MAX      = pa(111)
    F_WALL_ST_FMIN     = pa(112)
    F_WALL_ST_MAX      = pa(113)

    CLVDI       = pa(114)
    YIELDI      = pa(115)
    CSTUBI      = pa(116)

    DRYSTORI   = pa(117)
    FdepthI    = pa(118)
    SDEPTHI    = pa(119)
    TANAERI    = pa(120)
    WAPLI      = pa(121)
    WAPSI      = pa(122)
    WASI       = pa(123)
    WETSTORI   = pa(124)

    NRTI       = pa(125)
    NSHI       = pa(126)

    TILG1I     = pa(127)
    TILG2I     = pa(128)
    TILVI      = pa(129)

    WALI       = pa(130)
    O2I        = pa(131)

    CSOMF0     = pa(132)
    CSOMS0     = pa(133)
    NLITT0     = pa(134)
    NSOMF0     = pa(135)
    NSOMS0     = pa(136)

    ! extras for YASSO-BASGRA
    use_yasso = pa(137) > 0
    use_nitrogen = pa(138) > 0
    cn_h_min = pa(139)
    totc_init = pa(140)
    fract_legacy_c = pa(141)
    hist_mean_tempr = pa(142)
    hist_yearly_precip = pa(143)
    hist_tempr_ampl = pa(144)

    yasso_alpha_awen = pa(145:148)
    yasso_beta12 = pa(149:150)
    yasso_decomp_pc = pa(151:152)
    yasso_cstate_init = pa(153:157)
    yasso_nstate_init = pa(158)
    yasso_met_init = pa(159:160)
    
    ! Parameter transformations 
    CLVI  = LOG10CLVI
    CRESI = LOG10CRESI
    CRTI  = LOG10CRTI
    LAII  = LOG10LAII

    WCAD  = FWCAD  * WCST
    WCWP  = FWCWP  * WCST
    WCFC  = FWCFC  * WCST
    WCWET = FWCWET * WCST

    nc_h_max = 1 / cn_h_min

  end Subroutine set_params
end module set_params_mod
