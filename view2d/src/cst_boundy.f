! subversion Id for THIS file : $Id: cst_boundy.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/cst_boundy.f $
!-----------------------------------------------------------------------

      subroutine CST_BOUNDY( )
!----------------------------------------------------------------------
!    ... The lower, upper and (optionally) intermediate boundary conditions
!        which are time-independent are specified here.
!                                      v7s05 - July 2001 - simonc@oma.be
!----------------------------------------------------------------------
      use GRID_DIMS, only  : lmax, niz
      use PHYS_CST, only : Nav
      use ALLCO, only : phi
      use SPC_NAMES
      use BOUNDARIES
      use SIM_CONTROLS, only : data_dir
      use ASCII_UTILS, only : NAVU, LSKIP

      implicit none

!----------------------------------------------------------------------
!    ... Parameters
!----------------------------------------------------------------------
      real, parameter    :: conv = 1.e12 * Nav / ( 1.e4*3.1536e7 )

!----------------------------------------------------------------------
!    ... Local variables
!----------------------------------------------------------------------
      integer :: l, lat, iunit, month, ios
      real :: f1, f2, dun, dud, d, td, declin, latfile
      real :: wpan_y, whno3_y, wno3_y, wn2o5_y, whno4_y, wno2_y, wno_y
      real :: wdno2, wdno, wno_x, wdpan, wno2_x
      real :: wdhno3, wdhno4, wdn2o5, wdno3, noy
      real :: f113, f22, f1211, f1301, f114, f115
      real :: facn, facc, faco2, fa11, fa12, fa10, facm
      real, dimension(lmax) :: bn2o, bf10, bf11, bf12, bfcm, bch4,
     $                         bco, bco2, bnoy, bhno3, bh2o2,
     $                         bh2o, cly
      real, dimension(lmax) :: flxlbno, flxlbno2, flxlbh2o, flxlbnox
      real, dimension(lmax,3) :: fry
      character(len=64) :: filenm

!-----------------------------------------------------------------------
!         ... Relative boundary values as a function of latitude
!-----------------------------------------------------------------------
      data bn2o / 10*5.35,2*5.4,5.45,5.5,5.55,5.65,5.9,6.1,6.25,6.4,
     $            6.45,6.5,6.55,6.6,6.65,2*6.7,5*6.75,3*6.8/
      data bf10 / 7*.96,4*.97,2*.98,6*.99,4*1.01,2*1.02,4*1.03,
     $            2*1.04,2*1.05,2*1.06/
      data bf11 / 7*.87,4*.88,3*.9,2*.92,.94,.96,.98,1.,1.01,1.03,
     $            1.07,1.09,1.1,8*1.2,1.1,1.08/
      data bf12 / 7*.91,4*.92,3*.93,2*.95,.97,.98,.99,1.,2*1.01,
     $            1.02,1.05,1.06,8*1.15,1.06,1.04/
      data bfcm / 8*.85,2*.86,.87,2*.88,.89,.9,.92,.93,.96,1.,
     $            1.02,1.05,1.08,1.1,1.12,1.13,5*1.15,2*1.14,
     $            1.13,1.12,1.11/
      data bch4 / 12*.96,4*.97,.98,2*.99,.995,1.01,1.015,1.025,
     $            2*1.03,1.035,3*1.04,3*1.045,1.04,2*1.035/
      data bco /  2*.51,.52,.53,.56,.57,.58,.59,.62,.64,.67,.71,
     -            .77,.84,.91,.97,1.06,1.1,1.15,1.12,1.14,1.11,1.16,
     -            1.26,1.31,1.36,1.45,1.5,1.51,1.52,1.5,1.4,1.35,
     -            1.25,1.22/
      data bco2 / 8*-1.7,2*-1.6,-1.5,-1.4,-1.2,-1.1,-.8,-.6,-.4,
     $            -.1,.1,.4,.7,.9,1.1,1.3,1.5,1.6,1.7,1.8,3*1.9,
     $            4*2./
      data bnoy / 10*109.,127.,131.,145.,164.,173.,185.,204.,222.,354.,
     $  545.,636.,727.,796.,833.,1164.,1491.,1709.,1900.,1727.,1327.,
     $  979.,564.,418.,236.,164./
      data bhno3 / 12*100.,111.,123.,136.,154.,147.,144.,204.,284.,
     $  309.,323.,356.,377.,477.,582.,691.,800.,720.,410.,282.,204.,
     $  182.,154.,100./
      data bh2o2 /.11,.12,.13,.16,.19,.21,.31,.41,.51,.69,.87,1.04,
     $            1.18,1.32,1.46,1.64,1.83,2.02,2.03,2.05,2.04,
     $            1.98,1.92,1.86,1.71,1.55,1.39,1.3,1.22,1.14,.97,
     $            .79,.61,.54,.48/
      data bh2o / 4*6.,6.0,5.0,5.0,4.0,3*3.,2.,11*1.5,2.,3*3.,
     $             4.0,5.0,5.0,6.0,4*6.0/
!-----------------------------------------------------------------------
!         ... NO and NO2 fluxes at 0km : from Muller & Brasseur, 
!           JGR 1995 p.16461, where NO2 technological, NO from soils and
!           biomass burning (ligthnings parameterized elsewhere).
!           units are 1e10 molec/cm2/s            - Simon.
!-----------------------------------------------------------------------
      data flxlbno / 10*0.,.01,.02,.1,.2,.35,.5,.55,.65,.55,.55,.4,7*.3,
     $               .25,.2,.1,4*0./
      data flxlbno2 / 9*0.,.06,.2,.13,.13,.08,.01,.02,.03,.04,.05,.15,
     $               .12,.3,.6,1.2,2.2,3.25,2.5,3.8,2.2,.6,.25,.02,3*0./
  
!-------------------------------------------------------------
!     	... emission categories (Hough, 1991)
!     mmi = manmade-industrial
!     vgn = vegetation
!     saf = soils and fauna
!     bbg = biomass burning
!     ocn = oceans
!-------------------------------------------------------------
      bmmi(:lmax) = (/ 
     $  0.0000000000E+00, 0.0000000000E+00, 0.0000000000E+00,
     .  0.0000000000E+00, 0.0000000000E+00, 0.0000000000E+00,
     .  0.6674270223E-16, 0.1334854045E-15, 0.4004562134E-15,
     .  0.6674270223E-15, 0.4449513306E-15, 0.4894465007E-15,
     .  0.4004562134E-15, 0.3114659526E-15, 0.1557329895E-15,
     .  0.0000000000E+00, 0.8899027406E-16, 0.1779805481E-15,
     .  0.2669708354E-15, 0.3559610962E-15, 0.5116940857E-15,
     .  0.6674270223E-15, 0.2046776343E-14, 0.3426125240E-14,
     .  0.5650882899E-14, 0.7875638652E-14, 0.9388473377E-14,
     .  0.1094580444E-13, 0.8899027247E-14, 0.6852251327E-14,
     .  0.4271533366E-14, 0.1690815194E-14, 0.1023388171E-14,
     .  0.3559610962E-15, 0.1779805613E-15 /)
   
      bvgn(:lmax) = (/ 
     $  0.0000000000E+00, 0.0000000000E+00, 0.0000000000E+00,
     .  0.0000000000E+00, 0.0000000000E+00, 0.0000000000E+00,
     .  0.0000000000E+00, 0.0000000000E+00, 0.3922842513E-16,
     .  0.7845685026E-16, 0.2353705508E-15, 0.3922842248E-15,
     .  0.1529601213E-14, 0.2666918175E-14, 0.3922183945E-14,
     .  0.5177449503E-14, 0.5412687917E-14, 0.5647927177E-14,
     .  0.5255423121E-14, 0.4862919065E-14, 0.3686505497E-14,
     .  0.2510092142E-14, 0.1725743127E-14, 0.9413943234E-15,
     .  0.7847882017E-15, 0.6281819212E-15, 0.4709168079E-15,
     .  0.3136516682E-15, 0.3136516682E-15, 0.3136516946E-15,
     .  0.2352387577E-15, 0.1568258473E-15, 0.7845685026E-16,
     .  0.0000000000E+00, 0.0000000000E+00 /)
      
      bsaf(:lmax) = (/ 
     $  0.0000000000E+00, 0.0000000000E+00, 0.0000000000E+00,
     .  0.0000000000E+00, 0.0000000000E+00, 0.0000000000E+00,
     .  0.4624383986E-16, 0.9248768633E-16, 0.2312191894E-15,
     .  0.3699507188E-15, 0.1063608366E-14, 0.1757265881E-14,
     .  0.2173460285E-14, 0.2589655005E-14, 0.2635898769E-14,
     .  0.2682142533E-14, 0.2774630272E-14, 0.2867118012E-14,
     .  0.2820874248E-14, 0.2774630272E-14, 0.2959605963E-14,
     .  0.3144581229E-14, 0.3144581229E-14, 0.3144581229E-14,
     .  0.2774630272E-14, 0.2404679527E-14, 0.2219704260E-14,
     .  0.2034728993E-14, 0.1757265881E-14, 0.1479802875E-14,
     .  0.1202339869E-14, 0.9248767574E-15, 0.6936575681E-15,
     .  0.4624383787E-15, 0.3468287840E-15 /)
               
      bbbg(:lmax) = (/ 
     $  0.0000000000E+00, 0.0000000000E+00, 0.0000000000E+00,
     .  0.0000000000E+00, 0.0000000000E+00, 0.0000000000E+00,
     .  0.0000000000E+00, 0.0000000000E+00, 0.1186635455E-15,
     .  0.2373270910E-15, 0.8899766706E-15, 0.1542626144E-14,
     .  0.2017280326E-14, 0.2491934508E-14, 0.3144583982E-14,
     .  0.3797233456E-14, 0.3025920384E-14, 0.2254607523E-14,
     .  0.3025920595E-14, 0.3797233456E-14, 0.3500574566E-14,
     .  0.3203916099E-14, 0.2966588690E-14, 0.2729261705E-14,
     .  0.2432602815E-14, 0.2135943925E-14, 0.1898616728E-14,
     .  0.1661289743E-14, 0.1423962546E-14, 0.1186635561E-14,
     .  0.8899766706E-15, 0.5933177804E-15, 0.4153224357E-15,
     .  0.2373270910E-15, 0.1186635455E-15 /)
 
      bocn(:lmax) = (/ 
     $  0.2142439741E-15, 0.4284879482E-15, 0.1071219804E-14,
     .  0.1713951793E-14, 0.2853447323E-14, 0.3992942536E-14,
     .  0.4350800404E-14, 0.4708658695E-14, 0.4350800404E-14,
     .  0.3992942536E-14, 0.3279580917E-14, 0.2566218874E-14,
     .  0.2210715123E-14, 0.1855211372E-14, 0.1855211372E-14,
     .  0.1855211372E-14, 0.1855211583E-14, 0.1855211372E-14,
     .  0.1855211372E-14, 0.1855211583E-14, 0.1711597465E-14,
     .  0.1567983346E-14, 0.7910546574E-15, 0.1426723662E-14,
     .  0.1349030729E-14, 0.1285463765E-14, 0.1142085122E-14,
     .  0.9987064784E-15, 0.7135972001E-15, 0.4284879482E-15,
     .  0.2855801228E-15, 0.1426723503E-15, 0.1177164568E-15,
     .  0.9417317602E-16, 0.4708659132E-16 /)

!----------------------------------------------------------------------
!    ... Default deposition velocity, upper boundary mixing ratios,
!           and upper and lower boundary fluxes are zero
!----------------------------------------------------------------------
      lbc(:)%iz = 1
      lbc(:)%is_vmr = .false.
      ibc(:)%iz = -99
      ibc(:)%is_vmr = .true.
      ubc(:)%iz = niz
      ubc(:)%is_vmr = .false.
      do l = 1, lmax
         lbc(:)%vel(l) = 0.
         lbc(:)%val(l) = 0.
         ibc(:)%vel(l) = 0.
         ibc(:)%val(l) = 0.
         ubc(:)%vel(l) = 0.
         ubc(:)%val(l) = 0.
      end do

!----------------------------------------------------------------------
!         ... Specify the mixing ratio for several species at the
!           lower boundary. these values are susceptible to change
!           when perturbation studies are performed.
!           Note: the numerical values must be assigned in the
!                 parenthesized units 
!----------------------------------------------------------------------
!        1990 values 
!----------------------------------------------------------------------
      faco2 = 356.  ! CO2 (ppmv)
      facc = 1.715  ! CH4 (ppmv)
      facn = 310.   ! N2O (ppbv)
      fa11 = 270.   ! CFC-11 (pptv)
      fa12 = 465.   ! CFC-12 (pptv)
      facm = 153.   ! CH3CCl3 (pptv)
      fa10 = 108.   ! CCl4 (pptv)
      f113 = 70.    ! CFC-113 (pptv) 
      f114 = 10     ! CFC-114 (pptv) 
      f115 = 5.     ! CFC-115 (pptv) 
      f22 = 106.    ! CFC-22 (pptv) 
      f1211 = 2.9   ! Halon-1211 (pptv) 
      f1301 = 1.7   ! Halon-1301 (pptv) 

!======================================================================
!    ... Species which have specified vmr as lower boundary conditions
!======================================================================
      lbc(vid_cfc10)%is_vmr = .true.
      lbc(vid_cfc10)%val(:) = fa10 * bf10 * 1.e-12
      lbc(vid_cfc11)%is_vmr = .true.
      lbc(vid_cfc11)%val(:) = fa11 * bf11 * 1.e-12
      lbc(vid_cfc12)%is_vmr = .true.
      lbc(vid_cfc12)%val(:) = fa12 * bf12 * 1.e-12
      lbc(vid_cfc113)%is_vmr = .true.
      lbc(vid_cfc113)%val(:) = f113 * bf12 * 1.e-12
      lbc(vid_cfc114)%is_vmr = .true.
      lbc(vid_cfc114)%val(:) = f114 * bf12 * 1.e-12
      lbc(vid_cfc115)%is_vmr = .true.
      lbc(vid_cfc115)%val(:) = f115 * bf12 * 1.e-12
      lbc(vid_hcfc22)%is_vmr = .true.
      lbc(vid_hcfc22)%val(:) = f22 * bf12 * 1.e-12
      lbc(vid_ha1211)%is_vmr = .true.
      lbc(vid_ha1211)%val(:) = f1211 * bf12 * 1.e-12
      lbc(vid_ha1301)%is_vmr = .true.
      lbc(vid_ha1301)%val(:) = f1301 * bf12 * 1.e-12
      lbc(vid_ch3ccl3)%is_vmr = .true.
      lbc(vid_ch3ccl3)%val(:) = facm * bfcm * 1.e-12
      lbc(vid_ch3cl)%is_vmr = .true.
      lbc(vid_ch3cl)%val(:) = 6.e-10
      lbc(vid_ch3br)%is_vmr = .true.
      lbc(vid_ch3br)%val(1:18) = 9. * 1.e-12
      lbc(vid_ch3br)%val(19:lmax) = 11.5 * 1.e-12
      lbc(vid_chbr3)%is_vmr = .true. 
! Notice: chbr3 is badly known, short-lived, and provides unlimited Bry supply
! (removed by HBr washout)
      lbc(vid_chbr3)%val(:) = 2. * 1.e-12 ! 2 pptv 

      lbc(vid_ch4)%is_vmr = .true.
      lbc(vid_ch4)%val(:) = facc * bch4 * 1.e-6
      lbc(vid_n2o)%is_vmr = .true.
      lbc(vid_n2o)%val(:) = (facn + bn2o(:)) * 1.e-9
      lbc(vid_co2)%is_vmr = .true.
      lbc(vid_co2)%val(:) = (faco2 + bco2) * 1.e-6      

!======================================================================
!    ... Species which have specified flux and/or dry deposition velocity 
!         as lower boundary conditions
!======================================================================
!    ... no and no2  ( convert Tg-N/yr/m**2 into molecule/s/cm**2 ) 
!----------------------------------------------------------------------
      flxlbnox(:) = ( 23.3*bmmi + 6.6*bsaf + 4.9*bbbg ) * conv / 14.
      where( flxlbno(:) == 0. .and. flxlbno2(:) == 0. )
         lbc(vid_no)%val(:) = .5 * flxlbnox(:)
         lbc(vid_no2)%val(:) = .5 * flxlbnox(:)
       else where
         lbc(vid_no)%val(:) = flxlbnox(:) * flxlbno(:) 
     $                         / ( flxlbno(:) + flxlbno2(:) )
         lbc(vid_no2)%val(:) = flxlbnox(:) * flxlbno2(:)
     $                          / ( flxlbno(:) + flxlbno2(:) )
      end where

!----------------------------------------------------------------------
!    ... Fake species X (inert tracer) - v6s37 test
!----------------------------------------------------------------------
      lbc(vid_x)%val(:) = 1.e10   ! constant flux

!======================================================================
!    ... Species which have specified vmr as upper boundary conditions
!======================================================================
      ubc(vid_h2)%is_vmr = .true.
      ubc(vid_h2)%val(:) = 3.5e-6 
      ubc(vid_h2o)%is_vmr = .true.   ! to avoid accumulation in thermospheric
      ubc(vid_h2o)%val(:) = 3.e-8    ! polar night (v6s27-v7s10:1.e-7)
      ubc(vid_ch4)%is_vmr = .true.   ! v6s37, same reason
      ubc(vid_ch4)%val(:) = 1.e-9

!======================================================================
!    ... Specify an intermediate vmr boundary condition for H2O
!        and read the values in data file
!======================================================================
      ibc(vid_h2o)%iz = 18
      ibc(vid_h2o)%is_vmr = .true.
      filenm = data_dir(:LEN_TRIM(data_dir)) // 'h2o_uarsclim_17km.dat'
      iunit = NAVU()
      OPEN( unit = iunit, file = filenm, form = 'formatted',
     $      status = 'OLD', iostat = ios )
      if( ios /= 0 ) then
	 write(*,*) 'BOUNDY_INIT: Error reading ',filenm
	 write(*,*) '           ios Error = ',ios
	 stop 'BOUNDY_INIT: Error reading h2o_uarsclim_17km.dat'
      end if
      call LSKIP( 21, iunit)
      do lat = 2, lmax-1
         READ(iunit,*) latfile, ( ibc_vmr_h2o(month,lat), month=1,12 )
         if( latfile /= phi(lat) ) stop 'BOUNDY_INIT: lat mismatch'
      end do
      CLOSE( iunit )
      ibc_vmr_h2o(:,1) = ibc_vmr_h2o(:,2)
      ibc_vmr_h2o(:,lmax) = ibc_vmr_h2o(:,lmax-1)

      end subroutine CST_BOUNDY
