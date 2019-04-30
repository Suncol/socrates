! subversion Id for THIS file : $Id: pho_init.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/pho_init.f $
!-----------------------------------------------------------------------

      subroutine PHO_INIT(  ) 
!-----------------------------------------------------------------------
!    ... Initialize solar UV computations:
!           Read wavelength grid and solar flux in "solflux.dat"
!           solflux can be partly (122-417.5nm) overwritten by values
!                      from data file "solflux_lean97.dat"
!              The output dummy arg solflux (in photons/cm2/s) has  
!              3 columns: solflux(:,0) for minimum solar activity,
!              solflux(:,1) for solar average & solflux(:,2) for solmax
!           NOTICE - solflux will be scaled in SOLCYCLES, where solflux
!            at Lyman-a (bin lya_iv, 121.59-121.59nm) is overwritten
!           Read all absorption cross-sections and quantum yields
!                      from data file "crs2001.dat" to module PHO_VARS
!           Read several other data files for Schumann-Runge and 
!                       clouds/aerosol parameterizations
!           Compute Rayleigh cross-section
!
!              Based on crsget.f, socrates v6s25 - simonc@oma.be, Apr 2001
!-----------------------------------------------------------------------
      use SPC_NAMES
      use PHO_PARMS
      use PHO_VARS
      use SUN_VARS, only : maxnd, solflux, days0_s2k, e107_s2k, flya_s2k ! output
      use SIM_CONTROLS, only : missval, data_dir, mainsw
     $                       , sim_start_time, sim_stop_time
      use DATE_UTILS, only : MDY             ! for diags
      use ASCII_UTILS, only : NAVU, LSKIP

      implicit none

!-----------------------------------------------------------------------
!    ... Parameters
!-----------------------------------------------------------------------
      integer, parameter :: jo3 = 2, jno2 = 7, jhno3 = 8, jco2 = 5
     $,                     jclono2 = 16, jch2o = 24, jpan = 40
      real, parameter :: a1 = 0.887, a2 = 2.35, a3 = 57.
      real, parameter :: lamo1 = 302., lamo2 = 311.1, lamo3 = 313.9
      real, parameter :: om1 = 7.9, om2 = 2.2, om3 = 7.4

!-----------------------------------------------------------------------
!    ... Local variables
!-----------------------------------------------------------------------
      logical :: leap_year = .false.
      real, dimension(mxwvn) ::  lum, x, sigrar
      integer :: iunit, ios, ic, icof, ivwc, isize, nl, il
      integer :: iproc, iv, ivo2, idummy, lc
      integer :: id, year_f, cald_f, diffdays0
      real :: flya_f, e107_f, days0_f, days0_f1
      real :: solvar, dummy, fmin, fmax, al, bl
      character (len=7) :: datestr
      character (len=64) :: filenm

!-----------------------------------------------------------------------
!    ... Read in Time evolution of solar proxies E10.7 and Ly-a (SOLAR2K)
!-----------------------------------------------------------------------
      iunit = NAVU()
      filenm = data_dir(:LEN_TRIM(data_dir)) // 'S2K_v124_proxies.dat'
      OPEN( unit = iunit,
     $      file = filenm,
     $      status = 'OLD',
     $      form = 'formatted',
     $      iostat = ios )
      if( ios /= 0 ) then
         write(*,*) ' PHO_INIT: Failed to open ' // filenm
         write(*,*) '   Error code = ',ios
         stop ' PHO_INIT: Failed to open S2K_v124_proxies.dat'
      end if
      call LSKIP( 20, iunit )
      flya_s2k = missval
      e107_s2k = missval
      id = 1
      do 
         read( iunit, '(i5,x,i3,a7,27x,es9.2,9x,f6.1)', end = 100 )
     $      year_f, cald_f, datestr, flya_f, e107_f
         if( cald_f == 1 ) leap_year = .false.
         if( datestr == ' 29-FEB' ) then
            leap_year = .true.
            cycle
         end if
         if( leap_year ) cald_f = cald_f - 1
         days0_f = REAL( 365*year_f + cald_f )
         if( days0_f < sim_start_time%days0 ) cycle
         if( days0_f > sim_stop_time%days0 ) goto 100
         if( id > maxnd ) stop 'PHO_INIT error: maxnd reached'
         days0_s2k(id) = days0_f
         flya_s2k(id) = flya_f
         e107_s2k(id) = e107_f
         id = id + 1
      end do
  100 CLOSE( unit = iunit )
  
!-----------------------------------------------------------------------
!    ... If simulation starts before S2K era, insert missval 
!        (id *must* be simulation day counter)
!-----------------------------------------------------------------------
      if( ANY( e107_s2k /= missval ) ) then
         diffdays0 = INT( days0_s2k(1) - sim_start_time%days0 )
         if( diffdays0 > 0 ) then
            days0_s2k(diffdays0:diffdays0+id-1) = days0_s2k(1:id-1)
            e107_s2k(diffdays0:diffdays0+id-1)  = e107_s2k(1:id-1)
            flya_s2k(diffdays0:diffdays0+id-1)  = flya_s2k(1:id-1)
            days0_s2k(1:diffdays0-1) = missval
            e107_s2k(1:diffdays0-1)  = missval
            flya_s2k(1:diffdays0-1)  = missval
         end if
      end if
      
!      do id = 1, maxnd
!         if( e107_s2k(id) /= missval ) then
!            call MDY( INT(days0_s2k(id)), il, idummy, year_f )
!            write(*,'(a,i6,3(a,i4),a,es9.2,a,f6.1)') 'PHO_INIT: id= '
!     $         ,id,' ; date= ',year_f,'-',il,'-',idummy
!     $         ,' ; f_lya= ',flya_s2k(id),' ; e107= ',e107_s2k(id)
!         end if
!      end do
!      stop 'testing PHO_INIT'

!-----------------------------------------------------------------------
!    ... Read in wavelength grid and solar flux 
!-----------------------------------------------------------------------
      iunit = NAVU()
      filenm = data_dir(:LEN_TRIM(data_dir)) // 'solflux.dat'
      OPEN( unit = iunit,
     $      file = filenm,
     $      status = 'OLD',
     $      form = 'formatted',
     $      iostat = ios )
      if( ios /= 0 ) then
         write(*,*) ' PHO_INIT: Failed to open ' // filenm
         write(*,*) '   Error code = ',ios
         stop ' PHO_INIT: Failed to open solflux.dat'
      end if
      call LSKIP( 7, iunit )
      do iv = 1,mxwvn
         read(iunit,*) 
     $              idummy, alamb(iv), blamb(iv), solflux(iv,1), solvar
         solflux(iv,0) = solflux(iv,1) / ( 1. + 0.5*solvar )
         solflux(iv,2) = 2.*solflux(iv,1) - solflux(iv,0)
      end do
      CLOSE( unit = iunit )  
      wvn = .5 * ( blamb + alamb )
      dwvn = blamb - alamb
      where( dwvn == 0. )
         dwvn = 1.
      end where

!-----------------------------------------------------------------------
!    ... Read solar flux at solmin & solmax (122-417.5 nm) by lean/et-al-1997
!-----------------------------------------------------------------------
      if( mainsw(9) == 1 ) then
         iunit = NAVU()
         filenm = data_dir(:LEN_TRIM(data_dir)) // 'solflux_lean97.dat'
         OPEN( unit = iunit,
     $         file = filenm,
     $         status = 'OLD',
     $         form = 'formatted',
     $         iostat = ios )
         if( ios /= 0 ) then
            write(*,*) ' PHO_INIT: Failed to open ' // filenm
            write(*,*) '   Error code = ',ios
            stop ' PHO_INIT: Failed to open solflux_lean97.dat'
         end if
         call LSKIP( 3, iunit )
         read(iunit,*) nl
         call LSKIP( 3, iunit )
         do il = 1, nl
            read(iunit,*) iv, al, bl, fmin, fmax
            if( al /= alamb(iv) .or. bl /= blamb(iv) ) 
     $         stop 'PHO_INIT: error reading solflux_lean97.dat'
            solflux(iv,0) = fmin
            solflux(iv,2) = fmax
            solflux(iv,1) = 0.5 * ( fmin + fmax )
         end do
      end if

!-----------------------------------------------------------------------
!         ... Calculate Rayleigh scattering cross-sections (cm2) for
!           Earth following Nicolet(1984): Planet. Space Sci., 32, p1467
!-----------------------------------------------------------------------
      lum = 1.e-3 * wvn                        ! wavelengths in micrometers
      x(:) = 0.4
      where( lum(:) < 0.55 )
         x(:) = 0.389 * lum(:) + 0.09426 / lum(:) - 0.3228
      end where
      sigrar(:) = 4.02e-28 / lum(:)**(4.+x(:))
      do lc = 0,mxcly
         sigra(lc,:) = sigrar(:)
      end do

!-----------------------------------------------------------------------
!    ... Open and read cross-section and quantum yields file
!-----------------------------------------------------------------------
      iunit = NAVU()
      filenm = data_dir(:LEN_TRIM(data_dir)) // 'crs2002.dat' ! v6s34b, new crs(H2O)
      OPEN( unit = iunit,
     $      file = filenm,
     $      status = 'OLD',
     $      form = 'formatted',
     $      iostat = ios )
      if( ios /= 0 ) then
      write(*,*) ' PHO_INIT: Failed to open ' // filenm
      write(*,*) '    Error code = ',ios
      stop ' PHO_INIT: Failed to open crs2002.dat'
      end if
      crs(:,:) = 0.
      qy(:,:) = 1.
      
      do iproc = 1,phtmax
         call LSKIP( 4, iunit )
         read(iunit,1010) ivbegin(iproc), ivend(iproc)
         read(iunit,1002)
         do iv = ivbegin(iproc),ivend(iproc)
           read(iunit,*) idummy,dummy,dummy, crs(iproc,iv), qy(iproc,iv)
         end do
      end do
1002  format(1x)
1010  format(31x,i3,31x,i3)

      REWIND( iunit )

!-----------------------------------------------------------------------
!    ... Read in coefficients for parameterization of
!           T dependence of o3 cross sections.
!-----------------------------------------------------------------------
      do iproc = 1,jo3-1
         CALL LSKIP( 6+ivend(iproc)-ivbegin(iproc)+1, iunit )
      end do
      CALL LSKIP( 6, iunit )
      tb_o3(:) = 0.
      tc_o3(:) = 0.
      do iv = ivbegin(jo3),ivend(jo3)
         read(iunit,*) idummy, dummy, dummy, dummy, dummy, 
     $                 tb_o3(iv), tc_o3(iv)
      end do

!-----------------------------------------------------------------------
!    ... Read in CO2 cross-sections at 200K and 370K
!-----------------------------------------------------------------------
      do iproc = jo3+1,jco2-1
         CALL LSKIP( 6+ivend(iproc)-ivbegin(iproc)+1, iunit )
      end do
      CALL LSKIP( 6, iunit )
      crs200co2(:) = 0.
      crs370co2(:) = 0.
      do iv = ivbegin(jco2),ivend(jco2)
         read(iunit,*) idummy, dummy, dummy, dummy, dummy, 
     $                 crs200co2(iv), crs370co2(iv)
      end do

!-----------------------------------------------------------------------
!    ... Read in coefficient for parameterization of
!           T dependence of no2 cross sections.
!-----------------------------------------------------------------------
      do iproc = jco2+1,jno2-1
         CALL LSKIP( 6+ivend(iproc)-ivbegin(iproc)+1, iunit )
      end do
      CALL LSKIP( 6, iunit )
      ta_no2(:) = 0.
      do iv = ivbegin(jno2),ivend(jno2)
         read(iunit,*) idummy, dummy, dummy
     $,                dummy, dummy, ta_no2(iv)
      end do

!-----------------------------------------------------------------------
!         ... Read in coefficient for parameterization
!        of T dependence of hno3 cross sections.
!-----------------------------------------------------------------------
      CALL LSKIP( 6, iunit )
      tb_hno3(:) = 0.
      do iv = ivbegin(jhno3),ivend(jhno3)
         read(iunit,*) idummy, dummy, dummy, dummy, dummy, tb_hno3(iv)
      end do

!-----------------------------------------------------------------------
!    ... Read in coefficients for parameterization
!        of T dependence of clono2 cross sections.
!-----------------------------------------------------------------------
      do iproc = jhno3+1,jclono2-1
         CALL LSKIP( 6+ivend(iproc)-ivbegin(iproc)+1, iunit )
      end do
      CALL LSKIP( 6, iunit )
      ta1_clono2(:) = 0.
      ta2_clono2(:) = 0.
      do iv = ivbegin(jclono2),ivend(jclono2)
         read(iunit,*) idummy, dummy, dummy, dummy, dummy, 
     $                 ta1_clono2(iv), ta2_clono2(iv)
      end do

!-----------------------------------------------------------------------
!    ... Read in coefficients for parameterization
!        of T dependence of ch2o cross sections.
!-----------------------------------------------------------------------
      do iproc = jclono2+1,jch2o-1
         CALL LSKIP( 6+ivend(iproc)-ivbegin(iproc)+1, iunit )
      end do
      CALL LSKIP( 6, iunit )
      ta_ch2o(:) = 0.
      tb_ch2o(:) = 0.
      do iv = ivbegin(jch2o),ivend(jch2o)
         read(iunit,*) idummy, dummy, dummy, dummy, dummy, 
     $                 ta_ch2o(iv), tb_ch2o(iv)
      end do

!-----------------------------------------------------------------------
!    ... Read in coefficient for parameterization
!        of T dependence of pan cross sections.
!-----------------------------------------------------------------------
      do iproc = jch2o+1,jpan-1
         CALL LSKIP( 6+ivend(iproc)-ivbegin(iproc)+1, iunit )
      end do
      CALL LSKIP( 6, iunit )
      tb_pan(:) = 0.
      do iv = ivbegin(jpan),ivend(jpan)
         read(iunit,*) idummy, dummy, dummy, dummy, dummy, tb_pan(iv)
      end do
      
      CLOSE( iunit )

!-----------------------------------------------------------------------
!    ... Read in coefficients for
!        Kockart's Schuman Runge band Aproximation (iv=46 -61) 
!-----------------------------------------------------------------------
      filenm = data_dir(:LEN_TRIM(data_dir)) // 'rm40n.dat'
      OPEN( unit = iunit,
     $      file = filenm,
     $      form = 'formatted',
     $      status = 'OLD',
     $      iostat = ios )
      if( ios /= 0 ) then
      write(*,*) ' PHO_INIT: Failed to open >'// filenm //'<'
      write(*,*) ' Error code = ',ios
      stop ' PHO_INIT: Failed to open rm40n.dat'
      end if
      do ivo2 = 16,1,-1 
         call LSKIP( 1, iunit)
         read(iunit,*) (ako(ic,ivo2),ic=1,12)
      end do
      CLOSE(iunit)

      filenm = data_dir(:LEN_TRIM(data_dir)) // 'ro240n.dat'
      OPEN( unit = iunit,
     $      file = filenm,
     $      form = 'formatted',
     $      status = 'OLD',
     $      iostat = ios )
      if( ios /= 0 ) then
      write(*,*) ' PHO_INIT: Failed to open >'// filenm //'<'
      write(*,*) ' Error code = ',ios
      stop ' PHO_INIT: Failed to open ro240n.dat'
      end if
      do ivo2 = 16,1,-1
         call LSKIP( 1, iunit)
         read(iunit,*) (bko(ic,ivo2),ic=1,12)
      end do
      CLOSE(iunit)

!-----------------------------------------------------------------------
!    ... Read in water cloud parameters
!-----------------------------------------------------------------------
      filenm = data_dir(:LEN_TRIM(data_dir)) // 'solar.asymm'
      OPEN( unit = iunit,
     $      file = filenm,
     $      form = 'formatted',
     $      status = 'OLD',
     $      iostat = ios )
      if( ios /= 0 ) then
      write(*,*) ' PHO_INIT: Failed to open solar.asymm'
      write(*,*) ' Error code = ',ios
      stop ' PHO_INIT: Failed to open solar.asymm'
      end if
      do isize = 1,mxsiz
          call LSKIP(2, iunit)
          read(iunit,*) wvnwcz(isize), wvnwcz(isize)
          call LSKIP(1, iunit)
          do ivwc = 1,mxwvwc
             read(iunit,*) wcwvn(ivwc),
     $                     (wc_asy(isize, ivwc,icof),icof=1,mxcof)
             wcwvn(ivwc) = wcwvn(ivwc) * 1.e3  ! Change from microns to nm
         end do
      end do
      CLOSE(iunit)  

      filenm = data_dir(:LEN_TRIM(data_dir)) // 'solar.b_ext'
      OPEN( unit = iunit,
     $      file = filenm,
     $      form = 'formatted',
     $      status = 'OLD',
     $      iostat = ios )
      if( ios /= 0 ) then
      write(*,*) ' PHO_INIT: Failed to open solar.b_ext'
      write(*,*) ' Error code = ',ios
      stop ' PHO_INIT: Failed to open solar.b_ext'
      end if
      do isize = 1,mxsiz
         call LSKIP(2, iunit)
         read(iunit,*) wvnwcz(isize), wvnwcz(isize)
         call LSKIP(1, iunit)
         do ivwc = 1,mxwvwc
             read(iunit,*) wcwvn(ivwc),
     $                     (wc_ext(isize, ivwc,icof),icof=1,mxcof)
             wcwvn(ivwc) = wcwvn(ivwc) * 1.e3  ! Change from microns to nm
         end do
      end do
      CLOSE(iunit)  

      filenm = data_dir(:LEN_TRIM(data_dir)) // 'solar.ssalb'
      OPEN( unit = iunit,
     $      file = filenm,
     $      form = 'formatted',
     $      status = 'OLD',
     $      iostat = ios )
      if( ios /= 0 ) then
      write(*,*) ' PHO_INIT: Failed to open solar.ssalb'
      write(*,*) ' Error code = ',ios
      stop ' PHO_INIT: Failed to open solar.ssalb'
      end if
      do isize = 1,mxsiz
         call LSKIP(2, iunit)
         read(iunit,*) wvnwcz(isize), wvnwcz(isize)
         call LSKIP(1, iunit)
         do ivwc = 1, mxwvwc
             read(iunit,*) wcwvn(ivwc),
     $                     (wc_ssa(isize, ivwc,icof),icof=1,mxcof)
             wcwvn(ivwc) = wcwvn(ivwc) * 1.e3  ! Change from microns to nm
         end do
      end do
      CLOSE(iunit)  

!-----------------------------------------------------------------------
!	...Calc coeffs for quantum yield of J(O3)
!-----------------------------------------------------------------------
      qyo3cst(:) = 0.06 + a1 * EXP( - ((wvn(:)-lamo1)/om1)**4 )
      qyo3coeff2(:) = a2 * EXP( - ((wvn(:)-lamo2)/om2)**2 )
      qyo3coeff3(:) = a3 * EXP( - ((wvn(:)-lamo3)/om3)**2 )

!-----------------------------------------------------------------------
!	...Set coeffs for t-dep crs of CFC's (was in SIGCFC)
!-----------------------------------------------------------------------
      acfc(1,:) = (/ -37.104170, -5.821802e-1, 9.997399e-3,
     $               -4.676527e-5, 6.850102e-8 /)
      bcfc(1,:) = (/ 1.073919, -1.627543e-2, 8.814085e-5,
     $               -1.981057e-7, 1.502234e-10 /)
      acfc(2,:) = (/ 341.085191, -7.273362, 5.498387e-2,
     $               -1.827552e-4, 2.238640e-7 /)
      bcfc(2,:) = (/ -1.660090, 3.079969e-2, -2.106719e-4,
     $               6.264984e-7, -6.781342e-10 /)
      acfc(3,:) = (/ -299.796165,5.104685,-3.363002e-2,
     $               9.580545e-5,-1.013456e-7 /)
      bcfc(3,:) = (/ -7.172742,1.483679e-1,-1.146290e-3,
     $               3.918805e-6,-4.999362e-9 /)
      acfc(4,:) = (/ -1087.881207,20.004100,-1.391989e-1,
     $               4.282793e-4,-4.938351e-7 /)
      bcfc(4,:) = (/ 12.493465,-2.393714e-1,1.714214e-3,
     $               -5.439298e-6,6.454833e-9 /)
      acfc(5,:) = (/ -106.029241, 1.503771, -8.247614e-3,
     $               1.420607e-5, 0. /)
      bcfc(5,:) = (/ -1.339882e-1, 2.740485e-3, -1.802848e-5,
     $               3.853998e-8, 0. /)
      acfc(6,:) = (/ -134.797197, 1.708389, -9.153990e-3,
     $               2.164407e-5, -1.986293e-8 /)
      bcfc(6,:) = (/ 3.306975e-1, -5.095714e-3, 2.936073e-5,
     $               -7.619773e-8, 7.682522e-11 /)
      acfc(7,:) = (/ 62.563060, -2.006832, 1.659204e-2,
     $               -5.646547e-5, 6.745870e-8 /)
      bcfc(7,:) = (/ -9.175482e-1, 1.857479e-2, -1.385710e-4,
     $               4.506561e-7, -5.380311e-10 /)
      acfc(8,:) = (/ 46.52, -1.457962, 1.146929e-2,
     $               -3.762666e-5, 4.326408e-8 /)
      bcfc(8,:) = (/ 9.340858e-1, -1.688734e-2, 1.148689e-4,
     $               -3.488086e-7, 3.994462e-10 /)
      acfc(9,:) = (/ -160.495098, 2.480670, -1.520180e-2,
     $               3.841242e-5, -3.437259e-8 /)
      bcfc(9,:) = (/ -1.529573, 3.524763e-2, -2.995072e-4,
     $               1.112950e-6, -1.525877e-9 /)
      zlambd = (/ ( 1.e3/(8.021 - 5.054e-2*REAL(iv)), iv = 1,mxwvn ) /)


      end subroutine PHO_INIT
