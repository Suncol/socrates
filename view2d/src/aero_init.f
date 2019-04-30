! subversion Id for THIS file : $Id: aero_init.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/aero_init.f $
!-----------------------------------------------------------------------

      subroutine AERO_INIT( aero_surf )
!-----------------------------------------------------------------------
!        All the aerosols-related variables (for chemistry or radiation)
!        are initialized here
!-----------------------------------------------------------------------

      use ALLCO, only : zkm
      use SIM_CONTROLS, only : het_sw, data_dir
      use PHO_AERO, only : aeabs, aedat1, aerom
      use AEROR, only : aerorad
      use VEN1, only : t2d
      use TROPOPAUSE
      use ASCII_UTILS, only : NAVU, LSKIP

      implicit none

!-----------------------------------------------------------------------
!  	... Dummy arguments
!-----------------------------------------------------------------------
      real, intent(out) :: aero_surf(35,121)

!-----------------------------------------------------------------------
!  	... Local variables
!-----------------------------------------------------------------------
      integer  ::  il, iunit, ios, lat, iz
      character(len=64) :: filenm

!-----------------------------------------------------------------------
!  	... Read in aerosol extinction coeff from Hitchman et al. (1993)
!           6-35 km
!-----------------------------------------------------------------------
      aedat1 = 0.

      filenm = data_dir(:LEN_TRIM(data_dir)) // 'aero_hitchman.dat'
      iunit = NAVU()
      OPEN( unit = iunit,
     $      file = filenm,
     $      form = 'formatted',
     $      status = 'OLD',
     $      iostat = ios )
      if( ios /= 0 ) then
	 write(*,*) 'AERO_INIT: Error reading ',filenm
	 write(*,*) '           Error = ',ios
	 stop
      end if
      call LSKIP( 19, iunit)
      do lat = 1,35
         READ(iunit,*) (aedat1(lat,iz),iz=7,36)
      end do
      CLOSE( iunit )

!-----------------------------------------------------------------------
!  	... Calculate wavelength dependent coeff. for extinc coeff,
!           and single scattering albeo and assymetry factor for
!           typical aged volcanic size distribution.
!     (Handbood of Geophysics and Space Environment ,1985 Chap. 18)
!     (in aerosol2.f)
!-----------------------------------------------------------------------
      call WAVAERO()

      where( aedat1 == -999 )
	 aedat1 = 0.e0
      elsewhere
	 aedat1 = 1.01e-4 * aedat1
      endwhere

!-----------------------------------------------------------------------
!  	... Estimate aerosol mass concentration (g/cm3) through extinction coeff.
!           conversion from Jaager and Hofman (1991)
!-----------------------------------------------------------------------
      do lat = 1,35
         do iz = 1,121
            aerom(lat,iz) = aedat1(lat,iz)*1.e-9
     $                    * (.02*REAL(iz - izm(lat)) + .95)
         end do
      end do

!-----------------------------------------------------------------------
!   	... Estimate  aerosol absorption coeff. for CCMR (cm2/g) from
!           at 1 um
!-----------------------------------------------------------------------
      where( aerom == 0. )
	 aeabs = 0.
      elsewhere
         aeabs = aedat1 * 1.e-5 / aerom
      end where

!-----------------------------------------------------------------------
!    	... Wavelength dependence for CCMR  (100-3000 cm-1,  100cm-1 interval)
!           in aerosol2.f
!-----------------------------------------------------------------------
      call WAVAERO2()

!-----------------------------------------------------------------------
!     	... Aerosol mass mixing ratio for CCMR cooling
!-----------------------------------------------------------------------
      if( het_sw(7) == 1 ) then
         do lat = 1,35
!-----------------------------------------------------------------------
!	... Aerosol mass concentration/ air mass concentration
!-----------------------------------------------------------------------
            aerorad(:,lat) = aerom(lat,:)
     $                     / (1013.25*(EXP( -zkm(:)/7.) )
     $                               *3.478e-4/t2d(lat,:))
         end do
      else
         aerorad = 0.
      end if

      if( het_sw(6) == 1 ) then
!-----------------------------------------------------------------------
!   	... Read aerosol surface areas
!-----------------------------------------------------------------------
	 call AERO_SURF_AREA( aero_surf )
      else
         aero_surf(:,:) = 0.
      end if
      
      end subroutine AERO_INIT

!=======================================================================

      subroutine WAVAERO()

      use PHO_PARMS, only : mxwvn
      use PHO_VARS, only : wvn
      use PHO_AERO, only : coeff, omaer, gaer1
      use NUMERICAL, only : SPLINE, SPLINT

      implicit none

!---------------------------------------------------------------
!	... Local variables
!---------------------------------------------------------------
      integer :: i, iv, ier
      real, save :: wave(10)
      real, dimension(10) :: y1, y2, y3

!---------------------------------------------------------------
! 	... Extinc. coeff. (from 100 to 1000 nm)
!---------------------------------------------------------------
      real, save :: aaa(10) = (/ .0033, .0033, .0033, .0031,
     $                        .003, .0029, .0027, .0022, .002, .0016 /)
!---------------------------------------------------------------
! 	... Single scattering albedo
!---------------------------------------------------------------
      real, save :: bbb(10) = (/ .4, .6, .92, .94, .95, .95,
     $                           .96, .96, .96, .96 /)
!---------------------------------------------------------------
!	... Asymmetry factor
!---------------------------------------------------------------
      real, save :: ccc(10) = (/ .9, .83, .72, .71, .70,
     $                           .69, .685, .68, .67, .66/)

      wave = (/ (100.*FLOAT(i), i = 1,10) /)

!---------------------------------------------------------------
!	... Normalize coeff at observation wavelength
!           (hitchman et al. 1000 nm)
!---------------------------------------------------------------
      aaa = aaa / aaa(10)

      call SPLINE( wave, aaa, 10, 0., 0., y1, ier )
      call SPLINE( wave, bbb, 10, 0., 0., y2, ier )
      call SPLINE( wave, ccc, 10, 0., 0., y3, ier )
      do iv = 1,mxwvn
         call SPLINT( wave, aaa, y1, 10, wvn(iv), coeff(iv), ier ) 
         call SPLINT( wave, bbb, y2, 10, wvn(iv), omaer(iv), ier ) 
         call SPLINT( wave, ccc, y3, 10, wvn(iv), gaer1(iv), ier ) 
      end do

      end subroutine WAVAERO

!=======================================================================

      subroutine WAVAERO2()

      use GRID_DIMS, only : lmax
      use PHO_AERO, only : aeabs, aerabs

      implicit none

!---------------------------------------------------------------
!	... Local variables
!---------------------------------------------------------------
      integer :: i, l
!---------------------------------------------------------------
! 	... Extinc. coeff. (from 3.3 to 100 um)
!---------------------------------------------------------------
      real, save :: aaa(30) = (/ 0., 0., 4.e-5, 3.5e-5, 3.e-5,
     $                           4.5e-5, 5.e-5, 5.e-5, 5.e-5, 8.e-5,
     $                           1.e-4, 9.e-5, 7.e-5, 4.5e-5, 4.e-5,
     $                           3.e-5, 3.3e-5, 3.5e-5, 3.6e-5,
     $                           3.8e-5, 4.5e-5, 5.5e-5, 6.e-5,
     $                           7.5e-5, 9.5e-5, 1.e-4, 1.2e-4, 1.5e-4,
     $                           1.7e-4, 2.e-4 /)

!---------------------------------------------------------------
!	... Normalize coeff at observation wavelength
!           (hitchman et al. 1000 nm)
!---------------------------------------------------------------
      aaa = aaa / 1.6e-3
      do i = 1,30
         do l = 1,lmax
            aerabs(l,:,i) = aeabs(l,:) * aaa(i)
         end do
      end do
  
      end subroutine WAVAERO2
