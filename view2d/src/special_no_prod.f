! subversion Id for THIS file : $Id: special_no_prod.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/special_no_prod.f $
!-----------------------------------------------------------------------
      subroutine SPECIAL_NO_PROD( time, pcst_no, phve_no )
!----------------------------------------------------------------------
!     All extraneous productions of NOy are grouped here. We suppose
!     that they are always productions of NO.
!     pcst_no = lightning + cosmic rays + aurorae (energetic electrons)
!                  is independent of solar local time
!     phve_no = solar soft x-ray -> photoelectrons -> N2+ -> N(2D) -> NO
!     SLT dependency in CHEMDR, where prod_no = pcst_no + cos(sza)*phve_no
!     Values computed here are for Solar Minimum, dependency on solar 
!          activity is in SOLCYCLES
!                             v6s16 - simonc@oma.be - February 2001
!     v6s16c: results adjusted to CIRA96 (Barth-1996) results at solmin
!       -> prods by soft x-ray & aurorae divided by 4 
!     v6s17: solfactor, between 0 & 1, for solar activity level (SOLCYCLES)
!            influences pcst_no, phve_no & flxub(vid_no), the upper boundary 
!            flux for NO, transferred from BOUNDY to here
!     v6s25b: divide prods by 2, not 4, to adjust to Siskind/et-al-1998
!----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use ALLCO, only : phi
      use ZGRID, only : zgeo
      use SIM_CONTROLS, only : mlt_sw
      use DIAG_CONTROLS, only : diags, ldiag, zdiag
      use SPC_NAMES, only : vid_no
      use BOUNDARIES, only : ubc         ! ubc(vid_no) is output
      use SUN_VARS, only : solfactor
      use TIME_CONTROLS, only : TIMING
      use SUN_UTILS, only : SUN_DAYLENGTH
      use TRANSFORM, only : SMOOTHV, SMOOTHL

      implicit none

!----------------------------------------------------------------------
!	... Dummy args - productions in molec/cm3/s
!----------------------------------------------------------------------
      type( TIMING ), intent(in) :: time
      real, intent(out), dimension(lmax,niz) :: pcst_no, phve_no
      
!----------------------------------------------------------------------
!	... Local vars
!----------------------------------------------------------------------
      integer :: l
      real :: td, declin
      real, dimension(lmax,niz) :: pli, pcr, paur
      real, dimension(lmax,niz), save :: pcst, phve
      logical, save :: entered = .false.
      
      if( .not. entered ) then
         pli = 0.
         if( mlt_sw(4) > -2 ) call LIGHTNING( pli )
         pcr = 0.
         if( mlt_sw(4) > -1 ) call COSMIC( pcr )
         phve = 0.
         if( mlt_sw(4) >  0 ) call SOFTXRAY( phve )
         phve = 0.5 * phve
         paur = 0.
         if( mlt_sw(4) >  1 ) call AURORA( paur )
         paur = 0.5 * paur

         pcst = pli + pcr + paur
         call SMOOTHV( pcst, 1, niz, 1 )
         call SMOOTHL( pcst, 1, niz, 1 )
         entered = .true.
      end if

      pcst_no = pcst
      phve_no = phve

!----------------------------------------------------------------------
!    ... set NOy=NO upper boundary flux using table 3 in Solomon et.al, 
!        JGR, p. 7211, 1982: This is at 120km, solmin, december, 
!        DAYTIME AVERAGE, zero in polar night -> -4.e8*daylength/24. 
!        v6s16c: divide flux by 4 to adjust to CIRA96 at solmin
!----------------------------------------------------------------------
      ubc(vid_no)%val(:) = 0.
      if( mlt_sw(4) > 0 ) then
         do l = 1, lmax
            td = SUN_DAYLENGTH( zgeo(l,niz), phi(l), time%cal_day )
            ubc(vid_no)%val(l) = -4.e8 * td / 24.
         end do
         ubc(vid_no)%val(:) = 0.25 * ubc(vid_no)%val(:)
      end if

!-----------------------------------------------------------------------
!   ... Special productions of NO in MLT, and upper boundary flux of NO
!         depend on solar activity level set by solfactor in SOLCYCLES
!-----------------------------------------------------------------------
      if( mlt_sw(10) == 1 ) then  
         pcst_no(:,91:niz) = ( 1. + solfactor ) * pcst_no(:,91:niz)
         phve_no(:,:) = ( 1. + solfactor ) * phve_no(:,:)
         ubc(vid_no)%val(:) = ( 1. + solfactor ) * ubc(vid_no)%val(:)
      end if
     
      if( diags ) then
         write(*,'(3(a,f8.3),a,es12.3)') 'SPECIAL_NO_PROD: solfactor= ',
     $    solfactor,' ; pcst_no= ',pcst_no(ldiag,zdiag),' ; phve_no= ',
     $    phve_no(ldiag,zdiag),' ; flxub(NO)= ',ubc(vid_no)%val(ldiag)
      end if
      
      contains

!======================================================================
      
      subroutine LIGHTNING( pli ) 
!----------------------------------------------------------------------
!     Production of noy by lightning, only at -60S<lat<60N & 0<=z<=16km
!----------------------------------------------------------------------
      implicit none
      real, parameter  ::  plieq = 1.e3
      real, intent(out), dimension(lmax,niz) :: pli
      integer :: lat
 
      pli(12:24,1:16) =  plieq
      do lat = 7, 11
         pli(lat,1:13) =  plieq * REAL(lat - 6) / 6.
      end do
      do lat = 25, 29
         pli(lat,1:13) =  plieq * REAL(30 - lat) / 6.
      end do

      end subroutine LIGHTNING

!======================================================================
 
      subroutine COSMIC( pcr )
!----------------------------------------------------------------------
!     NOy production by cosmic-ray from Heaps, Planet. Space Sci., 
!      vol. 26, p. 513, 1978 (at solmax but we suppose pcr indep of 
!      solar activity). Notice we use *initial* hm2d, not interactive
!----------------------------------------------------------------------
      use ALLCO, only : phir, zkm
      use CONC, only : hm2d

      implicit none
      real, intent(out), dimension(lmax,niz) :: pcr
      integer :: lat
      real :: bf, alf, at, bt, ct, x, gan, gam, sin4, cof

      do lat = 1, lmax
         gan = .6 + .8 * COS( phir(lat) )
         gam = 1. - gan
         sin4 = SIN( phir(lat) )**4
         cof = 1.3*(1.74e-18 + 1.93e-17*sin4)
         bf = (3.03e17**gam) * (hm2d(lat,19)**gan)
         alf = -bf*gan/7.e5
         at = 14.4*alf - 2.24*bf
         bt = -2.6*alf + .36*bf
         ct = .1*alf - .01*bf

         pcr(lat,1:8) = 0.
         pcr(lat,9:18) = cof * (at + zkm(9:18) * (bt + zkm(9:18) * ct))
         pcr(lat,9:18) = MAX( 0., pcr(lat,9:18) )
         where( hm2d(lat,19:121) < 3.03e17 )
	    pcr(lat,19:121) = cof * hm2d(lat,19:121)
          else where
	    pcr(lat,19:121) = cof * (3.03e17**gam) *
     $                                     ( hm2d(lat,19:121)**gan )
         end where
      end do

      end subroutine COSMIC

!=======================================================================
      
      subroutine SOFTXRAY( phv )
!-----------------------------------------------------------------------
!    ... Production of NO by N(2D) + O2 : see Siskind/et-al-1990
!        solar soft X-rays (2-10nm) -> photoelectrons -> N2+ -> N(2D) -> NO
!        Crudely adapted from fig. 1 of Siskind/Rusch-1992 at solmin.
!        Max at 110km where phv=1000 molec/cm3/s, 100* bigger than pio_hv
!        in old NOYION ! Solar activity depedency done in SOLCYCLES
!                                 v6s16 - simonc@oma.be - February 2001
!-----------------------------------------------------------------------
      use ALLCO, only : phir, zkm
      use TRANSFORM, only : SMOOTHV
  
      implicit none
      real, intent(out), dimension(lmax,niz) :: phv
      integer :: iz, l
      
      phv(:,1:90) = 0.
      do iz = 91, 111
         phv(:,iz) = 10.**( 1. + ( zkm(iz)-90. ) / 10. )
      end do
      do l = 1, lmax
         phv(l,112:niz) = phv(l,111)
      end do
      call SMOOTHV( phv, 1, niz, 4 )
      
      end subroutine SOFTXRAY

!=======================================================================
      
      subroutine AURORA( pio_e )
!-----------------------------------------------------------------------
!     	... Production of NO by aurorae: from N2 + energetic particles
!           Extracted from NOYION at v6s16, the max was from 85 to 70
!           degrees lat, replaced by max *at* 70 degs and arbitrary f(lat)
!           since auroral activity centered on geomagnetic pole
!           at 70 degs, see max of NO at 70 degs, e.g. in Barth-1996 (CIRA96)
!           v6s25b: slower equatorward decrease, max at 105-115km, see
!           Solomon/et-al-1999                   simonc@oma.be - Apr 2001
!-----------------------------------------------------------------------
      implicit none

      real, intent(out), dimension(lmax,niz) :: pio_e
      real    :: pioy, f(lmax)
      integer :: l,iz

      f(:) = 0.
!                 85S   80S   75S   70S   65S   60S  55S  50S      lats, S.H.
      f(1:8) = (/ 0.15, 0.3,  0.6,   1.,  0.8,  0.6, 0.3, 0.15 /)
      f(lmax:28:-1) = f(1:8)                ! symmetrical, Northern Hemisphere
      do l = 1, lmax
         pio_e(l,107:115) = 1176.475 * f(l) ! -> 1000molec/cm3/s after smoothing
      end do
      do iz = 116, niz
         pio_e(:,iz) = 0.75 * pio_e(:,iz-1)
      end do
      do iz = 106, 91, -1
         pio_e(:,iz) = 0.8 * pio_e(:,iz+1)
      end do

      end subroutine AURORA

      end subroutine SPECIAL_NO_PROD
