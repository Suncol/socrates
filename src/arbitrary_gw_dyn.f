! subversion Id for THIS file : $Id: arbitrary_gw_dyn.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/arbitrary_gw_dyn.f $
!-----------------------------------------------------------------------
      subroutine ARBITRARY_GW_DYN( cal_day, fx )
!-----------------------------------------------------------------------
!	... Set an arbitrary fx (momentum deposition by gravity waves)
!       ... The tuning parameters are nw, izb, izc, and fxmax
!           where *_s for short "summer" season (see Time:) and *_ns for
!                       the rest of the year
!           nw "blocks" of fx will be set to fxmax as a function of...
!           Altitude: from izb to izc, EXP decays below izb & above izc
!           Latitude: SQRT(ABS(SIN(latitude)))
!           Time: see local function TDEP_FX_GW
!       ... The nw "blocks" are merged to form fx: at each gridpoint, fx is
!           set to the block which has the biggest absolute value
!
!                             socrates v6s30, june 2001, simonc@oma.be
!-----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use ALLCO, only : phi, phir
      use DIAG_CONTROLS, only : diags, ldiag
      use TRANSFORM, only : SMOOTHV, SMOOTHL

      implicit none
!-----------------------------------------------------------------------
!	... Parameters - *_s for summer values
!-----------------------------------------------------------------------
      integer, parameter :: nw = 3
      integer, parameter, dimension(nw) ::  
     $    izb_ns = (/ 101,   81,   61 /),    izb_s = (/ 91, 81, 61 /),
     $    izc_ns = (/ 111,   91,   71 /),    izc_s = (/101, 91, 71 /)
      real, parameter, dimension(nw) ::                             
     $  fxmax_ns = (/ -45., -20., -35. /), fxmax_s = (/ 85., 0., 0. /) ! m/s/day

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      real, intent(in) :: cal_day
      real, dimension(lmax,niz), intent(out) :: fx    ! m/s2

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer :: l, iz, k, izb(nw), izc(nw)
      real    :: tdep_sh, tdep_nh, tdep, fac
      real    :: fxmax(nw), fxprof(nw,niz) = 0.           ! m/s2
      
      tdep_nh = TDEP_FX_GW( cal_day )
      tdep_sh = TDEP_FX_GW( cal_day - 365./2 )
      
      do l = 1, lmax
         tdep = tdep_sh
         if( phi(l) > 0. ) tdep = tdep_nh
         if( tdep > 0. ) then                        ! summer
            izb(:) = izb_s(:)
            izc(:) = izc_s(:)
            fxmax(:) = fxmax_s(:) / 86400.
          else
            izb(:) = izb_ns(:)
            izc(:) = izc_ns(:)
            fxmax(:) = - fxmax_ns(:) / 86400.
         end if
         fac = tdep * ABS( SIN( 1.5*phir(l) ) ) ! SQRT( ABS( SIN( phir(l) ) ) )
         
         fx(l,:) = 0.
         do k = 1, nw
            fxprof(k,izb(k):izc(k)) = fac * fxmax(k)

!-----------------------------------------------------------------------
!	... Exponential decay below breaking levels & above critical levels
!-----------------------------------------------------------------------
            do iz = 21, izb(k) - 1                
               fxprof(k,iz) = EXP(REAL(iz-izb(k))/ 7.) *fxprof(k,izb(k))
            end do
            do iz = izc(k) + 1, niz              
               fxprof(k,iz) = EXP(REAL(izc(k)-iz)/ 7.) *fxprof(k,izc(k))
            end do
            
!-----------------------------------------------------------------------
!	... Combine the nw blocks with the MERGE function
!           i.e. set fx to fxk if fxk is the biggest, in absolute value
!-----------------------------------------------------------------------
            fx(l,:) = MERGE( fxprof(k,:), fx(l,:), 
     $             MASK= ( fxprof(k,:)>0. .and. fxprof(k,:)>fx(l,:) )
     $              .or. ( fxprof(k,:)<0. .and. fxprof(k,:)<fx(l,:) ) )
         end do
      end do
 
      call SMOOTHV( fx, MINVAL(izb_ns)-3, MAXVAL(izc_ns)+3, 3 )
      call SMOOTHL( fx, 15, niz, 3 )

!=======================================================================

      contains
      
      real function TDEP_FX_GW( cal_day )
!-----------------------------------------------------------------------
!	... Time dependency factor, cal_day=julian day of year (0->365)
!           for Northern Hemisphere, send cal_day-365/2 for Southern Hemisphere
!           This fct is positive during the "summer" season, from ds-nd
!           to ds+nd where ds is summer solstice and nd is *the* fudge parameter 
!-----------------------------------------------------------------------
      use PHYS_CST, only : pi

      real, intent(in) :: cal_day
      
      real, parameter :: ds = 172., ! calday for summer solstice in NH
     $                   nd = 71.   ! 51 at v6s30a

      real :: d
      
      d = cal_day
      if( d < 0 ) d = d + 365.

      if( ABS( d - ds ) < nd ) then     ! "summer" season
         TDEP_FX_GW = SQRT( ABS( COS( (d-ds) * pi / (2.*nd) ) ) )
       else
         if( d <= ds-nd ) d = d + 365.
         TDEP_FX_GW = -ABS( COS( (d-ds-0.5*365.) * pi / (365.-2.*nd) ) )
      end if
      
      end function TDEP_FX_GW

      end subroutine ARBITRARY_GW_DYN
