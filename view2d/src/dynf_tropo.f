! subversion Id for THIS file : $Id: dynf_tropo.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/dynf_tropo.f $
!-----------------------------------------------------------------------
      subroutine DYNF_TROPO( daynum, ftrop )
!-----------------------------------------------------------------------
!     	... Set the tropospheric forcing term for streamfunction equation
! Max (most negative) value is reached at WINTER solstice. The present 
! param has almost no influence on tropo circulation (symmetrical Hadley
! cell at solstice) and some (good) influence on tropo T near poles
!-----------------------------------------------------------------------
      use PHYS_CST, only : d2r
      use GRID_DIMS, only : lmax, niz
      use ALLCO, only : phir

!-----------------------------------------------------------------------
!     	... Dummy args
!-----------------------------------------------------------------------
      real, intent(in) :: daynum
      real, dimension(lmax,niz), intent(out) :: ftrop   ! m/s2

!-----------------------------------------------------------------------
!     	... Local variables
!-----------------------------------------------------------------------
      integer :: iz
      real, dimension(lmax) :: tfac = 0.

      tfac(1:11) = .5 * ( 3. + COS(daynum*d2r) )     ! Southern Hemisphere
      tfac(25:35) = 3. + COS( (daynum-182.)*d2r )    ! Northern Hemsphere
     
      do iz = 1, 21
         ftrop(:,iz) = -1.e-5 * tfac(:) 
     $     * ( 1. + COS(6.*phir(:)) ) 
     $     * ( EXP(-FLOAT(iz-8)**2/4.) 
     $        + 0.25*EXP(-1.5*FLOAT(iz-4)**2) )
      end do
      
      end subroutine DYNF_TROPO

