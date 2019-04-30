! subversion Id for THIS file : $Id: surf_cover.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/surf_cover.f $
!-----------------------------------------------------------------------

      subroutine SURF_COVER( tsurf, fry )
!-----------------------------------------------------------------------
!    ... Calculates the seaice and snow covers
!-----------------------------------------------------------------------
      use GRID_DIMS, only  : lmax
      use ALLCO, only : phi

      implicit none

!-----------------------------------------------------------------------
!    ... Dummy arguments
!-----------------------------------------------------------------------
      real, dimension(lmax), intent(in) :: tsurf
      real, dimension(lmax,3), intent(out) :: fry

!-----------------------------------------------------------------------
!    ... Local variables
!-----------------------------------------------------------------------
      real, dimension(lmax) :: robia, robib, robsa, robsb, tuse

      real, save :: tref(lmax)  = (/
     $ 225., 233., 245., 255., 265., 272., 276., 279.3,
     $ 283., 286.4, 289.3, 292.4, 294.3, 296.4, 297.7,
     $ 298.9, 299.5, 299.8, 299.7, 299.2, 298.7,298.,
     $ 296., 293., 290., 285.8, 282., 278., 275., 272.2,
     $ 267.5, 263.5, 260., 257.2, 253. /)

      real, save :: yoce(lmax) =  (/
     $  .00, .01, .30, .58, .94,1.00, .99, .97, .96, .95, .88, .79, .76,
     $  .75, .77, .78, .74, .77, .77, .75, .73, .67, .61, .56, .57, .53,
     $  .46, .40, .43, .36, .21, .39, .72, .76, .97 /)

!-----------------------------------------------------------------------
!         ... Ocean cover
!-----------------------------------------------------------------------
      fry(:,1) = yoce

!-----------------------------------------------------------------------
!         ... Sea ice cover
!-----------------------------------------------------------------------
      where( phi > 0. )
         robia = .173 + .00172*(4.9 - tref + 273.15)**2
         robib = -.0244 + .0000606*(3.1 - tref + 273.15)**2
         robib = MIN( robib,0. )
         fry(:,2) = robia + robib * (tsurf - 273.15)
      else where
         robia = -.1250 - .05625 * (tref - 273.15)
         robib = -.1028 - .005714 * (tref - 273.15)
         robib = MIN( MAX( robib,-.08 ),0. )
         robia = MIN( MAX( robia,0. ),1. )
         fry(:,2) = robia + robib * (tsurf - 273.15)
      end where
      where( phi > 0. .and. tref > 276.1 )
         fry(:,2) = 0.
      end where
      fry(:,2) = MIN( fry(:,2),.99 )
      where( fry(:,2) < .01 )
         fry(:,2) = 0.
      end where

!-----------------------------------------------------------------------
!         ... Land snow cover
!-----------------------------------------------------------------------
      tuse = MAX( tref,260.85 )
      where( phi < 0. )
         robsa = .95 - .004*(12.3 + tuse - 273.15)**2
         robsb = -.032 + .000035*(28.5 + tuse - 273.15)**2
         robsa = MIN( MAX( robsa,0. ),1. )
         robsb = MIN( robsb,0. )
         fry(:,3) = robsa + robsb*(tsurf - 273.15)
      else where
         robsa = .68 - .002*(12.3 + tuse - 273.15)**2
         robsb = -.062 + .000035*(28.5 + tuse - 273.15)**2
         robsa = MIN( MAX( robsa,0. ),1. )
         robsb = MIN( robsb,0. )
         fry(:,3) = robsa + robsb*(tsurf - 273.15)
      end where
      fry(:,3) = MIN( fry(:,3),1. )
      where( fry(:,3) < .01 )
         fry(:,3) = 0.
      end where

      end subroutine SURF_COVER
