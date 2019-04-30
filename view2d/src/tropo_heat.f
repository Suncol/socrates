! subversion Id for THIS file : $Id: tropo_heat.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/tropo_heat.f $
!-----------------------------------------------------------------------

      subroutine TROPO_HEAT( )
!-----------------------------------------------------------------------
!  	... Fixed heating rate in the troposphere. Created at v6s15a when
!           all CCM1 heating tropo calc was removed. Based on lat_heat.f
!           which calculated the latent heat released by water condensation
!           to clouds, as in Peixoto & Oort, "Physics of Climate", p.321
!                                           simonc@oma.be, Jan 2001  
!-----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use ALLCO, only :     zkm, phi
      use TROPIC, only :    clh                ! output of this routine, K/day
      use TRANSFORM, only : SMOOTHV, SMOOTHL

      implicit none

!-----------------------------------------------------------------------
!  	... Local variables
!-----------------------------------------------------------------------
      integer :: l

      clh(:,:) = 0.                         !   ... Initialize to zero
      clh(:,2) = 0.3
      do l = 5, 8                           ! -65 to -50
         clh(l,3:4) = 2.25 * zkm(3:4)
      end do
      do l = 9,11                           ! -45 to -35
         clh(l,5:6) = 2.15 * zkm(5:6)
      end do
      do l = 17, 19                         ! -5 to 5 
         clh(l,4:10) = 1.0 * zkm(4:10)
      end do
      do l = 25, 27                         ! 35 to 45
         clh(l,5:6) = 2.86 * zkm(5:6)
      end do
      do l = 28, 31                         ! 50 to 65 
         clh(l,3:4) = 2.75 * zkm(3:4)
      end do

      call SMOOTHL(clh, 1, 7, 30)
      call SMOOTHL(clh, 8, 20, 3)
      call SMOOTHV(clh, 1, 20, 10)

      clh(17:19, 12:14) = 2.0 * clh(17:19, 12:14)

      call SMOOTHV(clh, 1, 20, 1)
      call SMOOTHL(clh, 1, 20, 1)

      clh = 0.5 * clh
      
      end subroutine TROPO_HEAT
