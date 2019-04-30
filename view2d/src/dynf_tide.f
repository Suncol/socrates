! subversion Id for THIS file : $Id: dynf_tide.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/dynf_tide.f $
!-----------------------------------------------------------------------

      subroutine DYNF_TIDE()
!-----------------------------------------------------------------------
!      	... Calculate momentum deposit and diffusion generated 
!           by breaking of diurnal tide
!-----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use ALLCO, only : phir
      use TIDE1, only : dkt, dktz, ftx        ! output
      use TRANSFORM, only : SMOOTHV, SMOOTHL

      implicit none

!-----------------------------------------------------------------------
!	... Parameters
!-----------------------------------------------------------------------
      real, parameter    :: dzi = 1.e-3
      real, parameter    :: dz2i = .5 * dzi
!-----------------------------------------------------------------------
!      zbreak=85km
!      dt0     diffusion generated at (z.ge.85.and.le.110),
!                                      (phi.lt.abs(30))
!      fto     momentum  deposition at (z.ge.85.and.le.110),
!                                      (phi.lt.abs(30))
!-----------------------------------------------------------------------
      real, parameter  :: dto = 1.83e2
      real, parameter  :: fto = -1.888e-4

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer :: i, j, l
      real :: s1, s2, s3, s4, s5, s6
      real, dimension(niz) :: dt1, ft1

!-----------------------------------------------------------------------
!    	... Diffusion momentum flux below zbreak
!-----------------------------------------------------------------------
      do j = 1,86
         s1 = EXP( (FLOAT(j) - 85.)/5. )
         dt1(j) = dto*s1
         ft1(j) = fto*s1
      end do
!-----------------------------------------------------------------------
!  	... Diffusion and momentum flux from 85km to 110km
!-----------------------------------------------------------------------
      dt1(87:111) = dto
      ft1(87:111) = fto

!-----------------------------------------------------------------------
! 	... Diffusion and momentum flux at (z.gt.110km)
!-----------------------------------------------------------------------
      do j = 112,niz
         s2 = EXP( (85.-FLOAT(j)) /5. )
         dt1(j) = dto*s2
         ft1(j) = fto*s2
      end do

!-----------------------------------------------------------------------
!   	... Diffusion and momentum flux away from equator (phi.lt.-30)
!-----------------------------------------------------------------------
      do i = 1,11
         s3 = EXP( -(phir(i)/.3489)**2 )
         s5 = EXP( -(phir(i)/.3489)**2 )
         dkt(i,:) = dt1(:)*s5
         ftx(i,:) = ft1(:)*s3
      end do

!-----------------------------------------------------------------------
!     	... Diffusion and momentum flux in the equatorial region
!           (phi.eq.-30 to +30)
!-----------------------------------------------------------------------
      do i = 12,24
         dkt(i,:) = dt1(:)
         ftx(i,:) = ft1(:)
      end do

!-----------------------------------------------------------------------
!   	... Diffusion and momentum flux away from equator (phi.gt.30)
!-----------------------------------------------------------------------
      do i = 25,35
         s4 = EXP( -(phir(i)/0.3489)**2 )
         s6 = EXP( -(phir(i)/0.3489)**2 )
         dkt(i,:) = dt1(:)*s6
         ftx(i,:) = ft1(:)*s4
      end do
 
!-----------------------------------------------------------------------
!     	... Smooth in the meridional direction
!-----------------------------------------------------------------------
      call SMOOTHL( ftx, 15, niz, 3 )
      call SMOOTHL( dkt, 15, niz, 3 )
 
!-----------------------------------------------------------------------
!     	... Smooth in the vertical direction
!-----------------------------------------------------------------------
      call SMOOTHV ( ftx, 15, niz, 3 )
      call SMOOTHV ( dkt, 15, niz, 3 )
 
      dkt(:,:) = .05 * dkt(:,:)
  
      do l = 1,lmax
         dktz(l,2:niz-1) = (dkt(l,3:niz) - dkt(l,1:niz-2)) * dz2i
      end do
      dktz(:,1) = (dkt(:,2) - dkt(:,1)) * dzi
      dktz(:,niz) = (dkt(:,niz) - dkt(:,niz-1)) * dzi

      end subroutine DYNF_TIDE
