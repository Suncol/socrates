! subversion Id for THIS file : $Id: transform.mod.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/transform.mod.f $
!-----------------------------------------------------------------------
      module TRANSFORM
      
      use GRID_DIMS, only : lmax, niz
      implicit none
      PRIVATE
      PUBLIC :: HDERIV, VDERIV, SMOOTHV, SMOOTHL

      CONTAINS  

!=======================================================================

      subroutine HDERIV( base, basedl, basedll )
!-----------------------------------------------------------------------
!     	... Form horizontal derivatives
!-----------------------------------------------------------------------
      use ALLCO, only : dlatr

      implicit none

!-----------------------------------------------------------------------
!     	... Dummy args
!-----------------------------------------------------------------------
      real, intent(in) :: base(lmax,niz)
      real, optional, dimension(lmax,niz), intent(out) ::
     $                    basedl, basedll

!-----------------------------------------------------------------------
!     	... Local variables
!-----------------------------------------------------------------------
      integer :: l
      real    :: deriv(lmax,niz)
      real, save :: dl, dl2
      logical, save :: entered = .false.

      if( .not. entered ) then
         dl = 1. / dlatr
         dl2 = .5*dl
         entered = .true.
      end if

      if( .not. PRESENT( basedl ) .and.
     $    .not. PRESENT( basedll ) ) then
	 return
      end if
!-----------------------------------------------------------------------
!     	... First order derivs at the boundaries
!-----------------------------------------------------------------------
      deriv(1,:) = dl * (base(2,:) - base(1,:))
      deriv(lmax,:) = dl * (base(lmax,:) - base(34,:))
!-----------------------------------------------------------------------
!     	... First order derivs in the interior
!-----------------------------------------------------------------------
      do l = 2,34
         deriv(l,:) = dl2 * (base(l+1,:) - base(l-1,:))
      end do
      if( PRESENT( basedl ) ) then
	 basedl(:,:) = deriv(:,:)
      end if
!-----------------------------------------------------------------------
!     	... Second order deriv
!-----------------------------------------------------------------------
      if( PRESENT( basedll ) ) then
         do l = 2,34
            basedll(l,:) = dl2 * (deriv(l+1,:) - deriv(l-1,:))
         end do
         basedll(1,:) = dl * (deriv(2,:) - deriv(1,:))
         basedll(lmax,:) = dl * (deriv(lmax,:) - deriv(34,:))
      end if

      end subroutine HDERIV
       
!=========================================================================== 

      subroutine VDERIV( base, basedz, basedzz )
!-----------------------------------------------------------------------
!     	... Form vertical derivatives
!-----------------------------------------------------------------------

      implicit none

!-----------------------------------------------------------------------
!     	... Parameters
!-----------------------------------------------------------------------
      real, parameter :: dz = 1.e-3
      real, parameter :: dz2 = 5.e-4

!-----------------------------------------------------------------------
!     	... Dummy args
!-----------------------------------------------------------------------
      real, intent(in) :: base(lmax,niz)
      real, optional, dimension(lmax,niz), intent(out) ::
     $                    basedz, basedzz

!-----------------------------------------------------------------------
!     	... Local variables
!-----------------------------------------------------------------------
      integer :: l
      real    :: deriv(lmax,niz)

      if( .not. PRESENT( basedz ) .and.
     $    .not. PRESENT( basedzz ) ) then
	 return
      end if
!-----------------------------------------------------------------------
!     	... First order derivs at the boundaries
!-----------------------------------------------------------------------
      deriv(:,1) = dz * (base(:,2) - base(:,1))
      deriv(:,niz) = dz * (base(:,niz) - base(:,niz-1))
!-----------------------------------------------------------------------
!     	... First order derivs in the interior
!-----------------------------------------------------------------------
      do l = 1,lmax
         deriv(l,2:niz-1) = dz2 * (base(l,3:niz) - base(l,1:niz-2))
      end do
      if( PRESENT( basedz ) ) then
	 basedz(:,:) = deriv(:,:)
      end if
!-----------------------------------------------------------------------
!     	... Second order deriv
!-----------------------------------------------------------------------
      if( PRESENT( basedzz ) ) then
         do l = 1,lmax
            basedzz(l,2:niz-1) = dz2 
     $                           * (deriv(l,3:niz) - deriv(l,1:niz-2))
         end do
         basedzz(:,1) = dz * (deriv(:,2) - deriv(:,1))
         basedzz(:,niz) = dz * (deriv(:,niz) - deriv(:,niz-1))
      end if

      end subroutine VDERIV

!=========================================================================== 

      subroutine SMOOTHV( y, kl, ku, nsmov )
!----------------------------------------------------------------------
!     ... Smoothing by a triangular function (from j1+1 to j2-1 level)
!----------------------------------------------------------------------
      implicit none

      real, parameter    :: z = 2.
      real, parameter    :: factor = 1./(1. + z)

!----------------------------------------------------------------------
!  	... Dummy args
!----------------------------------------------------------------------
      integer, intent(in) :: kl, ku, nsmov
      real, intent(inout) :: y(lmax,*)

!----------------------------------------------------------------------
!  	... Local variables
!----------------------------------------------------------------------
      integer :: n, j, k
      real :: x(niz)

      do n = 1,nsmov
         do j = 1,lmax
            do k = kl+1,ku-1
               x(k) = .25*(y(j,k-1) + z*y(j,k) + y(j,k+1))
	    end do
            x(ku) = factor*(y(j,ku)*z + y(j,ku-1))
            x(kl) = factor*(y(j,kl)*z + y(j,kl+1))
            y(j,kl:ku) = x(kl:ku)
         end do
      end do
      
      end subroutine SMOOTHV

!=========================================================================== 

      subroutine SMOOTHL( y, kl, ku, nsmol )
!----------------------------------------------------------------------
!  	... Smoothing by a triangular function
!----------------------------------------------------------------------

      implicit none

      real, parameter    :: z = 2.
      real, parameter    :: factor = 1./(1. + z)
      
!----------------------------------------------------------------------
!  	... Dummy args
!----------------------------------------------------------------------
      integer, intent(in) :: kl, ku, nsmol
      real, intent(inout) :: y(35,*)

!----------------------------------------------------------------------
!  	... Local variables
!----------------------------------------------------------------------
      integer :: n, k, j
      real  ::  x(35)

      do n = 1,nsmol
         do k = kl,ku
            x(1) = factor*(z*y(1,k) + y(2,k))
            do j = 2,34
               x(j) = .25*(y(j-1,k) + z*y(j,k) + y(j+1,k))
	    end do
            x(35) = factor*(y(34,k) + z*y(35,k))
            y(:,k) = x(:)
         end do
      end do
      
      end subroutine SMOOTHL

!=========================================================================== 

      end module TRANSFORM
