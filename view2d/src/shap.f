! subversion Id for THIS file : $Id: shap.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/shap.f $
!-----------------------------------------------------------------------

      subroutine SHAP( z, j1, j2, nf )
!-----------------------------------------------------------------------
!     	... Shapiro filter
!-----------------------------------------------------------------------

      use GRID_DIMS, only : lmax, niz

      implicit none

!-----------------------------------------------------------------------
!     	... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in) :: j1, j2
      integer, intent(in) :: nf(niz)
      real, intent(inout) :: z(lmax,*)

!-----------------------------------------------------------------------
!     	... Local variables
!-----------------------------------------------------------------------
      integer ::  j, nfh, n
      real    ::  x(lmax),  y(lmax)
      real    ::  xfac

      do j = j1,j2
         nfh = nf(j)/2
	 xfac = 1. / (4.**nf(j))
         y = z(:,j)
         do n = 1,nfh
            x(2:lmax-1) = y(1:lmax-2) - 2.*y(2:lmax-1) + y(3:lmax)
            x(1) = y(2) - y(1)
            x(lmax) = y(lmax-1) - y(lmax)
            y(2:lmax-1) = x(1:lmax-2) - 2.*x(2:lmax-1) + x(3:lmax)
            y(1) = x(2) - x(1)
            y(lmax) = x(lmax-1) - x(lmax)
         end do
         z(:,j) = z(:,j) - y * xfac
      end do

      end subroutine SHAP
