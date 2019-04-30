! subversion Id for THIS file : $Id: tropk.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/tropk.f $
!-----------------------------------------------------------------------

      subroutine TROPK( )
!---------------------------------------------------------------------
!   Specify eddy diffusion coefficients in troposphere
!---------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use VEN12, only : dyytrop, dzztrop   ! output
      use TROPOPAUSE, only : izm, izmMax

      implicit none
!----------------------------------------------------------------------
!       ... Parameters
!----------------------------------------------------------------------
      real, parameter :: dzzMax = 15.0              ! m2/s;  at the surface 

!---------------------------------------------------------------------
!	... Local variables
!---------------------------------------------------------------------
      integer       :: l, im, k
 
!-----------------------------------------------------------------------
! 	... Kyy in the troposphere (i.e., from k=1, to k=izm(l)
!           based on the Felming et al. 1999 paper.
!-----------------------------------------------------------------------
      dyytrop = 0.

      dyytrop(:, 2) = 3.0E6                ! m2/s
      dyytrop(:, 3:4) = 2.5E6          
      dyytrop(:, 5:6) = 2.0E6 
      dyytrop(:, 7:10) = 1.5E6 
      dyytrop(:, 11:12) = 1.0E6 
      dyytrop(:, 13:izmMax) = 0.5E6 

      do l=1, lmax
        if( izm(l) >= izmMax ) cycle
        dyytrop(l, izm(l)+1:izmMax) = 0.0
      end do

!-----------------------------------------------------------------------
! 	... Kzz in the troposphere (i.e., from k=1, to k=izm(l))
!-----------------------------------------------------------------------
      dzztrop = 0.
      do  l = 1,35
         im = izm(l) 
         do k = 1, im
            dzztrop(l,k) = dzzMax * ( 1. - REAL(k)/REAL(im) )
         end do
      end do

      end subroutine TROPK
