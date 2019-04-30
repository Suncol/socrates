! subversion Id for THIS file : $Id: dyn_balance.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/dyn_balance.f $
!-----------------------------------------------------------------------

      subroutine DYN_BALANCE()
!-----------------------------------------------------------------------
!	... Correction for global mass balance for w
!-----------------------------------------------------------------------
      use ALLCO, only : phir, dlatr
      use CONC, only : hm2d
      use VEN6, only : w

      implicit none

!----------------------------------------------------------------
!	... Local variables
!----------------------------------------------------------------
      integer :: k
      real    :: a
      real, dimension(35) ::  side, side2

      do k = 1,121
         side(:) =  w(:,k) * COS( phir(:) )
         side2(:34) = .5*(side(:34) + side(2:35))
         a =  dlatr * SUM( side2(:34) )
         w(:,k) = w(:,k) - .5*a  
      end do

      end subroutine DYN_BALANCE
