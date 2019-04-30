! subversion Id for THIS file : $Id: colden_pho.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/colden_pho.f $
!-----------------------------------------------------------------------
      subroutine COLDEN_PHO( cdeno2, cdento, dens, zs )
!-------------------------------------------------------------------
!  	... Integrate over single layers to obtain column densities,
!            using exponential or linear interpolation.
!-------------------------------------------------------------------
      use PHO_PARMS, only : mxcly, maxden

      implicit none

!-------------------------------------------------------------------
!	... Parameters
!-------------------------------------------------------------------
      real, parameter :: epsiln = 1.e-5

!-------------------------------------------------------------------
!	... Dummy args
!-------------------------------------------------------------------
      real, intent(in)     ::  dens(0:mxcly,maxden)
      real, intent(in)     ::  zs(0:mxcly)               ! in kilometers
      real, dimension(mxcly), intent(out) ::  cdeno2, cdento

!-------------------------------------------------------------------
!	... Local variables
!-------------------------------------------------------------------
      integer :: lc
      real    :: deltaz

      do lc = 1, mxcly
         deltaz = (zs(lc-1) - zs(lc)) * 1.e5    ! from km to cm
!-------------------------------------------------------------------
!   	... o2 ( oxygen )
!-------------------------------------------------------------------
         if( dens(lc-1,3) > 0. .or. dens(lc,3) > 0. ) then 
            if( (ABS(1.0 - dens(lc,3)/dens(lc-1,3))) > epsiln ) then
               cdeno2(lc) = 1. / (LOG(dens(lc,3) / dens(lc-1,3))) * 
     $                      (dens(lc,3) - dens(lc-1,3)) * deltaz 
            else
               cdeno2(lc) = .5*ABS((dens(lc-1,3) + dens(lc,3))*deltaz)
            end if
         else
            cdeno2(lc) = .5*ABS((dens(lc-1,3) + dens(lc,3) )*deltaz)
         end if
!-------------------------------------------------------------------
!   	... air ( total density of the atmosphere )
!-------------------------------------------------------------------
         if( dens(lc-1,1) > 0. .or. dens(lc,1) > 0. ) then 
            if( (ABS(1.0 - dens(lc,1)/dens(lc-1,1))) > epsiln ) then
               cdento(lc) = 1. / (LOG(dens(lc,1) / dens(lc-1,1))) * 
     $                      (dens(lc,1) - dens(lc-1,1)) * deltaz 
            else
               cdento(lc) = .5*ABS((dens(lc-1,1) + dens(lc,1))*deltaz)
            end if
         else
            cdento(lc) = .5*ABS((dens(lc-1,1) + dens(lc,1) )*deltaz)
         end if
      end do

      end subroutine COLDEN_PHO
