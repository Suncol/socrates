! subversion Id for THIS file : $Id: kyy_spec.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/kyy_spec.f $
!-----------------------------------------------------------------------
      subroutine KYY_SPEC( cal_day, xky )
!----------------------------------------------------------------------
!       This routine is just a draft. Its purpose is to let the user
!       specify a latitude, altitude and day-of-the-year dependent 
!       horizontal diffusion coefficient (xky) instead of using the
!       coupled wave model by Wanli Wu.
!----------------------------------------------------------------------
      
      use GRID_DIMS, only : lmax, niz

      implicit none

!----------------------------------------------------------------------
!       ... Dummy args
!----------------------------------------------------------------------
      real, intent(in) :: cal_day
      real, dimension(lmax,niz), intent(out) :: xky
      
      xky(1:lmax,1:niz) = 3.e5			! m2/s

      if( cal_day < 180. ) then
         xky(21:33,51:91) = 2.e6
       else
         xky(3:17,51:91) = 2.e6
      end if
     
      end subroutine KYY_SPEC
