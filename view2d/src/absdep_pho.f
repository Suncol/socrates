! subversion Id for THIS file : $Id: absdep_pho.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/absdep_pho.f $
!-----------------------------------------------------------------------

      subroutine ABSDEP_PHO( xsect, dens, zs, babso, babso3 )
!------------------------------------------------------------------
!     SUBROUTINE ABSorption optical DEPth
!
!     Calculates optical depth due to absorption by oxygen (O2) , 
!     ozone (O3) and nitrogendioxyde (NO2) using the trapezoidal rule .
!------------------------------------------------------------------

      use PHO_PARMS, only : mxcly, maxden, phtmax

      implicit none

!------------------------------------------------------------------
!	... Dummy args
!------------------------------------------------------------------
      real, intent(in)    ::  xsect(phtmax,0:mxcly)
      real, intent(in)    ::  dens(0:mxcly,maxden)
      real, intent(in)    ::  zs(0:mxcly)
      real, dimension(mxcly), intent(out)   ::  babso,	! Total Abs.coeff.
     $                                          babso3 	! Abs coeff for O3 

!------------------------------------------------------------------
!	... Local variables
!------------------------------------------------------------------
      integer, parameter :: jo2 = 1, jo3 = 2, jno2 = 7
      real, dimension(mxcly) ::  deltaz,
     $                           babso2	             ! Abs coeff for O2

!------------------------------------------------------------------
!	... Note: zs is in km and deltaz in cm
!                 The 5.e4 factor is because the formulas below
!                 have an implicit .5 * deltaz
!------------------------------------------------------------------
      deltaz(:) = (zs(0:mxcly-1) - zs(1:mxcly)) * 5.e4
      babso2(:) = deltaz(:)
     $            *(dens(0:mxcly-1,3)*xsect(jo2,0:mxcly-1)
     $              + dens(1:mxcly,3)*xsect(jo2,1:mxcly))
      babso3(:) = deltaz(:)
     $            *(dens(0:mxcly-1,2)*xsect(jo3,0:mxcly-1)
     $              + dens(1:mxcly,2)*xsect(jo3,1:mxcly))
      babso(:) = babso2(:) + babso3(:)
     $         + deltaz(:)
     $           *(dens(0:mxcly-1,5)*xsect(jno2,0:mxcly-1)
     $             + dens(1:mxcly,5)*xsect(jno2,1:mxcly))

      end subroutine ABSDEP_PHO
