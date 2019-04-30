! subversion Id for THIS file : $Id: aerosol_pho.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/aerosol_pho.f $
!-----------------------------------------------------------------------

      subroutine AEROSOL_PHO( zs, babsae, bscae, gaer, iv )
!---------------------------------------------------------------
! 	... Aerosol climatology from hitchman et al. 1993.
!           aerex2 is the extinction coeff (1/km) : read in at    
!   	    main.f from 2-d array aedat1(lmax,niz)
!---------------------------------------------------------------
      use GRID_DIMS, only : niz
      use PHO_PARMS, only : mxwvn, mxcly
      use PHO_AERO, only : vaer, aerex2, omaer, coeff, gaer1, exabs

      implicit none

!---------------------------------------------------------------
!	... Dummy args
!---------------------------------------------------------------
      integer, intent(in) :: iv
      real, dimension(0:mxcly), intent(in) :: zs
      real, dimension(mxcly), intent(out) :: babsae, bscae, gaer

!---------------------------------------------------------------
!	... Local variables
!---------------------------------------------------------------
      real :: exabs1(niz)
      real :: dz(niz)

      dz(1:niz-1) = zs(mxcly-1:0:-1) - zs(mxcly:1:-1)
      vaer(1:niz-1) = dz(1:niz-1) * aerex2(1:niz-1)
      exabs1(1:niz) = aerex2(1:niz)
     $                * (1. - omaer(iv))*1.e-5*coeff(iv)     !(1/cm)
!---------------------------------------------------------------
!	... Adjust for wavelength
!           (hitchman compiled data at 1000 nm).
!---------------------------------------------------------------
      bscae(mxcly:1:-1) = vaer(1:niz-1)*coeff(iv)*omaer(iv)           ! scattering optical depth 
      babsae(mxcly:1:-1) = vaer(1:niz-1)*coeff(iv)*(1. - omaer(iv))   ! absorption optical depth 
      gaer(mxcly:1:-1) = gaer1(iv) 
      exabs(mxcly:0:-1,iv) = exabs1(1:niz)                            ! absorption coeff. (1/cm)

      end subroutine AEROSOL_PHO
      
