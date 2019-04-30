! subversion Id for THIS file : $Id: opticp_pho.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/opticp_pho.f $
!-----------------------------------------------------------------------
      subroutine OPTICP_PHO( babso, bscar, babsd, bscad, gd, dtauciv, 
     $                   ggiv, ssalbiv,  babsae, bscae, gaer )
!-----------------------------------------------------------------------
!     OPTICP summarizes the optical properties of the medium. It
!     calculates the total optical depth (absorption + scattering),  
!     the phase function and single scattering albedo.
!
!     Input variables:
!
!    babsx(lc)   lc = 1 to mxcly,
!                absorption coefficients for o2 and o3 (x = o),    
!                drops (e.g. clouds, x = d),
!                beta abs in s3 (dimensionless)   
!    bscax(lc)   lc = 1 to mxcly,
!                scattering coefficient for rayleigh scattering (x = r),    
!                drops (e.g. clouds, x = d), beta sca in s3 (dimensionless)   
!    gd(lc)      lc = 1 to mxcly,
!                asymmetry factor for drop particles
!
!           remainder are 'phodis' input variables
!
!     Output variables:
!
!    dtauciv(lc) lc = 1 to mxcly,
!                optical depths of computational layers at wl interval iv
!    ggiv(lc)    lc = 1 to mxcly,
!                1st coefficient in legendre polynomial expansion of
!                phase functions for computational layers, 
!                at wl interval iv : single scattering asymmetry factor
!    ssalbiv(lc) lc = 1 to mxcly, single-scatter albedos of computational
!                layers at wl interval iv
!-----------------------------------------------------------------------
      use PHO_PARMS, only : mxcly

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      real, dimension(mxcly), intent(in) :: babso, bscar, babsd,
     $                                      bscad, gd, babsae,
     $                                      bscae, gaer
      real, dimension(mxcly), intent(out) :: dtauciv, ggiv, ssalbiv
      
!-----------------------------------------------------------------------
!    	... Calculate optical depth, phase function and 
!           single scattering albedo ggiv(lc)
!-----------------------------------------------------------------------
      ggiv(:) = (bscad(:)*gd(:) + bscae(:)*gaer(:))
     $          / (bscar(:) + bscad(:) + bscae(:))

!-----------------------------------------------------------------------
!  	... Set optical depth and single scattering albedo (S3)
!-----------------------------------------------------------------------
      dtauciv(:) = babso(:) + babsd(:) + bscar(:) + bscad(:)
     $           + bscae(:) + babsae(:)
      ssalbiv(:) = (bscar(:) + bscad(:) + bscae(:))
     $             / (babso(:) + babsd(:) + bscar(:) + bscad(:)
     $                         + bscae(:) + babsae(:))
      
      end subroutine OPTICP_PHO
