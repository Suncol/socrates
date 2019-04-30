! subversion Id for THIS file : $Id: h2o_special.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/h2o_special.f $
!-----------------------------------------------------------------------
      subroutine H2O_MAX( T, maxh2ovap )
!-----------------------------------------------------------------------
!	... Calculate the max water vapor vmr maxh2ovap due to
!           cloud processes. Based on cld_partition.f by Rashid Koshravi
! frac = fraction of saturation mix ratio which is taken as the max allowable  
! water vapor m.r.; from the surface to tropopause, frac= relative humidity 
! (routine relhum_trop.f), above  frac= gamma with linear connection in 1st 10km
!     Notice! in v6s versions, T is Tmsis, not t2d (see main.f)
!-----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use ZGRID, only : pmb
      use TROPOPAUSE, only : izm, izmMax
      use DIAG_CONTROLS, only : diags, ldiag, zdiag

      implicit none
!------------------------------------------------------------------
!	... Parameters
!------------------------------------------------------------------
      real, parameter    :: gamma = 0.8

!------------------------------------------------------------------
!	... Dummy args
!------------------------------------------------------------------
      real, intent(in), dimension(lmax,niz) :: T           ! temperature (K)
      real, intent(out), dimension(lmax,niz) :: maxh2ovap  ! Max H2O vapor vmr

!----------------------------------------------------------------------
!       ... Local variables
!----------------------------------------------------------------------
      integer :: lat, iz, im
      real, dimension(niz) :: frac , es                              
      real, dimension(lmax,niz) :: h2oSat
      real, dimension(lmax,izmMax) :: RHtrop
                                                    
      call SATVAPP( T, es )        ! *** es is in Pascals ***
      call RELHUM_TROP( RHtrop )

      do lat = 1, lmax
         im = izm(lat)
         frac(1:im-1) = RHtrop(lat,1:im-1)
         do iz = im, im + 10
           frac(iz) = RHtrop(lat,im) 
     $              + REAL(iz-im) * ( gamma - RHtrop(lat,im) ) / 10.
         end do
         frac(im+11:niz) = gamma

         call SATVAPP( T(lat,:), es(:) )        ! *** es is in Pascals ***
         h2oSat(lat,:) = 1.e-2 * es(:) / pmb(:)
         maxh2ovap(lat,:) = frac(:) * h2oSat(lat,:)

         if( diags .and. lat == ldiag ) write(*,'(2(a,i3),5(a,es10.3))')
     $      'H2O_MAX @ (',ldiag,',',zdiag,'): es= ',es(zdiag),
     $      ' ; frac= ',frac(zdiag),' ; h2oSat= ',h2oSat(ldiag,zdiag),
     $      ' ; maxh2ovap= ',maxh2ovap(ldiag,zdiag)
 
      end do
      
      end subroutine H2O_MAX 

!=======================================================================

       subroutine SATVAPP( T, es )
!-----------------------------------------------------------------------
!       This function calculates the saturation vapor pressure of h2o 
!       over water or ice, in Pascals:     es = 6.11*10**( a*T/(b+T) ) 
!       where T is the temperature Celsius and the constants a and b are
!
!       a = 7.567,   b = 239.7 ;   T >= 0 C  \_ 
!         = 7.744,     = 245.2 ;   T <  0 C  / -> over water
!         = 9.716,     = 271.5 ;   T <  0 C  ->   over ice
!
!       This has been compared with three other  methods 
!       (see /a3/rashid/socrates2/miscsrc/es.f), and is in very good 
!       agreement with Appendix I of the ACD text book. 
!
!       In the following algorithm three temperature ranges are distinguished:
!            T <= Ti     saturation is over ice only
!       Ti < T < Tw             .......     ice/water solution
!            T >= Tw            .......     water only 
!       For the ice/water case the saturation vapor pressure (SVP) is 
!       calculated as a linear combination of SVP over water-only and 
!       ice-only (see below).
!                                    Rashid Khosravi       Sep. 15, 1998
!-----------------------------------------------------------------------
      use GRID_DIMS, only: niz

      implicit none

!-----------------------------------------------------------------------
!  	... Parameters
!-----------------------------------------------------------------------
       real, parameter :: es0 = 6.11 ! sat. vapor pressure of h2o at 0 C (mb)
       real, parameter :: a1 = 7.567,   b1 = 239.7   ! see formula above
       real, parameter :: a2 = 7.744,   b2 = 245.2
       real, parameter :: a3 = 9.716,   b3 = 271.5

!-----------------------------------------------------------------------
!  	... dummy arguments
!-----------------------------------------------------------------------
       real, intent(in), dimension(niz)  :: T  ! temperature (K)
       real, intent(out), dimension(niz) :: es ! h2o sat vapor pressure (Pa)

!-----------------------------------------------------------------------
!  	... local variables
!-----------------------------------------------------------------------
       real, dimension(niz) :: TC, esi, esw, alpha

       TC(:) = T(:) - 273.15               ! convert temperatures to Celsius

       where( TC(:) >= 0. )

          es(:) = a1*TC(:)/(b1 + TC(:))      ! sat. vapor pressure (mb) over water
          es(:) = es0*(10.**es(:))

        else where( TC(:) >= -5. )                ! assume liquid phase

          es(:) = a2*TC(:)/(b2 + TC(:))
          es(:) = es0*(10.**es(:))

        else where( TC(:) <= -25. )               ! assume ice

          es(:) = a3*TC(:)/(b3 + TC(:))   
          es(:) = es0*(10.**es(:))
        
        else where                                  ! water/ice solution

          esi(:) = a3*TC(:)/(b3 + TC(:))      ! over ice
          esi(:) = es0*(10.**esi(:))  
    
          esw = a2*TC(:)/(b2 + TC(:))           ! over water (constants for T < 0)
          esw = es0*(10.**esw)
    
          alpha = (TC(:) - (-25.)) / (-5.-(-25.))    ! fraction of water, (=0 when
                                                       ! all ice, =1 when all water)
          es(:) = alpha*esw + (1. - alpha)*esi
   
       end where
        
       es(:) = 100. * es(:)    ! convert from mb (hPa) to Pa

       end subroutine SATVAPP

!=======================================================================

      subroutine RELHUM_TROP( RHtrop ) 
!-----------------------------------------------------------------------
! This routine calculates the relative humidity from the surface up to izmMax 
! (highest tropopause level), based on a modified version of equation 4.83 
! of Brasseur/Solomon for the troposphere.
! The valid range of altitudes is from surf to just above the tropopause. 
! The original equation is:    RH = RHo[(P/P0 - 0.02) / 0.98]**X
! where:
!
!    X = 1 - .03(Tsurface - 288.0)
!    RHo = .77, but in the model we use RH_surf
!          to be consistent with the boundary condition (boundy.f)
!    P = pressure
!
! In the modified version the following expression is used for X in order to 
! increase the relative humidity at high latitudes 
! (based on data of Pexioto/Oort, p. 281 ):
!   X = .9 -.03(Tsurface + dT - 288)
!                                           Rashid Khosravi; July, 1999
!-----------------------------------------------------------------------
      use GRID_DIMS, only    : lmax
      use TROPOPAUSE, only   : izmMax
      use PHYS_CST, only     : pmb0
      use ZGRID, only        : pmb
      use ALLCO, only        : phi
      use VEN1, only         : t2d
      use TRANSFORM, only    : SMOOTHL

      IMPLICIT NONE

!-----------------------------------------------------------------------
!  	... Parameters
!-----------------------------------------------------------------------
! globally averaged Relative Humidity at surface as a function of latitude, 
! based on Fig. 12.4b of "Physics of Climate" by Pxioto & Oort; used in boundy.f
      real, parameter, dimension(lmax) :: RH_surf = 0.75
!         data RH_surf / 4*.7, 2*.75, .77, 2*.78, .75, 4*.7, .75, .77, 
!     &                  .8, 2*.85, .78, .77, .75, 4*.7, .75, 2*.77, 
!     &                  2*.75, .8, .82, 2*.8 /
! max increase factor for RH to increase the water vapor at the tropopause
      real, parameter :: facMax = 1.2        
                                             
!-----------------------------------------------------------------------
!  	... Dummy arguments
!-----------------------------------------------------------------------
      real, dimension(lmax,izmMax), intent(out) :: RHtrop 

!-----------------------------------------------------------------------
!       ... local variables 
!-----------------------------------------------------------------------
      integer :: l,k
      real, dimension(lmax) :: X 
      real, save :: dT(lmax), fac(izmMax)
      logical, save :: entered = .false.

      if( .not. entered ) then
         dT(1:11) = 15.0 + (30./35.)*(ABS(phi(1:11)) - 45.)
         dT(12:24) = 0.0
         dT(25:35) = 7.0 + (30./35.)*(ABS(phi(25:35)) - 45.)
         do k=1, izmMax 
           fac(k) = 1.0 + REAL(k-1) * (facMax-1.0)/REAL(izmMax-1)
         end do
         entered = .true.
      end if 

      X(:) = 0.9 - .03 * ( t2d(:,1) + dT(:) - 288.0 )

      do l = 1, lmax
        RHtrop(l,1:izmMax) = fac(:) * RH_surf(l) * 
     &                       ( (pmb(1:izmMax)/pmb0 - 0.02)/0.98 )**X(l)
      end do

      call SMOOTHL( RHtrop, 1, izmMax, 1)

      end subroutine RELHUM_TROP 
