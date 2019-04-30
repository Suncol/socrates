! subversion Id for THIS file : $Id: washout.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/washout.f $
!-----------------------------------------------------------------------
      subroutine WASHOUT( lat, t, hnm, n_h2o, wash_rates )
!--------------------------------------------------------------------------------------------------------
!  	... Calculates the washout rates for the soluble species
!     1) hno3     2) h2o2     3) ch3ooh     4) hcl     5) ho2no2
!     6) ch2o     7) hbr 
!       Notice this list of hetcnt=7 species *must* correpond to the 
!         "Heterogeneous" species list set in the preprocessor input file.
!     The physics of the process is described below the routine headers.
!--------------------------------------------------------------------------------------------------------
      use GRID_DIMS, only : niz
      use CHEM_MODS, only : hetcnt     ! = 7 as of v5s02
      use VEN8, only : p               ! pressure grid (Pa)
      use TROPOPAUSE, only : izm
      use PHYS_CST, only : pmb0, Nav

      implicit none
!----------------------------------------------------------------------
!	... Parameters
!----------------------------------------------------------------------
      real, parameter    :: dd = 1.
      real, parameter    :: pcorr = 2.
      real, parameter    :: rate = 1./7200.    ! 1/7200s = tstep for wash_rates
      real, parameter    :: Tmin = 190.        ! min tropo T (model T too warm)
      real, parameter    :: phfac = 1.0E5      ! ph factor for HNO3 Henry's law
      real, parameter    :: p0 = 100. * pmb0   ! surf pressure (Pa)
      real, parameter    :: invNav = 1. / Nav
!            EFFECTIVE Henry's law constant at room T (M/atm) :
      real, dimension(hetcnt), parameter :: He298 =   
     &   (/ 3.2E6*phfac, 8.33e4, 3.11e2, 2.0e6*phfac, 
     $      1.0e4, 6.3e3, 1.32e9*phfac/)
      real, dimension(hetcnt), parameter :: dhfac =   
     $   (/ 8700.0, 7379.0, 5241.0, 9000.0, 0.0, 6425.0, 10000.0 /) 

!----------------------------------------------------------------------
!	... Dummy args
!----------------------------------------------------------------------
      integer, intent(in) :: lat                 ! latitude index
      real, dimension(niz), intent(in) ::  t,    ! temperature (K)
     $                                     hnm,  ! total air nb density (cm-3)
     $                                     n_h2o ! water vapor nb density (cm-3)
      real, dimension(niz,hetcnt), intent(out) ::  wash_rates

!----------------------------------------------------------------------
!	... Local variables
!----------------------------------------------------------------------
      integer :: iz, i
      real :: n_h2osat                  ! density of h2o at saturation
      real :: fr, rlwc, henry, fx
      real, dimension(niz) :: adjT, es  ! adjusted T, H2O saturation vapor p
      real, dimension(hetcnt) :: xkh    ! Henry's law constant as function of T

!---------------------------------------------------------------------------------------------------------
! The data that is needed to calculate the washout of the above gases by rain 
! are the effective Henry's Law constant at room T (H298), and the enthalpy 
! factor (-dH/R) to account for the T dependence of the constant. 
! The effective Henry's law constants were obtained from:
! 1) Atmospheric Chemistry and Global Change; Guy Brasseur, J. Orlando, 
!      G. Tyndall; Oxford, 1999,   Chapter 4, App. J and K
! 2) Table II of Brimblecomb and Clegg, J. Atm. Chem. 1989, p.5
! 3) communications with Mary Barth, John Orlando, and Geoff Tyndall.
!
! The effective Henry's law constant takes into account not only the equilibrium
! of the gas and aqueous phases, but it also accounts for the ionization of the 
! species once in aqueous phase, according to eq. 4.34 of reference 1 :
!
!   He298 = H298*(1+Ka/[H+])
!
! where H298 = Henry's constant for the gas/aqueous equilibrium reaction 
! (also called the physical or pure HL constant), Ka is the equilibrium constant
! of the ionization reaction, and [H+] is the concentration of hydrogen ion in 
! the solution which determines its ph factor. For rain the ph factor
! is taken to be ~ 5 so that [H+] = 1.0E-5 atoms/cm3. Therefore for strong acids
! (HBr, HCl, HNO3) desolving in rain the ratio Ka/[H+] >= 1.0E6 and for weak 
! acids (H2O2, CH3OOH) it is << 1. 
! 
! The values of H298 for H2O2, CH3OOH, HO2NO2, and CH2O, & of H298*Ka for HCl 
! were obtained from Mary Barth. The value of H298*Ka for HNO3 was calculated 
! from reference 1, and that for HBr is taken from reference 2.
!
!                     Guy Brasseur & Rashid Khosravi, Sep. 1999
!
!---------------------------------------------------------------------------------------------------------
      wash_rates(:,:) = 0.
      adjT(:) = t(:)
      adjT(1:31) = MAX( adjT(1:31), Tmin )
      call SATVAPP( adjT, es )               ! file h2o_special.f

      do iz = 1, izm(lat)  
         n_h2osat = hnm(iz) * es(iz) / p(iz) 

!     washout of other species (troposphere only): Hough, JGR 1991, p. 7357
         fr = EXP( 2.* ( n_h2o(iz) - n_h2osat ) / n_h2o(iz) )
         rlwc = fr * n_h2o(iz) * 18. * invNav * pcorr

         do i = 1, hetcnt
           xkh(i) = He298(i) * EXP( dhfac(i)*( 1./adjT(iz) - 1./298.) )
           henry = 273.15 * p(iz) / ( xkh(i) * 22.4 * adjT(iz) * p0 )
           fx    = rlwc / ( dd*henry + rlwc )
           wash_rates(iz,i) = rate * fx / pcorr
         end do
      end do

!----------------------------------------------------------------------
!  	... Reduce hno3 washout by a factor of 2.and hbr washout by a factor of 10
!----------------------------------------------------------------------
      wash_rates(:,1) = wash_rates(:,1)*.5
      wash_rates(:,7) = wash_rates(:,7)*.1 ! v7s16f

      end subroutine WASHOUT
