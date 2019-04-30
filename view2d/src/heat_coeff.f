! subversion Id for THIS file : $Id: heat_coeff.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/heat_coeff.f $
!-----------------------------------------------------------------------

      subroutine HEAT_COEFF( )
!-----------------------------------------------------------------------
!	... Calculate the newtonian cooling coefficient alpha used in 
!           MAIN and POTTEMP. Method from AVERY and Garcia, Solomon ?
!       ... Calculate enrgy released by photolysis of O2 and O3 (UV solar
!           heating), was done in MAIN
!       ... Set efficiencies of chemical heating exothermic reactions
!       ... Calculate efficiencies for O3 and O2 solar heating
!           from Mlynczak/Solomon:1993
!                                                       simonc, mar 2000
!-----------------------------------------------------------------------
      use GRID_DIMS, only : niz
      use ALLCO, only : zkm
      use ZGRID, only : pmb
      use PHO_VARS, only : alamb, blamb, hv
      use ALP, only : alpha
      use AIRGLOW, only : efsho2, efsho3, efch, denerg1, denerg2
      use PHYS_CST, only : eV, hPl, clight
      use SIM_CONTROLS, only : mainsw       ! mainsw(6) sets chem_heat

      implicit none

!-----------------------------------------------------------------------
!  	... Parameters
!-----------------------------------------------------------------------
      real, parameter    :: ep1 = 5.11 * eV   ! Energy to break O2 : 5.11eV in J
      real, parameter    :: ep2 = 1.05 * eV   ! Energy to break O3 : 1.05eV in J

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer :: iv, k, n
      real, dimension(121) :: x, temp
      real :: cO3u(4) = (/ .66965, -.009682,  .033093,  .017938 /)
      real :: cO3l(4) = (/ .92621,  .133960, -.076863,  .006897 /)
      real :: cO2u(4) = (/ .75349,  .0036,    .059468, -.022795 /)

!-----------------------------------------------------------------------
!	... Calculate the newtonian cooling coefficient alpha
!-----------------------------------------------------------------------
      where( zkm(:) > 55. .and. zkm(:) < 94. )
         alpha(:) = .6 
      elsewhere
         alpha(:) = 0.05 + 0.3/COSH( (zkm(:) - 52.)/6. ) 
     $            + 5.*(1. + TANH( (zkm(:) - 105.)/7.25 ))
      end where
      do n = 1, 36         ! smoothing by triangular function, 36 passes
         do k = 2,119
            alpha(k) = .25*( alpha(k-1) + 2.*alpha(k) + alpha(k+1) )
         end do
      end do
      alpha(:) = alpha(:) / 86400.    ! convert from per day to per second

!-----------------------------------------------------------------------
!   	... Calculate energy released by O2 and O3 photolysis (J)
!-----------------------------------------------------------------------
      hv(:) = 2.e9 * hPl * clight / (alamb(:) + blamb(:))   ! hc/lambda in J
      denerg1(:) = MAX( hv(:) - ep1, 0. )     ! energy released by O2 photolysis
      denerg2(:) = MAX( hv(:) - ep2, 0. )     ! energy released by O3 photolysis

      if( mainsw(6) == -1 ) then
         efch(:,:) = 0.
         efsho2(:,:) = 1.
         efsho3(:,:) = 1.
         return
      end if
!-----------------------------------------------------------------------
!	... Efficiencies of chemical heating by exothermic reactions
!-----------------------------------------------------------------------
      efch(:,:) = 1.
      efch(:,4) = 0.6     ! effic of energy relased by H+O3, see M&S 1993
      if( mainsw(6) == 0 ) efch(71:niz,:) = 0.
         
!-----------------------------------------------------------------------
!	... The solar heating efficiencies are pressure-dependent
!-----------------------------------------------------------------------
      where( pmb <= .01 )
         x(:) = LOG10( pmb(:) ) + 3.
      elsewhere
         x(:) = LOG10( pmb(:) ) + 1.
      end where

!-----------------------------------------------------------------------
!	... Heating efficiency for O3 Hartley band (M&S:1993, p10525)
!-----------------------------------------------------------------------
      do k = 1, niz
         if( pmb(k) > 1. ) then
            temp(k) = 1.
          else if( pmb(k) > 0.01 ) then
            temp(k) = cO3l(1) 
     $              + x(k)*(cO3l(2) + x(k)*(cO3l(3) + x(k)*cO3l(4)))
          else
            temp(k) = cO3u(1) 
     $              + x(k)*(cO3u(2) + x(k)*(cO3u(3) + x(k)*cO3u(4)))
         end if
      end do
      do n = 1, 3         ! smoothing by triangular function, 3 passes
         do k = 2,119
            temp(k) = .25*( temp(k-1) + 2.*temp(k) + temp(k+1) )
         end do
      end do
      do iv = 60,95
	 efsho3(:,iv) = temp(:)
      end do

!-----------------------------------------------------------------------
!	... Heating  efficiency for O2 SR continuum (p 10527 M&S 1993)
!-----------------------------------------------------------------------
      where( pmb(:) <= .01 )
         temp(:) = cO2u(1) + x*(cO2u(2) + x*(cO2u(3) + x*cO2u(4)))
      else where( pmb(:) <= 1. )  ! Using scaled O3 results between 0.01 and 1mb
         temp(:) = efsho3(:,60) -
     $               (efsho3(82,60)-temp(82)) * (zkm(:)-48.) / (81.-48.)
      end where
      temp(1:55) = 1.
      do iv = 28,50
         efsho2(:,iv) = temp(:)
      end do

!-----------------------------------------------------------------------
!	... Heating  efficiency for O2 Lyman alpha band 
!-----------------------------------------------------------------------
      efsho2(:,8) = .95

      end subroutine HEAT_COEFF
