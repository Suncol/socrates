! subversion Id for THIS file : $Id: qbo.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/qbo.f $
!-----------------------------------------------------------------------

      subroutine QBO( daynum )
!-----------------------------------------------------------------------
!     	... A QBO will be kick-started by placing
!           a perturbation in temperature and zonal wind over the Equator.
!           The zonal wind perturbation's maximum value will be 25m/s,
!           and will be centered over the Equator. The perturbation will
!           decay exponentially (a Gaussian) in latitude with negative tails
!           beyond 15 degrees either side of the Equator (a sinusoidal
!           multiplicative factor) and sinusoidally
!           in altitude, between 16km and 40 km, with a node between
!           easterlies and westerlies that descends with time.
!           The descent has period 27 months (821 days) and
!           half-width of 27km, with maximum vertical amplitude at 28 km.
!
!     In version 8qs ONLY the temperature perturbation is
!     updated by a nudging technique, using the equation
!
!     T = T + (dT/dt)*deltat, where deltat = daypas
!
!     In this equation, the amplitude of the nudging factor
!     is ten times that of the original perturbation, to test
!     the hypothesis that kinetic energy partitioning forces
!     a thermally forced QBO to be proportionately higher.
!     The function (dT/dt) is evaluated at (daynum+(daypas/2)) to
!     further reduce drift in T over the subsequent timestep.
!
!                  Theresa Huang, final version
!
!  Note by SC: daypas was 5, I replaced it by 1 !! How to update this routine??
!-----------------------------------------------------------------------

      use ALLCO, only : phi, zkm
      use VEN14
      use PHYS_CST, only : pi

      implicit none

!-----------------------------------------------------------------------
!	... Parameters
!-----------------------------------------------------------------------
      real, parameter :: uamp0 = 25.
      real, parameter :: teqamp = 250. ! 200.
      real, parameter :: ueqamp = 200.
      real, parameter :: yefld = 15.
      real, parameter :: zhlfwd = 27.
      real, parameter :: tqbper = 821.
      real, parameter :: zbse0 = 28.
      real, parameter :: zsclq = 7.e3
      real, parameter :: rgasq = 287.
      real, parameter :: qbeta = 1.4584e-4
      real, parameter :: tqbof5 = uamp0 * zsclq * qbeta
     $                                  * yefld * yefld / (2.*rgasq)

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      real, intent(in) :: daynum

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer :: l, iz
      real    :: upzz, upzt, upzt0
      real    :: arg0, arg1, arg2, arg3
      real, save :: tqbof6
      real, dimension(35), save :: tqbof1, tqbof7
      real    :: tqbof2, tqbof3, tqbof4
      logical, save :: entered = .false.

      if( .not. entered ) then
         tqbof6 = -1.0 * teqamp * tqbof5 * (pi**2) / (tqbper*86400.)
         tqbof1(:) = EXP( -(phi(:)/yefld)**2 )
         tqbof7(:) = COS( pi*phi(:)/(2.*yefld) )
      entered = .true.
      end if

      arg0 = 2* pi / (tqbper*86400.)
      arg2 = (daynum + 7.5) / tqbper
      do l = 1,35
         do iz = 17,41
        arg3 = zkm(iz) - zbse0
        arg1 = arg3 / zhlfwd
        arg3 = arg3 * pi / 24.
            upzz = COS( arg3 )
            upzt = COS( 2.*pi*(daynum/tqbper + arg1) )
            upzt0 = sin( 2.*pi*(daynum/tqbper + arg1) )
            uqprt(l,iz) = -teqamp * uamp0 * tqbof1(l)
     $                            * upzz * upzt * tqbof7(l)
!            uqprt(l,iz) = ueqamp * uamp0 * arg0 * tqbof1(l)
!     $                           * upzz * upzt0 * tqbof7(l)
            f_e(l,iz) = teqamp * uamp0 * tqbof1(l) * upzz
     $                           *upzt0 * tqbof7(l) * arg0
!-----------------------------------------------------------------------
!     The temperature perturbation is derived from the zonal wind
!     wind perturbation assuming that the thermal wind law holds
!     even near the Equator. The beta plane has been invoked
!     to make the Coriolis parameter behave.
!-----------------------------------------------------------------------
        arg1 = 2. * pi * (arg1 + arg2)
            tqbof2 = SIN( arg3 ) * SIN( arg1 )
            tqbof3 = upzz * COS( arg1 )
            tqbof4 = tqbof2/24. - 2./zhlfwd*tqbof3
            t2dqprt(l,iz) = tqbof7(l) * tqbof6 * tqbof4 * tqbof1(l)
        end do
      end do

      end subroutine QBO
