! subversion Id for THIS file : $Id: asmith.gwave.dyn.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/asmith.gwave.dyn.f $
!-----------------------------------------------------------------------
      subroutine ASMITH_GWAVE_DYN( dzzgw, fx )
!----------------------------------------------------------------------
!	... Momentum deposition and vertical eddy diffusion coefficient
!           kzz by breaking/absorption of Gravity Waves: Lindzen 
!           parameterization using 9 gravity waves, implemented by
!                 Anne Smith, aksmith@ucar.edu
!       ... Notice: from izmMax to surface kzz is specified in tropk.f
!       ... Parameters to tune this parameterization 
!           (see table 1, Brasseur/Hitchman-1987):
! floc       ... inverse Prandtl number, between .3 to 1. Multiplier for Kzz, 
!                increase to increase Kzz (which decresases ChemHeat at 
!                mesopause) independently of fx & GW dyn forcing
! nwav       ... nb of GW
! c          ... phase speed of GW ; eastward/westward ; m/s
! a          ... amplitude coefficients of GW : m-2 s-1
! u00        ... breaking level coefficicent, ~ amplitude of GW launching level,
!                0.1 < u00 < 0.6 ; increase to lower GW breaking level, which
!                increases total GW forcing and circulation
! afac       ... accounts for temporal and spatial intermittency of GW ; 
!                increase to increase GW forcing; 1. < afac < 50.
!----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use VEN2, only : u
      use VEN3, only : uz
      use VEN8, only : bv
      use TRANSFORM, only : SMOOTHV, SMOOTHL, VDERIV

      implicit none
!----------------------------------------------------------------------
!	... Parameters
!----------------------------------------------------------------------
      integer, parameter :: nwav = 9
      real, parameter, dimension(nwav) :: c =     
     $              (/ -40., -30., -20., -10., 0., 10., 20., 30., 40. /)
      real, parameter, dimension(nwav) :: a = 
     $                 1.e-10 * (/ 3., 5., 8., 8., 8., 8., 8., 5., 3. /)
      real, parameter :: u00  = 0.35  ! 0.2        ! 0.35 in v3cII65
      real, parameter :: afac = 10.   ! 30.        ! 10 in v3cII65
      real, parameter :: floc = 1.  

!----------------------------------------------------------------------
!	... Dummy args
!----------------------------------------------------------------------
      real, dimension(lmax,niz), intent(out) :: dzzgw, fx

!----------------------------------------------------------------------
!	... Local variables
!----------------------------------------------------------------------
      integer :: l, k, ic, ib, iz
      real    :: xc, zb, fg, ampnew, exfac, uzlimit, fgbb, dzzbb
      real, dimension(niz) :: umclimit, umc
      real, save :: amplat(lmax)
      logical, save :: entered = .false.

      if( .not. entered ) then
         do l = 1,lmax
            amplat(l) = ( SIN((l-18.)*10.*0.0174532)) ** 2
            if(l <= 9) amplat(l) = 1.
            if(l >= 27) amplat(l) = 1.
         end do
         entered = .true.
      end if
     
!-----------------------------------------------------------------------
!   	... Initialization of momentum and eddy diff. coeff
!-----------------------------------------------------------------------
      dzzgw = 0.e0
      fx = 0.e0

!-----------------------------------------------------------------------
!  	... Loop through all latitudes
!-----------------------------------------------------------------------
      do l = 1, lmax
         do k = 1, nwav
            ampnew = 1.*(.8 + 1. * amplat(l)) * a(k)
            do iz = 1,niz
               umc(iz) =  u(l,iz) - c(k)
               if( ABS(umc(iz)) > 1. ) umclimit(iz) = umc(iz)
               if( ABS(umc(iz)) < 1. ) umclimit(iz) = SIGN(1.,umc(iz))
            end do
!-----------------------------------------------------------------------
!  	... Calculates height where waves are absorbed
!-----------------------------------------------------------------------
            ic = niz
            xc = .1 - c(k)
            do iz = niz, 21, -1
	       if( xc*umc(iz) < 0. ) ic = iz
            end do
            ib = niz
!-----------------------------------------------------------------------
!     	... Calculates breaking level
!-----------------------------------------------------------------------
            do iz = ic,21,-1
               zb = 21. * LOG( ABS( umclimit(iz) ) / u00 )
               if( REAL(iz) >= zb ) ib = iz
            end do
!-----------------------------------------------------------------------
!     	... Calculates momentum drag and eddy diffusion coefficient
!-----------------------------------------------------------------------
            if( ib <= ic ) then
               do iz = ib,ic
                  fg = - ampnew * afac * umc(iz) * umc(iz) * 
     $                      (umc(iz) - 21.e3*uz(l,iz))
                  fx(l,iz) = fx(l,iz) + fg
                  dzzgw(l,iz) = dzzgw(l,iz)
     $                         + ABS( fg*umc(iz)/(bv(l,iz)*bv(l,iz)) )
                  if(iz.eq.ib) then
                     fgbb = fg
                     dzzbb = ABS( fg*umc(iz)/(bv(l,iz)*bv(l,iz)) )
                  end if
               end do
!     	... Exponential decay below breaking level
                  do iz = 21,ib-1
                     exfac = EXP( REAL( iz - ib) / 7. )
                     fx(l,iz) = fx(l,iz) + fgbb * exfac
                     dzzgw(l,iz) = dzzgw(l,iz) + dzzbb * exfac
                  end do
            end if
         end do

      end do
      dzzgw(l,21:niz) = floc * dzzgw(l,21:niz)

!-----------------------------------------------------------------------
!     smooth in the meridional direction - all smoothing of dzzgw in MAIN
!-----------------------------------------------------------------------
      call SMOOTHL( fx, 15, niz, 15 )

!-----------------------------------------------------------------------
!     reduce near the top to avoid boundary problems
!-----------------------------------------------------------------------
      do iz = 114,niz
         fx(:,iz) = fx(:,113) * EXP( (REAL(113 - iz))*.3 )
      end do

!-----------------------------------------------------------------------
!     smooth in the vertical direction
!-----------------------------------------------------------------------
      call SMOOTHV( fx, 10, niz, 3 )

      end subroutine ASMITH_GWAVE_DYN
