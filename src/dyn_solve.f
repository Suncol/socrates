! subversion Id for THIS file : $Id: dyn_solve.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/dyn_solve.f $
!-----------------------------------------------------------------------

      subroutine DYN_SOLVE( month )
!-----------------------------------------------------------------------
!    ...  This code directly solves for the stream function
!         No iteration is involved. This is the old code used up to v4s17
!            The first loop "do l = 1, cdjac" is wrongly seen 
!                out-of-bound by HP & DEC (Compaq) f90 compilers
!-----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use PHYS_CST, only : R0
      use ALLCO, only : phir, dlatr
      use SIM_CONTROLS, only : mainsw
      use VEN5, only : cf
      use VEN6, only : x, v, w                      ! output
      use VEN7, only : xa, xb, xc, xd, xe, xf, xg
      use UPPER, only : wu, xu
      use NUMERICAL, only : BAND_FAC, BAND_SLV
      use TRANSFORM, only : SMOOTHL

      implicit none

!-----------------------------------------------------------------------
!	... Parameters
!-----------------------------------------------------------------------
      integer, parameter :: iz1 = 3, iz2 = 4
      integer, parameter :: nl = iz2, nu = niz - 1, nizz = nu - nl + 1
      integer, parameter :: bandw = lmax+1
      integer, parameter :: cdjac = nizz*lmax
      integer, parameter :: mdiag = bandw + 1
      integer, parameter :: rdjac = 2*lmax + 3
      real, parameter    :: dz2i = 5.e-4
      real, parameter    :: hi = 1. / 7.e3
      real, parameter    :: rei = 1.e-3 / R0

!-----------------------------------------------------------------------
!	... Dummy Arguments 
!-----------------------------------------------------------------------
      integer, intent(in) :: month

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer :: l, retcod
      real :: w1, w2, w3
      real, dimension(lmax,niz) ::  xz, xl
      real, dimension(lmax) :: xo
      real :: jac(rdjac,cdjac)
      real :: rhs(cdjac)
      real, save :: dli, dl2i
      logical, save :: entered = .false.

      if( .not. entered ) then
         dli = 1. / dlatr
         dl2i = .5 * dli
         entered = .true.
      end if

!-----------------------------------------------------------------------
!      	... Lower boundary condition on x - subroutines "contained" below
!-----------------------------------------------------------------------
      if( mainsw(10) == 1 ) then       ! xo at 2 km from downward control  
         call DWCNTRL_CHILBC_DYN( xo )
       else                            ! xo at 2 km by R. Khoshravi
         call SINVAR_CHILBC_DYN( month, xo )
      end if
   
!-----------------------------------------------------------------------
!    	... Set upper and lower bndy values 
!-----------------------------------------------------------------------
      x(:,iz1) = xo
      x(:,niz) = xu

      do l = 1,lmax
         cf(l,:) = cf(l,:)*COS( phir(l) )
      end do
      do l = 1,cdjac
         x(l,nl) = cf(l,nl)
      end do
      do l = 2,lmax-1
	 x(l,nl) = x(l,nl) - (xc(l,nl)*xo(l) 
     $                     +   xf(l,nl)*(xo(l-1) - xo(l+1)))
         x(l,nu) = x(l,nu) - (xb(l,nu)*xu(l)
     $                     +   xf(l,nu)*(xu(l+1) - xu(l-1)))
      end do

      x(1,nl) = x(1,nl) - xc(1,nl)*xo(1) + xf(1,nl)*xo(2)
      x(lmax,nl) = x(lmax,nl) - (xc(lmax,nl)*xo(lmax)
     $                        +   xf(lmax,nl)*xo(lmax-1))
      x(1,nu) = x(1,nu) - (xb(1,nu)*xu(1) + xf(1,nu)*xu(2))
      x(lmax,nu) = x(lmax,nu) - xb(lmax,nu)*xu(lmax)
     $                        + xf(lmax,nu)*xu(lmax-1)
      rhs = RESHAPE( x(1:lmax,nl:nu), (/cdjac/) )

!-----------------------------------------------------------------------
!     	... Zero the jacobian
!-----------------------------------------------------------------------
      jac = 0.

!-----------------------------------------------------------------------
!  	... Fill the upper, main, and lower block jacobian diagonals
!-----------------------------------------------------------------------
      do l = 1,cdjac
         jac(mdiag,l) = xa(l,nl)
      end do
      do l = 1,cdjac-1
         jac(mdiag+1,l) = xe(l+1,nl)
         jac(mdiag-1,l+1) = xd(l,nl)
      end do

      do l = 1,cdjac-lmax
	 jac(2,lmax+l) = xb(l,nl)
         jac(rdjac-1,l) = xc(l,nl+1)
      end do
      do l = 1,cdjac-(lmax+1)
	 jac(1,lmax+l+1) = xf(l,nl)
	 jac(3,lmax+l) = -xf(l+1,nl)
	 jac(rdjac-2,l+1) = -xf(l,nl+1)
         jac(rdjac,l) = xf(l+1,nl+1)
      end do

!-----------------------------------------------------------------------
!  	... Zero out the "boundary" points in the bands
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!	... Upper block diagonals
!-----------------------------------------------------------------------
      do l = lmax+1,cdjac,lmax
         jac(1,l) = 0.e0
      end do
      do l = 2*lmax,cdjac,lmax
         jac(3,l) = 0.e0
      end do

!-----------------------------------------------------------------------
!    	... Main block diagonals
!-----------------------------------------------------------------------
      do l = 1,cdjac,lmax
         jac(mdiag-1,l) = 0.e0
      end do
      do l = lmax,cdjac,lmax
         jac(mdiag+1,l) = 0.e0
      end do

!-----------------------------------------------------------------------
!   	... Lower block diagonals
!-----------------------------------------------------------------------
      do l = 1,cdjac-lmax,lmax
         jac(rdjac-2,l) = 0.e0
      end do
      do l = lmax,cdjac-lmax,lmax
         jac(rdjac,l) = 0.e0
      end do

!-----------------------------------------------------------------------
!   	... Factor the jacobian matrix
!-----------------------------------------------------------------------
      call BAND_FAC( jac, cdjac, bandw, bandw, retcod )
      if( retcod /= 0 ) then
	 write(*,*) 'DYN_SOLVE : Zero pivot element at row = ',retcod
	 stop 'DYN_SOLVE: Zero pivot element - fatal error'
      end if

!-----------------------------------------------------------------------
!    	... Solve for the stream function
!-----------------------------------------------------------------------
      call BAND_SLV( jac, cdjac, bandw, bandw, rhs )
      x(1:lmax,nl:nu) = RESHAPE( rhs, (/lmax,nizz/) )

!-----------------------------------------------------------------------
!     	... Extrapolation of X from 0 to 1 km
!-----------------------------------------------------------------------
      x(:,2) = .5 * xo
      x(:,1) = 0.

!-----------------------------------------------------------------------
!   	... Calculation of meridional and vertical wind components
!-----------------------------------------------------------------------
      do l = 1,lmax
         xz(l,2:niz-1) = (x(l,3:niz) - x(l,1:niz-2)) * dz2i
      end do
      xz(:,1) = xz(:,2)
      xz(:,niz) = xz(:,niz-1)
      do l = 2,lmax-1
         xl(l,:) = (x(l+1,:) - x(l-1,:)) * dl2i
      end do
      xl(1,:) = (x(2,:) - x(1,:)) * dli
      xl(lmax,:) = (x(lmax,:) - x(lmax-1,:)) * dli
      do l = 1,lmax
         w1 = 1. / COS( phir(l) )
         w2 = w1 * hi
         w3 = w1 * rei
         v(l,:) = -w1*xz(l,:) + w2*x(l,:)
         w(l,:) = w3*xl(l,:)
      end do

!-----------------------------------------------------------------------
!     	... Set vertical wind at upper boundary to wu
!-----------------------------------------------------------------------
      w(:,niz) = wu

!-----------------------------------------------------------------------
!     	... Set winds at -85 = -80 and 85 = 80
!-----------------------------------------------------------------------
      w(1,:) = w(2,:)
      w(lmax,:) = w(lmax-1,:)

!-----------------------------------------------------------------------
!     	... Latitudinal smoothing
!-----------------------------------------------------------------------
      call SMOOTHL( v, iz1, niz, 1 )
      call SMOOTHL( w, iz1, niz, 1 )

!=======================================================================

      contains

      subroutine DWCNTRL_CHILBC_DYN( xo )
!-------------------------------------------------------------------------
!  	... Estimate lower boundary streamfunction 
!                          by "downward control' principle
!-------------------------------------------------------------------------
      use VEN3, only  : ff
      use VEN5, only : ftot     ! total dyn forcing, calculated in DYNF_TOTAL

      implicit none

!-------------------------------------------------------------------------
!  	... Parameters
!-------------------------------------------------------------------------
      real, parameter    :: dz = 1.e3

!-------------------------------------------------------------------------
!  	... Dummy args
!-------------------------------------------------------------------------
      real, intent(out), dimension(lmax) :: xo

!-------------------------------------------------------------------------
!  	... Local variables
!-------------------------------------------------------------------------
      integer :: k, l

      do l = 1, lmax
         xo(l) = 0.
         do k = niz, 3, -1
            xo(l) = xo(l) - COS(phir(l))/(ff(l)*EXP(-2./7.))
     $              *(EXP( -REAL(k-1)/7. )*ftot(l,k)
     $                + EXP( -REAL(k-2)/7. )*ftot(l,k-1))
     $              *.5*dz
         end do
      end do

!-------------------------------------------------------------------------
!	... Interpolate xo between -15 to 15 deg lat
!-------------------------------------------------------------------------
      do l = 15, 21
         xo(l) = xo(14) + .125*REAL(l-14)*(xo(22) - xo(14))
      end do

      end subroutine DWCNTRL_CHILBC_DYN

!=======================================================================

      subroutine SINVAR_CHILBC_DYN( month, xo )
!-----------------------------------------------------------------------
!    	... Set lower bndy values 
! by Rashid Koshravi:
! xo1 and xo8 are the streamfct chi at 5km in Jan and Aug (1998) from 
! arch.3yr.II37.nc used here as the boundary condition at iz1; 
! seasonal variation ! for this BC is also implemented through a sin 
! function of the current month with which this routine is called
!-----------------------------------------------------------------------
      use PHYS_CST, only : pi

      implicit none

!-------------------------------------------------------------------------
!  	... Parameters
!-------------------------------------------------------------------------
      real, parameter, dimension(lmax) :: xo1 = (/
     &              -1.00e+00,  -1.50e+02,  -2.30e+02,  -2.90e+02,
     &              -3.30e+02,  -3.60e+02,  -3.90e+02,  -4.20e+02,
     &              -4.40e+02,  -4.60e+02,  -4.80e+02,  -5.00e+02,
     &              -5.20e+02,  -5.40e+02,  -5.56e+02,  -4.54e+02,
     &              -1.55e+02,   2.95e+02,   7.65e+02,   1.13e+03,
     &               1.30e+03,   1.27e+03,   1.02e+03,   7.55e+02,
     &               5.50e+02,   4.08e+02,   3.16e+02,   2.54e+02,
     &               2.04e+02,   1.57e+02,   1.12e+02,   7.31e+01,
     &               4.32e+01,   2.25e+01,   8.64e+00 /) ,
     &                                    xo8 = (/
     &              -1.56e+00,  -1.56e+00,  -1.56e+00,  -1.56e+00,
     &              -2.82e+00,  -9.45e+00,  -2.49e+01,  -5.12e+01,
     &              -8.82e+01,  -1.37e+02,  -2.08e+02,  -3.31e+02,  
     &              -5.37e+02,  -7.98e+02,  -9.37e+02,  -8.98e+02,
     &              -6.58e+02,  -2.81e+02,   9.87e+01,   3.50e+02,
     &               4.19e+02,   3.48e+02,   2.12e+02,   1.12e+02,
     &               7.59e+01,   7.19e+01,   6.69e+01,   5.89e+01,
     &               5.29e+01,   4.99e+01,   4.79e+01,   4.16e+01,
     &               3.17e+01,   1.98e+01,   8.40e+00  /)
     
!-------------------------------------------------------------------------
!  	... Dummy args
!-------------------------------------------------------------------------
      integer, intent(in) :: month
      real, intent(out), dimension(lmax) :: xo

      xo(:) = .5 * ( xo1(:) + xo8(:)) 
     $           * ( 1. + SIN( pi/2. + REAL(month-1)*pi/7. ) )

      end subroutine SINVAR_CHILBC_DYN

      end subroutine DYN_SOLVE
