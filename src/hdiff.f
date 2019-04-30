! subversion Id for THIS file : $Id: hdiff.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/hdiff.f $
!-----------------------------------------------------------------------
      subroutine HDIFF( dts, qn2noon )

!==========================================================================
!  	... Horizontal diffusion: dX/dt = 1/cos(phi)*d{cos(phi)*Kyy*dX/dy}/dy
!           After finite differencing: 
! X(N,l) = X(N+1,l) - {dt/[cos(phi(l))*dy*dy]}*{
!             cos(phi(l+0.5))*Kyy(l+0.5)*X(N+1,l+1) -
!   [cos(phi(l+0.5))*Kyy(l+0.5)+cos(phi(l-0.5))*Kyy(l-0.5)]*X(N+1,l) +
!             cos(phi(l-0.5))*Kyy(l-0.5)*X(N+1,l-1) }
! Defining dX/dt = Th*X(N+1),
!  Th(1,l,iz) = (5.e3/dy*dy)*{cos(phi(l-0.5))/cos(phi(l))}*[Kyy(l)+Kyy(l-1)]
!  Th(2,l,iz) = -Th(1,l,iz)-Th(3,l,iz)
!  Th(3,l,iz) = (5.e3/dz*dz)*{cos(phi(l+0.5))/cos(phi(l))}*[Kyy(l)+Kyy(l+1)]
! 	    Where the terms have been multiplied by 1.e4 to take
!           account of the fact that xkz and xky are in meters.
!  Th(2,l,iz) is the main diagonal of the 35*35 matrix Th(iz), and
!    Th(1,l,iz) and Th(3,l,iz) are the diagonals below and beneath the main
!    diagonal respectively. X(N,l) = (I-Th)*X(N+1,l)
!-----------------------------------------------------------------------
! 	... Horizontal Boundary Conditions:
!           Imposing a zero gradient condition at the poles by
!           assuming additional grid points at 90S and 90N with
!           the same mixing ratios as those at the adjacent point
!           in the grid yields;
!  [X(N+1,35)-X(N,35)]/dt = [cos(phi(35-0.5))/cos(phi(35))]*
!                        Kyy(35-0.5)/(dy*dy)*(X(N+1,35-1)-X(N+1,35))
!           or,
![X(N+1,35)-X(N,35)]/dt=xhl(35)*Kyy(35-0.5)*(X(N+1,35-1)-X(N+1,35)).
!           giving,
!  Th(1,35,iz) = xhl(35)*Kyy(35 - .5)
!  Th(2,35,iz) = -xhl(35)*Kyy(35 - .5)
!  Th(3,35,iz) = 0.
!
!           and at the south pole,
!  [X(N+1,1)-X(N,1)]/dt = [cos(phi(1+0.5))/cos(phi(1))]*
!                        Kyy(1+0.5)/(dy*dy)*(X(N+1,1+1)-X(N+1,1))
!           or,
!  [X(N+1,1)-X(N,1)]/dt = xhu(1)*Kyy(1+0.5)*(X(N+1,1+1)-X(N+1,1)).
!           giving,
!  Th(1,1,iz) = 0.
!  Th(2,1,iz) = -xhu(1)*Kyy(1 + .5)
!  Th(3,1,iz) = xhu(1)*Kyy(1 + .5)
!-----------------------------------------------------------------------
!   	... Top:
!    xnp1(l,niz-1) = xnp1(l,niz-1) + dt*xvu*[Kzz(niz)+Kzz(niz-1)]*ubmr(l,ic)
!    Tv(1,l,niz-1) = xvl*[Kzz(niz-1)+Kzz(niz-2)]
!    Tv(2,l,niz-1) = -xvu*(Kzz(niz-1)+Kzz(niz))-Tv(1,l,niz-1)
!    Tv(3,l,niz-1) = 0.
!-----------------------------------------------------------------------
!  	... We have           {I-dt*(Tv+Th)}X(N+1) = X(N)
!           approximate    (I-dt*Tv)*(I-dt*Th)*XP(N+1) = X(N)
!           define            (I-dt*Th)*X(N+1) = Z(N)
!           solve for Z        (I-dt*Tv)*Z(N) = X(N)
!            use Z to solve for XP(N+1)
!            correct X(N+1)   1. (I-dt*Th)*(I-dt*Tv)*X(N+1) = X(N) + dt*dt*Th*Tv*X(N+1)
!            giving          (I-dt*Th)*(I-dt*Tv)*(X(N+1) - XP(N+1)) = dt*dt*Th*Tv*X(N+1)
!            solve for diff(N) = X(N+1) - XP(N+1) and repeat if neccessary.
!=======================================================================
      use PHYS_CST, only : pi, d2r, R0
      use SPECIES_DIMS, only : nbcon
      use GRID_DIMS, only : lmax, niz
      use BOUNDARIES, only : lbc, ubc
      use ALLCO
      use CONC
      use VEN1, only : t2d
      use VEN9
      use SPC_NAMES
      use DIAG_CONTROLS, only : diags, ldiag, zdiag
      use NUMERICAL, only : TRIDEC, TRISLV

      implicit none

!-----------------------------------------------------------------------
!	... Parameters
!-----------------------------------------------------------------------
      integer, parameter :: nl = 1, nu = niz
      integer, parameter :: vdim = nu - nl + 1
      integer, parameter :: lower = 1, main = 2, upper = 3
      integer, parameter :: specified = 0
      real, parameter    :: re = R0 * 1.e5            ! radius of Earth in cm

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      real, intent(in)    :: dts                   ! time step = 1 day = 86400 s
      real, dimension(lmax,niz,nbcon), intent(inout) :: qn2noon

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer :: m, l, lm1
      real ::  xx
      real, save ::  dy, dysq
      real, dimension(lmax,nl:nu,3) ::  th, mh
      real, save, dimension(lmax)   ::  xhl, xhu
      logical, save :: entered = .false.

      if( .not. entered ) then
         dy = pi*re/REAL(lmax+1)
         dysq = dy*dy
         xx = 5.e3/dysq
         xhl = xx*COS( phir - d2r*2.5 )/COS( phir )
         xhu = xx*COS( phir + d2r*2.5 )/COS( phir )
         entered = .true.
      end if

!-----------------------------------------------------------------------
! 	... The diagonals of th (Transport horizontal)
!-----------------------------------------------------------------------
      th(lmax,nl:nu,lower) = 0.
      th(lmax,nl:nu,upper) = 0. 
      th(1,nl:nu,main)  = - xhu(1) * (xky(1,nl:nu) + xky(2,nl:nu))
      th(1,nl:nu,upper) = - th(1,nl:nu,main)
      th(lmax-1,nl:nu,lower) = xhl(lmax)
     $                       *(xky(lmax,nl:nu) + xky(lmax-1,nl:nu))
      th(lmax,nl:nu,main)  = - th(lmax-1,nl:nu,lower)
      do l = 2, lmax-1
	 lm1 = l - 1
         th(lm1,nl:nu,lower) = xhl(l)*(xky(l,nl:nu) + xky(lm1,nl:nu))
         th(l,nl:nu,upper) = xhu(l)*(xky(l,nl:nu) + xky(l+1,nl:nu))
         th(l,nl:nu,main)  = -(th(lm1,nl:nu,lower) + th(l,nl:nu,upper))
      end do

!-----------------------------------------------------------------------
! 	...  mh = I - dt*Th
!-----------------------------------------------------------------------
      do l = 1,lmax
         mh(l,nl:nu,lower) = -dts*th(l,nl:nu,lower)
         mh(l,nl:nu,main)  = 1. - dts*th(l,nl:nu,main) 
         mh(l,nl:nu,upper) = -dts*th(l,nl:nu,upper)
      end do
!-----------------------------------------------------------------------
! 	... Factor the systems
!-----------------------------------------------------------------------
      call TRIDEC( vdim,
     $             lmax,
     $             mh(1,nl,lower),
     $             mh(1,nl,main),
     $             mh(1,nl,upper) )

!-----------------------------------------------------------------------
! 	... Solve the systems
!-----------------------------------------------------------------------
      do m = 1, nbcon
!         if( m == vid_x .or. m == vid_o2dg .or. m == vid_o2s ) cycle
         call TRISLV( vdim,
     $                lmax,
     $                mh(1,nl,lower),
     $                mh(1,nl,main),
     $                mh(1,nl,upper),
     $                qn2noon(1,nl,m) )

!----------------------------------------------------------
! 	... Enforce boundary conditions
!----------------------------------------------------------
 	 if( ubc(m)%is_vmr ) qn2noon(:,niz,m) = ubc(m)%val(:)
 	 if( lbc(m)%is_vmr ) qn2noon(:,1,m) = lbc(m)%val(:)

      end do

      end subroutine HDIFF
