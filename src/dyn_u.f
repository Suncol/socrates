! subversion Id for THIS file : $Id: dyn_u.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/dyn_u.f $
!-----------------------------------------------------------------------
      subroutine DYN_U( ok )
!-----------------------------------------------------------------------
!     	... Calculate zonal wind u by solving thermal wind equation
!           See eq (4) of Brasseur/et-al:1990, using t2d i.o. pot temp
!           since EXP(kappa*z/7) cancels out - 
!           The equation is re-written as: du/dz * (1+au) = c(z), where
!           1+au must NOT be allowed to get close to zero
!           To be sure to avoid this, 1+au uses HWM values 
!                                       simonc@oma.be, v4s026c, feb 2000
!           Calculation now begins at 2km (LBC from HWM at 0 and 1km)
!                                       simonc@oma.be, v6s35a,  jan 2001
!-----------------------------------------------------------------------
      use TIME_CONTROLS
      use PHYS_CST, only : R0, g0
      use GRID_DIMS, only : lmax, niz
      use ALLCO, only : phir
      use VEN1, only  : t2d
      use VEN2, only : u                ! output
      use VEN3, only : ff, tl
      use SIM_CONTROLS, only : mainsw
      use BACKATM, only : u_hwm
      use TRANSFORM, only : SMOOTHL

      implicit none

!-----------------------------------------------------------------------
!     	... Parameters
!-----------------------------------------------------------------------
      real, parameter :: dzs = 1.e3          ! one vert steps in log-p grid
      real, parameter :: eps_au1  = 0.1      ! must be > 1.e-2

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      logical, intent(out) :: ok

!-----------------------------------------------------------------------
!     	... Local variables
!-----------------------------------------------------------------------
      integer :: iz, l
      real, dimension(niz) :: c, uk, au1
      real, dimension(lmax), save :: a, const0
      logical, save :: entered = .false.

      if( mainsw(3) == 0 ) then     
         u = u_hwm
         return
      end if
      
      if( .not. entered ) then
         a(:) = 2. * TAN(phir(:)) / ( ff(:) * R0*1.e3 )
         do l = 15, 21
            const0(l) = .125 * FLOAT(l-14)
         end do
         entered = .true.
      end if

      ok = .true.
      
      u(:,1:3) = u_hwm(:,1:3)     ! LBC from HWM at 0 and 1km - v6s35a

      do l = 1, lmax
         if( l >= 15 .and. l <= 21 ) cycle                   ! see below
         c(:) = - g0 * tl(l,:) / ( R0*1.e3 * t2d(l,:) * ff(l) )
         uk(:) = u(l,:) 
         if( mainsw(3) == 1 ) then
            au1(:) = 1. + a(l) * u_hwm(l,:)
          else if( mainsw(3) == 2 ) then
            au1(:) = 1. + a(l) * uk(:)
         end if
         
!-----------------------------------------------------------------------
!     	... Never let 1.+au get too close to 0. (singularity)
!-----------------------------------------------------------------------
         if( ANY( ABS(au1(1:niz-1)) < eps_au1 ) ) then
            write(*,*) 'DYN_U warning: singularity at lat= ',l
            ok = .false.
         end if

!-----------------------------------------------------------------------
!     	... Upward integration by Euler forward 2-points discretization
!-----------------------------------------------------------------------
         do iz = 3, niz-1
            uk(iz+1) = uk(iz) + c(iz)*dzs / au1(iz)
         end do
         u(l,4:niz) = uk(4:niz)
      end do

!-----------------------------------------------------------------------
!     	... Results not reliable in the tropics, connect -15S to 15N
!-----------------------------------------------------------------------
      do l = 15,21
         u(l,:) = u(14,:) + const0(l) * (u(22,:) - u(14,:))
      end do

      call SMOOTHL( u, 2, 121, 1 )

      end subroutine DYN_U
