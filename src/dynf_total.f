! subversion Id for THIS file : $Id: dynf_total.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/dynf_total.f $
!-----------------------------------------------------------------------
      subroutine DYNF_TOTAL( daynum )
!-----------------------------------------------------------------------
!     	... Calculates forcing term for streamfunction equation
!-----------------------------------------------------------------------
      use PHYS_CST, only : R0
      use GRID_DIMS, only : lmax, niz
      use ALLCO, only : phir, zkm
      use TROPIC,       only : clh
      use TAU0, only  : q
      use VEN1, only  : t2d
      use VEN2, only  : u
      use VEN3, only  : ff, uz, uzz
      use VEN5, only  : cf, fx, ftot, f_tropic
      use VEN10, only : mu
      use VEN12, only : ftrop
      use VEN13, only : fbnd, fmvisc
      use VEN14, only : t2dqprt, uqprt, f_e
      use ROSS, only  : fr
      use TIDE1, only : ftx
      use HEAT_TERMS, only : dh, dxt
      use SIM_CONTROLS, only : mainsw, mlt_sw
      use DIAG_CONTROLS, only :  diags, ldiag, zdiag
      use TRANSFORM, only : HDERIV, VDERIV, SMOOTHV, SMOOTHL

      implicit none

!-----------------------------------------------------------------------
!     	... Parameters
!-----------------------------------------------------------------------
      real, parameter :: dzi  = 1.e-3
      real, parameter :: dz2i  = 5.e-4
      real, parameter :: hi = 1. / 7.00e03
      real, parameter :: rg = 2.84e02
      real, parameter :: c = rg*hi / (R0*1.e3)

!-----------------------------------------------------------------------
!     	... Dummy args
!-----------------------------------------------------------------------
      real, intent(in) :: daynum

!-----------------------------------------------------------------------
!     	... Local variables
!-----------------------------------------------------------------------
      integer :: k, l
      real, dimension(lmax) :: tfac
      real, dimension(niz)  :: rfc, rfd, t1, t2
      real, dimension(lmax,niz) :: us, usz, uszz,  qtot, qtotl, ftotz

!-----------------------------------------------------------------------
!     	... Forcing by diabatic heating q
!           1st term of RHS of (eq.12) in Brasseur/et-al:1990
!-----------------------------------------------------------------------
!     In this version the diabatic heating term is altered
!     by addition of the QBO perturbation factor dT(qbo)/dt,
!     with units of K/day. This is an attempt to force the
!     meridional streamfunction to generate a QBO instead
!     of a brute-force thermal wind perturbation.
!-----------------------------------------------------------------------
      if( mainsw(8) == 1 ) then
          qtot = q + t2dqprt - dh + dxt
      else 
          qtot = q - dh + dxt
      end if
!      qtot = qtot - 0.5*clh/86400.        ! v6s37a test - to tune tropo dyn.
      call HDERIV( qtot, basedl = qtotl )
      cf(:,:) = c * qtotl(:,:)

!-----------------------------------------------------------------------
!     	... Forcing term (wind stress in lower troposphere)
!           is a vert eddy flux at surf (2->5km) from surf friction  *DH
!           Uses *smoothed* 2d derivative of u
!-----------------------------------------------------------------------
      us  = u
      call SMOOTHV( us, 1, 121, 3 )
      call VDERIV( us, basedz = usz )
      call SMOOTHV( usz, 1, 121, 1 )
      call VDERIV( usz, basedz = uszz )
      call SMOOTHV( uszz, 1, 121, 1 )
      fbnd(:,:) = 0.
      fbnd(:,1:5) = 100. * uszz(:,1:5)
       
!-----------------------------------------------------------------------
!     	... Forcing in the troposphere  
!-----------------------------------------------------------------------
      call DYNF_TROPO(daynum, ftrop )

      if( mainsw(4) == 0 ) then
!-----------------------------------------------------------------------
!     	... If no GW param, use forcing by rayleigh friction
!-----------------------------------------------------------------------
         rfc(1:50) = 5.e8
         rfc(51:121) = 5.e-8 + 5.e-9*(zkm(51:121)-49.)*(zkm(51:121)-49.)
         rfd(1:52) = 5.e8
         rfd(53:121) = 5.e-8 + 5.e-9*(zkm(53:121)-51.)*(zkm(53:121)-51.)
         do l = 1,lmax
            t2(2:121-1) = - rfc(2:121-1) * u(l,3:121)
            t1(2:121-1) = - rfd(2:121-1) * u(l,1:121-2)
            cf(l,2:121-1) = cf(l,2:121-1) 
     $                    + ff(l)*(t2(2:121-1) - t1(2:121-1))*dz2i
            t2(121) = - rfc(121) * u(l,121)
            t1(121) = - rfc(121-1) * u(l,121-1)
            cf(l,121) = cf(l,121) + ff(l)*(t2(121) - t1(121))*dzi
         end do
      end if

!-----------------------------------------------------------------------
!     	... Forcing by molecular viscosity. mu calc in ATMCOND.
!-----------------------------------------------------------------------
      if( mlt_sw(1) == 1 ) call VDERIV( mu*uz, basedz = fmvisc )
 
!-----------------------------------------------------------------------
!     	... Add all wave momentum forcings, get vertical gradient
!-----------------------------------------------------------------------
      ftot = fbnd + ftrop                   ! lower boundary and tropo forcings
      if( mainsw(4) > 0  ) ftot = ftot + fx        ! forcing by Gravity Waves breaking
      if( mainsw(5) /= 0 ) ftot = ftot + fr        ! forcing by Rossby waves
      if( mainsw(8) == 2 ) then 
          ftot = ftot + f_e                      ! forced by derived
      else if( mainsw(8) == 3 ) then
          ftot = ftot + f_tropic                    ! forcing by tropical Kelvin and Rossby-gravity waves
      else if ( mainsw(8) == 4) then
          ftot = ftot + f_tropic
      end if

      if( mlt_sw(1) == 1 ) ftot = ftot + fmvisc    ! forcing by molecular viscosity
      if( mlt_sw(2) == 1 ) ftot = ftot + ftx       ! forcing by tidal wave braking
      call VDERIV( ftot, basedz = ftotz )
      do l = 1, lmax
         cf(l,:) = cf(l,:) + ff(l) * ftotz(l,:)
      end do
      do k = 1,2
         call SMOOTHL( cf, 1, niz, 2 )
         call SMOOTHV( cf, 1, niz, 10 )
      end do

      if( diags ) then
         write(*,'(3(a,es11.3))') 'DYNF_TOTAL at (ldiag,zdiag): q: ',
     $   q(ldiag,zdiag),' ; t2dqprt= ',t2dqprt(ldiag,zdiag),' ; dh= ',
     $   dh(ldiag,zdiag)
         write(*,'(5x,2(a,es11.3))') '  dxt= ',dxt(ldiag,zdiag),
     $                     ' ; qtotl: ',qtotl(ldiag,zdiag)
         write(*,'(5x,3(a,es11.3))') '  fx= ',fx(ldiag,zdiag),
     $     ' ; fr= ',fr(ldiag,zdiag),' ; fmvisc= ',fmvisc(ldiag,zdiag)
         write(*,'(5x,3(a,es11.3))') '  ftx= ',ftx(ldiag,zdiag),
     $           ' ; ftot: ',ftot(ldiag,zdiag),' ; cf: ',cf(ldiag,zdiag)
      end if

      end subroutine DYNF_TOTAL
