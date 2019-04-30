! subversion Id for THIS file : $Id: advect_chem.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/advect_chem.f $
!-----------------------------------------------------------------------

      subroutine ADVECT_CHEM( dt, vmr )
!-----------------------------------------------------------------------
!  	... Advection of the chemical species by the Semi Lagrangian 
!           This routine was called SLT prior to v7s00
!-----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use SPECIES_DIMS, only : nbcon
      use BOUNDARIES, only : lbc, ubc
      use TRACNM, only : solsym                                   ! for diags
      use DIAG_CONTROLS, only : debug, diags, ldiag, zdiag, idiag
      use SPC_NAMES

      implicit none
!-----------------------------------------------------------------------
!  	... Parameters - nbiter was always 1, tried 10 at v6s31, no effect
!-----------------------------------------------------------------------
      integer, parameter :: specified = 0, nbiter = 2

!-----------------------------------------------------------------------
!  	... Dummy args
!-----------------------------------------------------------------------
      real, intent(in) :: dt
      real, intent(inout) :: vmr(lmax,niz,nbcon)

!-----------------------------------------------------------------------
!  	... Local variables
!-----------------------------------------------------------------------
      integer :: l, m
      real :: fixer, xf(lmax*niz)
      real, dimension(lmax,niz) :: y0, z0, oldvmr, newvmr
      real, dimension(nbcon) :: maxchg, nitot0, nitot1
      real, external :: TOTAL_MOLEC         ! function declaration

         maxchg(:) = 0.1
         maxchg( (/ vid_ch4, vid_h2o, vid_co2, vid_h2, vid_o2, 
     $                vid_o3, vid_h, vid_co, vid_o3p, vid_oh /) ) = 0.02

      if( diags ) write(*,'(2(a,es12.3))') 
     $   'ADVECT_CHEM begins, lbc(O3)= ',lbc(vid_o3)%val(ldiag),
     $        ' ; vmr(O3)= ',vmr(ldiag,zdiag,vid_o3)
      
!----------------------------------------------------------
!  	... Find the departure point
!----------------------------------------------------------
      call TRAJECT( dt, y0, z0, nbiter, .false. )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$OMP PARALLEL DO
!$OMP&PRIVATE( m, oldvmr, xf, newvmr )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      do m = 1,nbcon
         oldvmr = vmr(:,:,m)
         nitot0(m) = TOTAL_MOLEC( oldvmr )
         
!-----------------------------------------------------------------------
! 	... Interpolate long-lived for the
!	    noon value at the departure point
!-----------------------------------------------------------------------
         xf = RESHAPE( oldvmr, (/ lmax*niz /) )
         call LE3( xf, y0, z0, 1 )
         newvmr = RESHAPE( xf, (/ lmax,niz /) )
         vmr(:,:,m) = newvmr
      end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$OMP END PARALLEL DO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!-----------------------------------------------------------------------
! 	... Scaling (fixer) to conserve total nb of molecules
!-----------------------------------------------------------------------
      do m = 1,nbcon
         if( nitot0(m) == 0. ) then
            vmr(:,:,m) = 0.
            cycle
         end if
         nitot1(m) = TOTAL_MOLEC( vmr(:,:,m) )
         fixer = nitot0(m) / nitot1(m)
         if( fixer < (1.-maxchg(m)) .or. fixer > (1.+maxchg(m)) ) then
            write(*,*)'ADVECT_CHEM warning: trying to advect species ',
     $                                    solsym(m)
            write(*,*) '   nitot0= ',nitot0(m),' ; nitot1= ',nitot1(m),
     $           ' ; fixer= ',fixer
            write(*,*) '     fixer is out of bound 1+- ',maxchg(m)
            if( debug )stop'ADVECT_CHEM: out-of-bound fixer, see stdout'
         end if
         vmr(:,:,m) = fixer * vmr(:,:,m)
         if( diags .and. m == idiag ) write(*,'(3a,es12.3,a,es12.3)') 
     $        'ADVECT_CHEM, ',solsym(m),': nitot0= ',nitot0(m),
     $           ' ; nitot1= ',nitot1(m)

!----------------------------------------------------------
! 	... Enforce boundary conditions
!----------------------------------------------------------
 	 if( ubc(m)%is_vmr ) vmr(:,niz,m) = ubc(m)%val(:)
 	 if( lbc(m)%is_vmr ) vmr(:,1,m) = lbc(m)%val(:)
      end do

      if( diags ) write(*,'(a,es12.3)') 
     $     'ADVECT_CHEM ends, vmr(O3)= ',vmr(ldiag,zdiag,vid_o3)
     
      end subroutine ADVECT_CHEM
     
!=======================================================================

      subroutine LE3( xf, yd, zd, nonos )
!--------------------------------------------------------------------
!	... Interpolate grid data to find value at departure points
!--------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz

      implicit none

      integer, parameter :: nn = niz*lmax
      real, parameter    :: ep = 1.e-30

!--------------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------------
      integer, intent(in) :: nonos                       ! method flag
      real, intent(inout) :: xf(nn)                      ! field to interpolate
      real, dimension(lmax,niz), intent(in) :: yd, zd    ! departure coordinates

!--------------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------------
      integer :: ii, k
      integer :: jg0(nn), kg0(nn)
      real  ::  fl0, fl1, f1, un, ov, w, f0, u
      real  ::  ym1, y0, yp1
      real  ::  mx, mn 
      real  ::  y(nn,-1:1)
      real  ::  yd1(nn), zd1(nn)
      real  ::  x(0:lmax+1,0:122)

      real  :: a, y1, y2, xi
      real  :: TR2, PP, PN

      TR2(y1,y2,a) = a*.5*(y1 + y2 - a*(y2 - y1))
      PP(xi) = MAX( 0., xi )
      PN(xi) = MIN( 0., xi )

      yd1 = RESHAPE( yd(:,:),(/ nn /) )
      zd1 = RESHAPE( zd(:,:),(/ nn /) )
      x(1:lmax,1:niz) = RESHAPE( xf, (/ lmax,niz /) )
!--------------------------------------------------------------------
!  	... Constant grid extension :
!           no extrapolation in vertical or horizontal
!--------------------------------------------------------------------
      x(:,0) = x(:,1)
      x(:,122) = x(:,niz)
      x(0,:) = x(1,:)
      x(lmax+1,:) = x(lmax,:)

!--------------------------------------------------------------------
! 	... Calculate grid point nearest the departure point
!--------------------------------------------------------------------
      jg0 = NINT( yd1 )
      kg0 = NINT( zd1 )

!--------------------------------------------------------------------
!  	... Start of interpolation
!--------------------------------------------------------------------
      do k = -1,1
         if( nonos == 1 ) then
             do ii = 1,nn
                u   = REAL(jg0(ii)) - yd1(ii)
                ym1 = x(jg0(ii)-1,kg0(ii)+k)
                y0  = x(jg0(ii)  ,kg0(ii)+k)
                yp1 = x(jg0(ii)+1,kg0(ii)+k)
                f0  = TR2( ym1, y0, u )
                f1  = TR2( y0, yp1, u )
		if( u >= 0. ) then
		   fl0 = ym1 * u
		   fl1 = y0 * u
		else
		   fl0 = y0 * u
		   fl1 = yp1 * u
		end if
                w   = y0 - (fl1 - fl0) 
                mx  = MAX( ym1,y0,yp1,w )
                mn  = MIN( ym1,y0,yp1,w )
                f0  = f0 - fl0
                f1  = f1 - fl1
                ov  = (mx - w) / (-PN(f1) + PP(f0) + ep)
                un  = (w - mn) / (PP(f1) - PN(f0) + ep)
                ov  = MIN(1., ov)
                un  = MIN(1., un)
                f0  = PP(f0)*ov + PN(f0)*un
                f1  = PP(f1)*un + PN(f1)*ov
                y(ii,k) = w - (f1 - f0)
             end do
         else
             do ii = 1,nn
                u   = REAL(jg0(ii)) - yd1(ii)
                ym1 = x(jg0(ii)-1,kg0(ii)+k)
                y0  = x(jg0(ii)  ,kg0(ii)+k)
                yp1 = x(jg0(ii)+1,kg0(ii)+k)
                f0  = TR2(ym1, y0 ,u)
                f1  = TR2(y0  ,yp1,u)
                y(ii,k) = y0 - (f1 - f0)
            end do
         end if
      end do
      if( nonos == 1 ) then
         do ii = 1,nn
            u   = REAL(kg0(ii)) - zd1(ii)
            f0  = TR2( y(ii,-1), y(ii,0), u )
            f1  = TR2( y(ii, 0), y(ii,1), u )
	    if( u >= 0. ) then
	       fl0 = y(ii,-1) * u
	       fl1 = y(ii,0) * u
	    else
	       fl0 = y(ii,0) * u
	       fl1 = y(ii,1) * u
	    end if
            w   = y(ii,0) - (fl1 - fl0)
            mx  = MAX( y(ii,-1),y(ii,0),y(ii,1),w )
            mn  = MIN( y(ii,-1),y(ii,0),y(ii,1),w )
            f0  = f0 - fl0
            f1  = f1 - fl1
            ov  = (mx - w) / (-PN(f1) + PP(f0) + ep)
            un  = (w - mn) / (PP(f1) - PN(f0) + ep)
            ov  = MIN(1., ov)
            un  = MIN(1., un)
            f0  = PP(f0)*ov + PN(f0)*un
            f1  = PP(f1)*un + PN(f1)*ov
            xf(ii) = w - (f1 - f0)
         end do
      else
         do ii = 1,nn
            u  = REAL(kg0(ii)) - zd1(ii)
            f0 = TR2( y(ii,-1), y(ii,0), u )
            f1 = TR2( y(ii, 0), y(ii,1), u )
            xf(ii) = y(ii,0) - (f1 - f0) 
         end do
      end if

      end subroutine LE3
     
!=======================================================================

      subroutine TRAJECT( dt, y0, z0, nbiter, pt_flag )
!----------------------------------------------------------
! 	... Compute the departure point
!----------------------------------------------------------
      use PHYS_CST, only : R0, pi
      use VEN6, only : v, w
      use GRID_DIMS, only : lmax, niz

      implicit none

!----------------------------------------------------------
! 	... Parameters
!----------------------------------------------------------
      real, parameter :: dz = 1.e5
      real, parameter :: re = R0 * 1.e5     ! radius of Earth, cm

!----------------------------------------------------------
! 	... Dummy args
!----------------------------------------------------------
      integer, intent(in) :: nbiter
      real, intent(in)    :: dt
      real, dimension(lmax,niz), intent(out) :: y0, z0
      logical, intent(in) :: pt_flag

!----------------------------------------------------------
! 	... Local variables
!----------------------------------------------------------
      integer :: l, iz, iter, it
      real :: dy, dtn, gc2, gc3
      real, dimension(lmax,niz) :: vn, wn, vl2, vl3,
     $                             ym, zm

!----------------------------------------------------------
!   	... dy : angular size of a latitude interval.
!           dz : vertical grid spacing in cm.
!-----------------------------------------------------------
      dy = pi / REAL( lmax+1 )
      dtn = dt / REAL( nbiter )
      gc2 = dtn / (dy*re)
      gc3 = dtn / dz
      if( pt_flag ) then
	 gc2 = 100.*gc2
	 gc3 = 100.*gc3
      end if

      do l = 1,lmax
         vn(l,:) = v(l,:)*gc2
         wn(l,:) = w(l,:)*gc3
         y0(l,:) = REAL(l)
      end do
      do iz = 1,niz
         z0(:,iz) = REAL(iz)
      end do
      do iter = 1,nbiter
          ym = y0 - .5*vn
          zm = z0 - .5*wn
          call TRAJBC( ym, zm )
          do it = 1,2
             vl2 = vn
             vl3 = wn
             call LE3( vl2, ym, zm, 1 )
             call LE3( vl3, ym, zm, 1 )
             ym = y0 - .5*vl2
             zm = z0 - .5*vl3
             call TRAJBC( ym, zm )
          end do
          y0 = 2.*ym - y0
          z0 = 2.*zm - z0
          call TRAJBC( y0, z0 )
      end do

      end subroutine TRAJECT
     
!=======================================================================

      subroutine TRAJBC( y, z )
!--------------------------------------------------------------------
! 	... TRAJBC fixes departure points that go outside the domain,
!           that is, whose nearest grid point evaluates to an 
!           iz greater than niz or less than 1, or whose l evaluates
!	    to more than lmax or less than 1
!--------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz

      implicit none

!--------------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------------
      real, dimension(lmax,niz), intent(inout) :: y, z

!--------------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------------
      integer :: iz, l

      do iz = 1,niz
         do l = 1,lmax
           y(l,iz) = MIN( y(l,iz), 35.4999 )
           y(l,iz) = MAX( y(l,iz), .5001 )
           z(l,iz) = MIN( z(l,iz), 121.4999 )
           z(l,iz) = MAX( z(l,iz), .5001 )
         end do
      end do

      end subroutine TRAJBC

