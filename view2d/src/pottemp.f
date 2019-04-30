! subversion Id for THIS file : $Id: pottemp.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/pottemp.f $
!-----------------------------------------------------------------------
      subroutine POTTEMP( delt, lbpt, ubpt, pt, q, pt_relax )
!-----------------------------------------------------------------------
!     	... Calc potential temperature by solving thermodynamic equation. 
!           v6s35b: Correction of q to relax to ref (MSIS) T now done in MAIN
!-----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use PHYS_CST, only : R0
      use ALLCO, only : phir, dlatr
      use VEN6, only : v, w
      use VEN9, only : xky, xkyl, xkz
      use VEN10, only : xtz
      use TIDE1, only : dkt
      use ALP, only : alpha
      use TEMP_HIST, only : perm, this
      use DIAG_CONTROLS, only : diags, ldiag, zdiag         ! for diags
      use NUMERICAL, only : TRIDEC, TRISLV
      use TRANSFORM, only : VDERIV

      implicit none
!-----------------------------------------------------------------------
!     	... Parameters
!-----------------------------------------------------------------------
      integer, parameter :: nl = 4
      integer, parameter :: nu = niz-1
      integer, parameter :: nizz = nu - nl + 1
      integer, parameter :: lens = lmax*nizz + 1
      integer, parameter :: nal = nizz
      integer, parameter :: itmax = 4
      real, parameter    :: dzi = 1.e-3
      real, parameter    :: dz2i = .5 * dzi
      real, parameter    :: dzsqi = dzi * dzi
      real, parameter    :: rei = 1.e-3 / R0
      real, parameter    :: hi = 1. / 7.e3

!-----------------------------------------------------------------------
!     	... Dummy args
!-----------------------------------------------------------------------
      real, intent(in) :: delt                             ! s
      real, intent(in), dimension(lmax) :: lbpt, ubpt      ! Boundary conditions
      real, intent(in), dimension(lmax,niz) :: q
      real, intent(in), dimension(niz)      :: pt_relax    ! 1/s (as alpha)
      real, intent(inout), dimension(lmax,niz) :: pt       ! K
 
!-----------------------------------------------------------------------
!     	... Local variables
!-----------------------------------------------------------------------
      integer, save  ::  callno = 0
      integer  ::  syscnt, head
      integer  ::  i, k, iter, l, n
      real   ::    dt, p, xa, xb
      real, dimension(:,:), pointer ::  aa, aw
      real, dimension(nizz,lmax) :: bb, cc, dd
      real, dimension(lmax,nizz) :: bw, cw, dw
      real, dimension(lmax,niz) :: kzz, dkzz
      real, target   ::    aam(nizz,0:lmax)
      real, target   ::    awm(lmax,0:nizz)
      real, dimension(lmax,niz)  :: rhs, frcinv
      real, save   ::    wt(3) = (/ -.12, .64, -1.44 /)
      real, save   ::    dyi, dy2i, dysqi, tanlat(lmax)
      logical, save :: entered = .false.
 
!-----------------------------------------------------------------------
!     	... Initialization
!-----------------------------------------------------------------------
      if( .not. entered ) then
	 if( perm(1) == 1 ) then
	    callno = 6
	 else if( perm(1) == 2 ) then
	    callno = 4
	 else if( perm(1) == 3 ) then
	    callno = 5
         end if
         dyi = rei / dlatr
         dy2i = .5 * dyi
         dysqi = dyi*dyi
	 tanlat(:) = TAN( phir(:) )
	 entered = .true.
      end if

      if( diags ) write(*,'(a,i2,a,i3,6(a12,es11.3))') 
     $   'POTTEMP begins @(',ldiag,',',zdiag,'): pt: ',pt(ldiag,zdiag),
     $   ' ; q: ',q(ldiag,zdiag)
!     $   ' , xky= ',xky(1,113),' ; xkyl= ',xkyl(1,113)
!      write(*,'(10x,3(a,es12.5))') 'xkz= ',xkz(1,113),
!     $     ' ; dkt= ',dkt(1,113),' ; xtz= ',xtz(1,113)
!      write(*,'(10x,3(a,es12.5))') 'v= ',v(1,113),' ; w= ',
!     $           w(1,113),' ; q= ',q(1,113)
 
      aa => aam(1:nizz,1:lmax)
      aw => awm(1:lmax,1:nizz)
!-----------------------------------------------------------------------
!     	... Set diffusion for potential temp
!-----------------------------------------------------------------------
      kzz(:,:) = xkz(:,:) + xtz(:,:)
      call VDERIV( kzz, basedz = dkzz )
      if( callno >= 3 ) then
         dt = .48*delt
         frcinv(:,nl:nu) = wt(1)*this(:,nl:nu,perm(1))
     $                   + wt(2)*this(:,nl:nu,perm(2))
     $                   + wt(3)*this(:,nl:nu,perm(3))
     $                   + 1.92*pt(:,nl:nu) + dt* q(:,nl:nu) 
	 perm(:) = CSHIFT( perm(:), 1 )
      else
         dt = delt
         frcinv(:,nl:nu) = pt(:,nl:nu) + dt * q(:,nl:nu)
      end if

!-----------------------------------------------------------------------
!     	... Setup "history" array
!-----------------------------------------------------------------------
      head = MOD( callno,3 ) + 1
      this(:,:,head) = pt(:,:)

!-----------------------------------------------------------------------
!     	... Diffusion and advection
!-----------------------------------------------------------------------
      do l = 1,lmax
         xa = kzz(l,nl)*dt*dzsqi
         xb = (w(l,nl) + kzz(l,nl)*hi - dkzz(l,nl))*dt*dz2i
         frcinv(l,nl) = frcinv(l,nl) + (xa + xb)*lbpt(l)
         xa = kzz(l,nu)*dt*dzsqi
         xb = (w(l,nu) + kzz(l,nu)*hi - dkzz(l,nu))*dt*dz2i
         frcinv(l,nu) = frcinv(l,nu) + (xa - xb)*ubpt(l)
      end do 
 
      do l = 2,lmax-1
         do k = nl,nu
            xa = xky(l,k)*dt*dysqi
            xb = (v(l,k) + (xky(l,k)*tanlat(l)
     $                       - xkyl(l,k))*rei)*dt*dy2i
            rhs(l,k) = (xa - xb)*pt(l+1,k)
     $               - (2.*xa + .5*dt*(alpha(k)+pt_relax(k)))*pt(l,k)
     $               + (xa + xb)*pt(l-1,k)
         end do
      end do
      do k = nl,nu
         xa = xky(1,k)*dt*dysqi
         xb = (v(1,k) + (xky(1,k)*tanlat(1)
     $                   - xkyl(1,k))*rei)*dt*dy2i
         rhs(1,k) = (xa - xb)*pt(2,k)
     $            - (xa - xb + .5*dt*(alpha(k)+pt_relax(k)))*pt(1,k)
         xa = xky(lmax,k)*dt*dysqi
         xb = (v(lmax,k) + (xky(lmax,k)*tanlat(lmax)
     $                      - xkyl(lmax,k))*rei)*dt*dy2i
         rhs(lmax,k) = (xa + xb)*pt(lmax-1,k)
     $          - ( xa + xb + .5*dt*(alpha(k) + pt_relax(k)))*pt(lmax,k)
      end do

      do l = 1,lmax
         do k = nl,nu
            xa = kzz(l,k)*dt*dzsqi
            xb = (w(l,k) + kzz(l,k)*hi - dkzz(l,k))*dt*dz2i
            i = k - nl + 1
            aa(i-1,l) = -(xa + xb)
            bb(i,l) = 1. + 2.*xa
     $                   + .5 * ( alpha(k) + pt_relax(k) )*dt
            cc(i,l) = xb - xa
         end do
      end do
      aa(nal,:) = 0.
      cc(nal,:) = 0.

!-----------------------------------------------------------------------
!     	... Factor the "horizontal" scan
!-----------------------------------------------------------------------
      call TRIDEC( lmax, nal, aa, bb, cc )

      do k = 1,nal
         do l = 1,lmax
            xa = xky(l,k+nl-1)*dt*dysqi
            xb = (xky(l,k+nl-1)*tanlat(l)
     $            - xkyl(l,k+nl-1))*rei*dt*dy2i
            aw(l-1,k) = -(xa + xb)
            bw(l,k) = 1. + 2.*xa
     $                   + .5*(alpha(k+nl-1) + pt_relax(k+nl-1))*dt
            cw(l,k) = xb - xa
         end do
      end do
      bw(1,:) = bw(1,:) + awm(lmax,0:nal-1)
      bw(lmax,:) = bw(lmax,:) + cw(lmax,:)
      aw(lmax,:) = 0.
      cw(lmax,:) = 0.
 
!-----------------------------------------------------------------------
!     	... Factor the "vertical" scan
!-----------------------------------------------------------------------
      call TRIDEC( nizz, lmax, aw, bw, cw )

      do iter = 1,itmax
!-----------------------------------------------------------------------
!     	... Horizontal scan
!-----------------------------------------------------------------------
         if( iter == 1 ) then
            do l = 1,lmax
               dd(:,l) = frcinv(l,nl:nu) + rhs(l,nl:nu)
               rhs(l,nl:nu) = dd(:,l)
	    end do
         else
            do l = 1,lmax
               dd(:,l) = frcinv(l,nl:nu) + pt(l,nl:nu) - rhs(l,nl:nu)
               rhs(l,nl:nu) = dd(:,l)
	    end do
         end if
         if( diags ) write(*,*) 'POTTEMP, iter= ',iter
! zeno.oma.be, segmentation fault below if "ifort -O3"
         call TRISLV( lmax, nizz, aa, bb, cc, dd )
         do l = 1,lmax
            pt(l,nl:nu) = dd(:,l)
	 end do
 
!-----------------------------------------------------------------------
!     	... Vertical scan
!-----------------------------------------------------------------------
         do l = 1,lmax
            dw(l,:) = frcinv(l,nl:nu) + pt(l,nl:nu) - rhs(l,nl:nu)
            rhs(l,nl:nu) = dw(l,:)
	 end do
         call TRISLV( nizz, lmax, aw, bw, cw, dw )
         do l = 1,lmax
            pt(l,nl:nu) = dw(l,:)
	 end do
      end do
      callno = callno + 1
!-----------------------------------------------------------------------
!    	... Set potential temperature at the upper and lower boundaries
!-----------------------------------------------------------------------
      pt(:,nl-1) = lbpt(:)
      pt(:,niz) = ubpt(:)
      
      if(diags)write(*,'(a,es12.5)')'POTTEMP ends, pt= ',pt(ldiag,zdiag)

      end subroutine POTTEMP
