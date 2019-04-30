! subversion Id for THIS file : $Id: dynf_pwave.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/dynf_pwave.f $
!-----------------------------------------------------------------------
      subroutine DYNF_PWAVE( fr, dyypw, daynum, dtwave )
!-----------------------------------------------------------------------
!     	... The Planetary (Rossby)wave model employs a preturbed potential 
!       vorticity equation as Matsuno (1970) and andrew et al (1987) but 
!       newtonian cooling and rayleigh friction are included. 
!       Planetary wave breaking is taken
!       into account by adopting garcia's parametrization scheme(1991)
! 
! note: 2km is the vertical resolution of wave model. its lower boundary
!       is at 16km; upper boundary is at 120km. lower boundary forcing
!       is given by wave 1 climatoligical data  at 100mb (randel,1987). 
!            perturbation geopotential psi= a exp(-ipha)
!       here a is amplitute, pha is phase of wave number 1
!-----------------------------------------------------------------------
    
      use SIM_CONTROLS, only : start_from
      use GRID_DIMS       ! dimensions from 2d model
      use VEN2            ! u from 2d model

      use WAVE_PARMS      ! set ny, nz, Nwaves (values set in wave2.mod.f)
      use GEOM            ! geometric factors
      use DISSPA          ! real delta(ny,nz,Nwaves), phspd(ny,nz), ampl(ny,nz)
      use WAV_AMP         ! real ampl1(lmax,niz), ampl2(lmax,niz)
      use WVEN2           ! real uzonal(ny,nz), dudy(ny,nz), dudz(ny,nz),
                          !      dqdy(ny,nz)
      use WROSS           ! real wfr, wdyy
      use WW_VARS         ! complex pw(ny,nz), pwm(ny,nz,Nwaves)

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      real, intent(in)    ::  daynum           ! calendar day from 0. to 365.
      real, intent(in)    ::  dtwave           ! timestep (days)
      real, dimension(lmax,niz), intent(out) :: fr, dyypw

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer :: k, iz, ii, kwaveno, nklower
      real :: const0, tday
      logical, save :: entered = .false.

!-----------------------------------------------------------------------
! 	... Transfer zonal wind to wave model grid
!-----------------------------------------------------------------------
      nklower = niz - nz*2      ! nz=58 -> nklower=5
      do k = 1,nz
         iz = k*2 + nklower
         uzonal(:,k) = u(:,iz)
      end do 

!-----------------------------------------------------------------------
!   	... Set up parameters for wave model calculation. 
!-----------------------------------------------------------------------
      if( .not. entered ) then
         call SETUP( dtwave )
         call PRMTRS()
         
         if( start_from /= 'newnc' ) then
!-----------------------------------------------------------------------
!          A steady state solution for wave equation is produced for 
!          initial solution of the wave equation.
!-----------------------------------------------------------------------
            call PVMEAN
            do kwaveno = 1,2
               am = kwaveno
               call WBOUND( daynum, kwaveno )
               call DMATER
               call STEADY()
               call LKUOS( 0., kwaveno )
               call WTRANS (kwaveno)
               pwm(:,:,kwaveno) = pw(:,:)
               call DRATE_N (kwaveno)
               call WFIELD_N (kwaveno)
            end do
         end if
         entered = .true.
      end if

      fr  = 0.
      dyypw = 0.
      call PVMEAN
      tday = daynum
      do kwaveno = 1,2
         am = kwaveno
         call WBOUND (tday, kwaveno)
         call DMATER
         call PWTERMS
         call LKUOS (1., kwaveno)
         call WTRANS (kwaveno)
         call DRATE_N (kwaveno)
         pwm(:,:,kwaveno) = pw(:,:)
         call WFIELD_N (kwaveno)
      
!-----------------------------------------------------------------------
! 	... transfer e-p flux and kyy to 2-d model grid
!-----------------------------------------------------------------------
         do k = 1,nz
            iz = k*2 + nklower
            fr   (1:ny,iz) = fr   (1:ny,iz) + wfr(1:ny,k)
            dyypw(1:ny,iz) = dyypw(1:ny,iz) + wdyy(1:ny,k)
            if(kwaveno.eq.1) ampl1(1:ny,iz) = ampl(1:ny,k)
            if(kwaveno.eq.2) ampl2(1:ny,iz) = ampl(1:ny,k)
         end do 
      end do

      do iz = nklower+3,niz-1,2
         fr(:,iz)    = (fr(:,iz-1) + fr(:,iz+1) )*.5
         dyypw(:,iz) = (dyypw(:,iz-1) + dyypw(:,iz+1) )*.5
         ampl1(:,iz) = (ampl1(:,iz-1) + ampl1(:,iz+1) )*.5
         ampl2(:,iz) = (ampl2(:,iz-1) + ampl2(:,iz+1) )*.5
      end do 

      do iz = 1,nklower+1
         fr(:,iz) = fr(:,nklower+2) * exp(float(iz-(nklower+2))/7.)
      end do 

c  turn off planetary wave drag below 12 km, extrapolate down
cc     do iz = 1,nklower+8
cc        fr(:,iz) = fr(:,nklower+9) * exp(float(iz-(nklower+9))/7.)
cc     end do 

      end subroutine DYNF_PWAVE


      subroutine cdiffy (field, dfdy, d2fdy2, korder)

!-----------------------------------------------------------------------
!	... Numerical differentiation of complex variable in y direction
!	... assumes field is zero at poles
!-----------------------------------------------------------------------

      use WAVE_PARMS      ! set ny, nz
      use WCONST          ! constants
      use GEOM            ! geometric factors

      implicit none

      complex field(ny,nz), dfdy(ny,nz), d2fdy2(ny,nz)
      integer i, j, korder

      do j=1,nz
         dfdy(1,j) = field(2,j) * .5 / dtheta
         do i=2,ny-1
            dfdy(i,j) = (field(i+1,j) - field(i-1,j)) * .5 / dtheta
         end do
         dfdy(ny,j) = -field(ny-1,j) *.5 / dtheta
      end do

      if (korder.gt.1) then
         do j=1,nz
            d2fdy2(1,j) = (field(2,j) - 2.*field(1,j)) / dthsq
            do i=2,ny-1
               d2fdy2(i,j) = (field(i+1,j) - 2.*field(i,j) 
     $                     + field(i-1,j)) / dthsq
            end do
            d2fdy2(ny,j) = (-2.*field(ny,j) + field(ny-1,j))/dthsq
         end do
      end if

      end subroutine cdiffy


      subroutine cdiffz (field, dfdz, d2fdz2, korder)

!-----------------------------------------------------------------------
!	... Numerical differentiation of real variable in y direction
!	... assumes field is zero at poles
!-----------------------------------------------------------------------

      use WAVE_PARMS      ! set ny, nz
      use WCONST          ! constants
      use GEOM            ! geometric factors

      implicit none

      complex field(ny,nz), dfdz(ny,nz), d2fdz2(ny,nz)
      integer i, j, korder

      do i=1,ny
         dfdz(i,1) = (field(i,2) - field(i,1)) / dz
         do j=2,nz-1
            dfdz(i,j) = (field(i,j+1) - field(i,j-1)) * .5 / dz
         end do
         dfdz(i,nz) = (field(i,nz) - field(i,nz-1)) / dz
      end do

      if (korder.gt.1) then
         do i=1,ny
            do j=2,nz-1
               dfdz(i,j) = (field(i,j+1) - field(i,j-1)) * .5 / dz
               d2fdz2(i,j) = (field(i,j+1) - 2.*field(i,j) 
     $                     + field(i,j-1)) / dzsq
            end do
            d2fdz2(i,1) = d2fdz2(i,2)
            d2fdz2(i,nz) = d2fdz2(i,nz-1)
         end do
      end if

      end subroutine cdiffz


      subroutine diffy (field, dfdy, d2fdy2, korder)

!-----------------------------------------------------------------------
!	... Numerical differentiation of real variable in y direction
!	... assumes field is zero at poles
!-----------------------------------------------------------------------

      use WAVE_PARMS      ! set ny, nz
      use WCONST          ! constants
      use GEOM            ! geometric factors

      implicit none

      real field(ny,nz), dfdy(ny,nz), d2fdy2(ny,nz)
      integer i, j, korder

      do j=1,nz
         dfdy(1,j) = field(2,j) * .5 / dtheta
         do i=2,ny-1
            dfdy(i,j) = (field(i+1,j) - field(i-1,j)) * .5 / dtheta
         end do
         dfdy(ny,j) = -field(ny-1,j) *.5 / dtheta
      end do

      if (korder.gt.1) then
         do j=1,nz
            d2fdy2(1,j) = (field(2,j) - 2.*field(1,j)) / dthsq
            do i=2,ny-1
               d2fdy2(i,j) = (field(i+1,j) - 2.*field(i,j) 
     $                     + field(i-1,j)) / dthsq
            end do
            d2fdy2(ny,j) = (-2.*field(ny,j) + field(ny-1,j))/dthsq
         end do
      end if

      end subroutine diffy



      subroutine diffz (field, dfdz, d2fdz2, korder)

!-----------------------------------------------------------------------
!	... Numerical differentiation of real variable in z direction
!-----------------------------------------------------------------------

      use WAVE_PARMS      ! set ny, nz
      use WCONST          ! constants
      use GEOM            ! geometric factors

      implicit none

      real field(ny,nz), dfdz(ny,nz), d2fdz2(ny,nz)
      integer i, j, korder

      do i=1,ny
         dfdz(i,1) = (field(i,2) - field(i,1)) / dz
         do j=2,nz-1
            dfdz(i,j) = (field(i,j+1) - field(i,j-1)) * .5 / dz
         end do
         dfdz(i,nz) = (field(i,nz) - field(i,nz-1)) / dz
      end do

      if (korder.gt.1) then
         do i=1,ny
            do j=2,nz-1
               d2fdz2(i,j) = (field(i,j+1) - 2.*field(i,j) 
     $                     + field(i,j-1)) / dzsq
            end do
            d2fdz2(i,1) = d2fdz2(i,2)
            d2fdz2(i,nz) = d2fdz2(i,nz-1)
         end do
      end if

      end subroutine diffz


      subroutine DMATER
!-----------------------------------------------------------------------
! 	... Calculates material derivatives, including damping terms
!-----------------------------------------------------------------------
      use WAVE_PARMS      ! set ny, nz
      use WCONST          ! constants
      use GEOM            ! geometric factors
      use DAMPS           ! real rfr(nz), ac(nz), dacdz(nz)
      use DISSPA          ! real delta(ny,nz,Nwaves), phspd(ny,nz), ampl(ny,nz)
      use WBASIC          ! complex alpha(ny,nz), beta(ny,nz)
      use WVEN2           ! real uzonal(ny,nz), dudy(ny,nz), dudz(ny,nz),
                          !      dqdy(ny,nz)

      implicit none

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer :: i, j
      real, dimension(ny,nz) :: zmom
 
      do i = 1,ny
         do j = 1,nz
            zmom(i,j) = uzonal(i,j) * aecosi(i)
            alpha(i,j) = aim * am * zmom(i,j) + ac(j) + deltmax(i,j)
            beta(i,j) = aim * am * zmom(i,j) + rfr(j) + deltmax(i,j)
         end do

         do j=2,nz-1
!!!            dalphz(i,j) = aim * am * dudz(i,j) * aecosi(i) 
!!!     $                  + dacdz(j)
!!!     $                  + (deltmax(i,j+1)-deltmax(i,j-1)) *.5 /dz
            dalphz(i,j) = dacdz(j)
     $                  + (deltmax(i,j+1)-deltmax(i,j-1)) *.5 /dz
         end do
!!!         dalphz(i,1) = aim * am * dudz(i,1) * aecosi(i) 
!!!     $               + dacdz(1) + (deltmax(i,2)-deltmax(i,1)) /dz
!!!         dalphz(i,nz) = aim * am * dudz(i,nz) * aecosi(i) 
!!!     $                + dacdz(nz)
         dalphz(i,1) = dacdz(1) + (deltmax(i,2)-deltmax(i,1)) /dz
         dalphz(i,nz) = dacdz(nz)

      end do

      end subroutine DMATER


      subroutine DRATE_N (kwaveno)
!-----------------------------------------------------------------------
!  	... Calculates damping due to wave breaking
!-----------------------------------------------------------------------

      use WAVE_PARMS      ! set ny, nz
      use WVEN2           ! real uzonal(ny,nz), dudy(ny,nz), dudz(ny,nz),
                          !      dqdy(ny,nz)
      use WCONST          ! constants
      use GEOM            ! geometric factors
      use WW_VARS         ! complex pw(ny,nz), pwm(ny,nz,Nwaves)
      use DISSPA          ! real delta(ny,nz,Nwaves), phspd(ny,nz), ampl(ny,nz)

      implicit none

!-----------------------------------------------------------------------
! 	... Local variables
!-----------------------------------------------------------------------
      integer :: i, j, k, korder, kwaveno
      real :: wnk
      real    :: psil, rk, rdt, psimax
      real, dimension(ny,nz) :: bk2, cgy, cgz, wnl, wnm, gradpv, umc
      real, dimension(nz)    :: eps2
      complex, dimension(ny,nz) :: cderiv, cderiv2, ppv
      complex, dimension(ny,nz) :: dy1, dy2, dz1, dz2
 
!-----------------------------------------------------------------------
! 	... d(pw)/dtheta
!-----------------------------------------------------------------------
      korder = 2
      call cdiffy (pw, dy1, dy2, korder)
 
!-----------------------------------------------------------------------
! 	... d(pw)/dz
!-----------------------------------------------------------------------
      korder = 2
      call cdiffz (pw, dz1, dz2, korder)

!-----------------------------------------------------------------------
!  	... Compute phase speed
!           Assume all phase speeds are zero on boundary
!-----------------------------------------------------------------------
      rdt = 1. / delt
      phspd = 0.
      wnl = 0.
      wnm = 0.
      delta(:,:,kwaveno) = 0.
      do i = 1,ny 
         rk = rdt / am * ae * coslat(i)
         phspd(i,:) = rk * AIMAG( (pwm(i,:,kwaveno)-pw(i,:))
     $                    / (.5*(pw(i,:)+pwm(i,:,kwaveno)) + 1.e-20))
         wnl(i,:) = AIMAG( dy1(i,:)/(pw(i,:) + 1.e-20) )/ae
         wnm(i,:) = AIMAG( dz1(i,:)/(pw(i,:) + 1.e-20) )
      end do 

      do j = 1,nz 
	psimax = MAXVAL( CABS( pw(:,j) ) )
	where( CABS( pw(:,j) ) < .01*psimax )
           wnl(:,j) = 0.
           wnm(:,j) = 0.
	endwhere
	where( CABS( pw(:,j) ) < .05*psimax )
           phspd(:,j) = 0.
	endwhere
      end do
 
!-----------------------------------------------------------------------
!  	... Compute eddy potential vorticity and its meridonal derivative
!-----------------------------------------------------------------------
      do i = 1,ny
         do j=1,nz
            ppv(i,j) = g / aesq * exzhalf(j) / f(i) * 
     $                ((dy2(i,j) + dmterm(i)*sinsq(i) * dy1(i,j)
     $                           - am**2 * pw(i,j) * cossqi(i))
     $               + sinsq(i) / ss * (dz2(i,j) - .25*pw(i,j)/(hs*hs)))
         end do
      end do

!-----------------------------------------------------------------------
! 	... d(qprime)/dtheta
!-----------------------------------------------------------------------
      korder = 1
      call cdiffy (ppv, cderiv, cderiv2, korder)
      gradpv = CABS( cderiv )
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!  Rolando smoothes gradpv
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      umc = uzonal - phspd
      do i = 1,ny
         wnk = am * aecosi(i)
         cgy(i,:) = 0.
         cgz(i,:) = 0.
         delta(i,:,kwaveno) = 0.
         eps2 = f(i)**2 / bvsq
         bk2(i,:) = wnk * wnk + wnl(i,:) * wnl(i,:)
     $            + eps2 * (wnm(i,:)*wnm(i,:) + .25/(hs*hs))
     $            + 1.e-20
         cgy(i,:) = 2. * wnk * wnl(i,:) * umc(i,:) / bk2(i,:)
         cgz(i,:) = 2. * wnk * eps2 * wnm(i,:) * umc(i,:) / bk2(i,:)
      end do

      do i = 2,ny-1
         do j=2,nz-1
            if( gradpv(i,j) >= dqdy(i,j) .and.
     $          gradpv(i,j) >= 1.e-8 ) then
               delta(i,j,kwaveno) = MAX( .5 * cgz(i,j)/hs
     $                          - (.75 * cgz(i,j) * dudz(i,j)
     $                             + 1.25 * cgy(i,j) * dudy(i,j)/ae )
     $                            / (umc(i,j) + SIGN(1.,umc(i,j))),0. )
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!   Rolando's code multiplies delta by .5 here
               delta(i,j,kwaveno) = 0.5 * delta(i,j,kwaveno)
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            endif
         end do
      end do

      call SMOOTHWLB( delta(1,1,kwaveno), 1, nz, 1 )
      call SMOOTHWV( delta(1,1,kwaveno), 1, nz, 1 )
      do i=1,ny
         do j=1,nz
            deltmax(i,j) = amax1(delta(i,j,1),delta(i,j,2))
         end do
      end do

      end subroutine DRATE_N


      subroutine LKUOS( fnp, kwaveno )
!-----------------------------------------------------------------------
! 	... Lindzen-Kuo solver 
!	    fnp = 0  => steady state
!	    fnp = 1  => non-steady state
!-----------------------------------------------------------------------

      use WAVE_PARMS
      use WCONST
      use BOUND1
      use WW_VARS
      use TRNS
      use WUCOEF
      use NUMERICAL, only : CGEFA_F90, CGEDI_F90

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      real, intent(in) :: fnp

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer :: info, job, i1, i, j, kwaveno
      integer, dimension(ny) :: ipvt, iv
      complex, dimension(ny) :: g2
      complex :: ww(ny,ny), gg(ny,nz)
      complex :: a(ny,ny,nz)
      complex :: det(2)
      complex work(ny)

!-----------------------------------------------------------------------
! 	... Initialize working arrays
!-----------------------------------------------------------------------
      gg = (0.,0.)
      ww = (0.,0.)
      a = (0.,0.)
 
!-----------------------------------------------------------------------
!	... Sets up matrices for p.w. calculation
! 	    square matrix stored in ww and/or a ;  coeffient stored in gg
!-----------------------------------------------------------------------
      do i = 2,ny-1
         a(i,i-1,:) = cc(i,:) - dd(i,:)
         a(i,i,:) = qq(i,:) - 2.*(cc(i,:) + aa(i,:))
         a(i,i+1,:) = cc(i,:) + dd(i,:)
      end do
      a(1,1,:) = qq(1,:) - 2.*(cc(1,:) + aa(1,:))
      a(1,2,:) = cc(1,:) + dd(1,:)
      a(ny,ny-1,:) = cc(ny,:) - dd(ny,:)
      a(ny,ny,:) = qq(ny,:) - 2.*(cc(ny,:) + aa(ny,:))
 
!-----------------------------------------------------------------------
!  	... Lower boundary
!-----------------------------------------------------------------------
      g2 = bdy
 
!-----------------------------------------------------------------------
!	... Determines coefficients of recursion relations
!-----------------------------------------------------------------------
      do j = 1,nz
         do i1 = 1,ny
            ww(i1,:) = -(a(i1,:,j) 
     $                   + (aa(i1,j) - bb(i1,j))*ww(i1,:))
         end do
!-----------------------------------------------------------------------
!	... Factor matrix
!-----------------------------------------------------------------------
         call CGEFA_F90( ww, ny, ipvt, info )
         if( info /= 0 ) then
	    write(*,*) ' LKUOS: Zero pivot in CGEFA_F90 @ row = ',info
	    stop 'LKUOS: Numerical error'
	 end if
         job = 1

!-----------------------------------------------------------------------
!	... Form the inverse matrix
!-----------------------------------------------------------------------
         call CGEDI_F90( ww, ny, ipvt, det, job )

!         call clud (ny, ny, ww, ny, ww, iv)
!         call cilu (ny, ny, ww, iv, work)
 
         gg(:,j) = (aa(:,j) - bb(:,j))*g2(:) - fnp*dzsq*tr(:,j,kwaveno)
	 g2(:)   = MATMUL( ww,gg(:,j) )
	 gg(:,j) = g2(:)
         do i1 = 1,ny
            ww(i1,:) = (aa(:,j) + bb(:,j))*ww(i1,:)
            a(i1,:,j) = ww(i1,:)
         end do
      end do
 
!-----------------------------------------------------------------------
!	... Upper boundary condition*
!-----------------------------------------------------------------------
      pw(:,nz) = gg(:,nz)
 
!-----------------------------------------------------------------------
!	... Uses coefficients to determine real and imag. parts of p.w.
!	    streamfunction. stored in pw
!-----------------------------------------------------------------------
      do j = nz-1,1,-1
	 pw(:,j) = gg(:,j) + MATMUL( a(:,:,j),pw(:,j+1) )
      end do

      end subroutine LKUOS


      subroutine PRMTRS()
!-----------------------------------------------------------------------
!  	... Damping rates from Rolando
!           computes newtonian cooling and rayleigh friction parameters
!-----------------------------------------------------------------------

      use WAVE_PARMS
      use WCONST
      use GEOM
      use DAMPS

      implicit none

!-----------------------------------------------------------------------
! 	... Local variables
!-----------------------------------------------------------------------
      integer :: j
      real :: ac0, ac1, ac2, ac3, acmin, fr0, fr1, fr2
 
!-----------------------------------------------------------------------
! 	... Rayleigh friction
!-----------------------------------------------------------------------
      fr0 = 1./(0.5*86400.)
cc      fr1 = 1./(20.*86400.)
      fr1 = 1./(50.*86400.)
      fr2 = 1./(1.*86400.)
      do j=1,nz
         rfr(j) = fr1                                            !background
     $          + fr0 * 0.5 * (1.+tanh((z(j)-65.e3)/10.e3))      !gravity
     $          + fr2 * exp((z(j)-100.e3)/10.e3)                  !ion drag
      end do
!-----------------------------------------------------------------------
! 	... Newtonian cooling
!-----------------------------------------------------------------------
!      ac0 = 1./(50.*86400.)
      ac0 = 1./(20.*86400.)
cc      ac1 = 1./(8.*86400.)
      ac1 = 1./(10.*86400.)
      ac2 = 1./(2.0*86400.)
      ac3 = 1./(0.5*86400.)
      acmin = 1./(2.5*86400.)

cc      ac = ac0 + ac1/COSH((z-48.e3)/7.e3) 
      ac = ac0 + ac1/COSH((z-48.e3)/5.e3) 
     $         + ac2 * (1.+tanh((z-112.e3)/12.e3)) 
     $         + ac3 * (1.+tanh((z-120.e3)/20.e3))

      do j=1,nz
         if(z(j).ge.50.e3.and.ac(j).lt.acmin)ac(j) = acmin
      end do

      do j=2,nz-1
         dacdz(j) = (ac(j+1) - ac(j-1)) * .5 / dz
      end do
      dacdz(1) = (ac(2) - ac(1)) / dz
      dacdz(nz) = (ac(nz) - ac(nz-1)) / dz

      end subroutine PRMTRS


      subroutine PVMEAN
!-----------------------------------------------------------------------
! 	... Calculates zonal mean wind and pv gradients
!-----------------------------------------------------------------------

      use WAVE_PARMS      ! set ny, nz
      use WCONST          ! constants
      use GEOM            ! geometric factors
      use WVEN2           ! real uzonal(ny,nz), dudy(ny,nz), dudz(ny,nz),
                          !      dqdy(ny,nz)

      implicit none

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer :: i, j, korder
      real, dimension(ny,nz) :: d2udz2, d2udy2
 
      korder = 2
      call diffz (uzonal, dudz, d2udz2, korder)
      call diffy (uzonal, dudy, d2udy2, korder)

!  zonal mean pv gradient
      do j=1,nz
         do i=1,ny
            dqdy(i,j) = 2. * omega * coslat(i)
     $                + (uzonal(i,j)*cossqi(i) + tanlat(i) * dudy(i,j)
     $                   - d2udy2(i,j)) / ae
     $                + sinsq(i)/ss/ae * (dudz(i,j)/hs - d2udz2(i,j))
         end do
      end do
      end subroutine PVMEAN


      subroutine PWTERMS
!-----------------------------------------------------------------------
! 	... Planetary wave structure 
!-----------------------------------------------------------------------

      use WAVE_PARMS      ! set ny, nz
      use WCONST          ! constants
      use GEOM            ! geometric factors
      use WBASIC          ! complex alpha(ny,nz), beta(ny,nz)
      use WUCOEF          ! complex aa(ny,nz), bb(ny,nz), cc(ny,nz), 
                          !         dd(ny,nz), qq(ny,nz)
      use WVEN2           ! real uzonal(ny,nz), dudy(ny,nz), dudz(ny,nz),
                          !      dqdy(ny,nz)

      implicit none

!-----------------------------------------------------------------------
! 	... Local variables
!-----------------------------------------------------------------------
      integer :: i
      complex dtalph(nz), dtbeta(nz)

!-----------------------------------------------------------------------
! 	... Coefficients for numerical solution
!-----------------------------------------------------------------------
      do i = 1,ny
         dtalph = 1. + .5 * delt * alpha(i,:)
         dtbeta = 1. + .5 * delt * beta(i,:)
         aa(i,:) = dtalph / ss
         bb(i,:) = .25 * delt * dalphz(i,:) / ss * dz
         cc(i,:) = dtbeta * sinsqi(i) / dthsq * dzsq
         dd(i,:) = dtbeta * dmterm(i) * .5/dtheta * dzsq
         qq(i,:) = (.5 * delt * aim * am * dqdy(i,:) 
     $                      / coslat(i) * sinsqi(i)
     $           - dtbeta * am**2 * cossqi(i) * sinsqi(i)
     $           - dtalph * .25 / (hs*hs * ss)
     $           + .5 * delt * dalphz(i,:) / (2.*ss*hs)) * dzsq
      end do
      
      end subroutine PWTERMS


      subroutine SETUP( dtwave )
!-----------------------------------------------------------------------
! 	... Model constants and general arrays
!  	    ae=radius of the earth, hs=scale height, omega=earth's rotation
!  	    r=gas const, g=gravity, conv=pi/180
!  	    (all are in mks units)
!  	    delt=time step for p.w. calculation (sec)
!  	    am=zonal wavenumber
!  	    bottom= non-dimensional bottom height
!  	    dz=non-dimensional vertical spacing
!  	    dtheta=meridional spacing in radians
!  	    z=height
!  	    alat=latitude in radians
!  	    coslat, sinlat, tanlat = cos, sin, tan of latitude
!  	    f=coriolis parameter
!-----------------------------------------------------------------------

      use WAVE_PARMS      ! set ny, nz
      use WCONST          ! constants
      use GEOM            ! geometric factors

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      real, intent(in) :: dtwave
 
!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer :: i, j, ios
      real feq

      delt = 86400. * dtwave
      do j = 1,nz
         z(j) = FLOAT(j-1)*dz + bottom
         exzhalf(j) = exp(.5*z(j)/hs)
      end do
      do i = 1,ny
         alat(i) = FLOAT(i-1)*dtheta - 85.*conv
         coslat(i) = COS( alat(i) )
         sinlat(i) = SIN( alat(i) )
         if( i == 18 ) then
	    sinlat(i) = SIN( .5*conv )
	 end if
         tanlat(i) = TAN( alat(i) )
         f(i) = 2.*omega*sinlat(i)
         sinsq(i) = sinlat(i)**2
         sinsqi(i) = 1./sinsq(i)
         cossq(i) = coslat(i)**2
         cossqi(i) = 1./cossq(i)
         sincos(i) = sinlat(i)*coslat(i)
         aecosi(i) = 1. / (ae * coslat(i))
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!   NOTE - for Dickinson type wave model...
!          Also, change form of EP flux and divergence in subr. WFIELD
!
!         dmterm(i) = -1. / (sincos(i)*sinsq(i))
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!   NOTE - for Matsuno type wave model...
!
         dmterm(i) = -(1.+ cossq(i)) / (sincos(i)*sinsq(i))
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      end do

!  interpolate geometric factors across equator
      sinsqi(18) = (sinsqi(19) + sinsqi(17))/2.
      dmterm(18) = (dmterm(19) + dmterm(17))/2.
!      do i=17,19
!         feq = float(i-16)/4.
!         sinsqi(i) = feq * sinsqi(20) + (1. - feq) * sinsqi(16)
!         dmterm(i) = feq * dmterm(20) + (1. - feq) * dmterm(16)
!      end do
!      do i=16,20
!         feq = float(i-15)/6.
!         sinsqi(i) = feq * sinsqi(21) + (1. - feq) * sinsqi(15)
!         dmterm(i) = feq * dmterm(21) + (1. - feq) * dmterm(15)
!      end do

      end subroutine SETUP

      
      subroutine SMOOTHWLB( y,  j1, j2, nsmol )
!----------------------------------------------------------------------
!     	... Smoothing by a triangular function
!    ==>>   Includes assumption that field is zero at poles
!----------------------------------------------------------------------
      use WAVE_PARMS

      implicit none

!----------------------------------------------------------------------
!	... Dummy args
!----------------------------------------------------------------------
      integer, intent(in) :: j1, j2, nsmol
      real, intent(inout) :: y(ny,*)
      
!----------------------------------------------------------------------
!	... Local variables
!----------------------------------------------------------------------
      integer ::  i, j, n
      real    ::  x(ny)

      do n = 1,nsmol
         do j = j1,j2
            do i=2,ny-1
               x(i) = .25 * (y(i-1,j) + 2.*y(i,j) + y(i+1,j))
            end do
            x(1) =  .25 * (2.*y(1,j) + y(2,j))
            x(ny) = .25 * (y(ny-1,j) + 2.*y(ny,j))
            do i=1,ny
               y(i,j) = x(i)
            end do
	 end do
      end do
      
      end subroutine SMOOTHWLB

      
      subroutine SMOOTHWLN( y,  j1, j2, nsmol )
!----------------------------------------------------------------------
!     	... Smoothing by a triangular function
!           No boundary information
!----------------------------------------------------------------------
      use WAVE_PARMS

      implicit none

!----------------------------------------------------------------------
!	... Dummy args
!----------------------------------------------------------------------
      integer, intent(in) :: j1, j2, nsmol
      real, intent(inout) :: y(ny,*)
      
!----------------------------------------------------------------------
!	... Local variables
!----------------------------------------------------------------------
      integer ::  j, n
      real    ::  x(ny)

      do n = 1,nsmol
         do j = j1,j2
            x(2:ny-1) = .25*(y(1:ny-2,j) + 2.*y(2:ny-1,j) 
     $                                       + y(3:ny,j))
            y(2:ny-1,j) = x(2:ny-1)
	 end do
      end do
      
      end subroutine SMOOTHWLN

      subroutine SMOOTHWV( y, j1, j2, nsmov )
!----------------------------------------------------------------------
!     	... Smoothing by a triangular function (from j1+1 to j2-1 level)
!----------------------------------------------------------------------

      use WAVE_PARMS

      implicit none

!----------------------------------------------------------------------
!	... Dummy args
!----------------------------------------------------------------------
      integer, intent(in) :: j1, j2, nsmov
      real, intent(inout) :: y(ny,*)

!----------------------------------------------------------------------
!	... Local variables
!----------------------------------------------------------------------
      integer :: n, l
      real    :: x(nz)

      do n = 1,nsmov
         do l = 1,ny
            x(j1+1:j2-1) = .25*(y(l,j1:j2-2) + 2.*y(l,j1+1:j2-1)
     $                                       + y(l,j1+2:j2))
            y(l,j1+1:j2-1) = x(j1+1:j2-1)
         end do
      end do
      
      end subroutine SMOOTHWV


      subroutine STEADY()
!-----------------------------------------------------------------------
!  	... Assume d/dt = 0 to solve pde (patial differential equation)
!-----------------------------------------------------------------------

      use WAVE_PARMS      ! set ny, nz
      use WCONST          ! constants
      use GEOM            ! geometric factors
      use WBASIC          ! complex alpha(ny,nz), beta(ny,nz)
      use WUCOEF          ! complex aa(ny,nz), bb(ny,nz), cc(ny,nz), 
                          !         dd(ny,nz), qq(ny,nz)
      use WVEN2           ! real uzonal(ny,nz), dudy(ny,nz), dudz(ny,nz),
                          !      dqdy(ny,nz)

      implicit none

!-----------------------------------------------------------------------
!  	... Local variables
!-----------------------------------------------------------------------
      integer :: i

      do i = 1,ny  
         aa(i,:) = alpha(i,:) / ss
         bb(i,:) = dalphz(i,:) / ss * .5*dz
         cc(i,:) = beta(i,:) * sinsqi(i) / dthsq * dzsq
         dd(i,:) = beta(i,:) * dmterm(i) * .5 / dtheta * dzsq
         qq(i,:) = (aim*am*dqdy(i,:) * sinsqi(i) / coslat(i)
     $                 - alpha(i,:)*.25 / (hs*hs * ss)
     $                 + .5 * dalphz(i,:) / (hs*ss)
     $                 - beta(i,:) * am*am * cossqi(i) * sinsqi(i)) 
     $                * dzsq
      end do

      
      end subroutine STEADY


      subroutine WBOUND( daynum, kwaveno )
!-----------------------------------------------------------------------
!	... Calculates lower boundary eddy streamfcn*
!-----------------------------------------------------------------------

      use SIM_CONTROLS, only : data_dir
      use WAVE_PARMS      ! set ny, nz
      use WCONST          ! constants
      use BOUND1          ! complex bdy(ny)
      use ASCII_UTILS, only : NAVU

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      real, intent(in) :: daynum

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer :: mday, ios, ii, kwaveno, iunit
      character(len=64) :: filenm(2)
      complex, save :: cbnd1(ny,365), cbnd2(ny,365)
      logical, save :: entered = .false.

!-----------------------------------------------------------------------
!	... Read in lower boundary geopotential wave amp and phase
!           Daily values, units = meters
!-----------------------------------------------------------------------
      if( .not. entered )then
         iunit = NAVU()
         filenm(1) = data_dir(:LEN_TRIM(data_dir)) // 'wave1.pw500'
         filenm(2) = data_dir(:LEN_TRIM(data_dir)) // 'wave2.pw500'
         do ii = 1,2
            OPEN( iunit, file = filenm(ii), iostat = ios )
            if( ios /= 0 ) then
               write(*,*) ' WBOUND: Failed to open ' // filenm(ii)
               write(*,*) ' Error code returned = ', ios
               stop 'WBOUND: error opening data file'
            end if
            if( ii == 1 ) read( iunit,'(10f8.2)' ) cbnd1
            if( ii == 2 ) read( iunit,'(10f8.2)' ) cbnd2
            CLOSE( iunit )
         end do
         entered = .true.
      end if

      mday = MIN( INT(daynum),365 )

!-----------------------------------------------------------------------
! the following adjustments to the amplitude of wave1 at the lower 
! boundary of the wave model is based on comparison of max wave1 ampiltude
! at 30 km calculated by the model with the climatology given by Randel, 
! in both hemispheres; generally, the model amplitudes in the NH winter 
! are much smaller than climatology, whereas they are significantly 
! bigger in the SH winter; the ratios are not exactly the same as those 
! of the model to climatology in order to allow wave growth by the model 
!-----------------------------------------------------------------------
      if (kwaveno.eq.1) then 
                                                                ! SH
        if (mday >= 1 .and. mday <= 31) then                    ! Jan
          bdy(1:17) = .03 * cbnd1(1:17,mday)    
        else if (mday >= 32 .and. mday <= 59) then              ! Feb
          bdy(1:17) = .01 * cbnd1(1:17,mday)          
        else if (mday >= 60 .and. mday <= 90) then              ! March   
          bdy(1:17) = .2 * cbnd1(1:17,mday)           
        else if (mday >= 91 .and. mday <= 120) then             ! Apr
          bdy(1:17) = .4 * cbnd1(1:17,mday)             
        else if (mday >= 121 .and. mday <= 151) then            ! May 
          bdy(1:17) = .54 * cbnd1(1:17,mday)            
        else if (mday >= 152 .and. mday <= 181) then            ! June  
          bdy(1:17) = .52 * cbnd1(1:17,mday)            
        else if (mday >= 182 .and. mday <= 212) then            ! July  
          bdy(1:17) = .43 * cbnd1(1:17,mday)            
        else if (mday >= 213 .and. mday <= 243) then            ! Aug 
          bdy(1:17) = .35 * cbnd1(1:17,mday)            
        else if (mday >= 244 .and. mday <= 273) then            ! Sep 
          bdy(1:17) = .25 * cbnd1(1:17,mday)            
        else if (mday >= 274 .and. mday <= 304) then            ! Oct 
          bdy(1:17) = .23 * cbnd1(1:17,mday)              
        else if (mday >= 305 .and. mday <= 334) then            ! Nov 
          bdy(1:17) = .15 * cbnd1(1:17,mday)            
        else if (mday >= 335 .and. mday <= 365) then            ! Dec 
          bdy(1:17) = .03 * cbnd1(1:17,mday)            
        end if

                                                                ! NH
        if (mday >= 1 .and. mday <= 59) then                    ! Jan-Feb
          bdy(19:ny) = 1.6 * cbnd1(19:ny,mday)                      
        else if (mday >= 60 .and. mday <= 90) then              ! Mar 
          bdy(19:ny) = 1.2 * cbnd1(19:ny,mday)
        else if (mday >= 91 .and. mday <=120) then              ! Apr
          bdy(19:ny) = .1 * cbnd1(19:ny,mday)    
        else if (mday >= 121 .and. mday <=243) then             ! May-Aug 
          bdy(19:ny) = .02 * cbnd1(19:ny,mday)         
        else if (mday >= 244 .and. mday <=273) then             ! Sep 
          bdy(19:ny) = .5 * cbnd1(19:ny,mday)          
        else if (mday >= 274 .and. mday <=304) then             ! Oct  
          bdy(19:ny) = 1.2 * cbnd1(19:ny,mday)         
        else if (mday >= 305 .and. mday <=365) then             ! Nov-Dec  
          bdy(19:ny) = 1.6 * cbnd1(19:ny,mday)         
        end if
      else if (kwaveno. eq. 2) then          
        bdy(1:17) = .01 * cbnd1(1:17, mday)  ! above changes to wave1 amplifies wave2  
        bdy(19:ny) = .2 * cbnd1(19:ny, mday) ! significantly (compare II35 and II30)
      end if

      end subroutine WBOUND


      subroutine WFIELD_N (kwaveno)
!-----------------------------------------------------------------------
! 	... This routine determines the wave fields from the p.w. streamfcn
!  	    fr=ep flux divergence, dyy=transport coef (kyy)
!-----------------------------------------------------------------------

      use WAVE_PARMS      ! set ny, nz
      use WCONST          ! constants
      use GEOM            ! geometric factors
      use WVEN2           ! real uzonal(ny,nz), dudy(ny,nz), dudz(ny,nz),
                          !      dqdy(ny,nz)
      use WROSS           ! real wfr, wdyy
      use DISSPA          ! real delta(ny,nz,Nwaves), phspd(ny,nz), ampl(ny,nz)
      use WW_VARS         ! complex pw(ny,nz), pwm(ny,nz,Nwaves)

      implicit none

!-----------------------------------------------------------------------
! 	... Local variables
!-----------------------------------------------------------------------
      integer :: i, j, korder, kwaveno
      real :: rrk
      real, dimension(ny,nz) :: uv, vpz, fycos
      real, dimension(ny,nz) :: deriv, deriv2
      real, dimension(nz)    :: umc, dyymin
      complex, dimension(ny,nz) :: dqy, dqz, dqy2, dqz2, u1, v1
      complex ugeo, vgeo, uterm
      real ae2omegi, fhat, ftil, gamma, glimit, feq

      dyymin(:) = 1.e5 

!-----------------------------------------------------------------------
! 	... Get geopotential in m2/s2
!-----------------------------------------------------------------------
      do i = 1,ny
         pw(i,:) = pw(i,:) * exzhalf * g
      end do 
      ampl = CABS(pw)/g

!-----------------------------------------------------------------------
! 	... d(pw)/dtheta
!-----------------------------------------------------------------------
      korder = 1
      call cdiffy (pw, dqy, dqy2, korder)
 
!-----------------------------------------------------------------------
! 	... d(pw)/dz
!-----------------------------------------------------------------------
      korder = 1
      call cdiffz (pw, dqz, dqz2, korder)

!-----------------------------------------------------------------------
! 	... t prime, u prime, v prime
!-----------------------------------------------------------------------
      ae2omegi = 1. / (ae * 2. * omega)
      do j = 1,nz
         do i = 1,ny
!!           u1(i,j) = -dqy(i,j) / (ae*f(i))
!!           v1(i,j) = aim*am*pw(i,j)/(ae*coslat(i)*f(i))
            fhat = sinlat(i) - dudy(i,j) * ae2omegi +
     2             tanlat(i) * uzonal(i,j) * ae2omegi
            ftil = sinlat(i) + 2. * tanlat(i) * uzonal(i,j) * ae2omegi
            uterm = uzonal(i,j) * aim * am / coslat(i) * ae2omegi
            gamma = 1. / (fhat*ftil + uterm**2)
            ugeo = -dqy(i,j) / (ae*f(i))
            vgeo = aim * am * pw(i,j)/(ae*coslat(i)*f(i))
            glimit = - uterm**2/(fhat*ftil)
           if (glimit.lt.0.5.and.abs(alat(i)).ge.30.) then
               u1(i,j) = (-uterm*vgeo + fhat*ugeo) * sinlat(i) * gamma
               v1(i,j) = ( uterm*ugeo + ftil*vgeo) * sinlat(i) * gamma
            else
               u1(i,j) = ugeo
               v1(i,j) = vgeo
            end if
         end do
      end do

!-----------------------------------------------------------------------
! 	... interpolate u' and v' across the equator
!-----------------------------------------------------------------------
      do j = 1,nz
         do i=17,19
            feq = float(i-16)/4.
            u1(i,j) = feq * u1(20,j) + (1. - feq) * u1(16,j)
            v1(i,j) = feq * v1(20,j) + (1. - feq) * v1(16,j)
         end do
      end do

!-----------------------------------------------------------------------
! 	... Compute wave forcing of mean flow
!-----------------------------------------------------------------------
      uv = .5*(REAL(u1)*REAL(v1) + AIMAG(u1)*AIMAG(v1))
      vpz = .5*(REAL(v1)*REAL(dqz) + AIMAG(v1)*AIMAG(dqz))

!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!  normal EP flux and divergence - (Matsuno version)
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      korder = 1
      call diffy (uv, deriv, deriv2, korder)
      do j = 1,nz
         wfr(:,j) = -(deriv(:,j) - uv(:,j)*2.*tanlat(1:ny))/ae
      end do
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!  ALTERNATIVELY,
!    y component of EP flux and divergence modified for Dickinson 
!    wave model so that v'q' = del dot F
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!         wvv(:,j) = -uv(:,j) * abs(sinlat(1:ny))
!      do j = 1,nz
!         fycos(:,j) = wvv(:,j) * coslat(1:ny)
!      end do
!      korder = 1
!      call diffy (fycos, deriv, deriv2, korder)
!      do j = 1,nz
!         wfr(:,j) = deriv(:,j) * aecosi(1:ny)
!      end do
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      korder = 1
      call diffz (vpz, deriv, deriv2, korder)
      do i = 1,ny
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!  normal EP flux and divergence - (Matsuno version)
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
         wfr(i,:) = wfr(i,:) + (deriv(i,:) - vpz(i,:)/hs)*f(i)/bvsq
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!  Prevent positive values - added for numerical stability
!!         wfr(i,:) = MIN (wfr(i,:), -1.e-15)
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!  ALTERNATIVELY,
!    sine factor added for Dickinson wave model
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!         wfr(i,:) = (wfr(i,:) + (deriv(i,:) - vpz(i,:)/hs)*f(i)/bvsq)
!     $              / abs(sinlat(i))
!         www(i,:) = abs(sinlat(i)) * vpz(i,:) * f(i) * coslat(i) / bvsq
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      end do
  
      call SMOOTHWLB( wfr, 1, nz, 1 )
      call SMOOTHWV( wfr, 1, nz, 1 )

!-----------------------------------------------------------------------
!   	... Compute meridional diffusion coefficient
!-----------------------------------------------------------------------
       do i = 1,ny
          rrk = am**2 / (aesq * cossq(i))
          uv(i,:) = .5* (REAL(v1(i,:)) *REAL(v1(i,:))
     $                     +  AIMAG(v1(i,:))*AIMAG(v1(i,:)))
          wdyy(i,:) = dyymin(:) 
          umc = uzonal(i,:) - phspd(i,:)
          where( ABS(umc) < 2. )
	     umc = 2.
	  endwhere
          wdyy(i,:) = deltmax(i,:) * uv(i,:)
     $        /(rrk*umc*umc + deltmax(i,:)*deltmax(i,:) + 1.e-20)
          wdyy(i,:) = MAX( dyymin(:),wdyy(i,:) )
          wdyy(i,:) = MIN( wdyy(i,:),9.e6 )
       end do

      call SMOOTHWLN( wdyy, 1, nz, 1 )
      call SMOOTHWV( wdyy, 1, nz, 1 )

       korder = 1
       call diffy (wdyy, deriv, deriv2, korder)
     
      end subroutine WFIELD_N


      subroutine WTRANS (kwaveno)
!-----------------------------------------------------------------------
!  	... Calculates p.w. forcing due to wave transience
!  	    pw is streamfunction
!-----------------------------------------------------------------------

      use WAVE_PARMS      ! set ny, nz
      use WCONST          ! constants
      use GEOM            ! geometric factors
      use BOUND1          ! complex bdy(ny)
      use WBASIC          ! complex alpha(ny,nz), beta(ny,nz)
      use TRNS            ! complex tr(ny,nz,Nwaves)
      use WW_VARS         ! complex pw(ny,nz), pwm(ny,nz,Nwaves)
      use WVEN2           ! real uzonal(ny,nz), dudy(ny,nz), dudz(ny,nz),
                          !      dqdy(ny,nz)

      implicit none

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer :: i, j, korder, kwaveno
      complex, dimension(ny,nz) :: aa, bb, cc, dd, qq
      complex, dimension(ny,nz) :: dy1, dy2, dz1, dz2
      complex dtalph(nz), dtbeta(nz)
 
!-----------------------------------------------------------------------
! 	... Coefficients for numerical solution
!-----------------------------------------------------------------------
      do i = 1,ny
         dtalph = 1. - .5 * delt * alpha(i,:)
         dtbeta = 1. - .5 * delt * beta(i,:)
         aa(i,:) = dtalph / ss
         bb(i,:) = - delt * dalphz(i,:) / (2.*ss)
         cc(i,:) = dtbeta * sinsqi(i)
         dd(i,:) = dtbeta * dmterm(i)
         qq(i,:) = (-.5 * delt * aim*am*dqdy(i,:)
     $                      /coslat(i) * sinsqi(i)
     $              - dtbeta * am**2 * sinsqi(i) * cossqi(i) 
     $              - dtalph * .25 / (hs*hs * ss)
     $              - .25 * delt * dalphz(i,:) / (ss*hs))
      end do
 
!-----------------------------------------------------------------------
! 	... d(psi)/dtheta
!-----------------------------------------------------------------------
      korder = 2
      call cdiffy (pw, dy1, dy2, korder)
 
!-----------------------------------------------------------------------
! 	... d(psi)/dz
!-----------------------------------------------------------------------
      korder = 2
      call cdiffz (pw, dz1, dz2, korder)

!  Redefine at endpoints to include upper and lower boundary conditions
      do i=1,ny
         dz1(i,1) = (pw(i,2) - bdy(i)) *.5 / dz
         dz1(i,nz) = - pw(i,nz-1) *.5 / dz
         dz2(i,1) = (pw(i,2) - 2. * pw(i,1) + bdy(i)) / dzsq
         dz2(i,nz) = (pw(i,nz-1) - 2.*pw(i,nz)) / dzsq
      end do
 
!-----------------------------------------------------------------------
!	... Forcing function for p.w. eqn. due to wave transience
!-----------------------------------------------------------------------
      tr(:,:,kwaveno) = aa*dz2 + bb*dz1 + cc*dy2 + dd*dy1 + qq*pw

      end subroutine WTRANS
