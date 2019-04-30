! subversion Id for THIS file : $Id: vdiff.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/vdiff.f $
!-----------------------------------------------------------------------
      subroutine VDIFF( lat, dts, T, n, ni )
!-----------------------------------------------------------------------
!     	... Solve the vertical diffusion equation dni/dt = -d(niwi)/dz
!           where wi = wK + wD  (wK eddy diffusion, wD molecular diffusion)
!           Solve with a fully implicit scheme, 2-points staggered grid
!           finite differences, which yields a tridiagonal matrix
!       ... written for a log-pressure altitude (z*) grid with Hs height scale
!           and 1km vertical steps
!       ... The EDDY diffusion velocity is based on mixing ratios Xi :
!           wiK = -Kzz * ( dXi/dz ) / Xi , while MOLECULAR diffusion
!           velocity is based on nb densities ni:
!           wiD = -D12i * ( (dni/dz)/ni + 1/H + (1+alphat)*(dT/dz)/T )
!           Choice justified in CalcFluxDiff.xls and SOCRATES paper notebook.
!       ... nzh=n(iz+1/2) uses mix of n=SUM(ni) and n=(p0/kT)*EXP(-zstar/Hs)
!       ... Approx: gravity is cst, but at 100km (using g100 io g0)
!       ... The biblio reference for thory applied here is Banks and Kockarts, 
!           Aeronomy, chapter 15 (vol. 2), Academic Press, 1973.
!       ... New possibility to solve (VDIFF_SOLVE) in two different alt ranges
!           from iz=1 to izibc and iz=izibc to niz
!       ... Last modofoed by Simon Chabrillat (simonc@oma.be), July 2002
!-----------------------------------------------------------------------
      use GRID_DIMS, only : niz           ! number of vertical levels
      use SPECIES_DIMS, only : nbcon      ! number of species
      use PHYS_CST, only : R, D12cst, g0, R0
      use VEN9, only : xkz                ! Eddy diffusion coefficient (m2/s), fct of lat & alt
      use BACKATM, only : wmole           ! molar mass of air (g/mole), fct of lat & alt
      use ZGRID, only : Hair              ! atm scale height (km), fct of lat & alt
      use BOUNDARIES, only : lbc, ubc, ibc
      use SPC_NAMES                       ! i.e. vid_o2, vid_o3, ...
      use TRACNM    ! for diags
      use DIAG_CONTROLS, only : diags, ldiag, idiag
      use NUMERICAL, only : TRIDLA        ! routine to solve tridiag system
            
      implicit none
!-----------------------------------------------------------------------
!	... Parameters
!-----------------------------------------------------------------------
      integer, parameter :: nizm1 = niz - 1
      real, parameter :: g100 = 1.e2*g0 * ( R0 / (R0+100.) )**2    ! cm2/s
      real, parameter :: Hs = 7.            ! Scale height defining log-p alt zstar (km)
      real, parameter :: dzs = 1.           ! constant log-p alt steps (km)

!-----------------------------------------------------------------------
!	... Dummy arguments
!-----------------------------------------------------------------------
      integer, intent(in) :: lat                        ! latitude index
      real, intent(in) :: dts                           ! timestep (seconds)
      real, intent(in), dimension(niz) :: T, n   ! temperature(K), totdens(cm-3)
      real, intent(inout), dimension(niz,nbcon) :: ni   ! species nb densities

!-----------------------------------------------------------------------
!	... Type definition for species properties
!-----------------------------------------------------------------------
      type GTYPE
         real :: mass     ! in amu, i.e. g/mole
         real :: alphat   ! thermal diff factor (B&K 1973 Vol2 p43)
         real :: A, s     ! for molecular diff coeff D12 = A*T**s/n
      end type GTYPE
      
!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      logical :: diags_now
      integer :: i
      real, dimension(niz) :: Kzz, C
      real, dimension(nizm1) :: Kzzh, Tzh, nzh, b1, b2, a2tdep
      real, dimension(nizm1) :: Dizh, wmzh, Hratzh, invHizh
      real :: e, inve, invdzs, zgeoh
      type( GTYPE ), dimension(nbcon) :: gas
           
         invdzs = 1.e-5 / dzs
         e = EXP( 0.5 * dzs / Hs )    ! so that ni(zh)/ni(z+1) = e * T(iz+1)/Tzh
         inve = 1. / e                ! so that ni(zh)/ni(z) = inve * T(iz)/Tzh
         gas(:)%mass = 0.

!-----------------------------------------------------------------------
!          A & s below from Banks/Kockarts-1973 Vol2 tab15.1 p40
!        alphat from Chapman/Cowling-1952 (rigid spheres approx)
!        Species properties:       mass | alphat |  A    |   s   
!---------------------------------------|--------|-------|--------------
         gas(     vid_o2) = GTYPE(  32.   ,  0.  , -9.e99  , -9.e99 )
         gas(    vid_o1d) = GTYPE(  16.   ,  0.  ,  9.69e16,  0.774 )
         gas(     vid_oh) = GTYPE(  17.   ,  0.  , -9.e99  , -9.e99 )
         gas(     vid_cl) = GTYPE(  35.45 ,  0.  , -9.e99  , -9.e99 )
         gas(    vid_o3p) = GTYPE(  16.   ,  0.  ,  9.69e16,  0.774 )
         gas(     vid_o3) = GTYPE(  48.   ,  0.  , -9.e99  , -9.e99 )
         gas(    vid_ho2) = GTYPE(  33.   ,  0.  , -9.e99  , -9.e99 )
         gas(    vid_no2) = GTYPE(  46.   ,  0.  , -9.e99  , -9.e99 )
         gas(     vid_no) = GTYPE(  30.   ,  0.  , -9.e99  , -9.e99 )
         gas(     vid_br) = GTYPE(  79.9  ,  0.  , -9.e99  , -9.e99 )
         gas(      vid_n) = GTYPE(  14.   ,  0.  , -9.e99  , -9.e99 )
         gas(    vid_clo) = GTYPE(  51.45 ,  0.  , -9.e99  , -9.e99 )
         gas(    vid_bro) = GTYPE(  95.9  ,  0.  , -9.e99  , -9.e99 )
         gas(    vid_no3) = GTYPE(  62.   ,  0.  , -9.e99  , -9.e99 )
         gas(      vid_h) = GTYPE(   1.   , -0.38,  4.90e17,  0.708 )
         gas(    vid_n2o) = GTYPE(  44.   ,  0.  , -9.e99  , -9.e99 )
         gas(    vid_ch4) = GTYPE(  16.   ,  0.  ,  7.56e16,  0.747 )
         gas(    vid_h2o) = GTYPE(  18.   ,  0.  , -9.e99  , -9.e99 )
         gas(   vid_hno3) = GTYPE(  63.   ,  0.  , -9.e99  , -9.e99 )
         gas(   vid_n2o5) = GTYPE( 108.   ,  0.  , -9.e99  , -9.e99 )
         gas(     vid_co) = GTYPE(  28.   ,  0.  ,  8.22e16,  0.730 )
         gas(   vid_oclo) = GTYPE(  67.45 ,  0.  , -9.e99  , -9.e99 )
         gas(    vid_hcl) = GTYPE(  36.45 ,  0.  , -9.e99  , -9.e99 )
         gas( vid_clono2) = GTYPE(  97.45 ,  0.  , -9.e99  , -9.e99 )
         gas(   vid_hocl) = GTYPE(  52.45 ,  0.  , -9.e99  , -9.e99 )
         gas(    vid_cl2) = GTYPE(  70.9  ,  0.  , -9.e99  , -9.e99 )
         gas(   vid_h2o2) = GTYPE(  34.   ,  0.  , -9.e99  , -9.e99 )
         gas(  vid_clno2) = GTYPE(  81.45 ,  0.  , -9.e99  , -9.e99 )
         gas(    vid_hbr) = GTYPE(  80.9  ,  0.  , -9.e99  , -9.e99 )
         gas( vid_brono2) = GTYPE( 141.9  ,  0.  , -9.e99  , -9.e99 )
         gas( vid_ho2no2) = GTYPE(  79.   ,  0.  , -9.e99  , -9.e99 )
         gas(  vid_cl2o2) = GTYPE( 106.9  ,  0.  , -9.e99  , -9.e99 )
         gas(   vid_hobr) = GTYPE(  96.9  ,  0.  , -9.e99  , -9.e99 )
         gas(    vid_co2) = GTYPE(  44.   ,  0.1 ,  6.42e16,  0.735 ) ! alphat: Bernhardt-1979(JGR p.799)
         gas(   vid_ch2o) = GTYPE(  30.   ,  0.  , -9.e99  , -9.e99 )
         gas(     vid_h2) = GTYPE(   2.   , -0.25,  2.67e17,  0.750 ) ! alphat: Bernhardt-1979(JGR p.799)
         gas(  vid_cfc10) = GTYPE( 100.0  ,  0.  , -9.e99  , -9.e99 )
         gas(  vid_cfc11) = GTYPE( 100.0  ,  0.  , -9.e99  , -9.e99 )
         gas(  vid_cfc12) = GTYPE( 100.0  ,  0.  , -9.e99  , -9.e99 )
         gas( vid_cfc113) = GTYPE( 100.0  ,  0.  , -9.e99  , -9.e99 )
         gas( vid_cfc114) = GTYPE( 100.0  ,  0.  , -9.e99  , -9.e99 )
         gas( vid_cfc115) = GTYPE( 100.0  ,  0.  , -9.e99  , -9.e99 )
         gas( vid_hcfc22) = GTYPE( 100.0  ,  0.  , -9.e99  , -9.e99 )
         gas(  vid_ch3cl) = GTYPE(  50.45 ,  0.  , -9.e99  , -9.e99 )
         gas( vid_ha1211) = GTYPE( 100.0  ,  0.  , -9.e99  , -9.e99 )
         gas( vid_ha1301) = GTYPE( 100.0  ,  0.  , -9.e99  , -9.e99 )
         gas(  vid_ch3br) = GTYPE(  94.9  ,  0.  , -9.e99  , -9.e99 )
         gas(  vid_chbr3) = GTYPE( 252.7  ,  0.  , -9.e99  , -9.e99 )
         gas(  vid_ch3o2) = GTYPE(  47.   ,  0.  , -9.e99  , -9.e99 )
         gas( vid_ch3ooh) = GTYPE(  48.   ,  0.  , -9.e99  , -9.e99 )
         gas(   vid_brcl) = GTYPE( 115.35 ,  0.  , -9.e99  , -9.e99 )
         gas(     vid_hf) = GTYPE(  20.   ,  0.  , -9.e99  , -9.e99 )
         gas(vid_ch3ccl3) = GTYPE( 133.35 ,  0.  , -9.e99  , -9.e99 )
         gas(      vid_x) = gas(vid_co2)
         gas(   vid_o2dg) = gas(vid_o2)
         gas(    vid_o2s) = gas(vid_o2)

      Kzz(:) = 1.e4 * xkz(lat,:)      ! from m2/s to cm2/s

      C(:) = Hs * dts * invdzs / Hair(lat,:)
!-----------------------------------------------------------------------
!     ... Prepare species-independent quantities at iz*+1/2 (zh suffix)
!-----------------------------------------------------------------------
      Hratzh(:) = 2. * Hs / ( Hair(lat,1:nizm1) + Hair(lat,2:niz) )  ! d/dz=Hratzh*d/dzs
      Kzzh(:) = 0.5 * ( Kzz(1:nizm1) + Kzz(2:niz) )
      wmzh(:) = 0.5 * ( wmole(lat,1:nizm1) + wmole(lat,2:niz) )
      Tzh(:) = 0.5 * ( T(1:nizm1) + T(2:niz) )
      nzh(:) = 0.5 * ( n(1:nizm1) + n(2:niz) )
      b1(:) = - Kzzh(:) * Hratzh(:) * invdzs * nzh(:) / n(2:niz)
      b2(:) = Kzzh(:) * Hratzh(:) * invdzs * nzh(:) / n(1:nizm1)
      a2tdep(:) = Hratzh(:) * ( T(2:niz)-T(1:nizm1) ) * invdzs / Tzh(:)
      
      do i = 1, nbcon
!         if( i == vid_x .or. i == vid_o2dg .or. i == vid_o2s ) cycle
         diags_now = diags .and. (lat == ldiag) .and. (i == idiag)
         if( gas(i)%mass <= 0. ) then
             write(*,*) 'VDIFF_NEW, fatal error: undefined species ',i
             stop 'VDIFF_NEW, fatal error: undefined species'
         end if
         
!-----------------------------------------------------------------------
!     ... Calculate the molecular diffusion coefficient Dizh
!         following theory in Banks and Kockarts, vol.2, p.38, 1973
!-----------------------------------------------------------------------
         if( gas(i)%A > 0. ) then                            ! A, s known: 
            Dizh(:) = gas(i)%A * Tzh(:)**gas(i)%s / nzh(:)   ! B&K1972, (15.30)
          else                                               ! A, s unknown: 
            Dizh(:) = D12cst                                 ! B&K1972, (15.29)
     $           * SQRT( (1./gas(i)%mass + 1./wmzh(:))*Tzh(:) ) / nzh(:)
         end if
!         if( i == vid_co2 ) Dizh(:) = 0.  ! Um5 & v6s38a tests
         invHizh(:) = 1.e-7 * gas(i)%mass * g100 / ( R * Tzh(:) )

!-----------------------------------------------------------------------
!	... Solve the vert diff system, optionally on 2 different vert domains
!-----------------------------------------------------------------------
         if( diags_now ) write(*,*) 'VDIFF at lat= ',lat
     $       ,' : calling VDIFF_SOLVE for species ',solsym(i)
         if( ibc(i)%iz > 0 ) then
            call VDIFF_SOLVE( lbc(i), ibc(i) )
            call VDIFF_SOLVE( ibc(i), ubc(i) )
          else
            call VDIFF_SOLVE( lbc(i), ubc(i) )
         end if
      end do
      
      contains
      
!=======================================================================

      subroutine VDIFF_SOLVE( bc0, bc1 )

      use BOUNDARIES, only : BOUNDY_COND

      implicit none
!-----------------------------------------------------------------------
!	... Parameters
!-----------------------------------------------------------------------
      integer, parameter :: lower = 1, main = 2, upper = 3

!-----------------------------------------------------------------------
!	... Dummy arguments
!-----------------------------------------------------------------------
      type( BOUNDY_COND ), intent(in) :: bc0, bc1

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer :: iz0, iz0p1, iz1, iz1m1, iz1m2
      real, dimension(bc0%iz:bc1%iz) :: nid
      real, dimension(bc0%iz:bc1%iz-1) :: a1, a2
      real, dimension(bc0%iz:bc1%iz,3) :: tv

      iz0 = bc0%iz            
      iz0p1 = iz0 + 1
      iz1 = bc1%iz
      iz1m1 = iz1 - 1
      iz1m2 = iz1 - 2
      if(diags_now) write(*,*) 'VDIFF_SOLVE : iz0= ',iz0,' to iz1= ',iz1
      
!-----------------------------------------------------------------------
!     ... Calculate the coefficients of the tridiagonal matrix
!-----------------------------------------------------------------------
      tv(iz0,lower) = 0.
      tv(iz1,upper) = 0.
      a1(:) = Dizh(iz0:iz1m1) * Hratzh(iz0:iz1m1) * invdzs
      a2(:) = 0.5 * Dizh(iz0:iz1m1) * 
     $   ( invHizh(iz0:iz1m1) + (1.+gas(i)%alphat) * a2tdep(iz0:iz1m1) )
     
      tv(iz0p1:iz1,lower) = - C(iz0p1:iz1) 
     $                    * ( b2(iz0:iz1m1) + a1(:) - a2(:) )

      tv(iz0p1:iz1m1,main) = 1. + C(iz0p1:iz1m1) *
     $    ( b2(iz0p1:iz1m1) + a1(iz0p1:iz1m1) - a2(iz0p1:iz1m1) 
     $      - b1(iz0:iz1m2) + a1(iz0:iz1m2) + a2(iz0:iz1m2) )

      tv(iz0:iz1m1,upper) = C(iz0:iz1m1) 
     $                    * ( b1(iz0:iz1m1) - a1(:) - a2(:) )
      nid(:) = ni(iz0:iz1,i)

!-----------------------------------------------------------------------
!     ... Apply lower and upper boundary conditions
!-----------------------------------------------------------------------
      if( bc0%is_vmr ) then   
         tv(iz0,main) = 1.
         tv(iz0,upper) = 0.
         nid(iz0) = bc0%val(lat) * n(iz0)
       else 
!   use b0%val, the lower boundary flux (molec/cm2/s, >0 ->upward)
!   and/or b0%vel, dry deposition velocity (cm/s?, >0 ->downward)
         tv(iz0,main) = 1. + C(iz0) * ( b2(iz0) + a1(iz0)-a2(iz0) 
     $                                 + bc0%vel(lat) )
         nid(iz0) = nid(iz0) + C(iz0) * bc0%val(lat)
      end if
      if( bc1%is_vmr ) then
         tv(iz1,lower) = 0.
         tv(iz1,main) = 1.
         nid(iz1) = bc1%val(lat) * n(iz1)
       else  !   use flxub(lmax,nbcon), the upper boundary flux (molec/cm2/s, >0 ->upward)
         tv(iz1,main) = 1. - C(iz1)* ( b1(iz1m1) -a1(iz1m1) -a2(iz1m1) )
         nid(iz1) = nid(iz1) - C(iz1) * bc1%val(lat)
      end if

!-----------------------------------------------------------------------
!     ... Solve the tridiagonal system
!-----------------------------------------------------------------------
      call TRIDLA( iz1-iz0+1,
     $             tv(iz0,lower),
     $             tv(iz0,main),
     $             tv(iz0,upper),
     $             nid(iz0) )
         
      ni(iz0:iz1,i) = nid(:)
      
      end subroutine VDIFF_SOLVE

      end subroutine VDIFF
