! subversion Id for THIS file : $Id: hines97.gwave.dyn.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/hines97.gwave.dyn.f $
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!    Hines gravity wave parameterization adapted to SOCRATES v5s07 by
!                                              simonc@oma.be, june 2000.
!    This file is made from hines_driver.f and hines.f 
!    sent by A. Smith on 30 may 2000. hines.f had latest modifs brought 
!    by C. McLandress on July 1, 1996.
!    The tunable parameters are described in the modules HINES_DIMS and 
!    HINES_CONTROLS below. 
!    A full list of the work variables is at the end of the file.
!    The reference is Hines, JASTP, v. 59, pp. 371-400, 1997 (two papers)
!-----------------------------------------------------------------------

      module HINES_DIMS
      
      use GRID_DIMS, only : lmax, niz
      
      integer, parameter :: nlons = lmax, nlevs = niz ! 11 km = 200 mbar
      integer, parameter :: il1 = 1, il2 = lmax       ! latitude range
      integer, parameter :: levbot = 12, levtop = niz ! alt range, 11 km = 200 mbar
      integer, parameter :: nazmth = 16               ! do not change. Max for naz

      end module HINES_DIMS

!=======================================================================

      module HINES_CONTROLS
      
      use HINES_DIMS, only : nazmth, nlons
      
      integer, parameter :: iprnt1 = 0, iprnt2 = 0          ! diagnostics off
!-----------------------------------------------------------------------
!     * NAZ        = nb of horizontal azimuths used. Only the values 
!                       4, 8, 12 or 16 are possible. Try 8 or 12
!-----------------------------------------------------------------------
      integer, parameter :: naz = 8
!----------------------------------------------------------------------
C     * NSMAX      = number of times smoother applied ( >= 1),
C     *            = 0 means no smoothing performed.
C     * IHEATCAL   = 1 to calculate heating rates and diffusion coefficient.
C     *            = 0 means only drag and flux calculated.
C     * ICUTOFF    = 1 to exponentially damp off GWD, heating and diffusion 
C     *              arrays above ALT_CUTOFF; otherwise arrays not modified.
C     * ALT_CUTOFF = altitude in meters above which exponential decay applied.
C     * SLOPE      = slope of incident vertical wavenumber spectrum
C     *              Only the values 1., 1.5 or 2. are possible
!----------------------------------------------------------------------
      integer, parameter :: nsmax = 3, iheatcal = 1
      integer            :: icutoff = 0   
      real, parameter :: alt_cutoff = 105.E3, slope = 1.
!-----------------------------------------------------------------------
C     * F1         = "fudge factor" used in calculation of trial value of
C     *              azimuthal cutoff wavenumber M_ALPHA (1.2 <= F1 <= 1.9).
C     * F2         = "fudge factor" used in calculation of maximum
C     *              permissible instabiliy-induced cutoff wavenumber 
C     *              M_SUB_M_TURB (0.1 <= F2 <= 0.4).
C     * F3         = "fudge factor" used in calculation of maximum 
C     *              permissible molecular viscosity-induced cutoff wavenumber 
C     *              M_SUB_M_MOL (0.1 <= F3 <= 1.4).
C     * F5         = "fudge factor" used in calculation of heating rate
C     *              (1 <= F5 <= 3).
C     * F6         = "fudge factor" used in calculation of turbulent 
C     *              diffusivity coefficient. 0.25 from SOCRATES v6s07 to v6s25b
!-----------------------------------------------------------------------
      real, parameter :: f1 = 1.5, f2 = 0.3, f3 = 1., f5 = 1.
      real, parameter :: f6 = 0.15   ! was 0.25
!-----------------------------------------------------------------------
C     * KSTAR      = typical gravity wave horizontal wavenumber (1/m)
C     * SMCO       = smoother used to smooth cutoff vertical wavenumbers
C     *              and total rms winds before calculating drag or heating.
C     *              (==> a 1:SMCO:1 stencil used; SMCO must be >= 1.).
C     *              used in calculation of M_SUB_M_TURB.
!-----------------------------------------------------------------------
      real, parameter :: kstar = 14.E-6   ! try also KSTAR = 7.E-6
      real, parameter :: smco = 2.0
!-----------------------------------------------------------------------
C     * M_MIN      = minimum allowable cutoff vertical wavenumber, 
C     *              e.g., 1/(3km). This is used only for a spectral slope
C     *              (SLOPE) of one ==> for slope of 1.5 or 2 then M_MIN = 0. 
C     * RMS_WIND   = root mean square gravity wave wind at bottom (reference)
C     *              level (m/s).
C     * K_ALPHA    = horizontal wavenumber of each azimuth (1/m).
!-----------------------------------------------------------------------
      real :: m_min
      real, dimension(nlons) :: rms_wind
      real, dimension(nlons,nazmth) :: k_alpha

      end module HINES_CONTROLS

!=======================================================================

      module HINES_VARS
!-----------------------------------------------------------------------
!        All variables defined here are described at the end of the file
!-----------------------------------------------------------------------
      use HINES_DIMS, only : nlevs, nlons, nazmth
      
      real, parameter :: CPGAS = 1004.     ! specific heat at constant pressure
      real, parameter :: rms_min = 0.001, visc_min = 1.e-10, d13 = 1./3.
      real, parameter :: COS45 = 0.7071068, COS30 = 0.8660254, 
     $                 SIN30 = 0.5, COS22 = 0.9238795, SIN22 = 0.3826834

      integer :: iorder
      logical :: dragil(nlons,nlevs),drag(nlons), do_alpha(nlons,nazmth)
      real, dimension(nlons) :: bvfb, densb, ubot, vbot
      real, dimension(nlons,nlevs) :: density, alt, vel_u, vel_v, 
     $                                bvfreq, visc_mol
      real, dimension(nlons,nlevs) :: drag_u, drag_v, diffco, heat
      real, dimension(nlons,nlevs) :: flux_u, flux_v, sigma_t
      real, dimension(nlons,nazmth) :: ak_alpha, mmin_alpha, i_alpha, 
     $                                 specfac, v_alpha
      real, dimension(nlons,nlevs,nazmth) :: sigma_alpha, sigsqh_alpha,
     $                                       m_alpha
      
      end module HINES_VARS

!=======================================================================

      subroutine HINES97_GWAVE_DYN( dzzgw, fx )

      use GRID_DIMS, only : lmax, niz
      use SIM_CONTROLS, only : mainsw
      use VEN2, only : u
      use HEAT_TERMS, only : gwheat      ! heating rate (K/day): output
      use VEN6, only : v
      use VEN8, only : bv
      use VEN10, only : mu               ! molecular viscosity (m2/s)
      use BACKATM, only : ro,            ! mass density of air (g/m3)
     $                    u_hwm, v_hwm   ! horizontal winds by HWM (m/s)  
      use ALLCO, only : zkm, phir
      use TROPOPAUSE, only : izm, izmMax
      use HINES_DIMS, only : nlons, nlevs
      use HINES_CONTROLS, only : naz, icutoff, slope, m_min, rms_wind, 
     $                           k_alpha, f6
      use HINES_VARS, only : vel_u, vel_v, density, alt, visc_mol, 
     $                       bvfreq, drag_u, heat, diffco
      use TRANSFORM, only : SMOOTHV, SMOOTHL

      implicit none

!-----------------------------------------------------------------------
!       ... Dummy args
!-----------------------------------------------------------------------
      real, dimension(lmax,niz), intent(out) :: fx, dzzgw

!-----------------------------------------------------------------------
!       ... Local variables
!-----------------------------------------------------------------------
      integer :: iz, l, j, k, nn
      real, dimension(lmax,niz) :: gwdiff
      logical, save :: entered = .false.

      if( .not. entered ) then
         do j = 1, nlons
            alt(j,1:nlevs) = zkm(1:niz) * 1.e3
         end do
         m_min = 0.
!-----------------------------------------------------------------------
!       ... Hines setup. m_min, k_alpha and rms_wind can be tuned, see
!           module HINES_CONTROLS
!-----------------------------------------------------------------------
         if( slope == 1. ) m_min = 1. / 3.e3
         do nn = 1, naz
            k_alpha(1:nlons,nn) = 2.e-6  ! / ( COS(phir(1:lmax)) + .1 )
         end do
         rms_wind(1:nlons) = 2.    
         entered = .true.
      end if
      
!-----------------------------------------------------------------------
!       ... Transfer SOCRATES variables to HINES_VARS module
!-----------------------------------------------------------------------
      if ( mainsw(4) == 1 .or. mainsw(4) == -1 ) then
         icutoff = 1 
         vel_u(1:nlons,1:nlevs) = u_hwm(1:lmax,1:niz)  
         vel_v(1:nlons,1:nlevs) = v_hwm(1:lmax,1:niz)
       else
         icutoff = 0            !   should be 1, but meso circ get too weak
         vel_u(1:nlons,1:nlevs) = u(1:lmax,1:niz)  
         vel_v(1:nlons,1:nlevs) = v(1:lmax,1:niz)
      end if
      bvfreq(1:nlons,1:nlevs) = bv(1:lmax,1:niz)
      density(:,:) = 1.e3 * ro(:,:)                     ! mean density (kg/m3)
      visc_mol(1:nlons,1:nlevs) = mu(1:lmax,1:niz)

!-----------------------------------------------------------------------
!       ... Calculate Hines GW parameterization
!-----------------------------------------------------------------------
      call HINES_DSP1( )

!-----------------------------------------------------------------------
!       ... Transfer back from HINES_VARS module to SOCRATES variables
!           Notice some other outputs from HINES is left unused 
!-----------------------------------------------------------------------
      fx(1:lmax,1:niz) = drag_u(1:nlons,1:nlevs)
      gwheat = heat * 86400.                       ! from K/s to K/day
      dzzgw(:,1:20) = 0.
      dzzgw(1:lmax,21:niz) = diffco(1:nlons,21:nlevs) ! * floc (tuned now w/ f6)

!-----------------------------------------------------------------------
!       ... Artificially lower fx by 10km
!-----------------------------------------------------------------------
!      do iz = 41, 111
!         fx(1:lmax,iz) = drag_u(1:nlons,iz+10)
!      end do
!      do l = 1, lmax
!         fx(l,112:niz) = drag_u(l,nlevs)
!      end do

!-----------------------------------------------------------------------
!     smooth in the meridional and vertical directions
!-----------------------------------------------------------------------
      call SMOOTHL( fx, 1, niz, 4 )
      call SMOOTHL( dzzgw, izmMax+1, niz, 4 )
      call SMOOTHV (fx, 1, niz, 4)
      call SMOOTHV (dzzgw, izmMax+1, niz, 10)

!      write(*,'(2(a,es11.3))') 'HINES97 at (1,113): u= ',u(1,113),
!     $             ' ; v= ',v(1,113)
!      write(*,'(3(a,es11.3))') '       bv= ',bv(1,113),' ; mu= ',
!     $            mu(1,113),' ; ro= ',ro(1,113)
!      write(*,'(3(a,es11.3))') '       fx= ',fx(1,113),' ; dzzgw= ',
!     $            dzzgw(1,113),' ; gwheat= ',gwheat(1,113)

      end subroutine HINES97_GWAVE_DYN

!=======================================================================

      subroutine HINES_DSP1( )
!-----------------------------------------------------------------------
C  Main routine for Hines Doppler spread gravity wave parameterization
C  scheme which calculates zonal and meridional components of gravity 
C  wave drag, heating rates and diffusion coefficient on a longitude 
C  by altitude grid.
C  Output arguments:
C     * DRAG_U = zonal component of gravity wave drag (m/s2).
C     * DRAG_V = meridional component of gravity wave drag (m/s2).
C     * HEAT   = gravity wave heating (K/sec).
C     * DIFFCO = diffusion coefficient (m2/sec)
C     * FLUX_U = zonal component of vertical momentum flux (Pascals)
C     * FLUX_V = meridional component of vertical momentum flux (Pascals)
C  Input arguments: VEL_U, VEL_V, BVFREQ, DENSITY, VISC_MOL, ALT
!                   (see description at end of file) and variables set
!                   (and described) in module HINES_CONTROLS
!-----------------------------------------------------------------------
      use HINES_DIMS
      use HINES_CONTROLS
      use HINES_VARS
      
      implicit none
      
!-----------------------------------------------------------------------
!	... local variables 
!-----------------------------------------------------------------------
      integer :: ierror, i, n, l, lev1p, lev2m, nunit
      integer :: ilprt1, ilprt2, lev1, lev2, icount

C  Check that things set up correctly, abort if not.
C 
      ierror = 0
      if( naz /= 4 .and. naz /= 8 
     & .and. naz /= 12 .and. naz /= 16 )                    ierror = 20
      if( slope /= 1. .and. slope /= 1.5 .and. slope /= 2.) ierror = 30
      if( ierror /= 0 ) then
        write (6,*) 'HINES_DSP1 in HINES97_GWAVE_DYN: error ',ierror
        stop 'HINES_DSP1 in HINES97_GWAVE_DYN: fatal error'
      end if
C
C  Ordering of levels.
C
      if( levbot < levtop  ) then
        lev1 = levbot
        lev2 = levtop
        iorder = -1
      else
        lev1 = levtop
        lev2 = levbot
        iorder = 1
      end if
c
      lev1p = lev1 + 1
      lev2m = lev2 - 1
C
C  Initialize output and some work arrays.
C
      drag_u(il1:il2,:) = 0.
      drag_v(il1:il2,:) = 0.
      heat(il1:il2,:) = 0.
      diffco(il1:il2,:) = 0.
      flux_u(il1:il2,:) = 0.
      flux_v(il1:il2,:) = 0.
      sigma_t(il1:il2,:) = 0.
C
C  Initialize cutoff wavenumber array to minimum value. 
C
      m_alpha(il1:il2,:,1:naz) = m_min
C
C  Longitudes where drag to be calculated.
C
      icount = 0
      do i = il1,il2
         drag(i) = .false.
         if( rms_wind(i) >= rms_min ) then
            drag(i) = .true.
            icount = icount + 1
         end if
      end do
c
C  Return to calling program if no drag.
C
      if( icount == 0 ) return
C
C  Buoyancy, density and winds at bottom level.
C
      do i = il1, il2
        if( drag(i) ) then
          bvfb(i)  = bvfreq(i,levbot)
          densb(i) = density(i,levbot)
          ubot(i)  = vel_u(i,levbot)
          vbot(i)  = vel_v(i,levbot)
        end if
      end do
C
C  Calculate cutoff vertical wavenumber and velocity variances.
C
      CALL HINES_WAVNUM( )
C
C  Multiplicative spectral factor at each azimuth.
C
      do n = 1, naz
         do  i = il1, il2
            if( drag(i) ) specfac(i,n) = ak_alpha(i,n) * k_alpha(i,n)
         end do
      end do

C  Smooth cutoff wavenumbers and total rms velocity in the vertical 
C  direction NSMAX times
C   
      if( nsmax > 0 ) then
         do n = 1, naz
            call HINES_SMOOTH( m_alpha(1,1,n), lev1, lev2, smco, nsmax )
         end do
         call HINES_SMOOTH( sigma_t, lev1, lev2, smco, nsmax )
      end if
C
C  Calculate zonal and meridional components of the
C  momentum flux and drag.
C
      call HINES_DRAG( lev1, lev2 )
C
C  Heating rate and diffusion coefficient at midpoint between momentum levels.
C
      if( iheatcal == 1 ) call HINES_HEAT( )
C
C  Apply exponential decay to drag, heating and diffusion above alt_cutoff
C
      if( icutoff == 1 ) then		
         call HINES_EXP( drag_u, lev1, lev2 )
         call HINES_EXP( drag_v, lev1, lev2 )
         if( iheatcal == 1 ) then		
            call HINES_EXP( heat, lev1, lev2 )
            call HINES_EXP( diffco, lev1, lev2 )
         end if   
      end if   
C
C  Print out flux, drag, etc arrays for diagnostic purposes.
C
      if( iprnt1 == 1 ) then
         ilprt1 = 1
         ilprt2 = 1
         nunit  = 11
         call HINES_PRNT1( 1, 1, nunit, ilprt1, ilprt2, lev1, lev2 )
      end if
c
C  Print out azimuthal arrays for diagnostic purposes.
C
      if( iprnt2 == 1 ) then
         ilprt1 = 1
         ilprt2 = 1
         nunit  = 11
         call HINES_PRNT2( nunit, ilprt1, ilprt2, lev1, lev2 )
      end if

      end subroutine HINES_DSP1

!=======================================================================

      subroutine HINES_WAVNUM( )
!-----------------------------------------------------------------------
C  This routine calculates the cutoff vertical wavenumber and velocity
C  variances on a longitude by altitude grid needed for the Hines' Doppler 
C  spread gravity wave drag parameterization scheme.
C  Output arguements:
C     * M_ALPHA      = cutoff wavenumber at each azimuth (1/m).
C     * SIGMA_ALPHA  = total rms wind in each azimuth (m/s).
C     * SIGSQH_ALPHA = portion of wind variance from waves having wave
C     *                normals in the alpha azimuth (m/s).
C     * SIGMA_T      = total rms horizontal wind (m/s).
C     * AK_ALPHA     = spectral amplitude factor at each azimuth 
C     *                (i.e.,{AjKj}) in m$4/s$2.
C     * DRAGIL       = logical flag indicating longitudes and levels where 
C     *                calculations to be performed.
C  Input arguements: VEL_U, VEL_V, UBOT, VBOT, VISC_MOL, DENSITY, DENSB,
!                    BVFREQ, BVFB, RMS_WIND, DRAG, KSTAR, M_MIN, SLOPE,
C                    F1,F2,F3  (described at end of file)
!-----------------------------------------------------------------------
      use HINES_DIMS
      use HINES_CONTROLS
      use HINES_VARS
      
      implicit none

!----------------------------------------------------------------------
!	... local variables 
!----------------------------------------------------------------------
      real, dimension(nlons) :: n_over_m, sigfac
      integer :: i, l, n, lstart, lincr, lbelow, icount
      real :: denom, m_sub_m_turb, m_sub_m_mol, m_trial, mmsq
      real :: visc, azfac, sp1
C
      sp1 = slope + 1.
      mmsq = m_min**2
c
C  Indices of levels to process.
C
      if( levbot > levtop ) then
         lstart = levbot - 1     
         lincr  = -1
       else
         lstart = levbot + 1
         lincr  = 1
      end if
C
C  Initialize logical flags and determine number of longitudes
C  at bottom where calculations to be done.
C
      dragil(il1:il2,1:nlevs) = .false.
      icount = 0
      do i = il1,il2
         if( drag(i))  icount = icount + 1
         dragil(i,levbot) = drag(i)
      end do
      do n = 1,naz
         do_alpha(il1:il2,n) = drag(il1:il2)
      end do
C
C  Use horizontal isotropy to calculate azimuthal variances at bottom level.
C
      azfac = 1. / float(naz)
      do i = il1,il2
         if( drag(i) ) sigsqh_alpha(i,levbot,:) = azfac * rms_wind(i)**2
      end do
c
C  Velocity variances at bottom level.
C
      call HINES_SIGMA( drag, levbot )
C
C  Calculate cutoff wavenumber and spectral amplitude factor 
C  at bottom level where it is assumed that background winds vanish
C  and also initialize minimum value of cutoff wavnumber.
C
      if( slope == 1. ) then
         do i = il1, il2
            if( drag(i) ) then
               m_alpha(i,levbot,:) =  bvfb(i) / 
     $                            ( f1 * sigma_alpha(i,levbot,:) 
     $                            + f2 * sigma_t(i,levbot) )
               ak_alpha(i,:) = 2. * sigsqh_alpha(i,levbot,:) /
     $                         ( m_alpha(i,levbot,:)**2 - mmsq ) 
               mmin_alpha(i,:) = m_alpha(i,levbot,:)
            end if
         end do     
       else
         do i = il1, il2
            if( drag(i) ) then
               m_alpha(i,levbot,:) =  bvfb(i) / 
     $                            ( f1 * sigma_alpha(i,levbot,:) 
     $                            + f2 * sigma_t(i,levbot) )
               ak_alpha(i,:)   = sigsqh_alpha(i,levbot,:) 
     $                       / ( m_alpha(i,levbot,:)**sp1 / sp1 )
               mmin_alpha(i,:) = m_alpha(i,levbot,:)
            end if
         end do
      end if
C
C  Calculate quantities from the bottom upwards, 
C  starting one level above bottom.
C
      do l = lstart, levtop, lincr
         if( icount == 0 ) return  ! no more longitudes left to do.
         lbelow = l - lincr        ! Level beneath present level.
C
C  Compute azimuthal wind components from zonal and meridional winds.
C
         call HINES_WIND( l )
C
C  Calculate N/m_M where m_M is maximum permissible value of the vertical
C  wavenumber (i.e., m > m_M are obliterated) and N is buoyancy frequency.
C  m_M is taken as the smaller of the instability-induced 
C  wavenumber (M_SUB_M_TURB) and that imposed by molecular viscosity
C  (M_SUB_M_MOL). Since variance at this level is not yet known
C  use value at level below.
C
           do i = il1, il2
              if( dragil(i,lbelow) ) then
                 visc = AMAX1( visc_mol(i,l), visc_min )
                 m_sub_m_turb = bvfreq(i,l) / ( f2 * sigma_t(i,lbelow) )
                 m_sub_m_mol  = ( bvfreq(i,l) * kstar / visc )**d13 / f3
                 if( m_sub_m_turb < m_sub_m_mol  ) then
                    n_over_m(i) = f2 * sigma_t(i,lbelow)
                  else
                    n_over_m(i) = bvfreq(i,l) / m_sub_m_mol 
                 end if
              end if
           end do
C
C  Calculate cutoff wavenumber at this level.
C
           do n = 1, naz
              do i = il1, il2

                 if( do_alpha(i,n)  ) then 

C  Calculate trial value (since variance at this level is not yet known
C  use value at level below). If trial value is negative or if it exceeds 
C  minimum value found at lower levels (not permitted) then set it 
C  to this minimum value. 
C
                    denom =  f1 * sigma_alpha(i,lbelow,n)  
     $                    + n_over_m(i) + v_alpha(i,n)
                    if( ABS(denom) >= 1.e-5 ) then
                       m_trial = bvfb(i) / denom
                       if( m_trial <= 0. .or. m_trial> mmin_alpha(i,n) )
     $                    m_trial = mmin_alpha(i,n)
                     else
                          m_trial = mmin_alpha(i,n)
                    end if

                    m_alpha(i,l,n) = m_trial
C
C  Do not permit cutoff wavenumber to be less than minimum allowable value.
C
                    if( m_alpha(i,l,n) < m_min) m_alpha(i,l,n) = m_min
C
C  Reset minimum value of cutoff wavenumber if necessary.
C
                    if( m_alpha(i,l,n)  <  mmin_alpha(i,n) ) then
                       mmin_alpha(i,n) = m_alpha(i,l,n)
                    end if
C
                  else
c
                    m_alpha(i,l,n) = m_min
c
                 end if          
C
              end do
           end do
C
C  Calculate the Hines integral at this level.
C
           call HINES_INTGRL( dragil(1,l), l )
C
C  Calculate the velocity variances at this level.
C
           do i = il1, il2
              if( dragil(i,l) ) then
                 if( density(i,l) == 0. ) then
                    write(*,*) 'HINES_WAVNUM: density(',i,',',l,') = 0'
                    stop 'HINES_WAVNUM: density = 0. - fatal error'
                 end if
                 if( bvfb(i) == 0. ) then
                    write(*,*) 'HINES_WAVNUM: bvfb(',i,') = 0 for l= ',l
                    stop 'HINES_WAVNUM: bvfb = 0. - fatal error'
                 end if
                 sigfac(i) = ( densb(i) / density(i,l) )
     $                     * ( bvfreq(i,l) / bvfb(i) )
              end if
           end do
C
C  Calculate the velocity variances at this level.
C
           do n = 1, naz
              do i = il1, il2
                 if( dragil(i,l) )
     $              sigsqh_alpha(i,l,n) = sigfac(i) * ak_alpha(i,n) 
     $                                                    * i_alpha(i,n)
              end do
           end do
           call HINES_SIGMA( dragil(1,l), l )
c
C  If total rms wind zero (no more drag) then set DRAG to false 
C  and update longitude counter.
C
        do i = il1,il2 
           if( sigma_t(i,l)  ==  0. ) then
              dragil(i,l) = .false. 
              icount = icount - 1   
           end if
        end do

      end do                 !  End of level loop.

      end subroutine HINES_WAVNUM

!=======================================================================

      subroutine HINES_WIND( lev )
!----------------------------------------------------------------------
C  This routine calculates the azimuthal horizontal background wind components 
C  on a longitude at a single altitude for the case of 4, 8, 12 or 16 equally
C  spaced azimuths needed for the Hines' Doppler spread GWD parameterization 
C  scheme.
C  Output argument:
C     * V_ALPHA = background wind component at each azimuth (m/s). 
C     *           (note: first azimuth is in eastward direction
C     *            and rotate in counterclockwise direction.)
C  Input arguments: VEL_U, VEL_V, UBOT, VBOT, DRAG (described at end of file)
!----------------------------------------------------------------------
      use HINES_DIMS
      use HINES_CONTROLS
      use HINES_VARS
      
      implicit none

!----------------------------------------------------------------------
!	... Dummy arguments
!----------------------------------------------------------------------
      integer, intent(in) :: lev

!----------------------------------------------------------------------
!	... local variables 
!----------------------------------------------------------------------
      integer :: I
      real :: U, V

      if( naz == 4 ) then                    !  Case with 4 azimuths.
         do i = il1, il2
            if( drag(i) ) then
               u = vel_u(i,lev) - ubot(i)
               v = vel_v(i,lev) - vbot(i)
               v_alpha(i,1) = u 
               v_alpha(i,2) = v
               v_alpha(i,3) = - u
               v_alpha(i,4) = - v
            end if
         end do
       else if( naz == 8 ) then              !  Case with 8 azimuths.
         do i = il1, il2
            if( drag(i) ) then
               u = vel_u(i,lev) - ubot(i)
               v = vel_v(i,lev) - vbot(i)
               v_alpha(i,1) = u 
               v_alpha(i,2) = cos45 * ( v + u )
               v_alpha(i,3) = v
               v_alpha(i,4) = cos45 * ( v - u )
               v_alpha(i,5) = - u
               v_alpha(i,6) = - v_alpha(i,2)
               v_alpha(i,7) = - v
               v_alpha(i,8) = - v_alpha(i,4)
            end if
         end do
       else if( naz == 12 ) then             !  Case with 12 azimuths.
         do i = il1, il2
            if( drag(i) ) then
               u = vel_u(i,lev) - ubot(i)
               v = vel_v(i,lev) - vbot(i)
               v_alpha(i,1)  = u 
               v_alpha(i,2)  = cos30 * u + sin30 * v
               v_alpha(i,3)  = sin30 * u + cos30 * v
               v_alpha(i,4)  = v
               v_alpha(i,5)  = - sin30 * u + cos30 * v
               v_alpha(i,6)  = - cos30 * u + sin30 * v
               v_alpha(i,7)  = - u
               v_alpha(i,8)  = - v_alpha(i,2)
               v_alpha(i,9)  = - v_alpha(i,3)
               v_alpha(i,10) = - v
               v_alpha(i,11) = - v_alpha(i,5)
               v_alpha(i,12) = - v_alpha(i,6)
            end if
         end do
       else if( naz == 16 ) then             !  case with 16 azimuths.
         do i = il1, il2
            if( drag(i) ) then
               u = vel_u(i,lev) - ubot(i)
               v = vel_v(i,lev) - vbot(i)
               v_alpha(i,1)  = u 
               v_alpha(i,2)  = cos22 * u + sin22 * v
               v_alpha(i,3)  = cos45 * ( u + v )
               v_alpha(i,4)  = cos22 * v + sin22 * u
               v_alpha(i,5)  = v
               v_alpha(i,6)  = cos22 * v - sin22 * u
               v_alpha(i,7)  = cos45 * ( v - u )
               v_alpha(i,8)  = - cos22 * u + sin22 * v
               v_alpha(i,9)  = - u
               v_alpha(i,10) = - v_alpha(i,2)
               v_alpha(i,11) = - v_alpha(i,3)
               v_alpha(i,12) = - v_alpha(i,4)
               v_alpha(i,13) = - v
               v_alpha(i,14) = - v_alpha(i,6)
               v_alpha(i,15) = - v_alpha(i,7)
               v_alpha(i,16) = - v_alpha(i,8)
            end if
         end do
      end if

      end subroutine HINES_WIND

!=======================================================================

      subroutine HINES_DRAG( lev1, lev2 )
!-----------------------------------------------------------------------
C  Calculate zonal and meridional components of the vertical flux 
C  of horizontal momentum and corresponding wave drag (force per unit mass)
C  on a longitude by altitude grid needed for the Hines Doppler spread 
C  gravity wave parameterization scheme.
C  Output arguments:
C     * FLUX_U  = zonal component of vertical momentum flux (Pascals)
C     * FLUX_V  = meridional component of vertical momentum flux (Pascals)
C     * DRAG_U  = zonal component of drag (m/s$2).
C     * DRAG_V  = meridional component of drag (m/s$2).
C  Input arguments: ALT, DENSITY, DENSB, M_ALPHA, SPECFAC, DRAGIL, M_MIN,
!                   SLOPE  (described at end of file)
!-----------------------------------------------------------------------
      use HINES_DIMS
      use HINES_CONTROLS
      use HINES_VARS
      
      implicit none

!-----------------------------------------------------------------------
!	... Dummy arguments
!-----------------------------------------------------------------------
      integer, intent(in) :: lev1, lev2

!-----------------------------------------------------------------------
!	... local variables 
!-----------------------------------------------------------------------
      integer :: i, l, lev1p, lev2m
      real :: ddz, ddz2
      real :: flux2,  flux3,  flux4,  flux5,  flux6,  flux7,  flux8
      real :: flux9,  flux10, flux11, flux12, flux14, flux15, flux16
c
      lev1p = lev1 + 1
      lev2m = lev2 - 1
c
C  Sum over azimuths for case where SLOPE = 1 (NOTE: this is the only
C  value of the slope for which a nonzero M_MIN is used).
C
      if( slope == 1. ) then
      
         if( naz == 4 ) then                   !  Case with 4 azimuths.
            do l = lev1, lev2
               do i = il1,il2
                  if( dragil(i,l) ) then
                     flux_u(i,l) = densb(i) * (
     $                      specfac(i,1) * ( m_alpha(i,l,1) - m_min )
     $                    - specfac(i,3) * ( m_alpha(i,l,3) - m_min ) )
                     flux_v(i,l) = densb(i) * (
     $                      specfac(i,2) * ( m_alpha(i,l,2) - m_min )
     $                    - specfac(i,4) * ( m_alpha(i,l,4) - m_min ) )
                  end if
               end do
            end do
         else if( naz == 8 ) then              !  Case with 8 azimuths.
            do l = lev1, lev2
               do i = il1,il2
                  if( dragil(i,l) ) then
                     flux2 = specfac(i,2) * ( m_alpha(i,l,2) - m_min )
                     flux4 = specfac(i,4) * ( m_alpha(i,l,4) - m_min )
                     flux6 = specfac(i,6) * ( m_alpha(i,l,6) - m_min )
                     flux8 = specfac(i,8) * ( m_alpha(i,l,8) - m_min )
                     flux_u(i,l) = densb(i) * (
     $                      specfac(i,1) * ( m_alpha(i,l,1) - m_min )
     $                    - specfac(i,5) * ( m_alpha(i,l,5) - m_min )
     $                    + cos45 * ( flux2 - flux4 - flux6 + flux8 ) )
                     flux_v(i,l) = densb(i) * (
     $                      specfac(i,3) * ( m_alpha(i,l,3) - m_min )
     $                    - specfac(i,7) * ( m_alpha(i,l,7) - m_min )
     $                    + cos45 * ( flux2 + flux4 - flux6 - flux8 ) )
                  end if
               end do
            end do
         else if( naz == 12 ) then              !  Case with 12 azimuths.
            do l = lev1, lev2
               do i = il1,il2
                  if( dragil(i,l) ) then
                     flux2  = specfac(i,2)  * ( m_alpha(i,l,2) - m_min )
                     flux3  = specfac(i,3)  * ( m_alpha(i,l,3) - m_min )
                     flux5  = specfac(i,5)  * ( m_alpha(i,l,5) - m_min )
                     flux6  = specfac(i,6)  * ( m_alpha(i,l,6) - m_min )
                     flux8  = specfac(i,8)  * ( m_alpha(i,l,8) - m_min )
                     flux9  = specfac(i,9)  * ( m_alpha(i,l,9) - m_min )
                     flux11 = specfac(i,11) * ( m_alpha(i,l,11)- m_min )
                     flux12 = specfac(i,12) * ( m_alpha(i,l,12)- m_min )
                     flux_u(i,l) = densb(i) * (
     $                      specfac(i,1) * ( m_alpha(i,l,1) - m_min )
     $                    - specfac(i,7) * ( m_alpha(i,l,7) - m_min )
     $                    + cos30 * ( flux2 - flux6 - flux8 + flux12 ) 
     $                    + sin30 * ( flux3 - flux5 - flux9 + flux11 ) )
                     flux_v(i,l) = densb(i) * (
     $                      specfac(i,4)  * ( m_alpha(i,l,4)  - m_min )
     $                    - specfac(i,10) * ( m_alpha(i,l,10) - m_min )
     $                    + cos30 * ( flux3 + flux5 - flux9 - flux11 ) 
     $                    + sin30 * ( flux2 + flux6 - flux8 - flux12 ) )
                  end if
               end do
            end do
         else if( naz == 16 ) then              !  Case with 16 azimuths.
            do l = lev1, lev2
               do i = il1,il2
                  if( dragil(i,l) ) then
                     flux2  = specfac(i,2)  * ( m_alpha(i,l,2) - m_min )
                     flux3  = specfac(i,3)  * ( m_alpha(i,l,3) - m_min )
                     flux4  = specfac(i,4)  * ( m_alpha(i,l,4) - m_min )
                     flux6  = specfac(i,6)  * ( m_alpha(i,l,6) - m_min )
                     flux7  = specfac(i,7)  * ( m_alpha(i,l,7) - m_min )
                     flux8  = specfac(i,8)  * ( m_alpha(i,l,8) - m_min )
                     flux10 = specfac(i,10) * ( m_alpha(i,l,10)- m_min )
                     flux11 = specfac(i,11) * ( m_alpha(i,l,11)- m_min )
                     flux12 = specfac(i,12) * ( m_alpha(i,l,12)- m_min )
                     flux14 = specfac(i,14) * ( m_alpha(i,l,14)- m_min )
                     flux15 = specfac(i,15) * ( m_alpha(i,l,15)- m_min )
                     flux16 = specfac(i,16) * ( m_alpha(i,l,16)- m_min )
                     flux_u(i,l) = densb(i) * (
     $                      specfac(i,1) * ( m_alpha(i,l,1) - m_min )
     $                    - specfac(i,9) * ( m_alpha(i,l,9) - m_min )
     $                   + cos22 * ( flux2 - flux8 - flux10 + flux16 ) 
     $                   + cos45 * ( flux3 - flux7 - flux11 + flux15 ) 
     $                   + sin22 * ( flux4 - flux6 - flux12 + flux14 ) )
                     flux_v(i,l) = densb(i) * (
     $                      specfac(i,5)  * ( m_alpha(i,l,5)  - m_min )
     $                    - specfac(i,13) * ( m_alpha(i,l,13) - m_min )
     $                   + cos22 * ( flux4 + flux6 - flux12 - flux14 ) 
     $                   + cos45 * ( flux3 + flux7 - flux11 - flux15 ) 
     $                   + sin22 * ( flux2 + flux8 - flux10 - flux16 ) )
                  end if
               end do
            end do
         end if

C
C  Sum over azimuths for case where SLOPE not equal to 1
C  (NOTE: minimum wavenumber cutoff is zero).
C
      else if( slope /= 1. ) then
         if( naz == 4 ) then                   !  Case with 4 azimuths.
            do l = lev1, lev2
               do i = il1,il2
                  if( dragil(i,l) ) then
                     flux_u(i,l) = densb(i) / slope * (
     $                      specfac(i,1) * m_alpha(i,l,1)**slope
     $                    - specfac(i,3) * m_alpha(i,l,3)**slope )
                     flux_v(i,l) = densb(i) / slope * (
     $                      specfac(i,2) * m_alpha(i,l,2)**slope
     $                    - specfac(i,4) * m_alpha(i,l,4)**slope )
                  end if
               end do
            end do
         else if( naz == 8 ) then              !  case with 8 azimuths.
            do l = lev1, lev2
               do i = il1,il2
                  if( dragil(i,l) ) then
                     flux2 = specfac(i,2) * m_alpha(i,l,2)**slope
                     flux4 = specfac(i,4) * m_alpha(i,l,4)**slope
                     flux6 = specfac(i,6) * m_alpha(i,l,6)**slope
                     flux8 = specfac(i,8) * m_alpha(i,l,8)**slope
                     flux_u(i,l) = densb(i) / slope * (
     $                     specfac(i,1) * m_alpha(i,l,1)**slope
     $                   - specfac(i,5) * m_alpha(i,l,5)**slope
     $                   + cos45 * ( flux2 - flux4 - flux6 + flux8 ) )
                     flux_v(i,l) =  densb(i) / slope * (
     $                     specfac(i,3) * m_alpha(i,l,3)**slope
     $                   - specfac(i,7) * m_alpha(i,l,7)**slope
     $                   + cos45 * ( flux2 + flux4 - flux6 - flux8 ) )
                  end if
               end do
            end do
         else if( naz == 12 ) then              !  Case with 12 azimuths.
            do l = lev1, lev2
               do i = il1,il2
                  if( dragil(i,l) ) then
                     flux2  = specfac(i,2)  * m_alpha(i,l,2)**slope
                     flux3  = specfac(i,3)  * m_alpha(i,l,3)**slope
                     flux5  = specfac(i,5)  * m_alpha(i,l,5)**slope
                     flux6  = specfac(i,6)  * m_alpha(i,l,6)**slope
                     flux8  = specfac(i,8)  * m_alpha(i,l,8)**slope
                     flux9  = specfac(i,9)  * m_alpha(i,l,9)**slope
                     flux11 = specfac(i,11) * m_alpha(i,l,11)**slope
                     flux12 = specfac(i,12) * m_alpha(i,l,12)**slope
                     flux_u(i,l) = densb(i) / slope * (
     $                      specfac(i,1) * m_alpha(i,l,1)**slope
     $                    - specfac(i,7) * m_alpha(i,l,7)**slope
     $                    + cos30 * ( flux2 - flux6 - flux8 + flux12 ) 
     $                    + sin30 * ( flux3 - flux5 - flux9 + flux11 ) )
                     flux_v(i,l) =  densb(i) / slope * (
     $                      specfac(i,4)  * m_alpha(i,l,4)**slope
     $                    - specfac(i,10) * m_alpha(i,l,10)**slope
     $                    + cos30 * ( flux3 + flux5 - flux9 - flux11 ) 
     $                    + sin30 * ( flux2 + flux6 - flux8 - flux12 ) )
                  end if
               end do
            end do
         else if( naz == 16 ) then              !  Case with 16 azimuths.
            do l = lev1, lev2
               do i = il1,il2
                  if( dragil(i,l) ) then
                     flux2  = specfac(i,2)  * m_alpha(i,l,2)**slope
                     flux3  = specfac(i,3)  * m_alpha(i,l,3)**slope
                     flux4  = specfac(i,4)  * m_alpha(i,l,4)**slope
                     flux6  = specfac(i,6)  * m_alpha(i,l,6)**slope
                     flux7  = specfac(i,7)  * m_alpha(i,l,7)**slope
                     flux8  = specfac(i,8)  * m_alpha(i,l,8)**slope
                     flux10 = specfac(i,10) * m_alpha(i,l,10)**slope
                     flux11 = specfac(i,11) * m_alpha(i,l,11)**slope
                     flux12 = specfac(i,12) * m_alpha(i,l,12)**slope
                     flux14 = specfac(i,14) * m_alpha(i,l,14)**slope
                     flux15 = specfac(i,15) * m_alpha(i,l,15)**slope
                     flux16 = specfac(i,16) * m_alpha(i,l,16)**slope
                     flux_u(i,l) = densb(i) / slope * (
     $                      specfac(i,1) * m_alpha(i,l,1)**slope
     $                    - specfac(i,9) * m_alpha(i,l,9)**slope
     $                   + cos22 * ( flux2 - flux8 - flux10 + flux16 ) 
     $                   + cos45 * ( flux3 - flux7 - flux11 + flux15 ) 
     $                   + sin22 * ( flux4 - flux6 - flux12 + flux14 ) )
                     flux_v(i,l) =  densb(i) / slope * (
     $                      specfac(i,5)  * m_alpha(i,l,5)**slope
     $                    - specfac(i,13) * m_alpha(i,l,13)**slope
     $                   + cos22 * ( flux4 + flux6 - flux12 - flux14 ) 
     $                   + cos45 * ( flux3 + flux7 - flux11 - flux15 ) 
     $                   + sin22 * ( flux2 + flux8 - flux10 - flux16 ) )
                  end if
               end do
            end do
         end if

      end if
C
C  Calculate drag at intermediate levels using centered differences.
C      
      do l = lev1p, lev2m
         do i = il1, il2
            if( dragil(i,l) ) then
               ddz2 = density(i,l) * ( alt(i,l+1) - alt(i,l-1) )
               drag_u(i,l) = - ( flux_u(i,l+1) - flux_u(i,l-1) ) / ddz2
               drag_v(i,l) = - ( flux_v(i,l+1) - flux_v(i,l-1) ) / ddz2
            end if
         end do
      end do
C
C  Drag at first and last levels using one-side differences.
C 
      do i = il1, il2
         if( dragil(i,lev1) ) then
            ddz = density(i,lev1) * ( alt(i,lev1p) - alt(i,lev1) ) 
            drag_u(i,lev1) = - ( flux_u(i,lev1p) - flux_u(i,lev1) ) /ddz
            drag_v(i,lev1) = - ( flux_v(i,lev1p) - flux_v(i,lev1) ) /ddz
         end if
      end do
      do i = il1, il2
         if( dragil(i,lev2) ) then
            ddz = density(i,lev2) * ( alt(i,lev2) - alt(i,lev2m) )
            drag_u(i,lev2) = - ( flux_u(i,lev2) - flux_u(i,lev2m) ) /ddz
            drag_v(i,lev2) = - ( flux_v(i,lev2) - flux_v(i,lev2m) ) /ddz
         end if
      end do
 
      end subroutine HINES_DRAG

!=======================================================================

      subroutine HINES_HEAT( )
!-----------------------------------------------------------------------
C  This routine calculates the gravity wave induced heating and eddy 
C  diffusion coefficient on a longitude by altitude grid for the Hines 
C  Doppler spread parameterization scheme. The output is placed on the 
C  intermediate levels such that the highest (and lowest) levels for 
C  diffusion and heating are 1/2 grid step above (and below) the highest 
C  (and lowest) momentum levels. This routine can be used for nonzero 
C  minimum cutoff wavenumber (M_MIN) only in the case of spectral SLOPE = 1,
C  in which case M_MIN is not needed since its vertical derivative is zero.
C  Output arguments:
C     * HEAT   = gravity wave heating (K/sec)
C     * DIFFCO = diffusion coefficient (m$2/sec).
C  Input arguements: ALT, M_ALPHA, SPECFAC, DRAGIL, BVFREQ, DENSITY, DENSB
!                    SIGMA_T , VISC_MOL, KSTAR, SLOPE, F2,F3,F5,F6
!-----------------------------------------------------------------------
      use HINES_DIMS
      use HINES_CONTROLS
      use HINES_VARS
      
      implicit none

!-----------------------------------------------------------------------
!	... local variables 
!-----------------------------------------------------------------------
      integer :: i, l, n, l1, lh, lp1
      integer :: lev1, lev2, lev1p, lev2m
      real :: m_sub_m_turb, m_sub_m_mol, m_sub_m, heatng
      real :: visc, sm1, dmdz, malp, sigma, bvf, dens

C  For MAM level indices which increase from top down then levels of
C  diffusion coefficient and heating defined so that half level L=2 is 
C  half way between fulle levels L=1 (the model top)  and L=2, etc. 
C  For other models in which the indices increase from bottom up then 
C  first intermediate half level designated L=1.

      heat(:,:) = 0.
      l1 = 0
      lev1 = levbot
      lev2 = levtop - 1
      if( levbot > levtop ) then
         l1 = 1
         lev1 = levtop
         lev2 = levbot - 1
      end if
C
      if( slope == 1. ) then
         do n = 1, naz
            do l = lev1, lev2
               do i = il1,il2
                  if( dragil(i,l) ) then
                     heat(i,l+l1) = heat(i,l+l1) + specfac(i,n) 
     $                       * ( m_alpha(i,l+1,n) - m_alpha(i,l,n) ) 
     $                         / ( alt(i,l+1) - alt(i,l) )
                  end if
               end do
            end do
         end do
       else if( slope /= 1. ) then
        sm1 = slope - 1.
        do n = 1, naz
           do l = lev1, lev2
              lh  = l + l1
              lp1 = l + 1           
              do i = il1, il2
                 if( dragil(i,l) ) then
                    dmdz = ( m_alpha(i,lp1,n) - m_alpha(i,l,n) ) 
     $                   / ( alt(i,lp1) - alt(i,l) )
                    malp = 0.5 * ( m_alpha(i,lp1,n) + m_alpha(i,l,n) )
                    heat(i,lh) = heat(i,lh) + specfac(i,n) * dmdz 
     $                                                     * malp**sm1
                 end if
              end do
            end do
         end do
      end if
c
C  Avoid round off problems that may result in A**B where A < 0 by ensuring
C  that array HEAT does not change sign (which must be since dm/dz <= 0).
C  Note that sign of summed term HEAT is forced to be positive.
C
      do l = lev1,lev2
        heat(il1:il2,l+l1) = amin1 ( heat(il1:il2,l+l1), 0.0 )
        heat(il1:il2,l+l1) = abs (heat(il1:il2,l+l1))
      end do
C
C  Heating and diffusion.
C
      do l = lev1, lev2
         do i = il1, il2
            if( dragil(i,l) ) then
C
C  Interpolate quantities at half levels.
C
               bvf   = 0.5 * ( bvfreq(i,l)   + bvfreq(i,l+1)   )
               dens  = 0.5 * ( density(i,l)  + density(i,l+1)  )
               sigma = 0.5 * ( sigma_t(i,l)  + sigma_t(i,l+1)  )
               visc  = 0.5 * ( visc_mol(i,l) + visc_mol(i,l+1) )
C
C  For heating allow maximum permissible value of cutoff wavenumber to
C  be the smaller of the instability-induced wavenumber (M_SUB_M_TURB) 
C  and that imposed by molecular viscosity (M_SUB_M_MOL), while for
C  diffusion coefficient ignore molecular viscosity contribution
C  since want the turbulent generated diffusion.
C
               visc = AMAX1( visc, visc_min )
               m_sub_m_turb = bvf / ( f2 * sigma )
               m_sub_m_mol  = ( bvf * kstar / visc )**d13 / f3
c
C  Turbulent diffusion coefficient.
C
               heatng = heat(i,l+l1)
     $                * f5 * bvf / m_sub_m_turb * densb(i) / dens
               diffco(i,l+l1) = f6 * heatng**d13
     $                        / m_sub_m_turb**(1.+d13)
C
C  Heating rate.
C
               m_sub_m = AMIN1( m_sub_m_turb, m_sub_m_mol )
               heatng  = heat(i,l+l1)
     $                 * f5 * bvf / m_sub_m * densb(i) / dens
               heat(i,l+l1) = heatng / cpgas
            end if
         end do
      end do
      
C
C  Calculate diffusion and heating at top level for the case
C  where indices of vertical levels increase from top down.
C
      if( levbot > levtop ) then
         lev1p = levtop + 1
         do i = il1, il2
            if( dragil(i,lev1p) ) then
               diffco(i,levtop) = diffco(i,lev1p) 
               heat(i,levtop)   = heat(i,lev1p) 
            end if
         end do
         return
      end if
C
C  Calculate diffusion and heating at top level for the case
C  where indices of vertical levels increase from bottom up.
C
      if( levbot < levtop ) then
         lev2m = levtop - 1
         do i = il1, il2
            if( dragil(i,lev2m) ) then
               diffco(i,levtop) = diffco(i,lev2m) 
               heat(i,levtop)   = heat(i,lev2m) 
            end if
         end do
      end if

      end subroutine HINES_HEAT

!=======================================================================

      subroutine HINES_SIGMA( drag0, lev )
!-----------------------------------------------------------------------
C  This routine calculates the total rms and azimuthal rms horizontal 
C  velocities at a given level on a longitude by altitude grid for 
C  the Hines' Doppler spread GWD parameterization scheme.
C  NOTE: only 4, 8, 12 or 16 azimuths can be used.
C  Output arguements:
C     * SIGMA_T     = total rms horizontal wind (m/s).
C     * SIGMA_ALPHA = total rms wind in each azimuth (m/s).
C  Input arguments: SIGSQH_ALPHA, DRAG
!-----------------------------------------------------------------------
      use HINES_DIMS
      use HINES_CONTROLS
      use HINES_VARS
      
      implicit none
      
      real, parameter :: C22SQ = 0.8535534, C67SQ = 0.1464466

!-----------------------------------------------------------------------
!	... Dummy arguments
!-----------------------------------------------------------------------
      logical, intent(in) :: drag0(nlons)
      integer, intent(in) :: lev

!-----------------------------------------------------------------------
!	... local variables 
!-----------------------------------------------------------------------
      integer :: I, N
      real :: SUM_2468,   SUM_1357 
      real :: SUM_26812,  SUM_35911,  SUM_1379
      real :: SUM_461012, SUM_24810,  SUM_15711  
      real :: SUM_281016, SUM_371115, SUM_461214, SUM_13911  
      real :: SUM_481216, SUM_571315, SUM_241012, SUM_15913
      real :: SUM_681416, SUM_351113, SUM_261014, SUM_17915

C  Calculate azimuthal rms velocity
C
      if( naz == 4 ) then                  !  Case with 4 azimuths.
         do i = il1, il2
            if( drag0(i) ) then
               sigma_alpha(i,lev,1) = SQRT( sigsqh_alpha(i,lev,1)
     $                                     + sigsqh_alpha(i,lev,3) )
               sigma_alpha(i,lev,2) = SQRT( sigsqh_alpha(i,lev,2)
     $                                     + sigsqh_alpha(i,lev,4) )
               sigma_alpha(i,lev,3) = sigma_alpha(i,lev,1)
               sigma_alpha(i,lev,4) = sigma_alpha(i,lev,2)
            end if
         end do
       else if( naz == 8 ) then            !  Case with 8 azimuths.
         do i = il1, il2
            if( drag0(i) ) then
               sum_1357 = sigsqh_alpha(i,lev,1) + sigsqh_alpha(i,lev,3) 
     $                  + sigsqh_alpha(i,lev,5) + sigsqh_alpha(i,lev,7) 
               sum_2468 = sigsqh_alpha(i,lev,2) + sigsqh_alpha(i,lev,4)
     $                  + sigsqh_alpha(i,lev,6) + sigsqh_alpha(i,lev,8)
               sigma_alpha(i,lev,1) = SQRT( sigsqh_alpha(i,lev,1) 
     $                           + sigsqh_alpha(i,lev,5) + .5*sum_2468 )
               sigma_alpha(i,lev,2) = SQRT( sigsqh_alpha(i,lev,2) 
     $                           + sigsqh_alpha(i,lev,6) + .5*sum_1357 )
               sigma_alpha(i,lev,3) = SQRT( sigsqh_alpha(i,lev,3) 
     $                           + sigsqh_alpha(i,lev,7) + .5*sum_2468 )
               sigma_alpha(i,lev,4) = SQRT( sigsqh_alpha(i,lev,4) 
     $                           + sigsqh_alpha(i,lev,8) + .5*sum_1357 )
               sigma_alpha(i,lev,5) = sigma_alpha(i,lev,1)
               sigma_alpha(i,lev,6) = sigma_alpha(i,lev,2)
               sigma_alpha(i,lev,7) = sigma_alpha(i,lev,3)
               sigma_alpha(i,lev,8) = sigma_alpha(i,lev,4)
            end if
         end do
       else if( naz == 12 ) then            !  Case with 12 azimuths.
         do i = il1, il2
            if( drag0(i) ) then
               sum_26812 = sigsqh_alpha(i,lev,2) + sigsqh_alpha(i,lev,6)
     $                 + sigsqh_alpha(i,lev, 8) + sigsqh_alpha(i,lev,12) 
               sum_35911 = sigsqh_alpha(i,lev,3) + sigsqh_alpha(i,lev,5) 
     $                 + sigsqh_alpha(i,lev, 9) + sigsqh_alpha(i,lev,11) 
               sum_1379  = sigsqh_alpha(i,lev,1) + sigsqh_alpha(i,lev,3) 
     $                 + sigsqh_alpha(i,lev, 7) + sigsqh_alpha(i,lev, 9) 
               sum_461012 = sigsqh_alpha(i,lev,4)+ sigsqh_alpha(i,lev,6) 
     $                 + sigsqh_alpha(i,lev,10) + sigsqh_alpha(i,lev,12) 
               sum_24810 = sigsqh_alpha(i,lev,2) + sigsqh_alpha(i,lev,4) 
     $                 + sigsqh_alpha(i,lev, 8) + sigsqh_alpha(i,lev,10) 
               sum_15711 = sigsqh_alpha(i,lev,1) + sigsqh_alpha(i,lev,5) 
     $                 + sigsqh_alpha(i,lev, 7) + sigsqh_alpha(i,lev,11)
               sigma_alpha(i,lev,1)  = SQRT( sigsqh_alpha(i,lev,1) 
     $                               + sigsqh_alpha(i,lev,7) 
     $                               + 0.75*sum_26812 + 0.25*sum_35911 )
               sigma_alpha(i,lev,2)  = SQRT( sigsqh_alpha(i,lev,2) 
     $                               + sigsqh_alpha(i,lev,8) 
     $                               + 0.75*sum_1379 + 0.25*sum_461012 )
               sigma_alpha(i,lev,3)  = SQRT( sigsqh_alpha(i,lev,3) 
     $                               + sigsqh_alpha(i,lev,9) 
     $                               + 0.75*sum_24810 + 0.25*sum_15711 )
               sigma_alpha(i,lev,4)  = SQRT( sigsqh_alpha(i,lev,4) 
     $                               + sigsqh_alpha(i,lev,10) 
     $                               + 0.75*sum_35911 + 0.25*sum_26812 )
               sigma_alpha(i,lev,5)  = SQRT( sigsqh_alpha(i,lev,5) 
     $                               + sigsqh_alpha(i,lev,11) 
     $                               + 0.75*sum_461012 + 0.25*sum_1379 )
               sigma_alpha(i,lev,6)  = SQRT( sigsqh_alpha(i,lev,6) 
     $                               + sigsqh_alpha(i,lev,12) 
     $                               + 0.75*sum_15711 + 0.25*sum_24810 )
               sigma_alpha(i,lev,7)  = sigma_alpha(i,lev,1)
               sigma_alpha(i,lev,8)  = sigma_alpha(i,lev,2)
               sigma_alpha(i,lev,9)  = sigma_alpha(i,lev,3)
               sigma_alpha(i,lev,10) = sigma_alpha(i,lev,4)
               sigma_alpha(i,lev,11) = sigma_alpha(i,lev,5)
               sigma_alpha(i,lev,12) = sigma_alpha(i,lev,6)
            end if
         end do
       else if( naz == 16 ) then            !  Case with 16 azimuths.
         do i = il1, il2
            if( drag0(i) ) then
               sum_281016 = sigsqh_alpha(i,lev,2)+ sigsqh_alpha(i,lev,8)
     $                 + sigsqh_alpha(i,lev,10) + sigsqh_alpha(i,lev,16) 
               sum_371115 = sigsqh_alpha(i,lev,3)+ sigsqh_alpha(i,lev,7) 
     $                 + sigsqh_alpha(i,lev,11) + sigsqh_alpha(i,lev,15) 
               sum_461214 = sigsqh_alpha(i,lev,4)+ sigsqh_alpha(i,lev,6) 
     $                 + sigsqh_alpha(i,lev,12) + sigsqh_alpha(i,lev,14) 
               sum_13911 = sigsqh_alpha(i,lev,1) + sigsqh_alpha(i,lev,3) 
     $                 + sigsqh_alpha(i,lev, 9) + sigsqh_alpha(i,lev,11) 
               sum_481216 = sigsqh_alpha(i,lev,4)+ sigsqh_alpha(i,lev,8) 
     $                 + sigsqh_alpha(i,lev,12) + sigsqh_alpha(i,lev,16) 
               sum_571315 = sigsqh_alpha(i,lev,5)+ sigsqh_alpha(i,lev,7) 
     $                 + sigsqh_alpha(i,lev,13) + sigsqh_alpha(i,lev,15)
               sum_241012 = sigsqh_alpha(i,lev,2)+ sigsqh_alpha(i,lev,4) 
     $                 + sigsqh_alpha(i,lev,10) + sigsqh_alpha(i,lev,12) 
               sum_15913  = sigsqh_alpha(i,lev,1)+ sigsqh_alpha(i,lev,5) 
     $                 + sigsqh_alpha(i,lev, 9)+ sigsqh_alpha(i,lev,13)
               sum_681416 = sigsqh_alpha(i,lev,6)+ sigsqh_alpha(i,lev,8) 
     $                 + sigsqh_alpha(i,lev,14)+ sigsqh_alpha(i,lev,16)
               sum_351113 = sigsqh_alpha(i,lev,3)+ sigsqh_alpha(i,lev,5) 
     $                 + sigsqh_alpha(i,lev,11)+ sigsqh_alpha(i,lev,13)
               sum_261014 = sigsqh_alpha(i,lev,2)+ sigsqh_alpha(i,lev,6)
     $                 + sigsqh_alpha(i,lev,10)+ sigsqh_alpha(i,lev,14)
               sum_17915  = sigsqh_alpha(i,lev,1)+ sigsqh_alpha(i,lev,7)
     $                 + sigsqh_alpha(i,lev, 9)+ sigsqh_alpha(i,lev,15)
               sigma_alpha(i,lev,1)  = SQRT(  
     $                   sigsqh_alpha(i,lev, 1) + sigsqh_alpha(i,lev, 9) 
     $                 + c22sq * sum_281016     + 0.5 * sum_371115 
     $                 + c67sq * sum_461214 ) 
               sigma_alpha(i,lev,2)  = SQRT(  
     $                   sigsqh_alpha(i,lev, 2) + sigsqh_alpha(i,lev,10) 
     $                 + c22sq * sum_13911      + 0.5 * sum_481216 
     $                 + c67sq * sum_571315 ) 
               sigma_alpha(i,lev,3)  = SQRT(  
     $                   sigsqh_alpha(i,lev, 3) + sigsqh_alpha(i,lev,11) 
     $                 + c22sq * sum_241012     + 0.5 * sum_15913
     $                 + c67sq * sum_681416 ) 
               sigma_alpha(i,lev,4)  = SQRT(  
     $                   sigsqh_alpha(i,lev, 4) + sigsqh_alpha(i,lev,12) 
     $                 + c22sq * sum_351113     + 0.5 * sum_261014
     $                 + c67sq * sum_17915 ) 
               sigma_alpha(i,lev,5)  = SQRT(  
     $                   sigsqh_alpha(i,lev, 5) + sigsqh_alpha(i,lev,13) 
     $                 + c22sq * sum_461214     + 0.5 * sum_371115
     $                 + c67sq * sum_281016 ) 
               sigma_alpha(i,lev,6)  = SQRT(  
     $                   sigsqh_alpha(i,lev, 6) + sigsqh_alpha(i,lev,14) 
     $                 + c22sq * sum_571315     + 0.5 * sum_481216
     $                 + c67sq * sum_13911 ) 
               sigma_alpha(i,lev,7)  = SQRT(  
     $                   sigsqh_alpha(i,lev, 7) + sigsqh_alpha(i,lev,15) 
     $                 + c22sq * sum_681416     + 0.5 * sum_15913
     $                 + c67sq * sum_241012 ) 
               sigma_alpha(i,lev,8)  = SQRT(  
     $                   sigsqh_alpha(i,lev, 8) + sigsqh_alpha(i,lev,16) 
     $                 + c22sq * sum_17915      + 0.5 * sum_261014
     $                 + c67sq * sum_351113 ) 
               sigma_alpha(i,lev,9)  = sigma_alpha(i,lev,1)
               sigma_alpha(i,lev,10) = sigma_alpha(i,lev,2)
               sigma_alpha(i,lev,11) = sigma_alpha(i,lev,3)
               sigma_alpha(i,lev,12) = sigma_alpha(i,lev,4)
               sigma_alpha(i,lev,13) = sigma_alpha(i,lev,5)
               sigma_alpha(i,lev,14) = sigma_alpha(i,lev,6)
               sigma_alpha(i,lev,15) = sigma_alpha(i,lev,7)
               sigma_alpha(i,lev,16) = sigma_alpha(i,lev,8)
            end if
         end do
      end if

      sigma_t(il1:il2,lev) = 0.      !   Initialize rms wind.
C
C  Calculate total rms wind.
C
      do n = 1, naz
         do i = il1, il2
            if( drag0(i) ) then
               sigma_t(i,lev) = sigma_t(i,lev) + sigsqh_alpha(i,lev,n)
            end if
         end do
      end do
      do i = il1, il2
         if( drag0(i) ) then
            sigma_t(i,lev) = SQRT( sigma_t(i,lev) )
         end if   
      end do

      end subroutine HINES_SIGMA

!=======================================================================

      subroutine HINES_INTGRL( drag0, lev )
!-----------------------------------------------------------------------
C  This routine calculates the vertical wavenumber integral
C  for a single vertical level at each azimuth on a longitude grid
C  for the Hines' Doppler spread GWD parameterization scheme.
C  NOTE: (1) only spectral slopes of 1, 1.5 or 2 are permitted.
C        (2) the integral is written in terms of the product QM
C            which by construction is always less than 1. Series
C            solutions are used for small |QM| and analytical solutions
C            for remaining values.
C  Output arguments:
C     * I_ALPHA  = Hines' integral.
C     * DRAG0    = logical flag indicating longitudes where calculations
C     *            to be performed.
C     * DO_ALPHA = logical flag indicating azimuths and longitudes
C     *            where calculations to be performed (for SLOPE=1).
C  Input arguements: V_ALPHA, M_ALPHA, BVFB, DO_ALPHA , M_MIN, SLOPE
!-----------------------------------------------------------------------
      use HINES_DIMS
      use HINES_CONTROLS
      use HINES_VARS
      
      implicit none

!-----------------------------------------------------------------------
!	... Parameters
C     * QMIN = minimum value of Q_ALPHA (avoids indeterminant form of integral)
C     * QM_MIN = minimum value of Q_ALPHA * M_ALPHA (used to avoid numerical
C     *          problems).
!-----------------------------------------------------------------------
      real, parameter :: q_min = 1.0 , qm_min = 0.01
      
!-----------------------------------------------------------------------
!	... Dummy arguments
!-----------------------------------------------------------------------
      integer, intent(in) :: lev
      logical, intent(out) :: drag0(nlons)

!-----------------------------------------------------------------------
!	... local variables 
!-----------------------------------------------------------------------
      integer :: i, n
      real :: q_alpha, qm, qmm, sqrtqm, ival

      drag0(il1:il2) = .false.

      if( slope ==  1. ) then
         do n = 1, naz
            do i = il1, il2
c
C  Calculate integral only in regions where cutoff wavenumber
C  is greater than minimum allowable value, otherwise set integral
C  to zero and turn off flag to calculate this azimuth.
C
               if( m_alpha(i,lev,n) > m_min ) then

                  drag0(i) = .true.
                  q_alpha = v_alpha(i,n) / bvfb(i)
                  qm      = q_alpha * m_alpha(i,lev,n)
                  qmm     = q_alpha * m_min 
c
C  If |QM| is small then use first 4 terms series of Taylor series
C  expansion of integral in order to avoid indeterminate form of integral,
C  otherwise use analytical form of integral.
C
                  if( ABS(q_alpha) < q_min .or. ABS(qm) < qm_min ) then 
                     if( q_alpha  ==  0. ) then
                        ival = 0.5 * ( m_alpha(i,lev,n)**2 - m_min**2 )
                      else
                        ival = ( qm**2/2.  + qm**3/3.  + qm**4/4.  
     $                          + qm**5/5. - qmm**2/2. - qmm**3/3. 
     $                          - qmm**4/4. - qmm**5/5. ) / q_alpha**2
                     end if
                   else
                     ival = - ( ALOG(1.-qm) - ALOG(1.-qmm) + qm - qmm )
     $                    / q_alpha**2
                  end if
                  i_alpha(i,n) = AMAX1( ival, 0. )
                else
                  i_alpha(i,n)  = 0.
                  do_alpha(i,n) = .false.
               end if
            end do
         end do

       else if( slope  ==  2. ) then    !    NOTE: only for M_MIN = 0 !
         do n = 1, naz
            do i = il1, il2
               if( do_alpha(i,n) ) then
                  drag0(i) = .true.
                  q_alpha = v_alpha(i,n) / bvfb(i)
                  qm = q_alpha * m_alpha(i,lev,n)
C
C  If |QM| is small then use first 4 terms series of Taylor series
C  expansion of integral in order to avoid indeterminate form of integral,
C  otherwise use analytical form of integral.
C
                  if( ABS(q_alpha) < q_min .or. ABS(qm) < qm_min ) then
                     if( q_alpha  ==  0. ) then
                        ival = m_alpha(i,lev,n)**3 / 3.
                      else
                        ival = ( qm**3/3. + qm**4/4.  
     $                          + qm**5/5. + qm**6/6. ) / q_alpha**3
                     end if
                   else
                     ival = - ( ALOG(1.-qm) +qm+ .5*qm**2 ) / q_alpha**3
                  end if
                  i_alpha(i,n) = AMAX1( ival, 0. )
                ELSE
                  i_alpha(i,n) = 0.
               end if
            end do
         end do
 
       else if( SLOPE  ==  1.5 ) then   !    NOTE: only for M_MIN = 0 !
         do n = 1, naz
            do i = il1, il2
               if( do_alpha(i,n) ) then
                  drag0(i) = .true.
                  q_alpha = v_alpha(i,n) / bvfb(i)
                  qm = q_alpha * m_alpha(i,lev,n)
C
C  If |QM| is small then use first 4 terms series of Taylor series
C  expansion of integral in order to avoid indeterminate form of integral,
C  otherwise use analytical form of integral.
C
                  if( ABS(q_alpha) < q_min .or. ABS(qm) < qm_min ) then
                     if( q_alpha  ==  0. ) then
                        ival = m_alpha(i,lev,n)**2.5 / 2.5
                      else
                        ival = ( qm/2.5 + qm**2/3.5 + qm**3/4.5 
     $                          + qm**4/5.5 ) * m_alpha(i,lev,n)**1.5 
     $                       / q_alpha
                     end if
                   else
                     qm     = ABS(qm)
                     sqrtqm = SQRT(qm)
                     if( q_alpha  >=  0. ) then
                        ival = ( ALOG( (1.+sqrtqm)/(1.-sqrtqm) )
     $                          -2.*sqrtqm*(1.+qm/3.) ) / q_alpha**2.5
                      else
                        ival = 2. * ( atan(sqrtqm) + sqrtqm*(qm/3.-1.) )
     $                       / ABS(q_alpha)**2.5
                     end if
                  end if
                  i_alpha(i,n) = amax1 ( ival, 0. )
                else
                  i_alpha(i,n) = 0.
               end if
            end do
         end do
      end if

      end subroutine HINES_INTGRL

!=======================================================================

      subroutine HINES_SMOOTH( data, lev1, lev2, coeff, nsmooth )
!-----------------------------------------------------------------------
C  Smooth a longitude by altitude array "data" in the vertical over a
C  specified number of levels using a three point smoother. 
C     * COEFF   = smoothing coefficient for a 1:COEFF:1 stencil.
C     *           (e.g., COEFF = 2 will result in a smoother which
C     *           weights the level L gridpoint by two and the two 
C     *           adjecent levels (L+1 and L-1) by one).
C     * NSMOOTH = number of times to smooth in vertical.
C     *           (e.g., NSMOOTH=1 means smoothed only once, 
C     *           NSMOOTH=2 means smoothing repeated twice, etc.)
!-----------------------------------------------------------------------
      use HINES_DIMS
      use HINES_VARS, only : dragil
      
      implicit none

!-----------------------------------------------------------------------
!	... Dummy arguments
!-----------------------------------------------------------------------
      integer, intent(in) :: lev1, lev2, nsmooth
      real, intent(in)    :: coeff
      real, intent(inout) :: data(nlons,nlevs)

!-----------------------------------------------------------------------
!	... local variables
!-----------------------------------------------------------------------
      integer :: i, l, ns
      real :: sum_wts, work(nlons,nlevs)

      sum_wts = coeff + 2.    ! the sum of weights.
      do ns = 1, nsmooth              ! Smooth NSMOOTH times
         work(il1:il2,lev1:lev2) = data(il1:il2,lev1:lev2)
         do l = lev1+1, lev2-1
            do i = il1, il2
               if( dragil(i,l) ) DATA(I,L) = 
     $          ( work(i,l+1) + coeff*work(i,l) + work(i,l-1) ) /sum_wts
            end do
         end do
      end do

      end subroutine HINES_SMOOTH

!=======================================================================

      subroutine HINES_EXP( data, lev1, lev2 )
!-----------------------------------------------------------------------
C  This routine exponentially damps a longitude by altitude array "data"
C  of data above the specified altitude alt_cutoff
C     * IORDER	= 1 means vertical levels are indexed from top down 
C     *           (i.e., highest level indexed 1 and lowest level nlevs);
C     *            /=  1 highest level is index nlevs.
!-----------------------------------------------------------------------
      use HINES_DIMS
      use HINES_CONTROLS
      use HINES_VARS
      
      implicit none
      
      real, parameter :: HSCALE = 5.E3
      
!-----------------------------------------------------------------------
!	... Dummy arguments
!-----------------------------------------------------------------------
      integer, intent(in) :: lev1, lev2
      real, intent(inout) :: data(nlons,nlevs)

!-----------------------------------------------------------------------
!	... local variables 
!-----------------------------------------------------------------------
      integer :: levbot0, levtop0, lincr, i, l
      real, dimension(nlons) :: data_zmax   ! data values just above altitude alt_cutoff.
C
C  Index of lowest altitude level (bottom of drag calculation).
C
      levbot0 = lev2
      levtop0 = lev1
      lincr  = 1
      if( iorder /= 1  ) then
         levbot0 = lev1
         levtop0 = lev2
         lincr  = -1
      end if
C
C  Data values at first level above alt_cutoff.
C
      do i = il1, il2
         do l = levtop0, levbot0, lincr
            if( alt(i,l)  >=  alt_cutoff ) data_zmax(i) = data(i,l)
         end do
      end do
c
C  Exponentially damp field above alt_cutoff to model top at L=1.
C
      do l = 1, lev2 
        do i = il1, il2
          if( alt(i,l)  >=  alt_cutoff  ) then
            data(i,l) = data_zmax(i) 
     $                * EXP( ( alt_cutoff - alt(i,l) ) / hscale )
          end if
        end do
      end do

      end subroutine HINES_EXP

!=======================================================================

      subroutine HINES_PRNT1( iu_print, iv_print, nmessg,
     $                        ilprt1, ilprt2, levprt1, levprt2 )
!-----------------------------------------------------------------------
C  Print out altitude profiles of various quantities from
C  Hines Doppler spread gravity wave parameterization scheme.
C  (NOTE: only for NAZ = 4, 8 or 12). 
C     * IU_PRINT = 1 to print out values in east-west direction.
C     * IV_PRINT = 1 to print out values in north-south direction.
C     * NMESSG   = unit number for printed output.
C     * ILPRT1   = first longitudinal index to print.
C     * ILPRT2   = last longitudinal index to print.
C     * LEVPRT1  = first altitude level to print.
C     * LEVPRT2  = last altitude level to print.
!-----------------------------------------------------------------------
      use HINES_DIMS
      use HINES_CONTROLS
      use HINES_VARS
      
      implicit none

!-----------------------------------------------------------------------
!	... Dummy arguments
!-----------------------------------------------------------------------
      integer, intent(in) :: iu_print, iv_print, nmessg, ilprt1, ilprt2,
     $                       levprt1, levprt2

!-----------------------------------------------------------------------
!	... local variables 
!-----------------------------------------------------------------------
      integer :: n_east, n_west, n_north, n_south, i, l
C
C  Azimuthal indices of cardinal directions.
C
      N_EAST = 1
      if( NAZ == 4 ) then
        N_NORTH = 2
        N_WEST  = 3       
        N_SOUTH = 4       
      ELSE if( NAZ == 8 ) then
        N_NORTH = 3
        N_WEST  = 5       
        N_SOUTH = 7       
      ELSE if( NAZ == 12 ) then
        N_NORTH = 4
        N_WEST  = 7       
        N_SOUTH = 10       
      ELSE if( NAZ == 16 ) then
        N_NORTH = 5
        N_WEST  = 9       
        N_SOUTH = 13       
      END IF
C
C  Print out values for range of longitudes.
C
      DO 100 I = ILPRT1,ILPRT2
C
C  Print east-west wind, sigmas, cutoff wavenumbers, flux and drag.
C
        if( IU_PRINT == 1 ) then
          WRITE (NMESSG,*) 
          WRITE (NMESSG,6001) I
          WRITE (NMESSG,6005) 
 6001     FORMAT ( 'Hines GW (east-west) at longitude I =',I3)
 6005     FORMAT (15x,' U ',2x,'sig_E',2x,'sig_T',3x,'m_E',
     &            4x,'m_W',4x,'fluxU',5x,'gwdU')
          DO 10 L = LEVPRT1,LEVPRT2
            WRITE (NMESSG,6701) ALT(I,L)/1.E3, VEL_U(I,L),
     &                          SIGMA_ALPHA(I,L,N_EAST), SIGMA_T(I,L),
     &                          M_ALPHA(I,L,N_EAST)*1.E3, 
     &                          M_ALPHA(I,L,N_WEST)*1.E3,
     &                          FLUX_U(I,L)*1.E5, DRAG_U(I,L)*24.*3600.
  10      CONTINUE
 6701     FORMAT (' z=',f7.2,1x,3f7.1,2f7.3,f9.4,f9.3)
        END IF
C
C  Print north-south winds, sigmas, cutoff wavenumbers, flux and drag.
C
        if( IV_PRINT == 1 ) then
          WRITE(NMESSG,*) 
          WRITE(NMESSG,6002) 
 6002     FORMAT ( 'Hines GW (north-south) at longitude I =',I3)
          WRITE(NMESSG,6006) 
 6006     FORMAT (15x,' V ',2x,'sig_N',2x,'sig_T',3x,'m_N',
     &            4x,'m_S',4x,'fluxV',5x,'gwdV')
          DO 20 L = LEVPRT1,LEVPRT2
            WRITE (NMESSG,6701) ALT(I,L)/1.E3, VEL_V(I,L),
     &                          SIGMA_ALPHA(I,L,N_NORTH), SIGMA_T(I,L),
     &                          M_ALPHA(I,L,N_NORTH)*1.E3, 
     &                          M_ALPHA(I,L,N_SOUTH)*1.E3,
     &                          FLUX_V(I,L)*1.E5, DRAG_V(I,L)*24.*3600.
 20       CONTINUE
        END IF
C
 100  CONTINUE

      end subroutine HINES_PRNT1

!=======================================================================

      subroutine HINES_PRNT2( nmessg, ilprt1, ilprt2, levprt1, levprt2 )
!-----------------------------------------------------------------------
C  Print out altitude profiles of cutoff wavenumbers, rms winds and
C  background winds at each horizontal azimuth for the Hines Doppler spread 
C  gravity wave parameterization scheme.
C     * NMESSG   = unit number for printed output.
C     * ILPRT1   = first longitudinal index to print.
C     * ILPRT2   = last longitudinal index to print.
C     * LEVPRT1  = first altitude level to print.
C     * LEVPRT2  = last altitude level to print.
!-----------------------------------------------------------------------
      use HINES_DIMS
      use HINES_CONTROLS
      use HINES_VARS
      
      implicit none
!-----------------------------------------------------------------------
!	... Dummy arguments
!-----------------------------------------------------------------------
      integer, intent(in) :: ilprt1, ilprt2, levprt1, levprt2, nmessg

!-----------------------------------------------------------------------
!	... local variables 
!-----------------------------------------------------------------------
      integer :: IBIG, I, L, N, NAZ1
      PARAMETER  ( IBIG = 50 )
      real :: ZKM, WORK(IBIG)
C-----------------------------------------------------------------------     
      NAZ1 = NAZ
      if( NAZ > 12)  NAZ1 = 12
C
C  Print out values for range of longitudes.
C
      DO 100 I = ILPRT1,ILPRT2
C
C Print cutoff wavenumber at all azimuths.
C
        WRITE (NMESSG,*) 
        WRITE (NMESSG,6001) I
        WRITE (NMESSG,*) 
 6001   FORMAT ('Cutoff wavenumber (X 1.E3) at longitude I =',I3)
        DO 10 L = LEVPRT1,LEVPRT2
          ZKM = ALT(I,L)/1.E3
          DO 5 N = 1,NAZ1
            WORK(N) = M_ALPHA(I,L,N) * 1.E3
  5       CONTINUE
          WRITE (NMESSG,6100) ZKM, (WORK(N),N=1,NAZ1)
 10     CONTINUE
        if( NAZ > 12 ) then
          DO 11 L = LEVPRT1,LEVPRT2
            ZKM = ALT(I,L)/1.E3
            DO 6 N = 13,NAZ
              WORK(N) = M_ALPHA(I,L,N) * 1.E3
  6         CONTINUE
            WRITE (NMESSG,6100) ZKM, (WORK(N),N=13,NAZ)
 11       CONTINUE
        END IF
        WRITE (NMESSG,*) 
 6100   FORMAT (F5.1,'km',12F6.2)
C
C Print rms wind at all azimuths.
C
        WRITE (NMESSG,*) 
        WRITE (NMESSG,6002) I
        WRITE (NMESSG,*) 
 6002   FORMAT ('RMS wind (m/s) at longitude I =',I3)
        DO 20 L = LEVPRT1,LEVPRT2
          ZKM = ALT(I,L)/1.E3
          WRITE (NMESSG,6110) ZKM, (SIGMA_ALPHA(I,L,N),N=1,NAZ1)
 20     CONTINUE
        if( NAZ > 12 ) then
          DO 21 L = LEVPRT1,LEVPRT2
            ZKM = ALT(I,L)/1.E3
            WRITE (NMESSG,6110) ZKM, (SIGMA_ALPHA(I,L,N),N=13,NAZ)
 21       CONTINUE
        END IF
        WRITE (NMESSG,*) 
 6110   FORMAT (F5.1,'km',12F6.1)
C
C Print background wind at all azimuths.
C
        WRITE (NMESSG,*) 
        WRITE (NMESSG,6003) I
        WRITE (NMESSG,*) 
 6003   FORMAT ('Background wind (m/s) at longitude I =',I3)
        DO 30 L = LEVPRT1,LEVPRT2
          ZKM = ALT(I,L)/1.E3
          CALL HINES_WIND ( l )
          WRITE (NMESSG,6110) ZKM, (V_ALPHA(I,N),N=1,NAZ1)
 30     CONTINUE
        if( NAZ > 12 ) then
          DO 31 L = LEVPRT1,LEVPRT2
            ZKM = ALT(I,L)/1.E3
            CALL HINES_WIND( l )
            WRITE (NMESSG,6110) ZKM, (V_ALPHA(I,N),N=13,NAZ)
 31       CONTINUE
        END IF
C
 100  CONTINUE
      
      end subroutine HINES_PRNT2

!=======================================================================
!     Description of working variables:
!     ---------------------------------
!
C     * DRAG_U = zonal component of gravity wave drag (m/s2).
C     * DRAG_V = meridional component of gravity wave drag (m/s2).
C     * HEAT   = gravity wave heating (K/sec).
C     * DIFFCO = diffusion coefficient (m2/sec)
C     * FLUX_U = zonal component of vertical momentum flux (Pascals)
C     * FLUX_V = meridional component of vertical momentum flux (Pascals)
!
C
C     * VEL_U      = background zonal wind component (m/s) (positive is
C     *              eastward).
C     * VEL_V      = background meridional wind component (m/s) (positive
C     *              northward).
C     * BVFREQ     = background Brunt Vassala frequency (radians/sec).
C     * DENSITY    = background density (kg/m3) 
C     * VISC_MOL   = molecular viscosity (m2/s)
C     * ALT        = altitude of momentum, density, buoyancy levels (m)
C     *              (levels must be ordered so ALT(I,LEVTOP) > ALT(I,LEVBOT))
C     * RMS_WIND   = root mean square gravity wave wind at bottom (reference)
C     *              level (m/s).
C     * K_ALPHA    = horizontal wavenumber of each azimuth (1/m).
C     * IHEATCAL   = 1 to calculate heating rates and diffusion coefficient.
C     * IPRNT1     = 1 to print out flux, drag arrays at specified longitudes.
C     * IPRNT2     = 1 to print out azimuthal arrays at specified longitudes.
C     * ICUTOFF    = 1 to exponentially damp drag, heating and diffusion 
C     *              arrays above the altitude ALT_CUTOFF.
C     * ALT_CUTOFF = altitude in meters above which exponential decay applied.
C     * SMCO       = smoothing factor used to smooth cutoff vertical 
C     *              wavenumbers and total rms winds in vertical direction
C     *              before calculating drag or heating
C     *              (SMCO >= 1 ==>  a 1:SMCO:1 three-point stencil is used).
C     * NSMAX      = number of times smoother applied ( >= 1),
C     *            = 0 means no smoothing performed.
C     * KSTAR      = typical gravity wave horizontal wavenumber (1/m).
C     * M_MIN      = minimum allowable cutoff vertical wavenumber, 
C     *              e.g., 1/(3km). This is used only for a spectral slope
C     *              of one ==> for slope of 1.5 or 2 then M_MIN = 0. 
C     * SLOPE      = slope of incident vertical wavenumber spectrum
C     *              (SLOPE must equal 1., 1.5 or 2.).
C     * F1 to F6   = Hines's fudge factors (F4 not needed since used for
C     *              vertical flux of vertical momentum).
C     * NAZ        = number of horizontal azimuths used (4, 8, 12 or 16).
C     * IL1        = first longitudinal index to use (IL1 >= 1).
C     * IL2        = last longitudinal index to use (IL1 <= IL2 <= NLONS).
C     * LEVBOT     = index of bottom (reference) drag level.
C     * LEVTOP     = index of top drag level (if LEVBOT > LEVTOP then the
C     *              vertical indexes increase from top down, otherwise
C     *              indexes increase from bottom up).
C     * NLONS      = number of longitudes. = lmax (nb of latitudes) in SOCRATES
C     * NLEVS      = number of vertical levels.
C     * NAZMTH     = max number of horizontal azimuths = 16
C     * NAZ        = number of horizontal azimuths used (4, 8, 12 or 16).
!
!
C     * M_ALPHA      = cutoff vertical wavenumber at each azimuth (1/m).
C     * V_ALPHA      = background wind component at each azimuth (m/s).
C     *           (note: first azimuth is in eastward direction
C     *            and rotate in counterclockwise direction.)
C     * SIGMA_ALPHA  = total rms wind in each azimuth (m/s).
C     * SIGSQH_ALPHA = portion of wind variance from waves having wave
C     *                normals in the alpha azimuth (m/s).
C     * SIGMA_T      = total rms horizontal wind (m/s).
C     * AK_ALPHA     = spectral amplitude factor at each azimuth 
C     *                (i.e.,{AjKj}) in m4/s2.
C     * SPECFAC      = AK_ALPHA * K_ALPHA.
C     * DO_ALPHA     = logical flag indicating azimuths and longitudes
C     *                where calculations to be performed.
C     * DRAG         = logical flag indicating longitudes where calculations
C     *                to be performed.
C     * DRAGIL       = logical flag indicating longitudes and levels where 
C     *                calculations to be performed.
C     * I_ALPHA      = Hines' integral at a single level.
C     * MMIN_ALPHA   = minimum value of cutoff wavenumber.
C     * DENSB        = background density at reference level.
C     * BVFB         = background buoyancy (Brunt Vassala) frequency at bottom 
C                      reference and work array for ICUTOFF = 1.
C     * UBOT         = background zonal wind component at reference level.
C     * VBOT         = background meridional wind component at reference level.
