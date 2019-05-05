! subversion Id for THIS file : $Id: dyn_main.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/dyn_main.f $
!-----------------------------------------------------------------------
      subroutine DYN_MAIN( dyn_time, daynum, dyn_step, dtwave )
!-----------------------------------------------------------------------
!     	... Calculate the dynamics of the atmosphere 
!-----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use SIM_CONTROLS, only : mainsw, mlt_sw
      use DIAG_CONTROLS, only : debug, diags, ldiag,zdiag
      use VEN1, only  : t2d
      use VEN2, only : u
      use VEN3, only : uz, uzz, ul, tl, tz, tzz
      use VEN5, only : fx, f_tropic
      use VEN9, only : xky, xkyl, xkz, xkzz
      use VEN12, only : dyytrop, dzztrop                      ! set in tropk.f
      use TIDE1, only : dkt, ftx
      use ROSS, only : fr
      use TROPOPAUSE, only : izmMax
      use ARCH_WHAT, only : arch
      use TIME_CONTROLS, only : TIMING
      use TRANSFORM, only : SMOOTHV, SMOOTHL, VDERIV, HDERIV

      implicit none

!-----------------------------------------------------------------------
!  	... Parameters
!-----------------------------------------------------------------------
      real, parameter :: dzzMin = 0.4   ! tried 0.25, 0.05, 0.01 m2/s; the minimum kzz

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      type( TIMING), intent(in) :: dyn_time
      real, intent(in)          :: daynum
      integer, intent(in)       :: dyn_step
      real, intent(in)          :: dtwave    ! timestep for wave model (days)

!-----------------------------------------------------------------------
!     	... Local variables
!-----------------------------------------------------------------------
      integer :: iz, lat
      real :: slt_dyn, dyn_day
      real, dimension(lmax,niz) :: dzzgw, dyypw, junk
      logical :: ok
      logical, save :: entered = .false.

      if( .not. entered ) then
!-----------------------------------------------------------------------
!         ... Tidal wave breaking effect, time-independent
!-----------------------------------------------------------------------
         if( mlt_sw(2) == 1 ) then
            call DYNF_TIDE( )
          else
             dkt = 0.
             ftx = 0.
         end if
         entered = .true.
      end if

!-----------------------------------------------------------------------
!     	... get derivatives of temperature
!-----------------------------------------------------------------------
      call VDERIV( t2d, basedz = tz, basedzz = tzz )
      call HDERIV( t2d, basedl = tl )

!-----------------------------------------------------------------------
!  	... Calculate BV frequency - notice tz is floored to -6.5e-3
!-----------------------------------------------------------------------
      call DYN_BVFREQ( ok )
      if( .not. ok ) then
         write(*,*) 'DYN_MAIN: error in DYN_BVFREQ, dyn_step= ',dyn_step
         write(*,*) '    archiving and aborting...'
         arch(2)%active = .true.           ! arch(2)%mode = 'heat '
         arch(:)%days0do(1) = dyn_time%days0
         call ARCH_PLUG( dyn_time )
         stop 'DYN_MAIN: error in DYN_BVFREQ, see archive *.heat.nc'
      end if

!-----------------------------------------------------------------------
!     ... Set Upper boundary conds xu on stramfct and wu on vert wind w
!-----------------------------------------------------------------------
      call DYN_WUP( )

!-----------------------------------------------------------------------
!     	... Zonal wind
!-----------------------------------------------------------------------
      call DYN_U( ok )
      if( .not. ok .and. debug ) then
         write(*,*) 'DYN_MAIN: error in DYN_U, dyn_step= ',dyn_step
         write(*,*) '    archiving and aborting...'
         arch(2)%active = .true.           ! arch(2)%mode = 'heat '
         arch(:)%days0do(1) = dyn_time%days0
         call ARCH_PLUG( dyn_time )
         stop 'DYN_MAIN: error in DYN_U, see archive *.heat.nc'
       end if
      call VDERIV( u, basedz = uz, basedzz = uzz )
      call HDERIV( u, basedl = ul )

!-----------------------------------------------------------------------
!     	... Gravity wave parameterizations - calc dyn forcing fx and
!           vert eddy diff coeff by GW, dzzgw
!-----------------------------------------------------------------------         
      if( mainsw(4) == -1 ) then
         call HINES97_GWAVE_DYN( dzzgw, junk )
       else if( mainsw(4) == 0 ) then
         call KZZ_SPEC( dyn_time%cal_day, dzzgw )
       else if( mainsw(4) == 1 ) then
         call ARBITRARY_GW_DYN( dyn_time%cal_day, fx )
         call HINES97_GWAVE_DYN( dzzgw, junk )
       else  if( mainsw(4) == 2 ) then
         call ASMITH_GWAVE_DYN( dzzgw, fx )
       else if( mainsw(4) >= 3 ) then
         call HINES97_GWAVE_DYN( dzzgw, fx )
      end if

!-----------------------------------------------------------------------
! ... tropical wave parameterizations for QBO - calc dyn forcing f_tropic
!-----------------------------------------------------------------------
      if( mainsw(8) == 3 ) then
         call dynf_qbo( )
      elseif ( mainsw(8) == 4) then
          call dynf_qbo2( daynum )
      else if( mainsw(8) == 1 .or. mainsw(8) == 2) then
         call qbo( daynum )
      end if

!-----------------------------------------------------------------------
!     	... Coupled wave model (Wanli Wu, March 1994) for Planetary
!           (Rossby) Wave drag and Kyy calculation
!-----------------------------------------------------------------------
      slt_dyn = REAL( dyn_step ) * 24. * dtwave
      dyn_day = dyn_time%cal_day + slt_dyn / 24.
      if( mainsw(5) == 2 ) then
         call DYNF_PWAVE( fr, dyypw, dyn_day, dtwave )
       else if( mainsw(5) == 0 ) then
         call KYY_SPEC( dyn_time%cal_day, xky )
      end if

!-----------------------------------------------------------------------
!	...  Add all contribution to xky, smooth total and get derivative
!-----------------------------------------------------------------------
      xky = dyytrop + dyypw
      call SMOOTHV( xky, 1, 25, 5 )
      call SMOOTHL( xky, 1, 25, 5 )
      call SMOOTHV( xky, 1, niz, 1 )
      call SMOOTHL( xky, 1, niz, 1 )
      call HDERIV( xky, basedl = xkyl )

!-----------------------------------------------------------------------
!	... ATotal vert eddy diff coeff. Smooth & compute derivative
!-----------------------------------------------------------------------
      xkz = dzztrop + dzzgw + dkt
      xkz = MAX( xkz, dzzMin )
      call SMOOTHV( xkz, 1, izmMax+2, 3 )
      call SMOOTHL( xkz, 1, izmMax, 1 )
      call SMOOTHV( xkz, izmMax, niz, 10 )
      call SMOOTHL( xkz, izmMax, niz, 15 )
      call VDERIV( xkz, basedz = xkzz )
    
!-----------------------------------------------------------------------
!	... Calculate diffusion of heat
!-----------------------------------------------------------------------
      call DYN_DIFFUS_HEAT( )

!-----------------------------------------------------------------------
!     	... Forcing terms for streamfunction equation
!-----------------------------------------------------------------------
      call DYNF_TOTAL( daynum )

!-----------------------------------------------------------------------
!         ... Coefficients for stream-function equation
!-----------------------------------------------------------------------
      call DYN_ADAM( )
   
!-----------------------------------------------------------------------
!         ... Calculate streamfunction and meridional wind components
!-----------------------------------------------------------------------
      call DYN_SOLVE( dyn_time%month )

!-----------------------------------------------------------------------
!    ... Correction for global mass balance for w
!-----------------------------------------------------------------------
      call DYN_BALANCE( )

      if( diags ) then 
         write(*,'(4(a,i4))')  'DYN_MAIN: ',dyn_time%month,
     $      '/',dyn_time%day,'/',dyn_time%year,' ; dyn_step= ',dyn_step
         write(*,'(4(a,es12.3))') '   ftx= ',ftx(ldiag,zdiag),
     $                     ' ; dkt= ',dkt(ldiag,zdiag)
         write(*,'(4(a,es12.3))') '   u= ',u(ldiag,zdiag),
     $      ' ; uz= ',uz(ldiag,zdiag),' ; uzz= ',uzz(ldiag,zdiag)
      end if

      end subroutine DYN_MAIN
      

