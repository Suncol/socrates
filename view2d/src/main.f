! subversion Id for THIS file : $Id: main.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/main.f $
!-----------------------------------------------------------------------
      program SOCRATES

!============================================================================
!    ... SOCRATES : Simulation Of Chemistry, Radiation and Transport
!                      Of Environmentally important Species.
!           An interactive two-dimensional model of the terrestrial atmosphere 
!           (0-120 km) created at the Atmospheric Chemistry Division (NCAR).
!           Project leader : Guy Brasseur.
!============================================================================
!
!     Latest published description : 
!
!   Khosravi, R. and Brasseur, G. and Smith, A. and Rusch, D. and Walters, S. 
!      and Chabrillat, S. and Kockarts, G., Response of the mesosphere to 
!      human perturbations and solar variability calculated by a 2-D model,
!   accepted for publication in JGR, 2002
!                                             
!============================================================================
!
!     some important variables used in the program
!     --------------------------------------------
!
!     t2d        temperature field 
!     q          net heating rate
!     alpha      heat damping coefficient to stabilize temp calculation
!     hm2d       atmosph. density       
!     cf         forcing term (dynamics)
!     u          zonal wind
!     v          meridional wind 
!     w          vertical wind 
!     wu         vertical wind at upper boundary
!     a          rossby wave action
!     xky        total horizontal eddy diffusion coefficient
!     xkz        total vertical eddy diffusion coefficient
!     qn2noon    mixing ratios at noon
!     qn2da      diurnally averaged mixing ratios 
!  
!-----------------------------------------------------------------------
!
!     Definition of several parameters used in the program.
!     some of these parameters have to be specified by the user
!     in the input namelists according to the calculation to be performed.
!
!     daynum     current time (in days)
!     dt, dts    basic timestep (i.e. one day, NOT NEGOCIABLE) 
!     dyn_steps  temperature and winds are iterated dyn_steps times per day
!     mainsw, mlt_sw, het_sw, gcc_sw
!                Simulation Switches - see describe_sim.f and switches.txt
!     chemdtm    timestep used to solve the chemistry (minutes)
!
!-----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use SPECIES_DIMS, only : nbcon
      use SPC_NAMES
      use SPECIES, only : qn2da, qn2noon
      use PHYS_CST
      use BACKATM, only : Dmsis, Tmsis, tot, maxh2ovap
      use PHO_PARMS, only : mxwvn
      use PHO_AERO, only : aeabs, aerabs, aedat1, aerom
      use ALLCO, only : phi, phir, zkm, dlat, dlatr
      use ZGRID, only : pmb
      use CONC, only : ro_s, hm2d
      use TAU0, only : q, qcorr
      use VEN1, only : t2d
      use VEN3, only : ff, fl
      use VEN6, only : v, w
      use VEN8, only : p, bv
      use TROPOPAUSE, only : izmMax
      use TRELAX, only : trx
      use ALP, only : alpha
      use TIME_CONTROLS
      use SIM_CONTROLS
      use DIAG_CONTROLS
      use ARCH_WHAT, only : arch, desc_text
      use TRANSFORM, only : VDERIV, SMOOTHV, SMOOTHL
      use DATE_UTILS, only : TIME2DATE

      implicit none

!-----------------------------------------------------------------------
!    ... Parameters
!-----------------------------------------------------------------------
      real, parameter :: dt = 1. 
      real, parameter :: dts = 86400. 
      real, parameter :: ro0 = 100. * pmb0 / ( g0 * 7.e3 ) ! mass density at
                                                           !  surface (kg m-3)
!-----------------------------------------------------------------------
!    ... Local variables and default settings
!-----------------------------------------------------------------------
      integer  ::  i, indlat, ios, iz, l, dyn_step
      integer  ::  tstep, ilon, n, lat
      integer  ::  nf(niz)
      real     ::  daynum2, dtwave, dtwave_s, oldval
      real     ::  phid, ac, timer, start
      real, dimension(niz) ::   t2pt, pt_relax
      real, dimension(lmax) ::  lbpt, ubpt       ! *SC boundary conditions for...
      real, dimension(lmax,niz) :: pt            ! potential temperature
      real, dimension(11) :: cpusecs = 0.
      character(len=80) :: filenm
      logical ::   not_2d

!-----------------------------------------------------------------------
!    ... Simulation and I/O timing controls
!-----------------------------------------------------------------------
      type( TIMING )     ::  sim_time, phys_time
      type( ARCH_SETTING ) ::  arch_set(99)  ! set in input namelist "io_parms"

!-----------------------------------------------------------------------
!         ... Declare the namelist inputs
!-----------------------------------------------------------------------
      namelist / control_parms / 
     $           sim_start_time, sim_stop_time, model_type, start_from,
     $           dyn_steps, mainsw, mlt_sw, het_sw, gcc_sw, static_sim,
     $           jump_chem, chem_itermax, chemdtm, ncpus, daypas

      namelist / io_parms / 
     $           rstrt_filespec, data_dir, label_short, label_long,
     $           arch_dir, arch_set, arch_month_avg

      namelist / diag_parms / 
     $           debug, diags, ldiag, zdiag, idiag, jdiag
     
!-----------------------------------------------------------------------
!    ... Function declarations
!-----------------------------------------------------------------------
      real, external    :: SECOND

!=====================================================================
!    ... Part 0: Initialization
!======================================================================
      start = SECOND()
      pi = 4. * ATAN(1.)
      d2r = pi / 180.
      omega = 2. * pi / 86400.
      nf = 8

      rstrt_filespec = ' '                       ! assume not restarting
      arch_set(:)%increment = -1                 ! assume no archiving

!-----------------------------------------------------------------------
!         ... Read & check the simulation control & I/O parameters
!-----------------------------------------------------------------------
      read(*,control_parms)
      read(*,io_parms)
      read(*,diag_parms)
      call CHECK_CTRLS()

      sim_time = sim_start_time
      phys_time = sim_time
      daynum = MOD( phys_time%cal_day + 193., 365. ) 
      dtwave = dt / REAL(dyn_steps)    ! timestep for wave model (days)
      dtwave_s = dtwave*86400.         ! timestep for wave model (seconds)
      
!-----------------------------------------------------------------------
!         ... Set the archive times and groups: var arch, module ARCH_WHAT
!-----------------------------------------------------------------------
      call SET_ARCH( arch_set )

!-----------------------------------------------------------------------
!         ... Set the description of the simulation & print it
!-----------------------------------------------------------------------
      call DESCRIBE_SIM( desc_text )

!-----------------------------------------------------------------------
!         ... Read simulation control file for "preprocessed.f" code
!-----------------------------------------------------------------------
      call SOC_INTI()

!-----------------------------------------------------------------------
!         ... Define pressure vert axis from log-p alt levels
!-----------------------------------------------------------------------
      zkm(:) = (/ (REAL(iz), iz=0, niz-1) /) ! log-p alt, module ALLCO
      pmb(:) = pmb0 * EXP( -zkm(:) / 7. )    ! p in mb, module ZGRID
      p(:) = 100. * pmb(:)                   ! p in Pascals, module VEN8
      t2pt(:) = (pmb0/pmb(:))**.285
      ro_s(:) = ro0 * EXP( -zkm(:) / 7. )    ! tot mass dens(kg/m3), module CONC

!-----------------------------------------------------------------------
!         ... Define latitudes in degrees and radians
!-----------------------------------------------------------------------
      dlatr = dlat * d2r
      phi(:) = (/ ( dlat * REAL(lat) - 90., lat=1, lmax) /)
      phir(:) = phi(:) * d2r
      phir(18) = 0.0002 * d2r
      ff(:) = 2. * omega * SIN( phir(:) )
      fl(:) = 2. * omega * COS( phir(:) )
      
!-----------------------------------------------------------------------
!    ... Read in the initial values NetCDF file
!-----------------------------------------------------------------------
      if( start_from == 'ascii' ) then
         call INIT_FROM_ASCII( )
       else if( start_from == 'newnc' ) then
         filenm = rstrt_filespec
         call READ_SAVE( filenm, sim_time )
       else
         stop 'SOCRATES: start_from must be set to ascii or newnc'
      end if

      if( diags ) then
         write(*,'(2(a,i3),a,f8.3,a,es12.3)')' SOCRATES has read, @ (',
     $     ldiag,',',zdiag,') : t2d= ',t2d(ldiag,zdiag),
     $     ' ; qn2noon(HF)= ',qn2noon(ldiag,zdiag,vid_hf)
      end if

!-----------------------------------------------------------------------
!    ... Other initializations - WATCH OUT the order of these calls !
!-----------------------------------------------------------------------
      call PHO_INIT( )
      call SOLCYCLES( phys_time )
      call MSIS_HWM_PLUG( -99., phys_time )
      call ATMCOND( -9, sim_time )
      bv(:,:) = SQRT( 7.5e-3 * g0 / t2d(:,:) )
      call H2O_MAX( Tmsis, maxh2ovap )
      call MLYNCZAK93_PHO_INIT( )
      call BOUNDY( phys_time, t2d(:,1), qn2noon )
      if( mainsw(8) == 1 ) call QBO( daynum - 1. )

!-----------------------------------------------------------------------
!         ... pt_relax=rate of relaxation of T to climatology (see qcorr)
!-----------------------------------------------------------------------
      if( mainsw(1) == 0 ) then
         pt_relax = 0.
       else if( mainsw(1) == 1 ) then
         pt_relax(1:21) = 1. / 86400.
         do iz = 22, 50
            pt_relax(iz) = ( 1. - REAL(iz-21)/30. ) / 86400.
         end do
         pt_relax(51:niz) = 0.
       else
         pt_relax(:) = 1. / 86400.
      end if

!-----------------------------------------------------------------------
!    ... Calc Newtonian cooling coefficient alpha and solar heating
!           efficiencies efsho2 and efsho3
!-----------------------------------------------------------------------
      call HEAT_COEFF( )

!-----------------------------------------------------------------------
!    ... Specify the tropo eddy diffusion coeffs; they are time-indep
!-----------------------------------------------------------------------
      call TROPK( )

      not_2d = model_type == 'one_d' .or. model_type == 'zerod'
      if( not_2d ) then
         if( ldiag /= 1 ) then
            qn2noon(1:ldiag-1,:,:) = missval
            qn2da(1:ldiag-1,:,:) = missval
         end if
         if( ldiag /= lmax ) then
            qn2noon(ldiag+1:lmax,:,:) = missval
            qn2da(ldiag+1:lmax,:,:) = missval
         end if
      end if

!-----------------------------------------------------------------------
!         ... Setup the archive file(s) and, if asked for, archive initvals
!-----------------------------------------------------------------------
      if( ANY(arch(:)%active) ) call ARCH_PLUG( sim_time  )

!      call CHECK_TOTAL_MOLEC( 'after init', sim_time, vid_x )

      debug = debug .or. diags
      if( diags ) then
         write(*,'(2(a,i3),a,f8.3,a,es12.3)') ' SOCRATES begins @ (',
     $     ldiag,',',zdiag,') : t2d= ',t2d(ldiag,zdiag),
     $     ' ; qn2noon(HF)= ',qn2noon(ldiag,zdiag,vid_hf)
      end if
      if( diags .and. ntstep > 1000 ) then
         write(*,*) 'MAIN, warning: simulation duration > 1000 days ',
     $                   '-> diagnostics turned off'
         diags = .false.
      end if
      cpusecs(1) = SECOND() - start

!=======================================================================
!    ... Time integration loop starts here
!=======================================================================
50    timer = SECOND()

!-----------------------------------------------------------------------
!    ... Get time-dep solar radio flux f107 and solar spectrum fbeamr 
!           from time-avg solflux. Takes in account SIN fit of 11-year 
!           solcycle and earth-sun distance factor
!-----------------------------------------------------------------------
      call SOLCYCLES( phys_time )

!-----------------------------------------------------------------------
!    ... Get offline values for temperature, major species densities,
!           and offline wind by semi-empirical models MSIS and HWM
!-----------------------------------------------------------------------
      call MSIS_HWM_PLUG( -99., phys_time )
      do l = 1, lmax 
         qcorr(l,:) = pt_relax(:) * Tmsis(l,:)
      end do

!-----------------------------------------------------------------------
!         ... Calculate the new atmospheric density by hydrostatic
!        adjustment, the geometric altitude grid and the corresponding
!        atmospheric scale height
!-----------------------------------------------------------------------
      call ATMCOND( -1, sim_time )
      cpusecs(2) = cpusecs(2) + SECOND() - timer

      if( not_2d ) then
         t2d = Tmsis
         hm2d = Dmsis(:,:,tot)
         goto 300
      end if

      if( model_type == 'tests' ) then
         call TEST_SUN_UTILS( phys_time )
         goto 400
      end if

!======================================================================
!         ... Part 1: diabatic heating: calc the total heating rate q (K/s)
!======================================================================
      timer = SECOND()
      call TOTAL_HEAT_RATE( daynum, q )
      cpusecs(4) = cpusecs(4) + SECOND() - timer
      
      write(*,'(a)')'--------------------------------------------------'
      write(*,'(3(a,i3),''/'',i2,''/'',i4,2(a,f8.3),2(a,es10.3))')
     $       'MAIN  @ (',ldiag,',',zdiag,') : date= ',sim_time%month, 
     $       sim_time%day,sim_time%year,' ; t2d= ',t2d(ldiag,zdiag),
     $         ' ; q= ',q(ldiag,zdiag)
         
!======================================================================
!         ... Part 2 :  dynamics - executed dyn_steps times per day
!======================================================================
      daynum2 = daynum

      do dyn_step = 1, dyn_steps  
         
!-----------------------------------------------------------------------
!         ... LBC for temperature at surface and 1km
!-----------------------------------------------------------------------
         t2d(:,1:2) = Tmsis(:,1:2)

!-----------------------------------------------------------------------
!     	... Calculate the dynamics of the atmosphere 
!-----------------------------------------------------------------------
         timer = SECOND()
         call DYN_MAIN( phys_time, daynum2, dyn_step, dtwave )
         cpusecs(5) = cpusecs(5) + SECOND() - timer

!=======================================================================
!    ... Calculation of temperature field
!=======================================================================
!-----------------------------------------------------------------------
!         ... Artifical damping w/relax time 1/alpha, and (if mainsw(1)>0) 
!             relaxation to ref (MSIS) temperature through qcorr: 
!             done to calc T only, not dynamics
!-----------------------------------------------------------------------
200      timer = SECOND()
         do l = 1,lmax
            q(l,:) = q(l,:) + alpha(:) * t2d(l,:) + qcorr(l,:)
         end do

!-----------------------------------------------------------------------
!         ... Calculate potential temperature pt, adjust q to pot temp
!             Prepare boundary conds on pot temp
!-----------------------------------------------------------------------
         do l = 1,lmax
            pt(l,:) = t2d(l,:) * t2pt(:)
            q(l,:) = q(l,:) * t2pt(:)
         end do
         lbpt(:) = Tmsis(:,3) * t2pt(3)
         ubpt(:) = Tmsis(:,niz) * t2pt(niz)

!-----------------------------------------------------------------------
!         ... Solve thermodynamic equation
!-----------------------------------------------------------------------
         call POTTEMP( dtwave_s, lbpt, ubpt, pt, q, pt_relax )

!-----------------------------------------------------------------------
!         ... Back to classic temperature, remove damping & relaxation
!-----------------------------------------------------------------------
         do l = 1,lmax
            t2d(l,:) = pt(l,:) / t2pt(:)
            q(l,:) = q(l,:) / t2pt(:)
            q(l,:) = q(l,:) - alpha(:) * t2d(l,:) - qcorr(l,:)
         end do
         call CHECK_VALS( t2d, 80., 600., 't2d', dyn_step, sim_time,
     $                    't2pt->t2d conversion (MAIN)' )

!-----------------------------------------------------------------------
!         ... Smooth temperature in meridional direction with shapiro filter
!           (nf = order of the filter)
!-----------------------------------------------------------------------
         call SHAP( t2d, 2, 120, nf )

!-----------------------------------------------------------------------
!         ... Update hm2d, zgeo and Hair
!-----------------------------------------------------------------------
         call ATMCOND( dyn_step, sim_time )
         call H2O_MAX( Tmsis, maxh2ovap )

!-----------------------------------------------------------------------
!     The QBO perturbations in temperature and zonal wind are updated.
!-----------------------------------------------------------------------
!         if( mainsw(8) == on .and. daynum2 > 1. ) then
!            call QBO( daynum2 )
!         end if
!         cpusecs(6) = cpusecs(6) + SECOND() - timer

!-----------------------------------------------------------------------
!         ... Increment dynamical timesteps
!-----------------------------------------------------------------------
         daynum2 = daynum2 + dtwave
      
      end do                           ! dyn_step = 1,dyn_steps loop

!======================================================================
!    ... Part 3: chemistry
!======================================================================
!         ... Express winds in cm/sec
!-----------------------------------------------------------------------
      v(:,:) = v(:,:) * 100.
      w(:,:) = w(:,:) * 100.

!-----------------------------------------------------------------------
!         ... Calculate boundary conditions
!-----------------------------------------------------------------------
300   continue
      timer = SECOND()
      call BOUNDY( phys_time, t2d(:,1), qn2noon )
      cpusecs(7) = cpusecs(7) + SECOND() - timer

!-----------------------------------------------------------------------
!         ... Calc chemical composition of atmosphere: Solve advection, 
!     chemistry, vert diffusion, & hor. diffusion. OPENMP PARALLELIZED.
!-----------------------------------------------------------------------
      timer = SECOND()
      call CHEM( phys_time )
      cpusecs(8) = cpusecs(8) + SECOND() - timer

!-----------------------------------------------------------------------
!     ... Increment time
!-----------------------------------------------------------------------
      timer = SECOND()
400   continue
      sim_time%days0 = sim_time%days0 + NINT( dt )
      call TIME2DATE( sim_time )
      if( .not. static_sim ) then
         phys_time = sim_time
         daynum = daynum + dt
      end if

!-----------------------------------------------------------------------
!         ... Re-express winds in m/sec.
!-----------------------------------------------------------------------
      v(:,:) = 1.e-2 * v(:,:)
      w(:,:) = 1.e-2 * w(:,:)

!-----------------------------------------------------------------------
!    ... Calculate monthly averages and check for archive output
!-----------------------------------------------------------------------
      call MONTHLY_AVG( sim_time )
      if( ANY(arch(:)%active) ) call ARCH_PLUG( sim_time )

!-----------------------------------------------------------------------
!    ... Special outputs
!-----------------------------------------------------------------------
!      call CHECK_TOTAL_MOLEC( 'MAIN ends', sim_time, vid_x )
      cpusecs(9) = cpusecs(9) + SECOND() - timer

!-----------------------------------------------------------------------
!         ... Test to stop or continue the integration
!-----------------------------------------------------------------------
      if( sim_time%days0 < sim_stop_time%days0 ) then
         go to 50
      else
         call GLTIME( start, cpusecs )  ! printout lifetimes & subroutine timings
         write(*,*) 'SOCRATES model: simulation ran successfully'
      end if

      end program SOCRATES
