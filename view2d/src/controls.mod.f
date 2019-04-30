! subversion Id for THIS file : $Id: controls.mod.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/controls.mod.f $
!-----------------------------------------------------------------------
      module PHYS_CST      
      
         real, parameter :: clight = 2.99792458e8 ! speed of light  (m/s)
         real, parameter :: hPl = 6.626068e-34    ! Planck constant (J*s)
         real, parameter :: k = 1.38065e-23       ! Boltzmann constant (J/K/molec)
         real, parameter :: Nav = 6.022142e23     ! Avogadro constant (molec/mole)
         real, parameter :: R = k * Nav           ! Perfect gas constant (J/K/mole)
         real, parameter :: eV = 1.602177e-19     ! J/eV
         real, parameter :: D12cst = 1.52e18      ! coeff for D12 calc in B&K1972,(15.29)
         real, parameter :: pmb0 = 1013.25     ! p at planet surface (mb)
         real, parameter :: g0 = 9.80665       ! g at planet surface (m/s2)
         real, parameter :: R0 = 6356.7        ! effective planet radius (km)
         real, parameter :: albedo = 0.3       ! Surface albedo of planet
       
         real :: pi, d2r, omega
         
      end module PHYS_CST

!=======================================================================

      module GRID_DIMS           ! model dimensions in lat/alt

         implicit none

         integer, parameter :: lmax = 35
         integer, parameter :: niz = 121

      end module GRID_DIMS

!=======================================================================

      module TIME_CONTROLS

         type TIMING
            integer :: year, month, day, days0
            real ::    cal_day
         end type TIMING

         type ARCH_SETTING
            character(len=5)  :: mode                    ! see archive.f
            type( TIMING )    :: start_time, stop_time
            integer           :: increment               ! in days
         end type ARCH_SETTING

      end module TIME_CONTROLS

!=======================================================================

      module SIM_CONTROLS

      use TIME_CONTROLS

      integer, parameter :: on = 1, off = 0
      integer, parameter :: nextradim = 10
      real, parameter :: missval = -1.e29      ! the missing value

      integer :: ntstep = 0                    ! total simulation time steps
      integer :: np     = 0                    ! time step counter
      real    :: daynum = 0.                   ! elapsed days counter

!-----------------------------------------------------------------------
!        ... Control parameters to set in namelist control_parms
!-----------------------------------------------------------------------
      character(len=5) :: model_type = 'two_d', start_from = 'newnc'
      integer :: ncpus = 1

      type( TIMING ) :: sim_start_time = TIMING( 0, -1, 0, 0, 0. )
      type( TIMING ) :: sim_stop_time  = TIMING( 0, -1, 0, 0, 0. )
      
      logical :: static_sim = .false.
     $        ,  jump_chem = .false.
     
      integer :: daypas = 5          ! TABLES_PHO recalc every "daypas" day
      integer :: dyn_steps = 24      ! *SC Nb of winds-temp iters per day
      integer :: chem_itermax = 12   ! *SC max iters in IMP_SLV (chem solver)

      real :: chemdtm = 30.          ! tstep for chem solver, in minutes
                                     
!  The control switches below are explained in switches.txt and describe_sim.f
      integer, dimension(12) :: 
     $               mainsw = (/ 0, 0, 1, 1, 2, 2, 0, 0, 1, 0, 0, 0 /) ,
     $               mlt_sw = (/ 1, 0, 1, 2, 1, 3, 1, 1, 1, 1, 0, 0 /) ,
     $               het_sw = (/ 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 /) ,
     $               gcc_sw = (/ 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 /) 

!-----------------------------------------------------------------------
!        ... Input/output parameters to set in namelist io_parms
!-----------------------------------------------------------------------
      character(len=256) :: data_dir = './'
      character(len=256) :: arch_dir = './'
      character(len=16) :: label_short = ' '   ! Global attribute for NetCDF archive(s)
      character(len=64) :: label_long = ' '    ! Global attribute for NetCDF archive(s)
      character(len=256) :: rstrt_filespec = ''
      logical :: arch_month_avg = .false.

      end module SIM_CONTROLS

!=======================================================================

      module DIAG_CONTROLS    ! to set through input namelist 'diag_parms'
      
      use SPC_NAMES
      use RXT_NAMES

      logical :: debug = .true.       ! sim stops as soon as error trapped
      logical :: diags = .false.      ! monitoring of all processes (stdout)
      
      integer :: ldiag = 18           ! latitude index to monitor
      integer :: zdiag = 101          ! altitude index to monitor
      
      integer :: idiag = vid_o3       ! index of species to monitor
      integer :: jdiag = rid_jo3_op   ! index of J rate to monitor

      end module DIAG_CONTROLS

!=======================================================================


      module ARCH_WHAT
         
      use TIME_CONTROLS
      use SPECIES_DIMS, only : nbcon
         
      integer, parameter :: max_arch_dates = 3650, 
     $                      max_dvout_dates = 16,
     $                      n_arch_modes = 8,
     $                      max_nldesc = 50
            
      character(len=4)  :: vdims(3) = (/ 'lat ', 'lev ', 'time' /)
      integer :: nldesc                                     ! set in...
      character(len=80), dimension(max_nldesc) :: desc_text !   DESCRIBE_SIM

      type ARCHIVING
         logical :: active, defined
         character(len=5) :: mode
         character(len=256) :: flnm
         integer, dimension(max_arch_dates) :: days0do
         logical, dimension(nbcon) :: vid_qn2da, vid_qn2dv
      end type ARCHIVING
      
      type( ARCHIVING ), dimension(n_arch_modes) :: arch
      
      type DVOUTING
         logical :: active, defined, ascii_def
         character(len=256) :: flnm
         integer :: days0do
         logical, dimension(nbcon) :: vid_qn2dv
      end type DVOUTING
      
      type( DVOUTING ), dimension(max_dvout_dates) :: dvout

      end module ARCH_WHAT

!=======================================================================

      module ELAPSED_TIMES

      use GRID_DIMS, only: lmax

         real, dimension(10) :: realsecs1=0., cpusecs1=0.
         real, dimension(10,lmax) :: realsecs2=0., realsecs3=0.
     $                             , cpusecs2=0., cpusecs3=0.

      end module ELAPSED_TIMES
