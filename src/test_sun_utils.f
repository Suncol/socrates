! subversion Id for THIS file : $Id: test_sun_utils.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/test_sun_utils.f $
!-----------------------------------------------------------------------

      subroutine TEST_SUN_UTILS( date )
!-----------------------------------------------------------------------
!         ... SOCRATES v7s02     ;     simonc@oma.be, february 2002
!-----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use PHYS_CST, only : d2r
      use ALLCO, only : phi
      use ZGRID, only : Hair, zgeo
      use CHEM_TIMES, only : ntchem, chemdth, chemdts       ! output
      use SIM_CONTROLS, only : chemdtm, missval
      use DIAG_CONTROLS, only : diags
      use ARCH_WHAT, only : dvout, n_arch_modes, arch
      use TIME_CONTROLS, only : TIMING
      use SUN_UTILS, only : SUN_DAYLENGTH, SUN_LIGHT, SUN_CHAPMAN, 
     $                      SUN_COSZA

      implicit none

!-----------------------------------------------------------------------
!    ... Dummy args
!-----------------------------------------------------------------------
      type( TIMING ), intent(in) :: date

!-----------------------------------------------------------------------
!    ... Local variables
!-----------------------------------------------------------------------
      integer :: lat, it, iz, ier, j
      real :: calday, phid, slt, cosza
      real, dimension(lmax,niz) :: td, sza, scsza
      logical, save :: entered = .false.

!-----------------------------------------------------------------------
!         ... Function declarations
!-----------------------------------------------------------------------
      real, external :: SECOND

      if( .not. entered ) then
         if( MOD( 60.*12., chemdtm ) /= 0. ) then
            write(*,*) 'TEST_SUN_UTILS: error setting chemdtm= ',chemdtm
            write(*,*) '  chemdtm must fit in 12hr'
            stop 'TEST_SUN_UTILS: chemdtm must fit in 12hr'
         end if
         chemdts = chemdtm * 60.
         chemdth = chemdts / 3600.
         ntchem = 24. / chemdth
         entered = .true.
      end if

!-----------------------------------------------------------------------
!         ... Initializations - begins at NOON (slt0=12.),
!             will end at noon of next day (slt=36.)
!-----------------------------------------------------------------------
      calday = date%cal_day
      slt = 0.
      
      do it = 1, ntchem
         slt = slt + chemdth
         if( diags ) write(*,*) 'TEST_SUN_UTILS, slt= ',slt

         do lat = 1, lmax
            phid  = phi(lat)

            do iz = 1, niz
               if( it == 1 )
     $            td(lat,iz) = SUN_DAYLENGTH(zgeo(lat,iz), phid, calday)
               cosza = SUN_COSZA( phid, calday, slt )
               sza(lat,iz) = ACOS( cosza ) / d2r
               if( SUN_LIGHT( phid, zgeo(lat,iz), calday, slt ) ) then
                  scsza(lat,iz) = SUN_CHAPMAN(
     $                          zgeo(lat,iz), Hair(lat,iz), cosza, ier )
                  if(ier/=0)write(*,*) 'TEST_SUN_UTILS: SUN_CHAPMAN '//
     $              'failed with ier= ',ier,' at lat= ',lat,' ; iz= ',iz
                else
                  scsza(lat,iz) = missval
               end if
            end do

         end do
      
!-----------------------------------------------------------------------
!    ... Archive diurnal cycles, depends on "dvout"
!-----------------------------------------------------------------------
         if( ANY( dvout(:)%active ) .and. 
     $       ANY( dvout(:)%days0do == date%days0 ) ) then
            call TEST_SUN_DVARCH(dvout, date, slt, td, sza, scsza)
         end if     

      end do
      
!-----------------------------------------------------------------------
!    ... Archive daylength at each archiving date
!-----------------------------------------------------------------------
      do j = 1, n_arch_modes
         if( arch(j)%active .and. 
     $                    ANY( arch(j)%days0do(:) == date%days0 ) ) then
            write(*,'(a,i2,a,i2,a,i4)') arch(j)%mode // ' archive to ' 
     $        // TRIM( ADJUSTL( arch(j)%flnm ) ) // ' , date= ',
     $        date%month,'/',date%day,'/',date%year
               call ARCH_DAYLENGTH( arch(j), date, td )
         end if
      end do
      
      end subroutine TEST_SUN_UTILS

!=======================================================================

      subroutine TEST_SUN_DVARCH(dvout, date, slt, td, sza, scsza)
!-----------------------------------------------------------------------  
!	... Write diurnal variations netCDF file. 
!-----------------------------------------------------------------------  
      use GRID_DIMS, only : lmax, niz
      use SPECIES_DIMS, only : nbcon
      use TRACNM, only : solsym
      use SPECIES
      use TIME_CONTROLS, only : TIMING
      use ARCH_WHAT, only : max_dvout_dates, DVOUTING
      use NETCDF_UTILS

      implicit none

!-----------------------------------------------------------------------  
!	... Dummy args
!-----------------------------------------------------------------------  
      type( DVOUTING ),dimension(max_dvout_dates),intent(inout) :: dvout
      type( TIMING), intent(in)  ::  date
      real, intent(in) :: slt
      real, dimension(lmax,niz), intent(in) :: td, sza, scsza

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------  
      integer :: i, ic, idv, j, ju, slen
      real    :: my_slt, tsdiff
      character(len=16) :: var_name
      logical, save :: entered = .false.
      
!----------------------------------------------------------------------- 
!	... Find the dvout file corresponding to the present date
!-----------------------------------------------------------------------
      idv = 0
      do i = 1, max_dvout_dates
         if( dvout(i)%active .and. dvout(i)%days0do == date%days0 ) then
            idv = i
            exit
         end if
      end do
      if( idv == 0 ) return

!----------------------------------------------------------------------- 
!	... Initialize if necessary
!-----------------------------------------------------------------------
      if( .not. dvout(idv)%defined ) then
         call TEST_SUN_DVARCH_INIT( td )
       else                                  ! Open the file for writing
         call OPEN_FILE( dvout(idv)%flnm, nf_write )
      end if

!----------------------------------------------------------------------- 
!	... Set the solar local time
!-----------------------------------------------------------------------
      call SET_TIME( slt )
      call NETCDF_ARCH( 'time' )

!--------------------------------------------------------------------- 
!	... Write the diurnal variations
!--------------------------------------------------------------------- 
      call NETCDF_ARCH( 'sza', matrix = sza(:,:) )
      call NETCDF_ARCH( 'scsza', matrix = scsza(:,:) )
  
      call CLOSE_FILE()
      dvout(idv)%defined = .true.

      contains
      
!=======================================================================      

      subroutine TEST_SUN_DVARCH_INIT( td )
!--------------------------------------------------------------------- 
!	... Setup the diurnal variations netcdf file
!--------------------------------------------------------------------- 
      use GRID_DIMS, only : lmax, niz
      use SPECIES_DIMS, only : nbcon
      use ALLCO, only : phi, zkm
      use SIM_CONTROLS, only : missval, label_short, label_long
      use ARCH_WHAT, only : nldesc, desc_text, vdims
      use VEN1, only : t2d
      use CONC, only : hm2d
      use NETCDF

      implicit none

      real, dimension(lmax,niz), intent(in) :: td
!--------------------------------------------------------------------- 
!	... Local variables
!--------------------------------------------------------------------- 
      integer :: i, ic, slen, genid
      real :: junk(1)

      write(*,'(a,i2,a,i2,a,i4)') 'dvout archive to ' 
     $        // TRIM( ADJUSTL( dvout(idv)%flnm ) ) // ' , date= ',
     $        date%month,'/',date%day,'/',date%year

!--------------------------------------------------------------------- 
!	... Initialize archive netcdf file
!--------------------------------------------------------------------- 
      call NETCDF_INIT( dvout(idv)%flnm )

!--------------------------------------------------------------------- 
!	... Define the dimensions, vdims(3)='time'=0 for unlimited dim
!--------------------------------------------------------------------- 
      call NETCDF_DIM_DEF( TRIM( ADJUSTL( vdims(1) ) ), lmax )
      call NETCDF_DIM_DEF( TRIM( ADJUSTL( vdims(2) ) ), niz )
      call NETCDF_DIM_DEF( TRIM( ADJUSTL( vdims(3) ) ), 0 )

!--------------------------------------------------------------------- 
!	... Add attributes for dimensions
!--------------------------------------------------------------------- 
      call SET_ATTRIBUTE( 'lat', 'latitudes', 'global' )
      call SET_ATTRIBUTE( 'lev', 'levels', 'global' )

!--------------------------------------------------------------------- 
!	... Add useful global attributes
!--------------------------------------------------------------------- 
      call SET_ATTRIBUTE( 'model_name', 'SOCRATES', 'global' )
      call SET_ATTRIBUTE( 'run_label', label_short, 'global' )
      call SET_ATTRIBUTE( 'label_long', label_long, 'global' )
      junk(:) = missval
      call SET_ATTRIBUTE( 'missing_value', var_name='global', 
     $                    attr_val=junk(:) )
      junk(:) = date%days0 
      call SET_ATTRIBUTE( 'sim_days0', var_name='global', 
     $                    attr_val=junk(:) )
      
!--------------------------------------------------------------------------
!	... Define the spatial variables and attributes
!--------------------------------------------------------------------------
      call NETCDF_VAR_DEF( 'latitudes', nf_float, .false.,
     $                      vdims(1:1), genid )
      call NETCDF_VAR_DEF( 'levels', nf_float, .false.,
     $                      vdims(2:2), genid )
      call SET_ATTRIBUTE( 'units', 'angular degrees', 'latitudes' )
      call SET_ATTRIBUTE( 'units', 'kilometers', 'levels' )

!--------------------------------------------------------------------------
!	... Write the spatial variables
!--------------------------------------------------------------------------
      call NETCDF_ARCH( 'latitudes', vector = phi )
      call NETCDF_ARCH( 'levels', vector = zkm )

!--------------------------------------------------------------------------
!	... Define the time variable and attribute
!--------------------------------------------------------------------------
      call NETCDF_VAR_DEF( 'time', nf_float, .true.,
     $                      vdims(3:3), var_id = genid )
      call SET_ATTRIBUTE( 'units', 'solar local time, hours', 'time' )

!--------------------------------------------------------------------------
!	... Define and set the description text variable
!--------------------------------------------------------------------------
      call NETCDF_MULTITEXT( 'description', nldesc, desc_text(:nldesc) )

!--------------------------------------------------------------------------
!	... Define & Write  daylength time (solar time independant)
!--------------------------------------------------------------------------
      call NETCDF_VAR_DEF( 'daylength', nf_float, .false.,
     $                      vdims(1:2), genid )
      call SET_ATTRIBUTE( 'units', 'hours', 'daylength' )
      call NETCDF_ARCH( 'daylength', matrix = td(:,:) )

!--------------------------------------------------------------------- 
!	... Define the solar time-dependant values to archive
!--------------------------------------------------------------------- 
      call NETCDF_VAR_DEF( 'sza', nf_float, .true., vdims(1:2), genid )
      call SET_ATTRIBUTE( 'units', 'degrees', 'sza' )
      call NETCDF_VAR_DEF( 'scsza', nf_float, .true., vdims(1:2), genid)

      end subroutine TEST_SUN_DVARCH_INIT

      end subroutine TEST_SUN_DVARCH
      
!=======================================================================      

      subroutine ARCH_DAYLENGTH( arch, times, td )
!-----------------------------------------------------------------------
!	... archiving routine     ;     simonc@oma.be  - v7s02, Feb 2001
!-----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use TIME_CONTROLS, only : TIMING
      use ARCH_WHAT, only : ARCHIVING
      use NETCDF, only : nf_write
      use NETCDF_UTILS, only : OPEN_FILE,CLOSE_FILE,SET_TIME,NETCDF_ARCH

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      type( ARCHIVING ), intent(inout) :: arch
      type( TIMING), intent(in)  ::  times
      real, dimension(lmax,niz), intent(in) :: td
      
      if( .not. arch%defined ) then
         call ARCH_DAYLENGTH_INIT( )           ! contained at end of this file
       else                                    ! Open the file for writing
         call OPEN_FILE( arch%flnm, nf_write )
      end if

!----------------------------------------------------------------------- 
!	... Set the time & Output the next time
!----------------------------------------------------------------------- 
      call SET_TIME( REAL(times%days0) )
      call NETCDF_ARCH( 'time' )

!-----------------------------------------------------------------------
!	... Output daylength
!----------------------------------------------------------------------- 
      call NETCDF_ARCH( 'daylength', matrix = td(:,:) )

500   call CLOSE_FILE()
      arch%defined = .true.
      
      contains

      subroutine ARCH_DAYLENGTH_INIT( )
!-----------------------------------------------------------------------
!	... Setup the common parts of the archive netcdf files
!                                       simonc@oma.be  - v6s20, Feb 2001
!-----------------------------------------------------------------------
      use ALLCO, only : phi, zkm
      use ARCH_WHAT, only : vdims, desc_text, nldesc
      use SIM_CONTROLS, only : missval, label_short, label_long
      use NETCDF, only : nf_float
      use NETCDF_UTILS             ! contains all subroutines called here

      implicit none

!----------------------------------------------------------------------- 
!	... Local variables
!-----------------------------------------------------------------------
      integer :: genid
      real :: junk(1)

!----------------------------------------------------------------------
!	... Initialize archive netcdf file
!----------------------------------------------------------------------
      call NETCDF_INIT( arch%flnm )

!----------------------------------------------------------------------
!	... Define the dimensions, vdims(3)='time'=0 for unlimited dim
!----------------------------------------------------------------------
      call NETCDF_DIM_DEF( TRIM( ADJUSTL( vdims(1) ) ), lmax )
      call NETCDF_DIM_DEF( TRIM( ADJUSTL( vdims(2) ) ), niz )
      call NETCDF_DIM_DEF( TRIM( ADJUSTL( vdims(3) ) ), 0 )

!--------------------------------------------------------------------- 
!	... Add attributes for dimensions
!--------------------------------------------------------------------- 
      call SET_ATTRIBUTE( 'lat', 'latitudes', 'global' )
      call SET_ATTRIBUTE( 'lev', 'levels', 'global' )

!--------------------------------------------------------------------- 
!	... Add global attributes to label the run and say it's a d.a.
!--------------------------------------------------------------------- 
      call SET_ATTRIBUTE( 'model_name', 'SOCRATES', 'global' )
      call SET_ATTRIBUTE( 'run_label', label_short, 'global' )
      call SET_ATTRIBUTE( 'label_long', label_long, 'global' )
      junk(:) = missval
      call SET_ATTRIBUTE( 'missing_value', var_name='global', 
     $                    attr_val=junk(:) )

!--------------------------------------------------------------------------
!	... Define the spatial variables and attributes
!--------------------------------------------------------------------------
      call NETCDF_VAR_DEF( 'latitudes', nf_float, .false.,
     $                      vdims(1:1), genid )
      call NETCDF_VAR_DEF( 'levels', nf_float, .false.,
     $                      vdims(2:2), genid )
      call SET_ATTRIBUTE( 'units', 'angular degrees', 'latitudes' )
      call SET_ATTRIBUTE( 'units', 'kilometers', 'levels' )

!-----------------------------------------------------------------------
!	... Write the spatial variables
!-----------------------------------------------------------------------
      call NETCDF_ARCH( 'latitudes', vector = phi )
      call NETCDF_ARCH( 'levels', vector = zkm )

!--------------------------------------------------------------------------
!	... Define the time variable and attribute
!--------------------------------------------------------------------------
      call NETCDF_VAR_DEF( 'time', nf_float, .true.,
     $                      vdims(3:3), genid )
      call SET_ATTRIBUTE( 'units', 'days since 0/0/0, no leap years', 
     $                    'time' )

!--------------------------------------------------------------------------
!	... Define and set the description text variable
!--------------------------------------------------------------------------
      call NETCDF_MULTITEXT( 'description', nldesc, desc_text(:nldesc) )

      call NETCDF_VAR_DEF( 'daylength', nf_float, .true.,
     $                      vdims(1:2), genid )
      call SET_ATTRIBUTE( 'units', 'hours', 'daylength' )

      end subroutine ARCH_DAYLENGTH_INIT

      end subroutine ARCH_DAYLENGTH
