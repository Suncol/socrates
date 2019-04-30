
      subroutine ARCHIVE( filenm, times, nvars, tdep, var2d )
!-----------------------------------------------------------------------
!	... main archiving routine, depends on mode set in SET_ARCH
!           uses ARCHIVE_INIT *contained* here
!           Time-indep vars (tdep=.false.) are defined and added ONCE
!             on last call
!           Notice: the output netCDF file REMAINS OPEN on exit
!-----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use ARCH_WHAT, only : vdims
      use TYPE_DEF, only: TIMING, VAR2D_TYPE
      use NETCDF, only : nf_write, nf_float
      use NETCDF_UTILS

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      character(len=*), intent(in) :: filenm
      type( TIMING), intent(in)  ::  times
      integer, intent(in)  ::  nvars
      logical, intent(in) :: tdep   ! vars are time-dependent
      type( VAR2D_TYPE ), dimension(nvars) :: var2d

!----------------------------------------------------------------------- 
!	... Local variables
!-----------------------------------------------------------------------
      integer :: i, l, genid
      character(len=21) :: vname
      logical, save :: entered = .false.
      
      if( .not. entered ) then
!----------------------------------------------------------------------- 
!	... Initialize the output archive AND LEAVE IT OPEN
!----------------------------------------------------------------------- 
         call ARCHIVE_INIT( )                  ! contained at end of this file
      end if
      call SET_FILE( filenm )

!----------------------------------------------------------------------- 
!	... Set the time & Output the next time
!----------------------------------------------------------------------- 
      if( tdep ) then
         call SET_TIME( REAL(times%days0) )
         call NETCDF_ARCH( 'time' )
      end if

!-----------------------------------------------------------------------
!	... Output the variables
!----------------------------------------------------------------------- 
      do i = 1, nvars
         vname = var2d(i)%name
         l = LEN_TRIM( vname )
         if( .not. entered .or. .not. tdep ) then
            write(*,*) 'ARCHIVE: defining var >' // vname(:) 
     $       // '< with units >' // var2d(i)%units // '<'
            call NETCDF_VAR_DEF( TRIM(ADJUSTL( vname )), nf_float
     $                         , tdep, vdims(1:2), genid )
            call SET_ATTRIBUTE( 'units', var2d(i)%units
     $                        , TRIM(ADJUSTL( vname )) )
         end if
         call NETCDF_ARCH( TRIM(ADJUSTL( vname ))
     $                   , matrix = var2d(i)%vals(:,:) )
      end do

      entered = .true.
      
      contains
            
!=======================================================================      

      subroutine ARCHIVE_INIT( )
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
      call NETCDF_INIT( filenm )

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

      end subroutine ARCHIVE_INIT

      end subroutine ARCHIVE
