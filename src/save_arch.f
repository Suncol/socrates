! subversion Id for THIS file : $Id: save_arch.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/save_arch.f $
!-----------------------------------------------------------------------

      subroutine SAVE_ARCH( arch, times )
!--------------------------------------------------------------------- 
!	... Saving utility for restarting
!--------------------------------------------------------------------- 
      use TRACNM, only : solsym
      use SPECIES
      use VEN1
      use VEN2
      use VEN9, only : xky, xkz
      use TEMP_HIST
      use SOLTEST
      use TRELAX
      use WAVE_PARMS, only : nz
      use WW_VARS, only : pw
      use TRNS, only : tr
      use DISSPA, only : delta
      use TIME_CONTROLS, only : TIMING
      use ARCH_WHAT, only : ARCHIVING
      use NETCDF_UTILS

      implicit none

!--------------------------------------------------------------------- 
!	... Dummy args
!--------------------------------------------------------------------- 
      type( ARCHIVING ), intent(in) :: arch
      type( TIMING), intent(in)  ::  times
 
!--------------------------------------------------------------------- 
!	... Local variables
!--------------------------------------------------------------------- 
      integer :: ic, j, ju, slen
      real    :: wrk(lmax,niz)
      character(len=16) :: var_name
      logical, save :: entered = .false.
      
      if( .not. entered ) then
         call SAVE_ARCH_INIT( )
         entered = .true.
       else                                  ! Open the file for writing
         call OPEN_FILE( arch%flnm, nf_write )
      end if
      
!--------------------------------------------------------------------- 
!	... Output the present time
!--------------------------------------------------------------------- 
      call NETCDF_ARCH( 'save_time', scalar = REAL(times%days0) )

!--------------------------------------------------------------------- 
!	... Output the temp history indicies
!--------------------------------------------------------------------- 
      call NETCDF_ARCH( 'thstperm1', scalar = REAL(perm(1)) )
      call NETCDF_ARCH( 'thstperm2', scalar = REAL(perm(2)) )
      call NETCDF_ARCH( 'thstperm3', scalar = REAL(perm(3)) )

!--------------------------------------------------------------------- 
!	... Output the dynamics variables
!--------------------------------------------------------------------- 
      call NETCDF_ARCH( 'temperature', matrix = t2d )
      call NETCDF_ARCH( 'thst1', matrix = this(:,:,1) )
      call NETCDF_ARCH( 'thst2', matrix = this(:,:,2) )
      call NETCDF_ARCH( 'thst3', matrix = this(:,:,3) )
      call NETCDF_ARCH( 'trx', matrix = trx )
      call NETCDF_ARCH( 'srheat1', matrix = srheat1 )
      call NETCDF_ARCH( 'srheat2', matrix = srheat2 )
      call NETCDF_ARCH( 'srheat3', matrix = srheat3 )
      call NETCDF_ARCH( 'xky', matrix = xky )
      call NETCDF_ARCH( 'xkz', matrix = xkz )
      call NETCDF_ARCH( 'u', matrix = u )
      wrk(:,:) = 0.
      do j = 1,nz
         wrk(:,j) = REAL( pw(:,j) )
      end do
      call NETCDF_ARCH( 'rpw', matrix = wrk )
      do j = 1,nz
         wrk(:,j) = AIMAG( pw(:,j) )
      end do
      call NETCDF_ARCH( 'ipw', matrix = wrk )
      do j = 1,nz
         wrk(:,j) = REAL( tr(:,j,1) )
      end do
      call NETCDF_ARCH( 'rtr1', matrix = wrk )
      do j = 1,nz
         wrk(:,j) = REAL( tr(:,j,2) )
      end do
      call NETCDF_ARCH( 'rtr2', matrix = wrk )
      do j = 1,nz
         wrk(:,j) = AIMAG( tr(:,j,1) )
      end do
      call NETCDF_ARCH( 'itr1', matrix = wrk )
      do j = 1,nz
         wrk(:,j) = AIMAG( tr(:,j,2) )
      end do
      call NETCDF_ARCH( 'itr2', matrix = wrk )
      wrk(:,:nz) = delta(:,:nz,1)
      call NETCDF_ARCH( 'delta1', matrix = wrk )
      wrk(:,:nz) = delta(:,:nz,2)
      call NETCDF_ARCH( 'delta2', matrix = wrk )

      do ic = 1, nbcon
!--------------------------------------------------------------------- 
!	... Write the diurnal averages for o3, h2o, no, co2, o2 & o3p 
!           This set of species set in SET_ARCH & required by HEAT_IRCOOL
!--------------------------------------------------------------------- 
         if( arch%vid_qn2da(ic) ) then 
            var_name = solsym(ic)
            var_name = var_name(:LEN_TRIM(var_name)) // '-da'
            call NETCDF_ARCH( var_name, matrix = qn2da(:,:,ic) )
         end if

!--------------------------------------------------------------------- 
!	... Write noon values for all species
!--------------------------------------------------------------------- 
         var_name = solsym(ic)
	 var_name = var_name(:LEN_TRIM(var_name)) // '-noon'
         call NETCDF_ARCH( var_name, matrix = qn2noon(:,:,ic) )
      end do
      
      call CLOSE_FILE()

      contains
      
!=======================================================================      

      subroutine SAVE_ARCH_INIT( )
!--------------------------------------------------------------------- 
!	... Setup the save (restart) netcdf file
!--------------------------------------------------------------------- 
      use GRID_DIMS, only : lmax, niz
      use SPECIES_DIMS, only : nbcon
      use ALLCO, only : phi, zkm
      use SIM_CONTROLS, only : label_short, label_long
      use ARCH_WHAT, only : nldesc, desc_text, vdims
      use NETCDF

      implicit none

!--------------------------------------------------------------------- 
!	... Local variables
!--------------------------------------------------------------------- 
      integer :: i, ic, j, ju, slen, genid

!--------------------------------------------------------------------- 
!	... Initialize archive netcdf file
!--------------------------------------------------------------------- 
      call NETCDF_INIT( arch%flnm )

!--------------------------------------------------------------------- 
!	... Define the dimensions
!--------------------------------------------------------------------- 
      call NETCDF_DIM_DEF( TRIM( ADJUSTL( vdims(1) ) ), lmax )
      call NETCDF_DIM_DEF( TRIM( ADJUSTL( vdims(2) ) ), niz )

!--------------------------------------------------------------------- 
!	... Add useful global attributes
!--------------------------------------------------------------------- 
      call SET_ATTRIBUTE( 'model_name', 'SOCRATES', 'global' )
      call SET_ATTRIBUTE( 'run_label', label_short, 'global' )
      call SET_ATTRIBUTE( 'label_long', label_long, 'global' )
      
!--------------------------------------------------------------------------
!	... Define the spatial variables
!--------------------------------------------------------------------------
      call NETCDF_VAR_DEF( 'latitudes', nf_float, .false.,
     $                      vdims(1:1), genid )
      call NETCDF_VAR_DEF( 'levels', nf_float, .false.,
     $                      vdims(2:2), genid )

!--------------------------------------------------------------------------
!	... Define the time variable
!--------------------------------------------------------------------------
      call NETCDF_VAR_DEF( 'save_time', nf_float, .false.,
     $                      var_id = genid )

!--------------------------------------------------------------------------
!	... Define and set the description text variable
!--------------------------------------------------------------------------
      call NETCDF_MULTITEXT( 'description', nldesc, desc_text(:nldesc) )

!--------------------------------------------------------------------------
!	... Define the temperature history index variables
!--------------------------------------------------------------------------
      call NETCDF_VAR_DEF( 'thstperm1', nf_float, .false.,
     $                      var_id = genid )
      call NETCDF_VAR_DEF( 'thstperm2', nf_float, .false.,
     $                      var_id = genid )
      call NETCDF_VAR_DEF( 'thstperm3', nf_float, .false.,
     $                      var_id = genid )

!--------------------------------------------------------------------------
!	...  Define the dynamics variables
!--------------------------------------------------------------------------
      call NETCDF_VAR_DEF( 'temperature', nf_double, .false.,
     $                      vdims(1:2), genid )
      call NETCDF_VAR_DEF( 'thst1', nf_double, .false.,
     $                      vdims(1:2), genid )
      call NETCDF_VAR_DEF( 'thst2', nf_double, .false.,
     $                      vdims(1:2), genid )
      call NETCDF_VAR_DEF( 'thst3', nf_double, .false.,
     $                      vdims(1:2), genid )
      call NETCDF_VAR_DEF( 'trx', nf_double, .false.,
     $                      vdims(1:2), genid )
      call NETCDF_VAR_DEF( 'srheat1', nf_double, .false.,
     $                      vdims(1:2), genid )
      call NETCDF_VAR_DEF( 'srheat2', nf_double, .false.,
     $                      vdims(1:2), genid )
      call NETCDF_VAR_DEF( 'srheat3', nf_double, .false.,
     $                      vdims(1:2), genid )
      call NETCDF_VAR_DEF( 'xky', nf_double, .false.,
     $                      vdims(1:2), genid )
      call NETCDF_VAR_DEF( 'xkz', nf_double, .false.,
     $                      vdims(1:2), genid )
      call NETCDF_VAR_DEF( 'u', nf_double, .false.,
     $                      vdims(1:2), genid )
      call NETCDF_VAR_DEF( 'rpw', nf_double, .false.,
     $                      vdims(1:2), genid )
      call NETCDF_VAR_DEF( 'ipw', nf_double, .false.,
     $                      vdims(1:2), genid )
      call NETCDF_VAR_DEF( 'rtr1', nf_double, .false.,
     $                      vdims(1:2), genid )
      call NETCDF_VAR_DEF( 'itr1', nf_double, .false.,
     $                      vdims(1:2), genid )
      call NETCDF_VAR_DEF( 'rtr2', nf_double, .false.,
     $                      vdims(1:2), genid )
      call NETCDF_VAR_DEF( 'itr2', nf_double, .false.,
     $                      vdims(1:2), genid )
      call NETCDF_VAR_DEF( 'delta1', nf_double, .false.,
     $                      vdims(1:2), genid )
      call NETCDF_VAR_DEF( 'delta2', nf_double, .false.,
     $                      vdims(1:2), genid )

      do ic = 1, nbcon
!--------------------------------------------------------------------- 
!	... Define the diurnal averages for o3, h2o, no, co2, o2 & o3p 
!           This set of species set in SET_ARCH & required by HEAT_IRCOOL
!--------------------------------------------------------------------- 
         if( arch%vid_qn2da(ic) ) then
            var_name = solsym(ic)
            var_name = var_name(:LEN_TRIM(var_name)) // '-da'
            slen = LEN_TRIM( var_name )
            call NETCDF_VAR_DEF( var_name(:slen), nf_double, .false.,
     $                           vdims(1:2), genid )
         end if

!--------------------------------------------------------------------- 
!	... Define noon values for all species
!--------------------------------------------------------------------- 
         var_name = solsym(ic)
	 var_name = var_name(:LEN_TRIM(var_name)) // '-noon'
	 slen = LEN_TRIM( var_name )
	 call NETCDF_VAR_DEF( var_name(:slen), nf_double, .false.,
     $                           vdims(1:2), genid )
      end do

!--------------------------------------------------------------------------
!	... Write the spatial variables
!--------------------------------------------------------------------------
      call NETCDF_ARCH( 'latitudes', vector = phi )
      call NETCDF_ARCH( 'levels', vector = zkm )

      end subroutine SAVE_ARCH_INIT

      end subroutine SAVE_ARCH
