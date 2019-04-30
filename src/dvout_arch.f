! subversion Id for THIS file : $Id: dvout_arch.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/dvout_arch.f $
!-----------------------------------------------------------------------

      subroutine DVOUT_ARCH( dvout, date, slt, vmr )
!-----------------------------------------------------------------------  
!	... Write diurnal variations netCDF file. Done one latitude
!           at a time (called from CHEMDR). Solar local time management
!           is complex because the total nb of chem tsteps is lat-dependent
!           (module CHEM_TIMES, var nt(lmax)) while here we write data
!           only at the "normal" slt (see SUN_POSITION)
!                                    simonc@oma.be - v6s38 - jan 2002
!-----------------------------------------------------------------------  
      use GRID_DIMS, only : lmax, niz
      use SPECIES_DIMS, only : nbcon
      use TRACNM, only : solsym
      use SPECIES
      use RATES_MODS, only : rxt_rates
      use RXT_NAMES
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
      real, dimension(lmax,niz,nbcon), intent(in) :: vmr

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------  
      integer :: i, ic, idv, j, ju, slen, lat
      real    :: my_slt, tsdiff, jdiag(lmax,niz)
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
         call DVOUT_ARCH_INIT( )
       else                                  ! Open the file for writing
         call OPEN_FILE( dvout(idv)%flnm, nf_write )
      end if

!----------------------------------------------------------------------- 
!	... Set the solar local time
!-----------------------------------------------------------------------
      call SET_TIME( slt )
      call NETCDF_ARCH( 'time' )

!--------------------------------------------------------------------- 
!	... Write the diurnal variations for species set in SET_ARCH
!--------------------------------------------------------------------- 
      do ic = 1, nbcon
         if( dvout(idv)%vid_qn2dv(ic) ) then 
            var_name = solsym(ic)
            call NETCDF_ARCH( var_name, matrix = vmr(:,:,ic) )
         end if
      end do

!----------------------------------------------------------------------- 
!	... Archive a photodissociation process
!-----------------------------------------------------------------------
      do lat = 1, lmax
         jdiag(lat,:) = rxt_rates(:,rid_jo3_op,lat)
      end do
      call NETCDF_ARCH( 'J_O3-OP', matrix= jdiag )
      
      call CLOSE_FILE()
      dvout(idv)%defined = .true.

      contains
      
!=======================================================================      

      subroutine DVOUT_ARCH_INIT( )
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
!	... Define & Write temperature and total density (time independant)
!--------------------------------------------------------------------------
      call NETCDF_VAR_DEF( 'temperature', nf_float, .false.,
     $                      vdims(1:2), genid )
      call SET_ATTRIBUTE( 'units', 'K', 'temperature' )
      call NETCDF_ARCH( 'temperature', matrix = t2d(:,:) )

      call NETCDF_VAR_DEF( 'totdens', nf_float, .false.,
     $                      vdims(1:2), genid )
      call SET_ATTRIBUTE( 'units', 'molec/cm3', 'totdens' )
      call NETCDF_ARCH( 'totdens', matrix = hm2d(:,:) )

!--------------------------------------------------------------------- 
!	... Define the chem specices to archive, as set in SET_ARCH
!--------------------------------------------------------------------- 
      do ic = 1, nbcon
         if( dvout(idv)%vid_qn2dv(ic) ) then
            var_name = solsym(ic)
            slen = LEN_TRIM( var_name )
            call NETCDF_VAR_DEF( var_name(:slen), nf_float, .true.,
     $                           vdims(1:2), genid )
            call SET_ATTRIBUTE( 'units', 'vmr', var_name(:slen) )
         end if
      end do

!--------------------------------------------------------------------- 
!	... Define a photolysis rate to archive
!--------------------------------------------------------------------- 
      call NETCDF_VAR_DEF( 'J_O3-OP', nf_float, .true.,
     $                     vdims(1:2), genid )
      call SET_ATTRIBUTE( 'units', 's-1', 'J_O3-OP' )

      end subroutine DVOUT_ARCH_INIT

      end subroutine DVOUT_ARCH
