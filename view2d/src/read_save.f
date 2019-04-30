! subversion Id for THIS file : $Id: read_save.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/read_save.f $
!-----------------------------------------------------------------------

      subroutine READ_SAVE( filespec, current_time )
!--------------------------------------------------------------------- 
!	... Read initial conditions in
!           user-supplied NetCDF file, previously saved ("restart")
!--------------------------------------------------------------------- 
      use GRID_DIMS, only : lmax, niz
      use SPECIES_DIMS, only : nbcon
      use TRACNM
      use SPC_NAMES, only : vid_o3, vid_no, vid_no2, vid_o2, vid_co2
      use SPECIES, only : qn2da, qn2noon
      use VEN1
      use VEN2
      use VEN9, only : xky, xkz
      use TEMP_HIST
      use SOLTEST
      use TRELAX
      use WAVE_PARMS, only : nz
      use WW_VARS, only : pw
      use TRNS
      use DISSPA, only : delta
      use SIM_CONTROLS, only : mainsw
      use DIAG_CONTROLS, only : diags
      use ARCH_WHAT, only : arch
      use TIME_CONTROLS
      use NETCDF_UTILS
      use NETCDF, only : NF_INQ_LIBVERS
      use DATE_UTILS, only : TIME2DATE

      implicit none

!--------------------------------------------------------------------- 
!	... Dummy args
!--------------------------------------------------------------------- 
      type( TIMING), intent(in)  ::  current_time
      character(len=*), intent(in)  ::  filespec
 
!--------------------------------------------------------------------- 
!	... Local variables
!--------------------------------------------------------------------- 
      integer :: ic, j, m
      real    :: rtime, pindex
      real    :: iwrk(lmax,niz), rwrk(lmax,niz)
      character(len=16) :: var_name
      type( TIMING) :: save_time
 
      write(*,*) 'NetCDF library version ',NF_INQ_LIBVERS()
!--------------------------------------------------------------------- 
!	... Open the file
!--------------------------------------------------------------------- 
      call OPEN_FILE( filespec, NF_NOWRITE )
      
!--------------------------------------------------------------------- 
!	... Read and check the save file time
!--------------------------------------------------------------------- 
      call NETCDF_READ( 'save_time', scalar = rtime )
      save_time%days0 = NINT(rtime)
      call TIME2DATE( save_time )
      if( current_time%days0 /= save_time%days0 ) then
         write(*,*)
         write(*,*) 'READ_SAVE: Start time and save time do not match !'
	 write(*,'(5x,''Start time = '',i2,''/'',i2,''/'',i4)') 
     $     current_time%month, current_time%day, current_time%year
	 write(*,'(5x,''Save  time = '',i2,''/'',i2,''/'',i4)') 
     $     save_time%month, save_time%day, save_time%year
         if( current_time%cal_day /= save_time%cal_day ) then
            call CLOSE_FILE()
            stop 'READ_SAVE error: start day-of-year on file mismatch'
         end if
         write(*,*) 'READ_SAVE: simulation continues nonetheless'
         write(*,*)         
      end if
!--------------------------------------------------------------------- 
!	... Read the temp history indicies
!--------------------------------------------------------------------- 
      call NETCDF_READ( 'thstperm1', scalar = pindex )
      perm(1) = NINT( pindex )
      call NETCDF_READ( 'thstperm2', scalar = pindex )
      perm(2) = NINT( pindex )
      call NETCDF_READ( 'thstperm3', scalar = pindex )
      perm(3) = NINT( pindex )

!--------------------------------------------------------------------- 
!	... Read the dynamics variables
!--------------------------------------------------------------------- 
      call NETCDF_READ( 'temperature', matrix = t2d )
      call NETCDF_READ( 'thst1', matrix = this(:,:,1) )
      call NETCDF_READ( 'thst2', matrix = this(:,:,2) )
      call NETCDF_READ( 'thst3', matrix = this(:,:,3) )
      call NETCDF_READ( 'trx', matrix = trx )
      call NETCDF_READ( 'srheat1', matrix = srheat1 )
      call NETCDF_READ( 'srheat2', matrix = srheat2 )
      if( mainsw(6) > -1 ) then
         call NETCDF_READ( 'srheat3', matrix = srheat3 )
         if( mainsw(6) == 0 ) srheat3(:,71:niz) = 0.
       else
         srheat3 = 0.
      end if
      call NETCDF_READ( 'xky', matrix = xky )
      call NETCDF_READ( 'xkz', matrix = xkz )
      call NETCDF_READ( 'u', matrix = u )
      call NETCDF_READ( 'rpw', matrix = rwrk )
      call NETCDF_READ( 'ipw', matrix = iwrk )
      do j = 1,nz
         pw(:,j) = CMPLX( rwrk(:,j),iwrk(:,j) )
      end do
      call NETCDF_READ( 'rtr1', matrix = rwrk )
      call NETCDF_READ( 'itr1', matrix = iwrk )
      do j = 1,nz
         tr(:,j,1) = CMPLX( rwrk(:,j),iwrk(:,j) )
      end do
      call NETCDF_READ( 'rtr2', matrix = rwrk )
      call NETCDF_READ( 'itr2', matrix = iwrk )
      do j = 1,nz
         tr(:,j,2) = CMPLX( rwrk(:,j),iwrk(:,j) )
      end do
      call NETCDF_READ( 'delta1', matrix = rwrk )
      do j = 1,nz
         delta(:,j,1) = rwrk(:,j)
      end do
      call NETCDF_READ( 'delta2', matrix = rwrk )
      do j = 1,nz
         delta(:,j,2) = rwrk(:,j)
      end do

      do ic = 1, nbcon 
!--------------------------------------------------------------------- 
!	... Read the diurnal averages for o3, h2o, no, co2, o2 & o3p 
!           This set of species set in SET_ARCH & required by HEAT_IRCOOL
!--------------------------------------------------------------------- 
         if( arch(7)%vid_qn2da(ic) ) then   ! arch(7)%mode = 'save '
            var_name = solsym(ic)
            var_name = var_name(:LEN_TRIM(var_name)) // '-da'
            if( diags ) write(*,*) 'Reading variable ' // var_name
            call NETCDF_READ( var_name, matrix = qn2da(:,:,ic) )
         end if

!--------------------------------------------------------------------- 
!	... Read noon values for all species
!--------------------------------------------------------------------- 
         var_name = solsym(ic)
	 var_name = var_name(:LEN_TRIM(var_name)) // '-noon'
         if( diags ) write(*,*) 'Reading variable ' // var_name
         call NETCDF_READ( var_name, matrix = qn2noon(:,:,ic) )
      end do

      call CLOSE_FILE()

      end subroutine READ_SAVE
