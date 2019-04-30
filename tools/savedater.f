      program SAVEDATER
!--------------------------------------------------------------------- 
!	... Gives date of a save (restart) file
!---------------------------------------------------------------------
      use NETCDF
      use TIME_CONTROLS
      
      implicit none
      
!--------------------------------------------------------------------- 
!	... Local Variables
!---------------------------------------------------------------------
      integer   :: rcode, nc_file_id
      character(len=64)  ::  filespec
      real  :: rtime
      type( TIMING) :: save_time
 
      write(*,*) 'Give path and name of the save file between quotes'
      read(*,*) filespec
!--------------------------------------------------------------------- 
!	... Open the file
!--------------------------------------------------------------------- 
      nc_file_id = NCOPN( filespec, ncnowrit, rcode )   ! open the file
      if (rcode /= 0) then
         write(*,*) 'Error opening nc file : path and names incorrect ?'
         stop
      end if
      
!--------------------------------------------------------------------- 
!	... Read the save file time
!--------------------------------------------------------------------- 
      call NETCDF_READ( nc_file_id, 'save_time', rtime )
      save_time%days0 = NINT(rtime)
      call TIME2DATE( save_time )
      write(*,'('' Save  time = '',i2,''/'',
     $             i2,''/'',i4)') save_time%month,
     $                            save_time%day,
     $                            save_time%year
      
      
      end program savedater

!=====================================================================

      subroutine NETCDF_READ( nc_file_id, var_name, scalar )
!--------------------------------------------------------------------------
!	... General purpose netdcf variable read
!           Note in this routine we will handle all errors
!--------------------------------------------------------------------------
      use NETCDF

      implicit none

!--------------------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------------------
      integer, intent(in)           :: nc_file_id
      character(len=*), intent(in)  :: var_name
      real, optional, intent(out)   :: scalar

!--------------------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------------------
      integer :: i, rcode
      integer :: var_id, time_id
      integer :: max_tim_ind
      integer :: var_type 
      integer :: var_attrs 
      integer :: var_dims
      integer :: ncoptions
      integer :: var_dim_ids(3)
      integer :: start_index(3)
      integer :: dim_ext(3)
      integer :: siz(2)
      real, allocatable :: times(:)
      character(len=16) :: name
      logical :: arg_valid(3)
      logical :: time_dependent
      logical :: alloc_flag

!--------------------------------------------------------------------------
!	... Inquire about the variable
!--------------------------------------------------------------------------
      var_id = NCVID( nc_file_id, var_name, rcode )
      call NCVINQ( nc_file_id, var_id, name, var_type,
     $             var_dims, var_dim_ids, var_attrs, rcode )
!--------------------------------------------------------------------------
!	... First get present options
!--------------------------------------------------------------------------
      call NCGOPT( ncoptions )
!--------------------------------------------------------------------------
!	... Next set errors to none fatal
!--------------------------------------------------------------------------
      call NCPOPT( ncverbos )
      time_dependent = .false.
!--------------------------------------------------------------------------
!	... Now check to see if time argument matches time dependency
!--------------------------------------------------------------------------
      if( time_dependent  ) then
	 call NCPOPT( ncoptions )
	 return
      end if
      alloc_flag = ALLOCATED( times )
      if( time_dependent ) then
!--------------------------------------------------------------------------
!	... Get time "dimension" size and find the time point index
!--------------------------------------------------------------------------
         call NCDINQ( nc_file_id,
     $                time_id,
     $                name,
     $                max_tim_ind,
     $                rcode )
         if( rcode /= ncnoerr ) then
	    call NCPOPT( ncoptions )
	    return
         end if
	 if( alloc_flag ) then
	    DEALLOCATE( times )
	 end if
	 ALLOCATE( times(max_tim_ind), stat = rcode )
         if( rcode /= 0 ) then
	    call NCPOPT( ncoptions )
	    return
         end if
	 start_index(1) = 1
	 dim_ext(1) = max_tim_ind
	 call NCVGT( nc_file_id,
     $               time_id,
     $               start_index,
     $               dim_ext,
     $               times,
     $               rcode )
         if( rcode /= ncnoerr ) then
	    call NCPOPT( ncoptions )
	    DEALLOCATE( times )
	    return
         end if
      end if
!--------------------------------------------------------------------------
!	... Read variable
!--------------------------------------------------------------------------

            call NCVGT1( nc_file_id,
     $                   var_id,
     $                   1,
     $                   scalar,
     $                   rcode )
            if( rcode /= ncnoerr ) then
	       call NCPOPT( ncoptions )
	       if( alloc_flag ) then
	          DEALLOCATE( times )
	       end if
	       return
            end if

      end subroutine NETCDF_READ


!=====================================================================

      subroutine TIME2DATE( times )
!--------------------------------------------------------------
!	... Convert incoming time to date in days, months, years
!           Note: this routine does not take leap years into account
!--------------------------------------------------------------

      use TIME_CONTROLS

      implicit none

!--------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------
      type( TIMING ), intent(inout) :: times

!--------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------
      integer  ::  i, jul_day
      integer, save ::  mon(0:12) = (/ 0, 31, 59, 90, 120, 151, 181,
     $                                 212, 243, 273, 304, 334, 365 /)

      times%year = (times%days0 - 1) / 365
      jul_day = MOD( (times%days0 - 1),365 ) + 1
      times%cal_day = jul_day
      do i = 11,1,-1
	 if( jul_day > mon(i) ) then
	    exit
	 end if
      end do
      times%month = MAX( i+1,1 )
      times%day = jul_day - mon(i)

      end subroutine TIME2DATE

