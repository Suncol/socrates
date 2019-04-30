! subversion Id for THIS file : $Id: sub_scan.new.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/sub_scan.new.f $
!-----------------------------------------------------------------------

      subroutine SUB_SCAN( filecnt, &
                           lib_files, &
                           usr_paths, &
                           usr_files, &
                           lib_names, &
                           lib_alias, &
                           sub_cnt )
!---------------------------------------------------------------------
!	... This subroutine scans a "standard" library routine for
!           matching user routines which will override the library
!           routine.
!---------------------------------------------------------------------

      implicit none

!---------------------------------------------------------------------
!	... Dummy args
!---------------------------------------------------------------------
      integer, intent(in)    ::     filecnt          ! count of library subroutines
      integer, intent(inout) ::     sub_cnt          ! count of "user" subroutines
      character(len=*), intent(in)    ::  lib_names(*)  ! "aliased" library filenames
      character(len=*), intent(inout) ::  lib_files(*)  ! library filenames
      character(len=*), intent(inout) ::  usr_files(*)  ! user filenames
      character(len=*), intent(inout) ::  usr_paths(*)  ! user filepaths
      logical, intent(in) ::        lib_alias(500)   ! library routine alias flag

!---------------------------------------------------------------------
!	... Local variables
!---------------------------------------------------------------------
      integer ::   i, j, alls
      integer, allocatable :: mark(:)
      character(len=64), allocatable :: wrk_files(:)  ! work space
      character(len=64), allocatable :: wrk_paths(:)  ! work space
      logical, allocatable :: keep(:)

      if( filecnt > 0 .and. sub_cnt > 0 ) then
	 ALLOCATE( wrk_files(sub_cnt), wrk_paths(sub_cnt), keep(sub_cnt), stat = alls )
	 if( alls /= 0 ) then
	    write(*,*) ' SUB_SCAN : Failed to allocated wrk array'
	    stop 'Alloc err'
	 end if
	 ALLOCATE( mark(filecnt), stat = alls )
	 if( alls /= 0 ) then
	    write(*,*) ' SUB_SCAN : Failed to allocated wrk array'
	    stop 'Alloc err'
	 end if
	 wrk_files(:sub_cnt) = usr_files(:sub_cnt)
	 wrk_paths(:sub_cnt) = usr_paths(:sub_cnt)
	 do i = 1,filecnt
	    mark(i) = INDEX( lib_files(i)(:LEN_TRIM(lib_files(i))), &
			     '/', back = .true. ) + 1
	 end do
	 keep(:sub_cnt) = .true.
!---------------------------------------------------------------------
!	... First scan for aliases
!---------------------------------------------------------------------
	 do j = 1,sub_cnt
	    if( lib_alias(j) ) then
	       do i = 1,filecnt
		  if( lib_files(i)(mark(i):LEN_TRIM(lib_files(i))) == &
		      lib_names(j)(:LEN_TRIM(lib_names(j))) ) then
		     keep(j) = .false.
	             lib_files(i) = wrk_paths(j)(:LEN_TRIM(usr_paths(j))) &
			            // wrk_files(j)(:LEN_TRIM(wrk_files(j)))
		     exit
		  end if
	       end do
	    end if
	 end do
!---------------------------------------------------------------------
!	... Then scan for name matches
!---------------------------------------------------------------------
         do i = 1,filecnt
	    do j = 1,sub_cnt
	       if( keep(j) .and. &
	           lib_files(i)(mark(i):LEN_TRIM(lib_files(i))) == &
		   wrk_files(j)(:LEN_TRIM(wrk_files(j))) ) then
	          lib_files(i) = wrk_paths(j)(:LEN_TRIM(usr_paths(j))) &
			         // wrk_files(j)(:LEN_TRIM(wrk_files(j)))
		  keep(j) = .false.
	          exit
	       end if
	    end do
         end do

         if( COUNT( keep(:sub_cnt) ) /= sub_cnt ) then
	    usr_files(:sub_cnt) = PACK( wrk_files(:sub_cnt), mask = keep(:sub_cnt) )
	    usr_paths(:sub_cnt) = PACK( wrk_paths(:sub_cnt), mask = keep(:sub_cnt) )
	    sub_cnt = COUNT( keep )
	 end if
	 DEALLOCATE( wrk_files )
	 DEALLOCATE( wrk_paths )
	 DEALLOCATE( keep )
	 DEALLOCATE( mark )
      end if

      end subroutine SUB_SCAN
