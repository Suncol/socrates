
      module NETCDF_UTILS
!--------------------------------------------------------------------------
!       ... Developed by Stacy Walters (stacy@ucar.edu)
!
!	... New platform-independent implementation: all the 
!           integers used as arguments of functions from netCDF libraries
!           must have a KIND consistent with the local netCDF libraries
!           compilation (e.g., integer*4 for HP-UX but integer*8 for Cray).
!           This is accomplished by compiling the module NETCDF with
!           local default precision (eg, 32bits for HP-UX, 64 for Cray)
!           but compiling the present module with 64-bit precision.
!           We then use the kind of one of the parameters from module NETCDF
!           (e.g., nf_fill) to define the kind of integer arguments .
!                             Simon Chabrillat (simonc@oma.be) - may 1999
!
!       ... Complete error traceback and reporting - simonc - september 1999
!
!       ... NETCDF_ARCH improvements: 2 new optional dummy args 'time_index'
!           (to archive at times before the last archived time) & 'col_index'
!           (to write only a column in a matrix var) - simonc - v6s38 - jan 2002
!--------------------------------------------------------------------------
      use NETCDF

      implicit none

      PRIVATE   ! NETCDF_CREATE, FILE_CHECK, HANDLE_ERR
      
      PUBLIC :: NETCDF_INIT, NETCDF_DIM_DEF, NETCDF_VAR_DEF
     $        , SET_ATTRIBUTE, NETCDF_ARCH, NETCDF_MULTITEXT
     $        , NETCDF_DIM_READ, GET_ATTRIBUTE, NETCDF_READ
     $        , SET_TIME, SET_FILE
     $        , OPEN_FILE, CLOSE_FILE, CLOSE_ALL_FILES

!--------------------------------------------------------------------------
!	... Parameters
!--------------------------------------------------------------------------
      integer, parameter :: nf = KIND( nf_fill )
      integer(kind=nf), parameter :: nf1 = 1
      integer, parameter :: max_files = 5

!--------------------------------------------------------------------------
!	... The file id and filespec
!--------------------------------------------------------------------------
      integer(kind=nf), private :: nc_file_id                ! active file id
      integer(kind=nf), private :: file_ids(max_files)       ! filespec list
      integer(kind=nf), private :: file_cnt = 0              ! file count
      character(len=64), private :: nc_filespec              ! active filespec
      character(len=64), private :: file_specs(max_files)
!--------------------------------------------------------------------------
!	... The dimension ids
!--------------------------------------------------------------------------
      integer(kind=nf), private :: lat_id, lev_id, time_id
      character(len=4), private :: dim_names(3)  = (/ 'lat ',
     $                                                'lev ',
     $                                                'time' /)
!--------------------------------------------------------------------------
!	... The variable ids
!--------------------------------------------------------------------------
      integer(kind=nf), private :: times_id
      integer(kind=nf), private :: latitudes_id, levels_id
!--------------------------------------------------------------------------
!	... The dimension id vector
!--------------------------------------------------------------------------
      integer(kind=nf), private :: vdims(3)

      real, private :: output_time            ! output time in days since 0/0/0

      CONTAINS

!=======================================================================
!=======================================================================

      subroutine NETCDF_INIT( filespec )
!--------------------------------------------------------------------------
!	... Create and initialize netcdf file for output
!--------------------------------------------------------------------------

      implicit none

!--------------------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------------------
      character(len=*), intent(in) :: filespec

!--------------------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------------------
      integer(kind=nf) :: j, rcode

!--------------------------------------------------------------------------
!	... Check files for limits
!--------------------------------------------------------------------------
      if( file_cnt == max_files ) then
	 write(*,*) 'NETCDF_INIT: Reached max number files'
	 return
      end if
      nc_filespec = filespec(:LEN_TRIM(filespec))
      file_cnt = file_cnt + 1
      file_specs(file_cnt) = nc_filespec

!--------------------------------------------------------------------------
!	... Create the file and exit define mode
!--------------------------------------------------------------------------
      call NETCDF_CREATE()                   ! create netcdf file
      rcode = NF_ENDDEF( nc_file_id )        ! exit define mode
      if( rcode /= NF_NOERR ) then
         call HANDLE_ERR( 'NETCDF_INIT exiting def mode of '//filespec, 
     $                    rcode )
      end if

      end subroutine NETCDF_INIT

!=======================================================================

      subroutine NETCDF_CREATE()
!--------------------------------------------------------------------------
!	... Create the netcdf file
!--------------------------------------------------------------------------

      implicit none

!--------------------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------------------
      integer(kind=nf) :: slen, rcode

      slen = LEN_TRIM( nc_filespec )
      if( slen == 0 ) then
	 write(*,*) ' NETCDF_CREATE: Invalid filespec'
	 stop 'No file'
      end if
      rcode = NF_CREATE( nc_filespec(:slen), NF_CLOBBER, nc_file_id )
      if( rcode /= NF_NOERR ) then
         call HANDLE_ERR( 'NETCDF_CREATE', rcode )
      end if
      file_ids(file_cnt) = nc_file_id

      end subroutine NETCDF_CREATE

!=======================================================================

      subroutine NETCDF_DIM_DEF( dim_name, dim_size )
!--------------------------------------------------------------------------
!	... Define dimensions
!--------------------------------------------------------------------------

      implicit none

!--------------------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------------------
      character(len=*), intent(in) :: dim_name
      integer, intent(in) :: dim_size

!--------------------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------------------
      integer(kind=nf) :: rcode, gen_dim_id, slen, size

!--------------------------------------------------------------------------
!	... First place in define mode
!--------------------------------------------------------------------------
      rcode = NF_REDEF( nc_file_id )
      if( rcode /= NF_NOERR ) then
         call HANDLE_ERR( 'NETCDF_DIM_DEF calling NF_REDEF', rcode )
      end if
!--------------------------------------------------------------------------
!	... Check incoming name
!--------------------------------------------------------------------------
      slen = LEN_TRIM( dim_name )
      size = INT( dim_size, kind=nf )
      if( slen /= 0 ) then
         if( dim_name(:slen) == 'lat' ) then
            rcode = NF_DEF_DIM( nc_file_id, 'lat', size, lat_id )
            if( rcode /= NF_NOERR ) then
               call HANDLE_ERR( 'NETCDF_DIM_DEF defining lat', rcode )
            end if
         else if( dim_name(:slen) == 'lev' ) then
            rcode = NF_DEF_DIM( nc_file_id, 'lev', size, lev_id )
            if( rcode /= NF_NOERR ) then
               call HANDLE_ERR( 'NETCDF_DIM_DEF defining lev', rcode )
            end if
         else if( dim_name(:slen) == 'time' ) then
            rcode = NF_DEF_DIM( nc_file_id, 'time',
     $                            NF_UNLIMITED, time_id )
            if( rcode /= NF_NOERR ) then
               call HANDLE_ERR( 'NETCDF_DIM_DEF defining time', rcode )
            end if
         else
            rcode = NF_DEF_DIM( nc_file_id,
     $                          dim_name(:slen),
     $                          size, gen_dim_id )
            if( rcode /= NF_NOERR ) then
               call HANDLE_ERR( 'NETCDF_DIM_DEF defining '//
     $                           dim_name(:slen), rcode )
            end if
         end if
      end if
!--------------------------------------------------------------------------
!	... Take file out of define mode
!--------------------------------------------------------------------------
      rcode = NF_ENDDEF( nc_file_id )
      if( rcode /= NF_NOERR ) then
         call HANDLE_ERR( 'NETCDF_DIM_DEF leaving def mode', rcode )
      end if

      end subroutine NETCDF_DIM_DEF

!=======================================================================

      subroutine NETCDF_VAR_DEF( varname, vartype,
     $                           time_dep, var_dims, var_id )
!--------------------------------------------------------------------------
!	... Define general netcdf variable
!--------------------------------------------------------------------------

      implicit none

!--------------------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------------------
      integer(kind=nf), intent(in)  :: vartype
      integer, intent(out) :: var_id
      character(len=*), intent(in) :: varname
      character(len=*), optional, intent(in) :: var_dims(:)
      logical, intent(in) :: time_dep

!--------------------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------------------
      integer(kind=nf) :: slen, dimcnt, i, j, rcode, v_id
      integer(kind=nf) :: dims(3)

!--------------------------------------------------------------------------
!	... Load the vdims array
!--------------------------------------------------------------------------
      vdims = (/ lat_id, lev_id, time_id /)
      var_id = -1
!--------------------------------------------------------------------------
!	... First place in define mode
!--------------------------------------------------------------------------
      rcode = NF_REDEF( nc_file_id )
      if( rcode /= NF_NOERR ) then
         call HANDLE_ERR( 'NETCDF_VAR_DEF setting def mode', rcode )
      end if
!--------------------------------------------------------------------------
!	... Check incoming name
!--------------------------------------------------------------------------
      slen = LEN_TRIM( varname )
      if( slen == 0 ) then
	 stop 'NETCDF_VAR_DEF, fatal error: varname is empty string'
      end if
!--------------------------------------------------------------------------
!	... Check for time array
!--------------------------------------------------------------------------
      if( varname(:slen) == 'time' ) then
         rcode = NF_DEF_VAR( nc_file_id, varname(:slen), vartype, nf1,
     $                       vdims(3), times_id )
         if( rcode /= NF_NOERR ) then
            call HANDLE_ERR( 'NETCDF_VAR_DEF defining time', rcode )
         end if
         var_id = INT( times_id, kind=KIND(var_id) )
	 go to 100
      end if
!--------------------------------------------------------------------------
!	... Check incoming dimensions
!--------------------------------------------------------------------------
      if( PRESENT( var_dims ) ) then
         dimcnt = SIZE( var_dims )
         if( dimcnt < 1 .or. dimcnt > 2 ) then
            write(*,*)'NETCDF_VAR_DEF: error var >'//varname(:slen)//'<'
            write(*,*)'   has ',dimcnt,' dimensions, should be 1 *or* 2'
	    stop 'NETCDF_VAR_DEF: fatal error, dimcnt < 1 or dimcnt > 2'
	 end if
	 dims(:dimcnt) = -1
	 do i = 1,dimcnt
	    do j = 1,2
	       if( var_dims(i)(:LEN_TRIM(var_dims(i))) ==
     $             dim_names(j)(:LEN_TRIM(dim_names(j))) ) then
		  dims(i) =  vdims(j)
		  exit
	       end if
	    end do
	 end do
	 if( ANY( dims(:dimcnt) == -1 ) ) then
            write(*,*)'NETCDF_VAR_DEF: error var >'//varname(:slen)//'<'
            write(*,*)'   has unrecognized dimensions'
	    stop 'NETCDF_VAR_DEF: fatal error, unrecognized dimensions'
	 end if
      else
	 dimcnt = 0
      end if
!--------------------------------------------------------------------------
!	... Check for time dependence
!--------------------------------------------------------------------------
      if( time_dep ) then
	 dimcnt = dimcnt + 1
	 dims(dimcnt) = time_id
      end if

!--------------------------------------------------------------------------
!	... Now call netcdf define function
!--------------------------------------------------------------------------
      rcode = NF_DEF_VAR( nc_file_id, varname(:slen), vartype, dimcnt,
     $                    dims(:dimcnt), v_id )
      if( rcode /= NF_NOERR ) then
         call HANDLE_ERR( 'NETCDF_VAR_DEF defining >'//
     $                     varname(:slen)//'<', rcode )
      end if
      var_id = INT( v_id, kind=KIND(var_id) )
!--------------------------------------------------------------------------
!	... Finally end define mode
!--------------------------------------------------------------------------
100   rcode = NF_ENDDEF( nc_file_id )
      if( rcode /= NF_NOERR ) then
         call HANDLE_ERR( 'NETCDF_VAR_DEF leaving def mode', rcode )
      end if

      end subroutine NETCDF_VAR_DEF

!=======================================================================

      subroutine SET_ATTRIBUTE( attr_name, attr_string, var_name, 
     $                          attr_val )
!--------------------------------------------------------------------------
!	... Set a variable or global attribute
!--------------------------------------------------------------------------

      implicit none

!--------------------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------------------
      character(len=*), intent(in) :: attr_name
      character(len=*), optional, intent(in) :: attr_string
      real, optional, intent(in)             :: attr_val(:)
      character(len=*), optional, intent(in) :: var_name

!--------------------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------------------
      integer(kind=nf) :: slen, rcode, var_id

!--------------------------------------------------------------------------
!	... Minimal check for incoming attribute name and value
!--------------------------------------------------------------------------
      if( ( PRESENT(attr_string) .and. PRESENT(attr_val) ) .or.
     $    ( .not. PRESENT(attr_string) .and. .not. PRESENT(attr_val) )
     $   .or. LEN_TRIM(attr_name) == 0 ) then
         call HANDLE_ERR('SET_ATTRIBUTE, wrong optional args', NF_NOERR)
      end if

!--------------------------------------------------------------------------
!	... First place in define mode
!--------------------------------------------------------------------------
      rcode = NF_REDEF( nc_file_id )
      if( rcode /= NF_NOERR ) then
         call HANDLE_ERR( 'SET_ATTRIBUTE setting def mode', rcode )
      end if

      slen = LEN_TRIM( var_name )
!--------------------------------------------------------------------------
!	... Global or variable attribute ?
!--------------------------------------------------------------------------
      if( .not. PRESENT( var_name ) .or.
     $                          var_name(:slen) == 'global' ) then
	 var_id = NF_GLOBAL
      else
         rcode = NF_INQ_VARID( nc_file_id, var_name(:slen), var_id )
         if( rcode /= NF_NOERR ) then
            call HANDLE_ERR( 'SET_ATTRIBUTE finding var '//
     $                       var_name(:slen), rcode )
         end if
      end if

!--------------------------------------------------------------------------
!	... String or real (double) attribute type ?
!--------------------------------------------------------------------------
      if( PRESENT( attr_string ) ) then
         slen = LEN_TRIM( attr_string )
         rcode = NF_PUT_ATT_TEXT( nc_file_id, 
     $                            var_id,
     $                            attr_name(:LEN_TRIM(attr_name)),
     $                            slen,
     $                            attr_string(:slen) )
       else if( PRESENT( attr_val ) ) then
         slen = SIZE( attr_val )
         rcode = NF_PUT_ATT_DOUBLE( nc_file_id, 
     $                              var_id,
     $                              attr_name(:LEN_TRIM(attr_name)),
     $                              NF_DOUBLE,
     $                              slen,
     $                              attr_val(:) )
      end if

!--------------------------------------------------------------------------
!	... Leave define mode
!--------------------------------------------------------------------------
      rcode= NF_ENDDEF( nc_file_id )
      if( rcode /= NF_NOERR ) then
         call HANDLE_ERR( 'SET_ATTRIBUTE leaving def mode', rcode )
      end if

      end subroutine SET_ATTRIBUTE

!=======================================================================

      subroutine NETCDF_ARCH( var_name, scalar, matrix, 
     $                        vector, col_index, time_index )
!--------------------------------------------------------------------------
!	... General purpose netdcf variable write
!--------------------------------------------------------------------------

      implicit none

!--------------------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------------------
      character(len=*), intent(in)  :: var_name
      real, optional, intent(in)    :: scalar
      real, optional, intent(in)    :: matrix(:,:)
      real, optional, intent(in)    :: vector(:)
      integer, optional, intent(in) :: col_index, time_index

!--------------------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------------------
      integer(kind=nf) :: i, rcode, slen
      integer(kind=nf) :: uplim
      integer(kind=nf) :: var_id
      integer(kind=nf) :: last_time, arch_time
      integer(kind=nf) :: var_type 
      integer(kind=nf) :: var_attrs 
      integer(kind=nf) :: var_dims
      integer(kind=nf) :: var_dim_ids(3)
      integer(kind=nf) :: start_index(3)
      integer(kind=nf) :: dim_ext(3)
      integer(kind=nf) :: siz(2)
      character(len=16) :: name
      logical :: arg_valid(3)
      logical :: time_dependent

!--------------------------------------------------------------------------
!	... Inquire about the variable
!--------------------------------------------------------------------------
      slen = LEN_TRIM( var_name )
      rcode = NF_INQ_VARID( nc_file_id, var_name(:slen), var_id )
      if( rcode /=NF_NOERR ) call HANDLE_ERR( 'NETCDF_ARCH finding var '
     $                                        //var_name(:slen), rcode )
      rcode = NF_INQ_VAR( nc_file_id, var_id, name, var_type,
     $                    var_dims, var_dim_ids, var_attrs )
      if( rcode /= NF_NOERR ) call HANDLE_ERR( 'NETCDF_ARCH getting '//
     $                          'info on var '//var_name(:slen), rcode )
      
!--------------------------------------------------------------------------
!	... If var is time-dependent variable, get or check time index
!--------------------------------------------------------------------------
      time_dependent =  ANY( var_dim_ids(1:var_dims) == time_id )
      if( time_dependent ) then
         rcode = NF_INQ_DIM( nc_file_id, time_id, name, last_time )
         if( rcode /= NF_NOERR ) call HANDLE_ERR( 'NETCDF_ARCH getting '
     $              //'time dim size; var '//var_name(:slen), rcode )
         if( PRESENT( time_index ) ) then
            if( time_index > last_time ) call HANDLE_ERR( 'NETCDF_ARCH '
     $           //'checking time_index; var '//var_name(:slen), rcode )
            arch_time = time_index
          else
            arch_time = last_time
         end if
      end if

      if( var_name(:slen) == 'time' ) then
!--------------------------------------------------------------------------
!	... Write next time value & Increment time "count" for next write
!--------------------------------------------------------------------------
	 start_index(1) = last_time + 1
         rcode = NF_PUT_VAR1_DOUBLE( nc_file_id,
     $                               var_id,
     $                               start_index,
     $                               output_time )
         if( rcode /= NF_NOERR ) call HANDLE_ERR( 'NETCDF_ARCH writing '
     $                                         //'the time var', rcode )
         return
      end if
      
!--------------------------------------------------------------------------
!	... Check for arg validity; exactly one must exist
!--------------------------------------------------------------------------
      arg_valid(1) = PRESENT( matrix )
      arg_valid(2) = PRESENT( vector )
      arg_valid(3) = PRESENT( scalar )
      if( COUNT( arg_valid ) /= 1 ) call HANDLE_ERR( 'NETCDF_ARCH '//
     $      'checking argument validity', NF_NOERR )
     
      if( arg_valid(3) ) then
!--------------------------------------------------------------------------
!	... Write time-independent scalar
!--------------------------------------------------------------------------
         rcode = NF_PUT_VAR1_DOUBLE( nc_file_id, var_id, nf1, scalar )
         if( rcode /= NF_NOERR ) call HANDLE_ERR( 'NETCDF_ARCH '//
     $          'writing scalar var '// var_name(:slen), rcode )
	 return
      end if

!--------------------------------------------------------------------------
!	... Compute start_index & dim_ext of the array var. 
!           Their last elements elements are for time. 
!           They can be modified later in the special "column vector" case
!--------------------------------------------------------------------------
      uplim = var_dims
      if( time_dependent ) uplim = var_dims - 1
      start_index(1:uplim) = 1
      if( time_dependent ) then
         start_index(var_dims) = arch_time
         dim_ext(var_dims) = 1       ! assume we write always for *one* time
      end if

      do i = 1, uplim
         rcode = NF_INQ_DIM( nc_file_id, var_dim_ids(i), 
     $                       name, dim_ext(i) )
         if( rcode /= NF_NOERR ) call HANDLE_ERR( 'NETCDF_ARCH '//
     $          'getting a dim for var '//var_name(:slen), rcode )
      end do

      if( arg_valid(1) ) then
!--------------------------------------------------------------------------
!	... Check dims of matrix array and write it
!--------------------------------------------------------------------------
         siz(1) = SIZE( matrix, 1 )
         siz(2) = SIZE( matrix, 2 )
         if( siz(1) /= dim_ext(1) .or. siz(2) /= dim_ext(2) )
     $       call HANDLE_ERR( 'NETCDF_ARCH checking dims for matrix '
     $        //var_name(:slen), NF_NOERR )
         rcode = NF_PUT_VARA_DOUBLE( nc_file_id,
     $                               var_id,
     $                               start_index,
     $                               dim_ext,
     $                               matrix )
         if( rcode /= NF_NOERR ) call HANDLE_ERR('NETCDF_ARCH writing '
     $                         //'matrix var '//var_name(:slen), rcode )

       else if( arg_valid(2) ) then    ! vector data was passed to the routine
         siz(1) = SIZE( vector, 1 )

         if( .not. PRESENT( col_index) ) then
!--------------------------------------------------------------------------
!	... Check dims of vector array (most probably axes coords) & write it
!--------------------------------------------------------------------------
            if( siz(1) /= dim_ext(1) ) call HANDLE_ERR( 'NETCDF_ARCH '//
     $          'checking dim, vector '//var_name(:slen), NF_NOERR )
            rcode = NF_PUT_VARA_DOUBLE( nc_file_id,
     $                                  var_id,
     $                                  start_index(1),
     $                                  dim_ext(1),
     $                                  vector )
            if( rcode /= NF_NOERR ) call HANDLE_ERR( 'NETCDF_ARCH '//
     $               'writing vector var '//var_name(:slen), rcode )

          else 
!--------------------------------------------------------------------------
!	... col_index present -> vector is *must* be a *column* in a matrix
!--------------------------------------------------------------------------
            if( siz(1) /= dim_ext(2) ) call HANDLE_ERR( 'NETCDF_ARCH '//
     $          'checking dim, col vector '//var_name(:slen), NF_NOERR )
            start_index(1) = col_index
            dim_ext(1) = 1
            rcode = NF_PUT_VARA_DOUBLE( nc_file_id,
     $                                  var_id,
     $                                  start_index,
     $                                  dim_ext,
     $                                  vector )
            if( rcode /= NF_NOERR ) call HANDLE_ERR( 'NETCDF_ARCH '//
     $               'writing col vector var '//var_name(:slen), rcode )
         end if
      end if

      end subroutine NETCDF_ARCH
      
!=======================================================================

      subroutine NETCDF_MULTITEXT( varname, nlt, multitext )
!-----------------------------------------------------------------------
!	... This routine defines and sets multiline text (vector of 
!           character strings) as a time-independent variable
!                                       simonc@oma.be - september 2000
!-----------------------------------------------------------------------

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      character(len=*), intent(in) :: varname
      integer, intent(in) :: nlt
      character(len=*), dimension(nlt), intent(in) :: multitext      

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer(kind=nf) :: rcode, nl, nr, slen, nl_id, nr_id, v_id
      
      nl = INT( nlt, kind=nf )
      nr = LEN( multitext(1) )
      slen = LEN_TRIM( varname )

!-----------------------------------------------------------------------
!	... Place in define mode, Define the dims of the var, 
!           define the var, end define mode
!-----------------------------------------------------------------------
      rcode = NF_REDEF( nc_file_id )
      if( rcode /= NF_NOERR ) then
         call HANDLE_ERR( 'NETCDF_MULTITEXT setting def mode', rcode )
      end if
      rcode = NF_DEF_DIM( nc_file_id, 'nltext', nl, nl_id )
      if( rcode /= NF_NOERR ) then
         call HANDLE_ERR( 'NETCDF_MULTITEXT defining dim nl for '
     $                    //'var >'//varname(:slen)//'<', rcode )
      end if
      rcode = NF_DEF_DIM( nc_file_id, 'nrtext', nr, nr_id )
      if( rcode /= NF_NOERR ) then
         call HANDLE_ERR( 'NETCDF_MULTITEXT defining dim nr for '
     $                    //'var >'//varname(:slen)//'<', rcode )
      end if
      rcode = NF_DEF_VAR( nc_file_id, varname(:slen), nf_char, 2,
     $                    (/ nr_id, nl_id /), v_id )
      if( rcode /= NF_NOERR ) then
         call HANDLE_ERR( 'NETCDF_MULTITEXT defining var >'
     $                    //varname(:slen)//'<', rcode )
      end if
      rcode = NF_ENDDEF( nc_file_id )
      if( rcode /= NF_NOERR ) then
         call HANDLE_ERR( 'NETCDF_MULTITEXT leaving def mode', rcode )
      end if

!-----------------------------------------------------------------------
!	... Set the text variable
!-----------------------------------------------------------------------
      rcode = NF_PUT_VAR_TEXT( nc_file_id, v_id, multitext )
      if( rcode /= NF_NOERR ) then
         call HANDLE_ERR( 'NETCDF_MULTITEXT writing variable >'
     $                    //varname(:slen)//'<', rcode )
      end if

      end subroutine NETCDF_MULTITEXT
      
!=======================================================================

      subroutine NETCDF_DIM_READ( dim_name, dim_size )
!--------------------------------------------------------------------------
!	... Read dimensions
!--------------------------------------------------------------------------

      implicit none

!--------------------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------------------
      character(len=*), intent(in) :: dim_name
      integer, intent(out) :: dim_size

!--------------------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------------------
      integer(kind=nf) :: rcode, slen, dim_id, size
      character(len=16) :: name
      
      slen = LEN_TRIM( dim_name )
      rcode = NF_INQ_DIMID( nc_file_id, dim_name(:slen), dim_id )
      if( rcode /= NF_NOERR ) then
         call HANDLE_ERR( 'NETCDF_DIM_READ finding dim '//
     $                    dim_name(:slen),rcode )
      end if
      rcode = NF_INQ_DIM( nc_file_id, dim_id, name, size )
      if( rcode /= NF_NOERR ) then
         call HANDLE_ERR( 'NETCDF_DIM_READ getting dim '//
     $                    dim_name(:slen),rcode )
      end if
      dim_size = size
      
      end subroutine NETCDF_DIM_READ

!=======================================================================

      subroutine GET_ATTRIBUTE( attr_name, var_name, attr_string
     $                        , attr_val )
!-----------------------------------------------------------------------
!	... Read a variable or global attribute
!-----------------------------------------------------------------------

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      character(len=*), intent(in) :: attr_name
      character(len=*), optional, intent(in)  :: var_name
      character(len=*), optional, intent(out) :: attr_string
      real, optional, intent(out)             :: attr_val(:)

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer(kind=nf) :: vslen, aslen, rcode, var_id, rlen, nf_type

!-----------------------------------------------------------------------
!	... Minimal check for incoming attribute name and value
!-----------------------------------------------------------------------
      if( ( PRESENT(attr_string) .and. PRESENT(attr_val) ) .or.
     $    ( .not. PRESENT(attr_string) .and. .not. PRESENT(attr_val) )
     $   .or. LEN_TRIM(attr_name) == 0 ) then
         call HANDLE_ERR('GET_ATTRIBUTE, wrong optional args', NF_NOERR)
      end if

      vslen = LEN_TRIM( var_name )
!-----------------------------------------------------------------------
!	... Global or variable attribute ?
!-----------------------------------------------------------------------
      if( .not. PRESENT( var_name ) .or.
     $                          var_name(:vslen) == 'global' ) then
	 var_id = NF_GLOBAL
      else
         rcode = NF_INQ_VARID( nc_file_id, var_name(:vslen), var_id )
         if( rcode /= NF_NOERR ) then
            call HANDLE_ERR( 'GET_ATTRIBUTE finding var '//
     $                       var_name(:vslen), rcode )
         end if
      end if

!-----------------------------------------------------------------------
!	... String or real (double) attribute type ?
!-----------------------------------------------------------------------
      aslen = LEN_TRIM( attr_name )
      rcode = NF_INQ_ATT( nc_file_id, var_id, attr_name(:aslen)
     $                  , nf_type, rlen )
      if( rcode /= NF_NOERR ) then
         call HANDLE_ERR( 'GET_ATTRIBUTE inq., att. '//attr_name(:aslen)
     $                    //' of var '//var_name(:vslen), rcode )
      end if
      if( PRESENT( attr_string ) ) then
         if( rlen > LEN( attr_string ) ) then
            call HANDLE_ERR( 'GET_ATTRIBUTE container too small for '//
     $                'text att. of var '// var_name(:vslen), NF_NOERR )
         end if
         rcode = NF_GET_ATT_TEXT( nc_file_id, var_id, attr_name(:aslen),
     $                            attr_string(:rlen) )
         if( rcode /= NF_NOERR ) then
            call HANDLE_ERR( 'GET_ATTRIBUTE getting text att. of var '//
     $                       var_name(:vslen), rcode )
         end if
         attr_string = TRIM(ADJUSTL( attr_string(:rlen) ))
       else if( PRESENT( attr_val ) ) then
         if( rlen > SIZE( attr_val ) ) then
            call HANDLE_ERR( 'GET_ATTRIBUTE container too small for '//
     $               'float att. of var '// var_name(:vslen), NF_NOERR )
         end if
         rcode = NF_GET_ATT_DOUBLE( nc_file_id, var_id
     $                            , attr_name(:aslen), attr_val(:) )
         if( rcode /= NF_NOERR ) then
            call HANDLE_ERR( 'GET_ATTRIBUTE getting float att. of var '
     $                     // var_name(:vslen), rcode )
         end if
      end if

      end subroutine GET_ATTRIBUTE

!=======================================================================

      subroutine NETCDF_READ( var_name,
     $                        time,
     $                        matrix,
     $                        vector,
     $                        scalar )
!--------------------------------------------------------------------------
!	... General purpose netdcf variable read
!           Note in this routine we will handle all errors
!--------------------------------------------------------------------------

      implicit none

!--------------------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------------------
      real, optional, intent(out)   :: matrix(:,:)
      real, optional, intent(out)   :: vector(:)
      real, optional, intent(out)   :: scalar
      real, optional, intent(in)    :: time
      character(len=*), intent(in)  :: var_name

!--------------------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------------------
      integer(kind=nf) :: i, rcode, slen, read_time_id
      integer(kind=nf) :: uplim
      integer(kind=nf) :: var_id
      integer(kind=nf) :: tim_ind
      integer(kind=nf) :: max_tim_ind
      integer(kind=nf) :: var_type 
      integer(kind=nf) :: var_attrs 
      integer(kind=nf) :: var_dims
      integer(kind=nf) :: var_dim_ids(3)
      integer(kind=nf) :: start_index(3)
      integer(kind=nf) :: dim_ext(3)
      integer(kind=nf) :: siz(2)
      real, allocatable :: times(:)
      character(len=16) :: name
      logical :: arg_valid(3)
      logical :: time_dependent
      logical :: alloc_flag

!--------------------------------------------------------------------------
!	... Inquire about the variable
!--------------------------------------------------------------------------
      slen = LEN_TRIM( var_name )
      rcode = NF_INQ_VARID( nc_file_id, var_name(:slen), var_id )
      if( rcode /= NF_NOERR ) then
         call HANDLE_ERR( 'NETCDF_READ finding var '//var_name(:slen),
     $                    rcode )
      end if
      rcode = NF_INQ_VAR( nc_file_id, var_id, name, var_type,
     $                    var_dims, var_dim_ids, var_attrs )
      if( rcode /= NF_NOERR ) then
         call HANDLE_ERR( 'NETCDF_READ getting info for var '//
     $                    var_name(:slen),rcode )
      end if
!-----------------------------------------------------------------------
!	... Time dependent variable ?
!-----------------------------------------------------------------------
      time_dependent = .false.
      rcode = NF_INQ_DIMID( nc_file_id, 'time', read_time_id )
      if( rcode == NF_NOERR ) then
         rcode = NF_INQ_DIM( nc_file_id,
     $                       read_time_id,
     $                       name,
     $                       max_tim_ind )
         if( rcode /= NF_NOERR ) then
            call HANDLE_ERR( 'NETCDF_READ getting time dim '//
     $                       'size for var '//var_name(:slen), rcode )
         end if
         time_dependent = ( max_tim_ind > 1 )
      end if
!	... Exclude the case where we want to read the whole 'time' vector
      time_dependent = time_dependent .and. 
     $    .not. ( PRESENT( vector ) .and. var_name(:slen)=='time' )

!-----------------------------------------------------------------------
!	... Now check to see if time argument matches time dependency
!-----------------------------------------------------------------------
      if( time_dependent .and. .not. PRESENT( time ) ) then
         call HANDLE_ERR('NETCDF_READ no time given for time-dep var '//
     $                   var_name(:slen), NF_NOERR )
      end if
      alloc_flag = ALLOCATED( times )

      if( time_dependent ) then
!-----------------------------------------------------------------------
!	... Get time "dimension" size and find the time point index
!-----------------------------------------------------------------------
	 if( alloc_flag ) DEALLOCATE( times )
	 ALLOCATE( times(max_tim_ind), stat = rcode )
         if( rcode /= 0 ) then
            call HANDLE_ERR( 'NETCDF_READ allocating times for var '//
     $                       var_name(:slen), NF_NOERR )
         end if
	 start_index(1) = 1
	 dim_ext(1) = max_tim_ind
	 rcode = NF_GET_VARA_DOUBLE(
     $               nc_file_id,
     $               read_time_id,
     $               start_index,
     $               dim_ext,
     $               times )
         if( rcode /= NF_NOERR ) then
	    DEALLOCATE( times )
            call HANDLE_ERR( 'NETCDF_READ reading time '//
     $                       'for var '//var_name(:slen), rcode )
         end if
	 do tim_ind = 1,max_tim_ind
	    if( time == times(tim_ind) ) exit
	 end do
	 if( tim_ind > max_tim_ind ) then
            write(*,*) 'NETCDF_READ for var '//var_name(:slen)//': '
            write(*,*) '      input time= ',time,' not found in file '
     $                                            // nc_filespec       
            stop 'NETCDF_READ did not find input time in netCDF file'
	    DEALLOCATE( times )
	    return
	 end if
      end if
!--------------------------------------------------------------------------
!	... Read variable
!--------------------------------------------------------------------------
      if( time_dependent ) then
	 uplim = var_dims - 1
      else
	 uplim = var_dims
      end if
!--------------------------------------------------------------------------
!	... Check for arg validity; exactly one must exist
!--------------------------------------------------------------------------
      arg_valid(1) = PRESENT( matrix )
      arg_valid(2) = PRESENT( vector )
      arg_valid(3) = PRESENT( scalar )
      if( COUNT( arg_valid ) /= 1 ) then
         call HANDLE_ERR( 'NETCDF_READ reading var '//var_name(:slen)
     $                  // ' :wrong nb of args', rcode )
      end if

!--------------------------------------------------------------------------
!	... Read scalar variable
!--------------------------------------------------------------------------
      if( PRESENT( scalar ) ) then
         rcode = NF_GET_VAR1_DOUBLE(
     $                   nc_file_id,
     $                   var_id,
     $                   nf1,
     $                   scalar )
         if( alloc_flag ) DEALLOCATE( times )
         if( rcode /= NF_NOERR ) then
            call HANDLE_ERR( 'NETCDF_READ reading scalar var '//
     $                           var_name(:slen), rcode )
         end if
         return
      end if

!--------------------------------------------------------------------------
!	...Prepare reading arrays: find dimensions of array variable
!--------------------------------------------------------------------------
      do i = 1,uplim
         rcode = NF_INQ_DIM( nc_file_id,
     $                       var_dim_ids(i),
     $                       name,
     $                       dim_ext(i) )
         if( rcode /= NF_NOERR ) then
            if( alloc_flag ) then
               DEALLOCATE( times )
            end if
            call HANDLE_ERR( 'NETCDF_READ getting a dim id '//
     $                       'for var '//var_name(:slen), rcode )
         end if
      end do
      if( PRESENT( matrix ) ) then
         siz(1) = SIZE( matrix, 1 )
         siz(2) = SIZE( matrix, 2 )
         if( siz(1) /= dim_ext(1) .or. siz(2) /= dim_ext(2) ) then
            if( alloc_flag ) then
               DEALLOCATE( times )
            end if
            call HANDLE_ERR( 'NETCDF_READ dims do not match for'//
     $                       ' matrix '//var_name(:slen),NF_NOERR)
         end if
       else if( PRESENT( vector ) ) then
         siz(1) = SIZE( vector, 1 )
         if( siz(1) /= dim_ext(1) ) then
            if( alloc_flag ) DEALLOCATE( times )
            call HANDLE_ERR( 'NETCDF_READ dim do not match for'//
     $                       ' vector '//var_name(:slen),NF_NOERR)
         end if
       else
         if( alloc_flag ) DEALLOCATE( times )
         call HANDLE_ERR( 'NETCDF_READ internal error for array var '//
     $                    var_name(:slen), NF_NOERR)
      end if
      start_index(1:uplim) = 1
      if( time_dependent ) then
         start_index(var_dims) = tim_ind
         dim_ext(var_dims) = 1              ! assume last index == time
      end if

!--------------------------------------------------------------------------
!	... Read matrix variable
!--------------------------------------------------------------------------
      if( PRESENT( matrix ) ) then
         rcode = NF_GET_VARA_DOUBLE(
     $                  nc_file_id,
     $                  var_id,
     $                  start_index,
     $                  dim_ext,
     $                  matrix )
         if( alloc_flag ) DEALLOCATE( times )
         if( rcode /= NF_NOERR ) then
            call HANDLE_ERR( 'NETCDF_READ reading matrix var '//
     $                        var_name(:slen), rcode )
         end if
         return
      end if
         
!--------------------------------------------------------------------------
!	... *or* Read vector variable
!--------------------------------------------------------------------------
      if( PRESENT( vector ) ) then
         rcode = NF_GET_VARA_DOUBLE(
     $                  nc_file_id,
     $                  var_id,
     $                  start_index,
     $                  dim_ext,
     $                  vector )
         if( alloc_flag ) DEALLOCATE( times )
         if( rcode /= NF_NOERR ) then
            call HANDLE_ERR( 'NETCDF_READ reading vector var '//
     $                        var_name(:slen), rcode )
         end if
         return
      end if

      end subroutine NETCDF_READ

!=======================================================================

      subroutine SET_TIME( time )
!--------------------------------------------------------------------------
!	... Set the internal time variable
!--------------------------------------------------------------------------

      implicit none

!--------------------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------------------
      real, intent(in) :: time           ! days since 0/0/0

      output_time = time

      end subroutine SET_TIME

!=======================================================================

      subroutine SET_FILE( filespec )
!--------------------------------------------------------------------------
!	... Set the netcdf filespec
!--------------------------------------------------------------------------

      implicit none

!--------------------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------------------
      character(len=*), intent(in) :: filespec

!--------------------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------------------
      integer(kind=nf) :: i

!--------------------------------------------------------------------------
!	... Check against active files
!--------------------------------------------------------------------------
      i = FILE_CHECK( filespec )
      if( i /= 0 ) then
         nc_filespec = filespec
	 nc_file_id = file_ids(i)
      else
         call HANDLE_ERR( 'SET_FILE:  '//filespec(:LEN_TRIM(filespec))
     $                  //' not in active list', NF_NOERR)
      end if

      end subroutine SET_FILE

!=======================================================================

      subroutine OPEN_FILE( filespec, mode )
!--------------------------------------------------------------------------
!	... Open an existing file for reading or writing
!--------------------------------------------------------------------------
      
      implicit none

!--------------------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------------------
      integer(kind=nf), intent(in) :: mode
      character(len=*), intent(in) :: filespec

!--------------------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------------------
      integer(kind=nf) :: slen, rcode, fileid, fillmode

      slen = LEN_TRIM( filespec )
      if( slen == 0 ) then
	 write(*,*) ' OPEN_FILE : Null filespec'
	 stop
      end if
      rcode = FILE_CHECK( filespec )
      if( rcode /= 0 ) then
	 write(*,'('' OPEN_FILE : '',a,'' already open'')')
     $         filespec(:slen)
	 stop
      end if
      if( mode /= NF_NOWRITE .and. mode /= NF_WRITE ) then
	 write(*,'('' OPEN_FILE : '',i5,'' is invalid mode'')')
     $         mode
	 stop
      end if
      if( file_cnt < max_files ) then
	 rcode = NF_OPEN( filespec(:slen), mode, fileid )   ! open the file
         if( rcode /= NF_NOERR ) then
            call HANDLE_ERR( 'OPEN_FILE opening '//filespec(:slen), 
     $                        rcode )
         end if
	 if( mode == NF_WRITE ) then
	    rcode = NF_SET_FILL( fileid, NF_NOFILL, fillmode )  ! set no fill mode
            if( rcode /= NF_NOERR ) then
               call HANDLE_ERR( 'OPEN_FILE setting no fill mode for '//
     $                        filespec(:slen), rcode )
            end if
	 end if
	 file_cnt = file_cnt + 1
	 nc_filespec = filespec(:slen)                    ! make it active
	 nc_file_id = fileid
	 file_specs(file_cnt) = nc_filespec
	 file_ids(file_cnt) = nc_file_id
      else
	 write(*,'('' OPEN_FILE : Reached max number files'')')
      end if

      end subroutine OPEN_FILE

!=======================================================================

      subroutine CLOSE_FILE()
!--------------------------------------------------------------------------
!	... Close active netcdf file
!--------------------------------------------------------------------------

      implicit none

!--------------------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------------------
      integer(kind=nf) :: i, j, rcode

!--------------------------------------------------------------------------
!	... Check for file active
!--------------------------------------------------------------------------
      i = FILE_CHECK( nc_filespec )
      if( i /= 0 ) then
         rcode = NF_CLOSE( nc_file_id )
         do j = i,file_cnt-1
	    file_specs(j) = file_specs(j+1)
	    file_ids(j) = file_ids(j+1)
         end do
         file_cnt = file_cnt - 1
      else
	 write(*,'('' CLOSE_FILE: '',a,'' NOT ACTIVE'')')
     $        nc_filespec(:LEN_TRIM(nc_filespec))
      end if

      end subroutine CLOSE_FILE

!=======================================================================

      subroutine CLOSE_ALL_FILES()
!--------------------------------------------------------------------------
!	... Close all active netcdf files
!--------------------------------------------------------------------------

      implicit none

!--------------------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------------------
      integer(kind=nf) :: i, local_file_cnt

      if( file_cnt /= 0 ) then
	 local_file_cnt = file_cnt
	 do i = 1,local_file_cnt
	    call SET_FILE( file_specs(1) )
	    call CLOSE_FILE
	 end do
      else
	 write(*,'('' CLOSE_ALL_FILES: No active files'')')
      end if

      end subroutine CLOSE_ALL_FILES

!=======================================================================

      integer function FILE_CHECK( filespec )
!--------------------------------------------------------------------------
!	... Check for file activity and return index
!--------------------------------------------------------------------------

      implicit none

!--------------------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------------------
      character(len=*), intent(in) :: filespec

!--------------------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------------------
      integer :: i

!--------------------------------------------------------------------------
!	... Check against active files
!--------------------------------------------------------------------------
      FILE_CHECK = 0
      do i = 1,file_cnt
	 if( filespec(:LEN_TRIM(filespec)) == file_specs(i) ) then
	    FILE_CHECK = i
	    exit
	 end if
      end do

      end function FILE_CHECK

!=======================================================================

      subroutine HANDLE_ERR( loc, errno )
!--------------------------------------------------------------------------
!	... Print error and quit
!--------------------------------------------------------------------------

      implicit none

      character(len=*) :: loc
      integer(kind=nf), intent(in) :: errno

      character(len=80) :: errstring

      write(*,*) 'NETCDF Error in ',TRIM(ADJUSTL(loc))
      write(*,*) '   Active file: ',nc_filespec
      if( errno == NF_NOERR ) then
         write(*,*)'Error reported by code in NETCDF_UTILS module'
       else
         write(*,*) 'Error reported by netCDF library. Error message:'
         errstring = NF_STRERROR( errno )
         write(*,*) '      ' // errstring
      end if
      call CLOSE_ALL_FILES
      stop 'fatal error HANDLE_ERR in NETCDF_UTILS'

      end subroutine HANDLE_ERR

      end module NETCDF_UTILS
