      module NETCDF

      implicit none

!---------------------------------------------------------------------------     
!     	... Functions in the FORTRAN interface
!---------------------------------------------------------------------------     
      interface
	 integer function NCOPN( fname, omode, retcod )
	    character(len=*), intent(in) :: fname
	    integer, intent(in)  :: omode
	    integer, intent(out) :: retcod
	 end function NCOPN

	 integer function NCCRE( path, cmode, retcod )
	    character(len=*), intent(in) :: path
	    integer, intent(in)  :: cmode
	    integer, intent(out) :: retcod
	 end function NCCRE

	 integer function NCDDEF( ncid, dimnam, dimsiz, retcod )
	    character(len=*), intent(in) :: dimnam
	    integer, intent(in)  :: ncid
	    integer, intent(in)  :: dimsiz
	    integer, intent(out) :: retcod
	 end function NCDDEF

	 integer function NCDID( ncid, dimnam, retcod )
	    character(len=*), intent(in) :: dimnam
	    integer, intent(in)  :: ncid
	    integer, intent(out) :: retcod
	 end function NCDID

	 integer function NCVDEF( ncid, varnam, vartyp, nvdims,
     $                            vdims, retcod )
	    character(len=*), intent(in) :: varnam
	    integer, intent(in)  :: ncid
	    integer, intent(in)  :: vartyp
	    integer, intent(in)  :: nvdims
	    integer, intent(in)  :: vdims(*)
	    integer, intent(out) :: retcod
	 end function NCVDEF

	 integer function NCVID( ncid, varnam, retcod )
	    character(len=*), intent(in) :: varnam
	    integer, intent(in)  :: ncid
	    integer, intent(out) :: retcod
	 end function NCVID

	 integer function NCTLEN( type, retcod )
	    integer, intent(in)  :: type
	    integer, intent(out) :: retcod
	 end function NCTLEN

	 integer function NCSFIL( ncid, fillmode, retcod )
	    integer, intent(in)  :: ncid
	    integer, intent(in)  :: fillmode
	    integer, intent(out) :: retcod
	 end function NCSFIL
      end interface

      integer, parameter :: NCBYTE = 1
      integer, parameter :: NCCHAR = 2
      integer, parameter :: NCSHORT = 3
      integer, parameter :: NCLONG = 4
      integer, parameter :: NCFLOAT = 5
      integer, parameter :: NCDOUBLE = 6
      
!---------------------------------------------------------------------------     
!     	... Masks for the struct NC flag field; passed in as 'mode' arg to
!           nccreate and ncopen.
!---------------------------------------------------------------------------     
      
!---------------------------------------------------------------------------     
!     	... read/write, 0 => readonly 
!           in create phase, cleared by ncendef 
!---------------------------------------------------------------------------     
      integer, parameter :: NCRDWR = 1
      integer, parameter :: NCCREAT = 2
!     on create destroy existing file 
      integer, parameter :: NCEXCL = 4
!     in define mode, cleared by ncendef 
      integer, parameter :: NCINDEF = 8
!     synchronise numrecs on change (X'10')
      integer, parameter :: NCNSYNC = 16
!     synchronise whole header on change (X'20')
      integer, parameter :: NCHSYNC = 32
!     numrecs has changed (X'40')
      integer, parameter :: NCNDIRTY = 64
!     header info has changed (X'80')
      integer, parameter :: NCHDIRTY = 128
!     prefill vars on endef and increase of record, the default behavior
      integer, parameter :: NCFILL = 0
!     don't fill vars on endef and increase of record (X'100')
      integer, parameter :: NCNOFILL = 256
!     isa link (X'8000')
      integer, parameter :: NCLINK = 32768
      
!---------------------------------------------------------------------------     
!     	... 'mode' arguments for nccreate and ncopen
!---------------------------------------------------------------------------     
      integer, parameter :: NCNOWRIT = 0
      integer, parameter :: NCWRITE = NCRDWR
      integer, parameter :: NCCLOB = 11
      integer, parameter :: NCNOCLOB = 15

!---------------------------------------------------------------------------     
!     	... 'size' argument to ncdimdef for an unlimited dimension
!---------------------------------------------------------------------------     
      integer, parameter :: NCUNLIM = 0
      
!---------------------------------------------------------------------------     
!     	... attribute id to put/get a global attribute
!---------------------------------------------------------------------------     
      integer, parameter :: NCGLOBAL  = 0
!---------------------------------------------------------------------------     
!     	... Advisory Maximums
!---------------------------------------------------------------------------     
      integer, parameter :: MAXNCOP = 32
      integer, parameter :: MAXNCDIM = 32
      integer, parameter :: MAXNCATT = 512
      integer, parameter :: MAXNCVAR = 512
!---------------------------------------------------------------------------     
!     	... Not enforced 
!---------------------------------------------------------------------------     
      integer, parameter :: MAXNCNAM = 128
      integer, parameter :: MAXVDIMS = MAXNCDIM
      
!---------------------------------------------------------------------------     
!     	... The netcdf data types
!           Global netcdf error status variable
!           Initialized in error.c
!---------------------------------------------------------------------------     
!     No Error 
      integer, parameter :: NCNOERR = 0
!     Not a netcdf id 
      integer, parameter :: NCEBADID = 1
!     Too many netcdfs open 
      integer, parameter :: NCENFILE = 2
!     netcdf file exists && NCNOCLOB
      integer, parameter :: NCEEXIST = 3
!     Invalid Argument 
      integer, parameter :: NCEINVAL = 4
!     Write to read only 
      integer, parameter :: NCEPERM = 5
!     Operation not allowed in data mode 
      integer, parameter :: NCENOTIN = 6
!     Operation not allowed in define mode 
      integer, parameter :: NCEINDEF = 7
!     Coordinates out of Domain 
      integer, parameter :: NCECOORD = 8
!     MAXNCDIMS exceeded 
      integer, parameter :: NCEMAXDS = 9
!     String match to name in use 
      integer, parameter :: NCENAME = 10
!     Attribute not found 
      integer, parameter :: NCENOATT = 11
!     MAXNCATTRS exceeded 
      integer, parameter :: NCEMAXAT = 12
!     Not a netcdf data type 
      integer, parameter :: NCEBADTY = 13
!     Invalid dimension id 
      integer, parameter :: NCEBADD = 14
!     NCUNLIMITED in the wrong index 
      integer, parameter :: NCEUNLIM = 15
!     MAXNCVARS exceeded 
      integer, parameter :: NCEMAXVS = 16
!     Variable not found 
      integer, parameter :: NCENOTVR = 17
!     Action prohibited on NCGLOBAL varid 
      integer, parameter :: NCEGLOB = 18
!     Not a netcdf file 
      integer, parameter :: NCENOTNC = 19
      integer, parameter :: NCESTS = 20
      integer, parameter :: NCENTOOL = 21
      integer, parameter :: NCFOOBAR = 32
      integer, parameter :: NCSYSERR = -1
      
!---------------------------------------------------------------------------     
!     	... Global options variable. Used to determine behavior of error handler.
!           Initialized in lerror.c
!---------------------------------------------------------------------------     
      integer, parameter :: NCFATAL = 1
      integer, parameter :: NCVERBOS = 2

      end module NETCDF
