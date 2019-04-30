! subversion Id for THIS file : $Id: exe_opts.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/exe_opts.f $
!-----------------------------------------------------------------------

      subroutine EXE_OPTS( options )
!-----------------------------------------------------------------------
!	... Set the execution options
!-----------------------------------------------------------------------

      use IO

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      logical, intent(out) ::  options(3)

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer  ::  kpar, nchar, k
      integer  ::  parsw(3)
      character(len=20) :: parkey(3), keywrd
      logical  :: found

      parkey(1) = 'QSUBFILE'
      parkey(2) = 'SUBMIT'
      parkey(3) = 'FIXER'

      parsw = 0

!-----------------------------------------------------------------------
!   	... Scan for valid option keyword
!-----------------------------------------------------------------------
      do
         call CARDIN( lin, buff, nchar )
	 buffh = buff
	 call UPCASE ( buffh )
         if( buffh == 'ENDENT' ) then
            return
	 end if
	 k = INDEX( buffh(:nchar), '=' )
         if( k /= 0 ) then
	    found = .false.
	    keywrd = buff(:k-1)
            do kpar = 1,6
               if( keywrd == parkey(kpar) ) then
		  found = .true.
		  exit
	       end if
	    end do
	    if( .not. found ) then
               call ERRMES ( ' # is an invalid options' &
                          // ' parameter keyword@', lout, keywrd, &
                             LEN_TRIM(keywrd), buff )
            end if
         else
!-----------------------------------------------------------------------
!  	... Invalid parameter keyword; terminate the program
!-----------------------------------------------------------------------
            call ERRMES ( ' option specification has no = operator@', &
                          lout, buff, 1, buff )
         end if

!-----------------------------------------------------------------------
!     	... Valid parameter keyword; now check for duplicate keyword
!-----------------------------------------------------------------------
         if( parsw(kpar) /= 0 ) then
            call ERRMES( '0 *** # has already been specified@', &
                          lout, parkey(kpar), k, ' ' )
         end if

!-----------------------------------------------------------------------
!     	... Set individual options
!-----------------------------------------------------------------------
	 if( buffh(k+1:nchar) == 'ON' .or. &
             buffh(k+1:nchar) == 'YES' ) then
	    options(kpar) = .true.
	 else
	    options(kpar) = .false.
	 end if
	 parsw(kpar) = 1
      end do

      end subroutine EXE_OPTS
