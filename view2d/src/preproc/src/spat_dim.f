! subversion Id for THIS file : $Id: spat_dim.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/spat_dim.f $
!-----------------------------------------------------------------------
      subroutine SPAT_DIMS( dimensions )
!-----------------------------------------------------------------------
!   	... Set the simulation spatial dimensions
!-----------------------------------------------------------------------

      use IO, only : lin, lout, buff

      implicit none

!-----------------------------------------------------------------------
!   	... Dummy args
!-----------------------------------------------------------------------
      integer, intent(out)             ::  dimensions(5)

!-----------------------------------------------------------------------
!   	... Local variables
!-----------------------------------------------------------------------
      integer  ::  kpar, nchar, retcod, i, k
      character(len=20) :: parkey(5),     keywrd
      logical  ::  found
      logical  ::  processed(5)

      parkey(1) = 'LONGITUDEPOINTS'
      parkey(2) = 'LATITUDEPOINTS'
      parkey(3) = 'VERTICALPOINTS'
      parkey(4) = 'NXPT'
      parkey(5) = 'JINTMX'

      processed = .false.
!-----------------------------------------------------------------------
!   	... Scan for valid numerical control parameter keyword
!-----------------------------------------------------------------------
      do
         call CARDIN( lin, buff, nchar )
	 call UPCASE( buff )
         if( buff == 'ENDENT' ) then
            return
	 end if
	 k = INDEX( buff(:nchar), '=' )
         if( k /= 0 ) then
	    keywrd = buff(:k-1)
            found = .false.
            do kpar = 1,5
               if( keywrd == parkey(kpar) ) then
		  found = .true.
		  exit
	       end if
	    end do
	    if( .not. found ) then
               call ERRMES ( ' # is an invalid numerical control' &
                          // ' parameter keyword@', lout, keywrd, &
                             LEN_TRIM(keywrd), buff )
	    end if
         else
!-----------------------------------------------------------------------
!  	... Invalid parameter keyword; terminate the program
!-----------------------------------------------------------------------
            call ERRMES ( ' numerical specification has no = operator@', &
                          lout, buff, 1, buff )
         end if

!-----------------------------------------------------------------------
!     	... Valid parameter keyword; now check for duplicate keyword
!-----------------------------------------------------------------------
         if( processed(kpar) ) then
            call ERRMES( '0 *** # has already been specified@', &
                          lout, parkey(kpar), k, ' ' )
         end if
         call INTCON ( buff(k+1:), &
                       nchar-k, &
                       dimensions(kpar), &
                       retcod )
!-----------------------------------------------------------------------
!     	... Check for numeric parameter syntax error
!-----------------------------------------------------------------------
         if( retcod /= 0 ) then
            call ERRMES ( ' # is an invalid real or integer in ' &
                        // 'numeric controls@', lout, buff(k+1:), &
                        LEN_TRIM( buff(k+1:)), buff )
         end if
         processed(kpar) = .true.
      end do

      end subroutine SPAT_DIMS
