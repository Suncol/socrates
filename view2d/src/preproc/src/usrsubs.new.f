! subversion Id for THIS file : $Id: usrsubs.new.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/usrsubs.new.f $
!-----------------------------------------------------------------------

      subroutine USRSUBS ( sub_names, lib_names, lib_alias, promote, sub_cnt )
!-----------------------------------------------------------------------
!       ... Read the user subroutine filepaths
!-----------------------------------------------------------------------

      use IO

      implicit none

!-----------------------------------------------------------------------
!       ... Dummy args
!-----------------------------------------------------------------------
      integer, intent(out) ::  sub_cnt                    ! count of user subroutines
      character(len=64), intent(out) :: sub_names(*)      ! user filenames
      character(len=64), intent(out) :: lib_names(*)      ! library "aliased" routine
      logical, intent(out) ::  lib_alias(500)             ! flag for lib alaising
      logical, intent(out) ::  promote(500)               ! flag for routine promotion in order

!-----------------------------------------------------------------------
!       ... Local variables
!-----------------------------------------------------------------------
      integer  ::  sublim = 500
      integer  ::  nchar
      integer  ::  toklen(20)
      integer  ::  j, count, marker, begpos
      integer  ::  no_tokens

      character(len=120) :: temp
      character(len=120) :: tokens(20)

      logical  ::  lexist

      integer, parameter ::  symlen = 120

      sub_cnt = 0
      count = 0
!-----------------------------------------------------------------------
!       ... Read the subroutine pathnames
!-----------------------------------------------------------------------
      do
         call CARDIN ( lin, buff, nchar )
         buffh = buff
         call UPCASE( buffh )
         if( buffh /= 'ENDENT' ) then
            call GETTOKENS(  buff,     nchar,    ',',      symlen, &
                             tokens,   toklen,   20,       no_tokens )
            if( no_tokens == 0 ) then
               call ERRMES( ' Files input line in error@', lout, buff, 1, ' ' )
            end if
            do j = 1,no_tokens
               count = count + 1
               if( count > sublim ) then
                  call ERRMES( ' Files count exceeds limit@', lout, buff, 1, buff )
               end if
!-----------------------------------------------------------------------
!       ... Check for promotion operator
!-----------------------------------------------------------------------
               if( tokens(j)(:1) == '+' ) then
                  begpos = 2
		  promote(count) = .true.
		  temp = tokens(j)(2:)
		  tokens(j) = temp
	       else
                  begpos = 1
	       end if
!-----------------------------------------------------------------------
!       ... Check for alias
!-----------------------------------------------------------------------
	       marker = INDEX( tokens(j)(:toklen(j)),'->' )
	       if( marker /= 0 ) then
		  lib_alias(count) = .true.
		  promote(count) = .false.
		  lib_names(count) = tokens(j)(begpos:marker-1)
		  temp = tokens(j)(marker+2:toklen(j))
		  tokens(j) = temp
		  toklen(j) = toklen(j) - (marker + 1)
	       else
		  lib_alias(count) = .false.
	       end if
	       INQUIRE( file  = tokens(j)(:toklen(j)), exist = lexist )
	       if( .not. lexist ) then
                  call ERRMES( ' File # does NOT exist@', lout, tokens(j), toklen(j), buff )
	       end if
	       sub_names(count) = tokens(j)
            end do
            cycle
         else
            sub_cnt = count
	    exit
         end if
      end do

      end subroutine USRSUBS
