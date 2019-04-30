! subversion Id for THIS file : $Id: soc_chprs.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/soc_chprs.f $
!-----------------------------------------------------------------------

      subroutine TIMCON ( buff, &
                          time, &
                          lout )
!-----------------------------------------------------------------------
!       ... Convert incoming character time string to real time
!-----------------------------------------------------------------------

      implicit none

!-----------------------------------------------------------------------
!       ... Dummy arguments
!-----------------------------------------------------------------------
      integer, intent(in)           ::    lout          ! output unit number
      character (len=*), intent(in) ::    buff          ! input time character string
      real, intent(out)             ::    time          ! converted time in seconds


!-----------------------------------------------------------------------
!       ... Local variables
!-----------------------------------------------------------------------
      integer ::   retcod, l, i, j, k, slen
      integer ::   mnth
      real    ::   t0
      real    ::   days
      real    ::   ndym(12)
      character(len=80) ::  buffh
      character(len=8)  ::  number
      character(len=3)  ::  timsym(6)
      character(len=1)  ::  char
      logical ::   colon

!-----------------------------------------------------------------------
!       ... Function declarations
!-----------------------------------------------------------------------
      logical  ::  ISNUM

      data timsym / 'Y', 'MON', 'D', 'H', 'MIN', 'S' /
      data ndym / 31., 28., 31., 30., 31., 30., &
                  31., 31., 30., 31., 30., 31. /

      slen = LEN_TRIM( buff )
      l    = 0
      time = 0.
      colon = .false.
      buffh = buff(:slen)
      call UPCASE( buffh )
      do i = 1,slen
        char = buffh(i:i)
        if( char == ':' ) then
           colon = .true.
           go to 110
        end if
        l = l + 1
        number(l:l) = char
        if( i == slen ) then
           go to 110
        end if
        cycle

110     continue
        do k = l,1,-1
           if( ISNUM( number(k:k) ) ) then
              exit
           end if
        end do
        if( k /= l ) then
           do j = 1,6
              if( number(k+1:l) == timsym(j) ) then
                 exit
              end if
           end do
        else if( colon ) then
           call ERRMES( 'time format error in input #@', &
                        lout, &
                        buff, &
                        slen, &
                        ' ' )
        else
           j = 0
        end if

        if( j /= 2 ) then
           call RELCON( number, k, t0, retcod )
           if( retcod /= 0 ) then
            call ERRMES( 'number format error in time input #@', &
                         lout, &
                         number(:k), &
                         k, &
                         buff )
           end if
        else
           call INTCON( number, k, mnth, retcod )
           if( retcod /= 0 ) then
            call ERRMES( 'number format error in time input #@', &
                         lout, &
                         number(:k), &
                         k, &
                         buff )
           end if
        end if
        l = 0
        if( j == 1 ) then
           time = time + 3.1536e7*t0
        else if( j == 2 ) then
           days = 0.
           do k = 1,mnth-1
              days = days + ndym(k)
           end do
           time = time + 8.64e4*days
        else if( j == 3 ) then
           time = time + 8.64e4*t0
        else if( j == 4 ) then
           time = time + 3600.*t0
        else if( j == 5 ) then
           time = time + 60.*t0
        else
           time = time + t0
        end if
      end do

      end subroutine TIMCON

      subroutine CARDIN ( lin, &
                          card, &
                          chars )
!-----------------------------------------------------------------------
!       ... Cardin reads on logical input unit lin an 80 character
!           card image right filled with blanks.
!           The image has all imbedded whitespace removed and
!           non whitespace character count returned in chars
!-----------------------------------------------------------------------

      implicit none

!-----------------------------------------------------------------------
!       ... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in)  ::   lin
      integer, intent(out) ::   chars

      character(len=*), intent(out) ::  card

!-----------------------------------------------------------------------
!       ... Local variables
!-----------------------------------------------------------------------
      character(len=1) ::  chr
      character(len=6) ::  format

      integer      ::  i, slen
      integer      ::  ht = 9

      format = ' '
      slen = LEN( card )
      if( slen < 10 ) then
         write(format,'(''(a'',i1,'')'')') slen
      else if( slen < 100 ) then
         write(format,'(''(a'',i2,'')'')') slen
      else if( slen < 1000 ) then
         write(format,'(''(a'',i3,'')'')') slen
      end if
20    chars = 0
      read( unit=lin, fmt=format, end = 30) card
!-----------------------------------------------------------------------
!       ... Remove blanks and horizontal tabs
!-----------------------------------------------------------------------
      do i = 1,slen
        if( card(i:i) /= ' ' .and.  ICHAR( card(i:i) ) /= ht ) then
           chars = chars + 1
           chr   = card(i:i)
           card(chars:chars) = chr
        end if
      end do

!-----------------------------------------------------------------------
!       ... Ignore "blank" or comment card
!-----------------------------------------------------------------------
      if( card(:1) == '*' .or. card == ' ' ) then
         go to 20
      else
         if( chars == slen ) call ERRMES(                                 &
              'CARDIN: Line too long in driver file@',6, card, 1, card )
         card(chars+1:) = ' '
         write(*,*) 'CARDIN exit, read card(1:10)= ',card(1:10)
         return
      end if

30    call ERRMES( 'CARDIN: Unexpected EOF in driver file@', 6, card, 1, card )

      end subroutine CARDIN

      subroutine ERRMES( string, &
                         lout, &
                         instng, &
                         count, &
                         card )
!-----------------------------------------------------------------------
!     	... Print the input string error message
!           and halt the pre-processor
!-----------------------------------------------------------------------

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in) ::   count,  lout
      character(len=*), intent(in) :: string, instng, card

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer  ::  ls, i
      character(len=120)  :: copy

      copy = '0 *** '
      ls  = 6
      do i = 1,80
        if( string(i:i) == '@' ) then
          exit
        else if( string(i:i) == '#' ) then
          copy(ls+1:ls+count) = instng(:count)
          ls = ls + count
        else
          ls = ls + 1
          copy(ls:ls) = string(i:i)
        end if
      end do

      write (lout,'(a)') copy(:ls)
      if( card /= ' ' ) then
         write (lout,'('' input line:'')')
         write (lout,'(1x,a80)') card
      end if

      stop 'abort'

      end subroutine ERRMES

      subroutine ALTCON( string, &
                         ls, &
                         alt, &
                         retcod )
!-----------------------------------------------------------------------
!        altcon converts the input character string altcon
!        to a real number returned in alt.  The input string
!        must have length ls and can be of the following forms:
!                  1.  %km
!                  2.  %
!        where % is a generalized e format
!        Successful conversion returns zero in retcod and -12
!        otherwise
!
!     inputs:
!
!       string  =  character string to convert
!       ls      =  length of string (max value = 80)
!
!     outputs:
!
!       nout    =  integer value if retcod = 0
!       retcod  =  error flag; = 0 => proper format for string
!-----------------------------------------------------------------------

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in)  ::  ls
      integer, intent(out) ::  retcod
      real, intent(out)    ::  alt
      character(len=*), intent(in) ::  string

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer  ::  len

      if( string(ls:ls) == 'm' .or.  string(ls:ls) == 'M' ) then
        len = ls - 1
      else
        len = ls
      end if

      retcod = -12
      if( len /= 0 ) then
        call RELCON( string, &
                     len, &
                     alt, &
                     retcod )
        if( retcod == 0 ) then
          if( len == ls ) then
             alt = alt*1e5
          else
             alt = alt*1e2
          end if
        end if
      end if

      end subroutine ALTCON

      subroutine INTCON( string, &
                         ls, &
                         nout, &
                         retcod )
!-----------------------------------------------------------------------
!     intcon converts a character string of length ls to an integer
!     format errors are trapped and retcod is set to -12
!
!     inputs:
!
!       string  =  character string to convert
!       ls      =  length of string (max value = 80)
!
!     outputs:
!
!       nout    =  integer value if retcod = 0
!       retcod  =  error flag; = 0 => proper format for string
!-----------------------------------------------------------------------

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in)  :: ls
      integer, intent(out) :: nout, retcod
      character(len=*), intent(in) :: string

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer  ::  ios

      read (string(:ls),*,iostat=ios) nout
      if( ios == 0 ) then
         retcod = 0
      else
         retcod = -12
      end if

      end subroutine INTCON

      subroutine RELCON( string, &
                         ls, &
                         flpout, &
                         retcod )
!-----------------------------------------------------------------------
!     relcon converts a character string of length ls to a real number
!     format errors are trapped and retcod is set to -12
!
!     inputs:
!
!       string  =  character string to convert
!       ls      =  length of string (max value = 80)
!
!     outputs:
!
!       flpout  =  real value if retcod = 0
!       retcod  =  error flag; = 0 => proper format for string
!-----------------------------------------------------------------------

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in)  ::  ls
      integer, intent(out) ::  retcod
      real, intent(out)    ::  flpout
      character(len=*), intent(in) ::  string

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer  ::  ios, l

      l = LEN_TRIM( string(:ls) )
      if( l == 0 ) then
	 retcod = -12
      else
         read(string(:l),*,iostat=ios) flpout
         if( ios == 0 ) then
	    retcod = 0
         else
	    retcod = -12
         end if
      end if

      end subroutine RELCON

      subroutine NUMCON( string, &
                         num, &
                         jus )
!-----------------------------------------------------------------------
!     numcon converts a real number to a generalized
!     e format character string
!
!     inputs:
!
!      num      =  real number to convert 
!      jus      =  character code for output string justification
!                  'l' = left justify
!                  'c' = center
!                  'r' = right justify
!
!     outputs:
!
!       char    =  converted character string 
!-----------------------------------------------------------------------

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      real, intent(in) ::  num
      character(len=*), intent(inout) ::  string
      character(len=1), intent(in) ::  jus

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer  ::  wpart, il, i, iu
      real     ::  frac
      character(len=16) :: mask
      character(len=8)  :: copy(2)

      equivalence (mask,copy)

      if( num == 0. ) then
         string  =  '0'
      else
         wpart  =  INT( num )
         if( wpart == 0 ) then
            il  =  9
         else
            write(copy(1),'(i8)') wpart
            do i = 1,8
              if( copy(1)(i:i) /= ' ') then
                exit
              end if
            end do
            il  =  i
         end if
         frac  =  num - INT( num )
         if( frac == 0. ) then
            iu  =  8
         else
            if( wpart == 0 .and. frac < 0.e0 ) then
               write(copy(2),'(f8.6)') frac
            else
               write(copy(2),'(f8.7)') ABS( frac )
            end if
            do i = 8,2,-1
               if( copy(2)(i:i) /= '0') then
                 exit
               end if
            end do
            iu = i + 8
         end if
         string = mask(il:iu)
      end if
      if( jus == 'c' ) then
         call CENTER( string, 16 )
      end if

      end subroutine NUMCON

      subroutine PRTTIT( buffer, &
                         num, &
                         lout )

      implicit none

      integer      num,      lout

      character buffer(1)*80

      integer      i

      do i = 1,num
        write(lout,'(a80)') buffer(i)
      end do

      end subroutine PRTTIT

      subroutine CENTER( string, ls )
!-----------------------------------------------------------------------
!     	... Center character string of length ls
!-----------------------------------------------------------------------

      implicit none

!-----------------------------------------------------------------------
!     	... Dummy args
!-----------------------------------------------------------------------
      character(len=*), intent(inout) ::  string
      integer, intent(in) ::  ls

      integer      offset, i, il, iu, len, j

!-----------------------------------------------------------------------
!     	... Local variables
!-----------------------------------------------------------------------
      character(len=120) :: copy

      if( LEN_TRIM( string(:ls) ) == 0 ) then
         return
      end if
      copy(:ls) = string(:ls)
      do i = 1,ls
        if( copy(i:i) /= ' ' ) then
           exit
        end if
      end do

      il = i
      do i = ls,il,-1
        if( copy(i:i) /= ' ' ) then
           exit
        end if
      end do

      iu  = i
      len = i - il + 1
      offset = (ls - len)/2 + 1
      string(:ls) = ' '
      do i = il,iu
        j = offset + i - il
        string(j:j) = copy(i:i)
      end do

      end subroutine CENTER

      integer function ALTCHK( alt, sptgrd, np )
      
      implicit none

      integer      np
      real         alt, sptgrd(np)
      
      integer i
      
      do i = 1,np
         if( sptgrd(i) == alt ) then
            ALTCHK = 1
            return
         end if
      end do

      ALTCHK = 0
      
      end function ALTCHK

      subroutine UPCASE( string )
!----------------------------------------------------------------------
!       ... Convert character string "string" to upper case
!----------------------------------------------------------------------
      implicit none

!----------------------------------------------------------------------
!       ... Dummy args
!----------------------------------------------------------------------
      character(len=*), intent(inout) :: string

!----------------------------------------------------------------------
!       ... Local variables
!----------------------------------------------------------------------
      integer ::  i

      do i = 1,LEN_TRIM( string )
         if( ICHAR(string(i:i)) >= 97 .and.  ICHAR(string(i:i)) <= 122 ) then
            string(i:i) = CHAR(ICHAR(string(i:i)) - 32)
         end if
      end do

      end subroutine UPCASE

      subroutine PARSE_FLPTH( fullpath, filename, filepath )

      implicit none

!-------------------------------------------------------
!       ... Dummy args
!-------------------------------------------------------
      character(len=*), intent(in) ::    fullpath   ! incoming full pathname
      character(len=*), intent(out) ::   filename   ! the file name
      character(len=*), intent(out) ::   filepath   ! the file path

!-------------------------------------------------------
!       ... Local variables
!-------------------------------------------------------
      integer  ::    i

      do i = LEN_TRIM(fullpath),1,-1
         if( fullpath(i:i) == '/' ) then
            exit
         end if
      end do

      filename = fullpath(i+1:)
      if( i /= 0 ) then
         filepath = fullpath(:i) 
      else
         filepath = ' '
      end if

      end subroutine PARSE_FLPTH

      subroutine GETHOSTNAME( hostname )
!-------------------------------------------------------
!       ... Get host machine name
!-------------------------------------------------------

      implicit none

!-------------------------------------------------------
!       ... Dummy args
!-------------------------------------------------------
      character(len=16), intent(out) ::   hostname

!-------------------------------------------------------
!       ... Local variables
!-------------------------------------------------------
      logical :: lexist

      INQUIRE( file = 'ctm.tmp', exist = lexist )

      if( lexist ) then
         call SYSTEM( 'rm ctm.tmp' )
      end if

      CLOSE( 40 )
      OPEN( file   = 'ctm.tmp', & 
            unit   = 40, & 
            status = 'new' )
      call SYSTEM( 'hostname > ctm.tmp' )
      REWIND 40
      read(40,'(a)')  hostname
      CLOSE(40)
      call SYSTEM( 'rm ctm.tmp' )

      end subroutine GETHOSTNAME

      subroutine GETJOBNAME( jobname )

      implicit none

      character*16   jobname

      logical lexist

      INQUIRE( file = 'ctm.tmp', exist = lexist )

      if( lexist ) then
         call SYSTEM( 'rm ctm.tmp' )
      end if

      CLOSE( 40 )
      OPEN( file   = 'ctm.tmp', &
            unit   = 40, &
            status = 'new' )
      call SYSTEM( '/u/stacy/ctm/ftend/bin/jobname ctm.tmp' )
      REWIND 40
      read(40,'(a)')  jobname
      CLOSE(40)
      call SYSTEM( 'rm ctm.tmp' )

      end subroutine GETJOBNAME

      logical function ISNUM( chr )
!-------------------------------------------------------
!	... Check incoming character for numeric status
!-------------------------------------------------------

      implicit none

!-------------------------------------------------------
!	... Dummy args
!-------------------------------------------------------
      character(len=1), intent(in) ::   chr

      if( chr <= '9' .and. chr >= '0' ) then
         ISNUM = .true.
      else
         ISNUM = .false.
      end if

      end function ISNUM

      subroutine MKDATE( time, date )
!-------------------------------------------------------
!       ... Change numeric time into character date
!-------------------------------------------------------

      implicit none

!-------------------------------------------------------
!       ... Dummy args
!-------------------------------------------------------
      real, intent(in) ::     time                ! time to convert in seconds
      character(len=6), intent(out) :: date    ! date in form yymmdd

!-------------------------------------------------------
!       ... Local variables
!-------------------------------------------------------
      integer  :: itime
      integer  :: mnth
      integer  :: mdys(0:12) = &
         (/ 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 /)
      real     :: ttime

      itime = INT( time/3.1536e7 )
      if( itime > 1900 ) then
         itime = itime - 1900
      end if
      write(date(1:2),'(i2)') itime

      ttime = time - FLOAT(itime)*3.1536e7
      itime = INT( ttime/8.64e4 )
      do mnth = 1,12
         if( itime <= mdys(mnth) ) then
            exit
         end if
      end do

      mnth = MAX( 1,MIN( 12, mnth ) )
      if( mnth < 10 ) then
         write(date(3:4),'(''0'',i1)') mnth
      else
         write(date(3:4),'(i2)') mnth
      end if

      itime = itime - mdys(mnth-1)
      if( itime < 10 ) then
         write(date(5:6),'(''0'',i1)') itime
      else
         write(date(5:6),'(i2)') itime
      end if
      
      end subroutine MKDATE

      integer function INCLIST( target, list, cnt )
!-------------------------------------------------------
!       ... Check for match in character list
!-------------------------------------------------------

      implicit none

!-------------------------------------------------------
!       ... Dummy args
!-------------------------------------------------------
      integer, intent(in) ::  cnt              ! no elements in list
      character(len=*), intent(in) :: target   ! match string
      character(len=*), intent(in) :: list(*)  ! list to search

!-------------------------------------------------------
!       ... Local variables
!-------------------------------------------------------
      integer :: i

      INCLIST = 0
      do i = 1,cnt
         if( target == list(i) ) then
            INCLIST = i
            exit
         end if
      end do

      end function INCLIST

      integer function INILIST( target, list, cnt )
!-------------------------------------------------------
!       ... Check for match in integer list
!-------------------------------------------------------

      implicit none

!-------------------------------------------------------
!       ... Dummy args
!-------------------------------------------------------
      integer, intent(in) ::    cnt      ! no elements in list
      integer, intent(in) ::    target   ! match integer
      integer, intent(in) ::    list(*)  ! list to search

!-------------------------------------------------------
!       ... Local variables
!-------------------------------------------------------
      integer :: i

      INILIST = 0
      do i = 1,cnt
         if( target == list(i) ) then
            INILIST = i
            exit
         end if
      end do

      end function INILIST

      subroutine R2C( string, num, jus )
!-----------------------------------------------------------------------
!     r2c converts a real number to a generalized e format character string
!
!     inputs:
!
!      num      =  real number to convert 
!      jus      =  character code for output string justification
!                  'l' = left justify
!                  'c' = center
!                  'r' = right justify
!
!     outputs:
!
!       char    =  converted character string 
!-----------------------------------------------------------------------

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      real, intent(in) ::  num
      character(len=*), intent(out) ::  string
      character(len=1), intent(in)  ::  jus

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer  ::  wpart, il, i, iu
      integer  ::  power
      real     ::  frac
      real     ::  wrk_num
      real     ::  epsilon = .0000005
      character(len=24) :: mask
      character(len=8)  :: copy(3)

      equivalence (mask,copy)

      if( num == 0. ) then
         string  =  '0'
      else
         wrk_num = num
         power = 0
         if( ABS(wrk_num) < 1.e-4 ) then
            power = 1
            wrk_num = wrk_num
            do while( power < 40  )
               wrk_num = 10.*wrk_num
               if( ABS(wrk_num) >= 1. ) then
                  go to 100
               end if
               power = power + 1
            end do
            string = '0'
            return
         end if
100      wrk_num = wrk_num + epsilon
         wpart  =  INT(wrk_num)
         if( wpart == 0 ) then
            il = 9
         else
            write(copy(1),'(i8)') wpart
            do i = 1,8
              if( copy(1)(i:i) /= ' ' ) then
	         exit
	      end if
            end do
            il = i
         end if
         frac  =  wrk_num - INT(wrk_num)
         if( frac == 0. ) then
            mask(9:9) = '.'
            iu = 9
         else
            if( wpart == 0 .and. frac < 0. ) then
               write(copy(2),'(f8.6)') frac
            else
               write(copy(2),'(f8.7)') ABS(frac)
            end if
            do i = 8,2,-1
               if( copy(2)(i:i) /= '0' ) then
	          exit
	       end if
            end do
            iu = i + 8
         end if
         if( frac /= 0. ) then
            if( mask(iu-3:iu-1) == '000') then
               iu = iu - 4
            else if( mask(iu-4:iu-2) == '000' ) then
               iu = iu - 5
            end if
         end if
         if( power /= 0 ) then
            if( power < 10 ) then
               write(mask(iu+1:),'(''e'',i2)') -power
               iu = iu + 3
            else
               write(mask(iu+1:),'(''e'',i3)') -power
               iu = iu + 4
            end if
         end if
         string = mask(il:iu)
      end if
      if( jus == 'c' ) then
         call CENTER( string, 16 )
      end if

      end subroutine R2C

      integer function XLATE( clsmap, match )
!------------------------------------------------------------------------
!	... Translate between overall indexing and method indexing
!------------------------------------------------------------------------
     
      use LIMITS, only : var_lim

      implicit none

!------------------------------------------------------------------------
!	... Dummy args
!------------------------------------------------------------------------
      integer, intent(inout) ::     match
      integer, intent(in)    ::     clsmap(var_lim,*)
      
!------------------------------------------------------------------------
!	... Local variables
!------------------------------------------------------------------------
      integer  :: class
      
      do class = 1,5
        if( clsmap(match,class) /= 0 ) then
           match = clsmap(match,class)
           XLATE = class
           return
        end if
      end do
      
      XLATE = 0
      
      end function XLATE
