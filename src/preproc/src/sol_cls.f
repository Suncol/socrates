! subversion Id for THIS file : $Id: sol_cls.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/sol_cls.f $
!-----------------------------------------------------------------------

      subroutine SOL_CLS ( spccnt, &
                           spcsym, &
                           clscnt, &
                           clsmap )
!-----------------------------------------------------------------------
!	... Map solution species to solution method groups
!-----------------------------------------------------------------------

      use IO
      use LIMITS, only : var_lim

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in) ::  spccnt
      integer, intent(out) ::  clscnt(5), clsmap(var_lim,5,2)
      character(len=8), intent(in) ::  spcsym(*)

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer, parameter ::  symlen = 8

      integer  ::  kpar, i, parsw(5), nchar
      integer  ::  toklen(20)
      integer  ::  j, l
      integer  ::  no_tokens
      integer  ::  class
      character(len=16) :: tokens(20)
      character(len=10) :: clshdr(5) =  (/ 'EXPLICIT  ', 'EBI       ', &
					   'HOV       ', 'IMPLICIT  ', &
					   'RODAS     ' /)
      character(len=1)  :: char
      logical  :: found

      parsw  = 0 ; clscnt = 0 ; clsmap = 0

      call CARDIN( lin, buff, nchar )
      buffh = buff
      call UPCASE( buffh )
      if( buffh /= 'SOLUTIONCLASSES' ) then
         call ERRMES( '"Solution classes" card missing; run terminated@', &
                      lout, char, 1, buff )
      end if

      do
         call CARDIN(lin, buff, nchar )
         buffh = buff
         call UPCASE( buffh )
         if( buffh == 'ENDPAR' ) then
            exit
         end if

	 found = .false.
         do kpar = 1,5
           if( buffh == clshdr(kpar) ) then
	      found = .true.
	      exit
	   end if
         end do
	 if( .not. found ) then
            call ERRMES( '# is an invalid class header@',  &
                         lout, &
                         buff(:8), &
                         8, &
                         buff )
         else if( parsw(kpar) /= 0 ) then
            call ERRMES( '# solution class already declared@', &
                      lout, &
                      clshdr(kpar), &
                      LEN_TRIM(clshdr(kpar)), &
                      buff )
         else
            parsw(kpar) = 1
         end if

!-----------------------------------------------------------------------
!       ... Read the solution class members
!-----------------------------------------------------------------------
Methods : &
         do
            call CARDIN(lin, buff, nchar)
            buffh = buff
            call UPCASE( buffh )
            if( buffh /= 'ENDLST' ) then
               if( buffh(:nchar) == 'ALL' ) then
                  clscnt(kpar) = spccnt
                  do j = 1,spccnt
                     clsmap(j,kpar,1) = j
                     clsmap(j,kpar,2) = j
                  end do
                  cycle
               end if
               call GETTOKENS( buff, &
                               nchar, &
                               ',', &
                               symlen, &
                               tokens, &
                               toklen, &
                               20, &
                               no_tokens )
               if( no_tokens == 0 ) then
                  call ERRMES( ' Species input line in error@', lout, buff, 1, ' ' )
               end if

Tok_loop:      do j = 1,no_tokens
                  do l = 1,spccnt
                     if( tokens(j) == spcsym(l) ) then
                        clscnt(kpar) = clscnt(kpar) + 1
                        if( clscnt(kpar) > spccnt ) then
                           call ERRMES( ' Species count exceeds limit@', &
                                        lout, &
                                        buff, 1, buff )
                        end if
		        do class = 1,5
		           if( clsmap(l,class,1) /= 0 ) then
                              call ERRMES( ' # in two or more classes@', &
                                           lout, &
                                           tokens(j), &
                                           toklen(j), &
                                           buff )
		           end if
		        end do
                        clsmap(l,kpar,1)            = clscnt(kpar)
                        clsmap(clscnt(kpar),kpar,2) = l
                        cycle tok_loop
                     end if
                  end do
                  call ERRMES( ' Class member # not in solution list@', &
                               lout, &
                               tokens(j), &
                               toklen(j), &
                               buff )
               end do Tok_loop
	    else
	       exit
            end if
	 end do Methods
      end do

      end subroutine SOL_CLS
