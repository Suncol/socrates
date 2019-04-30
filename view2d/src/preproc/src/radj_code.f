! subversion Id for THIS file : $Id: radj_code.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/radj_code.f $
!-----------------------------------------------------------------------

      subroutine MAKE_RADJ( fixmap, &
                            fixcnt, &
                            rxmap, &
                            rxmcnt, &
                            phtcnt, &
                            rxt_alias, &
			    f90 )
!-----------------------------------------------------------------------
!        ... Write the reaction rate "adjustment" code
!-----------------------------------------------------------------------

      use LIMITS

      implicit none

!-----------------------------------------------------------------------
!        ... The arguments
!-----------------------------------------------------------------------
      integer, intent(in) ::  phtcnt
      integer, intent(in) ::  rxmcnt
      integer, intent(in) ::  fixcnt(2)
      integer, intent(in) ::  fixmap(var_lim,3,2)
      integer, intent(in) ::  rxmap(rxt_lim)
      character(len=8), intent(in) :: rxt_alias(rxt_lim)
      logical, intent(in) ::  f90

!-----------------------------------------------------------------------
!        ... The local variables
!-----------------------------------------------------------------------
      integer  ::   j, k, l, rxno, ll
      character(len=72) :: line
      character(len=5)  :: num
      logical  ::  first
      logical  ::  divide
      logical  ::  doloop
      logical  ::  lexist
      
      INQUIRE( file = 'adjrxt.F', exist = lexist )
      if( lexist ) then
	 call SYSTEM( 'rm adjrxt.F' )
      end if
      OPEN( unit = 30, file = 'adjrxt.F' )

      line = ' '
      write(30,100) line
      line(7:) = 'subroutine ADJRXT( rate'
      write(30,100) line
      line(6:) = '$,                  inv )'
      write(30,100) line
      line = ' '
      write(30,100) line
      if( f90 ) then
         line(7:) = 'use RXT_NAMES'
         write(30,100) line
         line = ' '
         write(30,100) line
      end if
      line(7:) = 'implicit none '
      write(30,100) line
      line = ' '
      write(30,100) line
      if( f90 ) then
         line = '!--------------------------------------------------------------------'
         write(30,100) line
         line = '!       ... Dummy args'
         write(30,100) line
         line = '!--------------------------------------------------------------------'
         write(30,100) line
         line = '      real, intent(in) ::      inv(PLEV,NFS)'
         write(30,100) line
         line = '      real, intent(inout) ::   rate(PLEV,RXNCNT)'
         write(30,100) line
         line = ' '
      else
         line = '!--------------------------------------------------------------------'
         write(30,100) line
         line = '!       ... Input args'
         write(30,100) line
         line = '!--------------------------------------------------------------------'
         write(30,100) line
         line = '      real      inv(PLEV,NFS)'
         write(30,100) line
         line = '!--------------------------------------------------------------------'
         write(30,100) line
         line = '!       ... Input/Output args'
         write(30,100) line
         line = '!--------------------------------------------------------------------'
         write(30,100) line
         line = '      real      rate(PLEV,RXNCNT)'
         write(30,100) line
         line = ' '
         write(30,100) line
         line = '!--------------------------------------------------------------------'
         write(30,100) line
         line = '!       ... Local variables'
         write(30,100) line
         line = '!--------------------------------------------------------------------'
         write(30,100) line
         line(7:) = 'integer   j'
      end if
      write(30,100) line
      line = ' '
      write(30,100) line
      
      
      first = .true.
      divide = .false.
      doloop = .false.
!---------------------------------------------------------
!	... First check reactions with invariants for 
!	    potential modification
!---------------------------------------------------------
      do j = 1,2
         do k = 1,fixcnt(j)
            rxno = ABS( fixmap(k,1,j) )
            if( j == 2 .or. rxno > phtcnt ) then
               if( first ) then
		  if( .not. f90 ) then
                     line(7:) = 'do j = 1,PLEV'
                     write(30,100) line
                     line = ' '
		  end if
                  first = .false.
                  doloop = .true.
               end if
               if( .not. divide .and. fixmap(k,1,j) < 0 ) then
		  if( .not. f90 ) then
                     line(10:) = 'im(j) = 1. / m(j)'
		  else
                     line(7:) = 'im(:) = 1. / m(:)'
		  end if
                  write(30,100) line
                  divide = .true.
               end if
               write(num,'(i5)') rxno
	       num = ADJUSTL( num )
	       ll = LEN_TRIM( num )
	       if( .not. f90 ) then
                  line(10:) = 'rate(j,'
	          line(LEN_TRIM(line)+1:) = num(:ll) // ') = rate(j,' // num(:ll) // ')'
	       else
                  line(7:) = 'rate(:,rid_' // rxt_alias(rxno)(:LEN_TRIM(rxt_alias(rxno))) &
			     // ') = rate(:,rid_' // rxt_alias(rxno)(:LEN_TRIM(rxt_alias(rxno))) // ')'
	       end if
               do l = 2,j+1
		  if( .not. f90 ) then
                     line(LEN_TRIM(line)+1:) = ' * inv(j,'
		  else
                     line(LEN_TRIM(line)+1:) = ' * inv(:,'
		  end if
                  write(num,'(i5)') fixmap(k,l,j)
	          num = ADJUSTL( num )
                  line(LEN_TRIM(line)+1:) = num(:LEN_TRIM(num)) // ')'
               end do
               if( fixmap(k,1,j) < 0 ) then
		  if( .not. f90 ) then
                     line(LEN_TRIM(line)+1:) = ' * im(j)'
		  else
                     line(LEN_TRIM(line)+1:) = ' * im(:)'
		  end if
               end if
               write(30,100) line
            end if
         end do
      end do

      if( doloop .and. .not. f90 ) then
         line = ' '
         line(7:) = 'end do'
         write(30,100) line
      end if
      line = ' '
      write(30,100) line
      if( f90 ) then
         line(7:) = 'end subroutine ADJRXT'
      else
         line(7:) = 'end'
      end if
      write(30,100) line
      
      CLOSE(30)
      
100   format(a72)      
      
      end subroutine MAKE_RADJ
