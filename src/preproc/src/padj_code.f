! subversion Id for THIS file : $Id: padj_code.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/padj_code.f $
!-----------------------------------------------------------------------

      subroutine MAKE_PADJ( fixmap, &
                            fixcnt, &
                            phtcnt, &
			    rxt_alias, &
			    f90 )
!-----------------------------------------------------------------------
!        ... Write the photorate adjustment code
!-----------------------------------------------------------------------

      use LIMITS

      implicit none

!-----------------------------------------------------------------------
!        ... The arguments
!-----------------------------------------------------------------------
      integer, intent(in) ::  fixcnt
      integer, intent(in) ::  phtcnt
      integer, intent(in) ::  fixmap(var_lim,2)
      character(len=8), intent(in) :: rxt_alias(rxt_lim)
      logical, intent(in) ::  f90

!-----------------------------------------------------------------------
!        ... The local variables
!-----------------------------------------------------------------------
      integer  ::   k, rxno
      character(len=72) :: line
      logical  ::  first
      logical  ::  lexist

      
      INQUIRE( file = 'phtadj.F', exist = lexist )
      if( lexist ) then
	 call SYSTEM( 'rm phtadj.F' )
      end if
      OPEN( unit = 30, file = 'phtadj.F' )

      line = ' '
      write(30,100) line
      line(7:) = 'subroutine PHTADJ( p_rate'
      write(30,100) line
      line(6:) = '$,                  inv )'
      write(30,100) line
      line = ' '
      write(30,100) line
      line(7:) = 'use RXT_NAMES'
      write(30,100) line
      line = ' '
      write(30,100) line
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
         line = '      real, intent(inout) ::      p_rate(PLEV,PHTCNT)'
      else
         line = '!--------------------------------------------------------------------'
         write(30,100) line
         line = '!       ... Input args'
         write(30,100) line
         line = '!--------------------------------------------------------------------'
         write(30,100) line
         line = '      real      inv(PLNPLV,NFS)'
         write(30,100) line
         line(7:) = 'real      m(PLNPLV)'
         write(30,100) line
         line = '!--------------------------------------------------------------------'
         write(30,100) line
         line = '!       ... Input/Output args'
         write(30,100) line
         line = '!--------------------------------------------------------------------'
         write(30,100) line
         line = '      real      p_rate(PLNPLV,PHTCNT)'
         write(30,100) line
         line = ' '
         write(30,100) line
         line = '!--------------------------------------------------------------------'
         write(30,100) line
         line = '!       ... Local variables'
         write(30,100) line
         line = '!--------------------------------------------------------------------'
         write(30,100) line
         line = '      integer   j'
         write(30,100) line
         line = '      real      im(PLNPLV)'
      end if
      write(30,100) line
      line = ' '
      write(30,100) line
      
      first = .true.
      do k = 1,fixcnt
         rxno = ABS( fixmap(k,1) )
         if( fixmap(k,1) < 0 .and. rxno <= phtcnt ) then
            if( first ) then
	       if( .not. f90 ) then
                  line(7:) = 'do j = 1,PLNPLV'
                  write(30,100) line
		  line = ' '
                  line(7:) = '   im(j) = 1. / m(j)'
                  write(30,100) line
	       end if
               line = ' '
               first = .false.
            end if
	    if( .not. f90 ) then
               line(10:) = 'p_rate(j,   ) = p_rate(j,   )'
               write(line(19:21),'(i3)') rxno
               write(line(35:37),'(i3)') rxno
               line(LEN_TRIM(line)+2:) = ' * inv(j,'
               write(line(LEN_TRIM(line)+1:),'(i2)') fixmap(k,2)
               line(LEN_TRIM(line)+1:) = ') * m(j)'
	    else
               line(7:) = 'p_rate(:,   ) = p_rate(:,   )'
               line(7:) = 'p_rate(:,rid_' // rxt_alias(rxno)(:LEN_TRIM(rxt_alias(rxno))) &
                          // ') = p_rate(:,rid_' // rxt_alias(rxno)(:LEN_TRIM(rxt_alias(rxno))) // ')'
!              write(line(16:18),'(i3)') rxno
!              write(line(32:34),'(i3)') rxno
               line(LEN_TRIM(line)+2:) = ' * inv(:,'
               write(line(LEN_TRIM(line)+1:),'(i2)') fixmap(k,2)
               line(LEN_TRIM(line)+1:) = ')'
	    end if
            write(30,100) line
         end if
      end do

      if( .not. f90 .and. .not. first ) then
         line = ' '
         line(7:) = 'end do'
         write(30,100) line
      end if
      line = ' '
      write(30,100) line
      if( f90 ) then
         line(7:) = 'end subroutine PHTADJ'
      else
         line(7:) = 'end'
      end if
      write(30,100) line
      
      CLOSE(30)
      
100   format(a72)      
      
      end subroutine MAKE_PADJ
