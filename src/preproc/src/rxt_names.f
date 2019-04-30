! subversion Id for THIS file : $Id: rxt_names.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/rxt_names.f $
!-----------------------------------------------------------------------

      subroutine MAKE_RXT_NAME_MOD( rxtcnt, gascnt, phtcnt, rxt_alias )
!--------------------------------------------------------------------------------
!	... Makes a module of parameter reaction names
!--------------------------------------------------------------------------------

      use LIMITS

      implicit none

!--------------------------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------------------------
      integer, intent(in) :: rxtcnt, gascnt, phtcnt
      character(len=8), intent(inout) :: rxt_alias(rxt_lim)

!--------------------------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------------------------
      integer :: i
      character(len=80) :: buff
      character(len=5)  :: num
      logical :: lexist

!--------------------------------------------------------------------------------
!	... Check mod file existence; remove if found
!--------------------------------------------------------------------------------
      INQUIRE( file = 'rxt_names.mod', exist = lexist )
      if( lexist ) then
         call SYSTEM( 'rm rxt_names.mod' )
      end if
      OPEN( unit = 30, &
	    file = 'rxt_names.mod' )
             
      buff = ''
      write(30,'(a)') buff
      buff(7:) = 'module RXT_NAMES'
      write(30,'(a)') buff
      buff = ''
      write(30,'(a)') buff
      do i = 1,rxtcnt
	 if( rxt_alias(i) /= ' ' ) then
	    write(buff(7:),'(''integer, parameter :: rid_'',a,1x,''='',1x,i4)') &
	                rxt_alias(i)(:LEN_TRIM(rxt_alias(i))), i
	 else
	    write(num,'(i5)') i+10000
	    if( i <= phtcnt ) then
	       write(buff(7:),'(''integer, parameter :: rid_j'',a,1x,''='',1x,i4)') &
	                   num(2:5), i
	       write(rxt_alias(i)(:5),'(''j'',a)') num(2:5)
	    else
	       write(buff(7:),'(''integer, parameter :: rid_r'',a,1x,''='',1x,i4)') &
	                   num(2:5), i
	       write(rxt_alias(i)(:5),'(''r'',a)') num(2:5)
	    end if
	 end if
         write(30,'(a)') buff
      end do
      buff = ''
      write(30,'(a)') buff
      buff(7:) = 'end module RXT_NAMES'
      write(30,'(a)') buff
      CLOSE(30)

      end subroutine MAKE_RXT_NAME_MOD
