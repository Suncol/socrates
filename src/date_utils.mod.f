! subversion Id for THIS file : $Id: date_utils.mod.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/date_utils.mod.f $
!-----------------------------------------------------------------------
      module DATE_UTILS
      
      use TIME_CONTROLS
      implicit none
      PRIVATE
      PUBLIC :: DATE2TIME, TIME2DATE, MDY

      CONTAINS  

!=======================================================================

      subroutine DATE2TIME( times )
!--------------------------------------------------------------
!	... Convert incoming date in days, months, years to
!           elapsed time since 0/0/0 
!           Note: this routine does not take leap years into account
!--------------------------------------------------------------

!--------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------
      type( TIMING ), intent(inout) :: times

!--------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------
      integer  ::  i, jul_day
      integer, save ::  mon(12) = (/ 31, 28, 31, 30, 31, 30,
     $                               31, 31, 30, 31, 30, 31 /)

      times%days0 = times%year * 365
      jul_day = 0
      do i = 1,times%month-1
	 jul_day = jul_day + mon(i) 
      end do
      times%cal_day = REAL( jul_day + times%day )
      times%days0 = times%days0 + jul_day + times%day

      end subroutine DATE2TIME

!=========================================================================== 

      subroutine TIME2DATE( times )
!--------------------------------------------------------------
!	... Convert incoming time to date in days, months, years
!           Note: this routine does not take leap years into account
!--------------------------------------------------------------

!--------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------
      type( TIMING ), intent(inout) :: times

!--------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------
      integer  ::  i, jul_day
      integer, save ::  mon(0:12) = (/ 0, 31, 59, 90, 120, 151, 181,
     $                                 212, 243, 273, 304, 334, 365 /)

      times%year = (times%days0 - 1) / 365
      jul_day = MOD( (times%days0 - 1),365 ) + 1
      times%cal_day = REAL( jul_day )
      do i = 11,1,-1
	 if( jul_day > mon(i) ) then
	    exit
	 end if
      end do
      times%month = MAX( i+1,1 )
      times%day = jul_day - mon(i)

      end subroutine TIME2DATE

!=========================================================================== 

      subroutine MDY( days0, lm, ld, ly )
!--------------------------------------------------------------
!	... Convert incoming date in days since 0/0/0 to a date
!           (lm,ld,ly) in days, months, years
!--------------------------------------------------------------

      implicit none

!--------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------
      integer, intent(in)  :: days0
      integer, intent(out) :: lm, ld, ly

!--------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------
      integer  ::  ldt, id, i
      integer, save ::  mon(12) = (/ 31, 28, 31, 30, 31, 30,
     $                               31, 31, 30, 31, 30, 31 /)

      ly = INT( days0 / 365. )
      ldt = days0 - 365*ly
      lm = 1
      ld = 1
      id = 0
      do i = 1,12
         id = id + mon(i)
         if( ldt <= id ) then
            ld = mon(i) - id + ldt
            exit
         end if
         lm = lm + 1
      end do

      end subroutine MDY

!=========================================================================== 

      end module DATE_UTILS
