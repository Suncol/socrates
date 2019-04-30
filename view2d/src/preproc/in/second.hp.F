! subversion Id for THIS file : $Id: second.hp.F 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/in/second.hp.F $
!-----------------------------------------------------------------------
      real function SECOND()
!-------------------------------------------------------------
!	... Elapsed timer function
!-------------------------------------------------------------

      implicit none
      REAL*4 :: NULL = 0.

      SECOND = DBLE( SECNDS(NULL) )

      end function SECOND
