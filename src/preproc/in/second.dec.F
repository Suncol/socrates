! subversion Id for THIS file : $Id: second.dec.F 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/in/second.dec.F $
!-----------------------------------------------------------------------
      real function SECOND()
!-------------------------------------------------------------
!	... Elapsed timer function
!-------------------------------------------------------------

      implicit none

      real(4) :: times(2), eltime
      real(4) :: ETIME

      eltime = ETIME( times )
      SECOND = DBLE( eltime )

      end function SECOND
