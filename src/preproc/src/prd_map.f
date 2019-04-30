! subversion Id for THIS file : $Id: prd_map.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/prd_map.f $
!-----------------------------------------------------------------------

      subroutine PRD_MAP( template, clsmap )
!-----------------------------------------------------------------------
!	... Form production indicies
!-----------------------------------------------------------------------
     
      use LIMITS, only : var_lim
      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      integer, intent(inout) ::      template(5,*)
      integer, intent(in)    ::      clsmap(var_lim,*)
      
!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer  ::  i, clsno
      
      integer  ::  XLATE

      template(1:5,1:2) = 0

      do i = 1,4
         if( template(i,3) < 0 ) then
            cycle
         else if( template(i,3) == 0 ) then
            exit
         else
            clsno = XLATE( clsmap, template(i,3) )
            template(i,2) = clsno
            template(clsno,1) = template(clsno,1) + 1
         end if
      end do
      
      end subroutine PRD_MAP
