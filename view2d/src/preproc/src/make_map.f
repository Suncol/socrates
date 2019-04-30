! subversion Id for THIS file : $Id: make_map.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/make_map.f $
!-----------------------------------------------------------------------

      subroutine MAKE_MAP( cls_rxt_map, &
                           cls_rxt_cnt, &
                           clsno, &
                           rxno, &
                           cls_prd_cnt, &
                           template )
     
      use LIMITS, only : rxt_lim

      implicit none

!------------------------------------------------------------------------
!	... Dummy args
!------------------------------------------------------------------------
      integer, intent(in) ::   clsno,  rxno,  cls_prd_cnt
     
      integer, intent(out) ::   cls_rxt_cnt
      integer, intent(out) ::   cls_rxt_map(rxt_lim,7),  template(5,*)
     
!------------------------------------------------------------------------
!	... Local variables
!------------------------------------------------------------------------
      integer  ::  count
      integer  ::  row
      integer  ::  k, kp3
     
      count = 0
      cls_rxt_cnt = cls_rxt_cnt + 1
      row   = cls_rxt_cnt
      cls_rxt_map(row,1) = rxno
      do k = 1,4
         kp3 = k + 3
         if( template(k,2) == clsno ) then
            count = count + 1
            cls_rxt_map(row,kp3) = template(k,3)
            if( count == cls_prd_cnt ) then
               exit
            end if
         else
            cls_rxt_map(row,kp3) = -99
         end if
      end do

      end subroutine MAKE_MAP
