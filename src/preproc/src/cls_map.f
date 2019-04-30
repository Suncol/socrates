! subversion Id for THIS file : $Id: cls_map.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/cls_map.f $
!-----------------------------------------------------------------------

      subroutine CLS_MAPS( cls_rxt_map, &
                           cls_rxt_cnt, &
                           clsmap, &
                           rxmap, &
                           rxmcnt, &
                           prdmap, &
                           prdcnt, &
                           hetmap, &
                           hetcnt, &
                           usrmap, &
                           usrcnt, &
                           extcnt )
!-----------------------------------------------------------------------
!        ... Form the individual method reaction maps from the
!            overall reaction map
!-----------------------------------------------------------------------

      use LIMITS

      implicit none
     
!-----------------------------------------------------------------------
!        ... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in)  ::  prdcnt
      integer, intent(in)  ::  hetcnt
      integer, intent(in)  ::  usrcnt
      integer, intent(in)  ::  rxmap(rxt_lim,7,2), &
                               rxmcnt(2), &
                               prdmap(var_lim,5), &
                               hetmap(*), &
                               usrmap(*), &
			       clsmap(var_lim,5)
      integer, intent(out) ::  cls_rxt_map(rxt_lim,7,5), &
                               cls_rxt_cnt(4,5), &
                               extcnt(5)
     
!-----------------------------------------------------------------------
!        ... Local variables
!-----------------------------------------------------------------------
      integer  ::   template(5,3)
      integer  ::   i, j
      integer  ::   class
      integer  ::   index
      integer  ::   row
      integer  ::   rxno
      integer  ::   rxtnt(2), rxtnt_cls(2)
      
      integer  ::  XLATE
      
!-----------------------------------------------------------------------
!        ... Initialize all return arguments
!-----------------------------------------------------------------------
      cls_rxt_cnt = 0 ; cls_rxt_map = 0 ; extcnt = 0

!-----------------------------------------------------------------------
!   	... In the following the 1st column of template
!           represents the count of products in each class.
!           The 2nd column represents the class # of the product.
!           On input the 3rd column represents the master
!           product number and on output represents the product
!           number in the specific class.
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!        ... Scan the base "pure" production map
!            for class pure production entries
!-----------------------------------------------------------------------
      do i = 1,prdcnt
         template(:4,3) = prdmap(i,2:5)
         call PRD_MAP( template, clsmap )
	 rxno = prdmap(i,1)
         do class = 1,5
            if( template(class,1) /= 0 ) then
               call MAKE_MAP( cls_rxt_map(1,1,class), &
                              cls_rxt_cnt(1,class), &
                              class, &
                              rxno, &
                              template(class,1), &
                              template )
            end if
         end do
      end do

!-----------------------------------------------------------------------
!        ... Scan the base linear reaction map
!            for class pure production entries
!-----------------------------------------------------------------------
      do i = 1,rxmcnt(1)
         template(:4,3) = rxmap(i,3:6,1)
         call PRD_MAP( template, clsmap )
         rxtnt(1)     = ABS( rxmap(i,2,1) )
         rxtnt_cls(1) = XLATE( clsmap, rxtnt(1) )
	 rxno = rxmap(i,1,1)
         do class = 1,5
            if( class /= rxtnt_cls(1) .and. template(class,1) /= 0 ) then
               call MAKE_MAP( cls_rxt_map(1,1,class), &
                              cls_rxt_cnt(1,class), &
                              class, &
                              rxno, &
                              template(class,1), &
                              template )
               row = cls_rxt_cnt(1,class)
               cls_rxt_map(row,2,class) = ABS( rxmap(i,2,1) )
            end if
         end do
      end do

!-----------------------------------------------------------------------
!        ... Scan the base nonlinear reaction map
!            for class pure production entries
!-----------------------------------------------------------------------
      do i = 1,rxmcnt(2)
         template(:4,3) = rxmap(i,4:7,2)
         call PRD_MAP( template, clsmap )
         rxtnt     = ABS( rxmap(i,2:3,2) )
	 rxno = rxmap(i,1,2)
         do j = 1,2
            rxtnt_cls(j) = XLATE( clsmap, rxtnt(j) )
         end do
         do class = 1,5
            if( class /= rxtnt_cls(1) .and. class /= rxtnt_cls(2) .and. template(class,1) /= 0 ) then
               call MAKE_MAP( cls_rxt_map(1,1,class), &
                              cls_rxt_cnt(1,class), &
                              class, &
                              rxno, &
                              template(class,1), &
                              template )
               row = cls_rxt_cnt(1,class)
               cls_rxt_map(row,2:3,class) = ABS( rxmap(i,2:3,2) )
            end if
         end do
      end do
      
!-----------------------------------------------------------------------
!        ... Scan the base linear reaction map
!            for entries in the class linear map
!-----------------------------------------------------------------------
      do i = 1,rxmcnt(1)
         template(:4,3) = rxmap(i,3:6,1)
         call PRD_MAP( template, clsmap )
         rxtnt(1)     = ABS( rxmap(i,2,1) )
         class        = XLATE( clsmap, rxtnt(1) )
	 rxno         = rxmap(i,1,1)
         if( template(class,1) /= 0 ) then
            index = MAX( cls_rxt_cnt(1,class)+1,1 )
            call MAKE_MAP( cls_rxt_map(index,1,class), &
                           cls_rxt_cnt(2,class), &
                           class, &
                           rxno, &
                           template(class,1), &
                           template )
            row = cls_rxt_cnt(2,class) + index - 1
            cls_rxt_map(row,2,class) = rxmap(i,2,1)
         else if( rxmap(i,2,1) > 0 ) then
	    cls_rxt_cnt(2,class) = cls_rxt_cnt(2,class) + 1
            row = SUM( cls_rxt_cnt(1:2,class) )
            cls_rxt_map(row,1:2,class) = rxmap(i,1:2,1)
         end if 
      end do
      
!-----------------------------------------------------------------------
!        ... Scan the base nonlinear reaction map
!            for entries in the class linear map
!-----------------------------------------------------------------------
      do i = 1,rxmcnt(2)
         do j = 1,2
            rxtnt(j)     = ABS( rxmap(i,j+1,2) )
            rxtnt_cls(j) = XLATE( clsmap, rxtnt(j) )
         end do
         if( rxtnt_cls(1) /= rxtnt_cls(2) ) then
            template(:4,3) = rxmap(i,4:7,2)
            call PRD_MAP( template, clsmap )
            do j = 1,2
               class = rxtnt_cls(j)
               if( template(class,1) /= 0 ) then
                  index = MAX( cls_rxt_cnt(1,class) + 1, 1 )
                  call MAKE_MAP( cls_rxt_map(index,1,class), &
                                 cls_rxt_cnt(2,class), &
                                 class, &
                                 rxmap(i,1,2), &
                                 template(class,1), &
                                 template )
                  row = cls_rxt_cnt(2,class) + index - 1
                  cls_rxt_map(row,2,class) = rxmap(i,j+1,2)
                  if( j == 1 ) then
                     cls_rxt_map(row,3,class) = ABS( rxmap(i,3,2) )
                  else
                     cls_rxt_map(row,3,class) = ABS( rxmap(i,2,2) )
                  end if
               else if( rxmap(i,j+1,2) > 0 ) then
		  cls_rxt_cnt(2,class) = cls_rxt_cnt(2,class) + 1
                  row = cls_rxt_cnt(1,class) + cls_rxt_cnt(2,class)
                  cls_rxt_map(row,1,class) = rxmap(i,1,2)
                  if( j == 1 ) then
                     cls_rxt_map(row,2:3,class) = ABS( rxmap(i,2:3,2) )
                  else
                     cls_rxt_map(row,2:3,class) = ABS( rxmap(i,3:2:-1,2) )
                  end if
               end if
            end do
         end if
      end do

!-----------------------------------------------------------------------
!        ... Scan the base nonlinear reaction map
!            for entries in the class nonlinear map
!-----------------------------------------------------------------------
      do i = 1,rxmcnt(2)
         do j = 1,2
            rxtnt(j)     = ABS( rxmap(i,j+1,2) )
            rxtnt_cls(j) = XLATE( clsmap, rxtnt(j) )
         end do
         if( rxtnt_cls(1) == rxtnt_cls(2) ) then
            template(:4,3) = rxmap(i,4:7,2)
            call PRD_MAP( template, clsmap )
            class = rxtnt_cls(1)
            if( template(class,1) /= 0 ) then
               index = SUM( cls_rxt_cnt(1:2,class) ) + 1
               index = MAX( 1,index)
               call MAKE_MAP( cls_rxt_map(index,1,class), &
                              cls_rxt_cnt(3,class), &
                              class, &
                              rxmap(i,1,2), &
                              template(class,1), &
                              template )
               row = cls_rxt_cnt(3,class) + index - 1
               cls_rxt_map(row,2:3,class) = rxmap(i,2:3,2)
            else if( rxmap(i,2,2) > 0 .or. rxmap(i,3,2) > 0 ) then
	       cls_rxt_cnt(3,class) = cls_rxt_cnt(3,class) + 1
               row = SUM( cls_rxt_cnt(1:3,class) )
               cls_rxt_map(row,1:3,class) = rxmap(i,1:3,2)
            end if
         end if
      end do

!-----------------------------------------------------------------------
!        ... Scan the heterogeneous reactions
!-----------------------------------------------------------------------
      do i = 1,hetcnt
	 rxtnt(1)     = ABS( hetmap(i) )
         class        = XLATE( clsmap, rxtnt(1) )
	 cls_rxt_cnt(4,class) = cls_rxt_cnt(4,class) + 1
         index = SUM( cls_rxt_cnt(1:4,class) )
	 cls_rxt_map(index,1,class) = i
	 cls_rxt_map(index,2,class) = rxtnt(1)
      end do

!-----------------------------------------------------------------------
!        ... Scan the extraneous forcing
!-----------------------------------------------------------------------
      do i = 1,usrcnt
	 rxtnt(1)     = ABS( usrmap(i) )
         class        = XLATE( clsmap, rxtnt(1) )
	 extcnt(class) = extcnt(class) + 1
         index = SUM( cls_rxt_cnt(1:4,class) ) + extcnt(class)
	 cls_rxt_map(index,1,class) = i
	 cls_rxt_map(index,2,class) = rxtnt(1)
      end do

      end subroutine CLS_MAPS 
