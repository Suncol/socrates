! subversion Id for THIS file : $Id: hist_out.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/hist_out.f $
!-----------------------------------------------------------------------
      subroutine HIST_OUT( lin, &
                           lout, &
                           histout, &
                           histout_cnt, &
                           histout_map, &
                           user_hst_names, &
                           spcsym, &
                           spccnt, &
                           indexh2o, &
                           srf_flx_cnt, &
                           dvel_cnt, &
                           hetcnt, &
                           hetmap, &
                           usrcnt, &
                           usrmap, &
                           gascnt, &
                           phtcnt )
!-----------------------------------------------------------------------
!	... Process all output history tape controls
!-----------------------------------------------------------------------

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in)  ::  lin,      lout
      integer, intent(in)  ::  indexh2o              ! h2o in invariants
      integer, intent(in)  ::  srf_flx_cnt           ! species with srf flux
      integer, intent(in)  ::  dvel_cnt              ! species with dep vel
      integer, intent(in)  ::  hetcnt                ! cnt of hetero processes
      integer, intent(in)  ::  usrcnt                ! cnt of external frcing
      integer, intent(in)  ::  gascnt                ! cnt of total reaction rates
      integer, intent(in)  ::  phtcnt                ! cnt of total photo rates
      integer, intent(in)  ::  hetmap(*)             ! map of hetero processes
      integer, intent(in)  ::  usrmap(*)             ! map of ext frcing
      integer, intent(in)  ::  spccnt(*)
      character (len=8), intent(in) ::  spcsym(64,*)
      integer, intent(out) ::      histout_cnt(20,2)
      integer, intent(out) ::      histout_map(64,20,2)
      character (len=64), intent(out) :: histout(6)          ! hist tape outputs
      character (len=8), intent(out)  ::  user_hst_names(64,4)

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer, parameter :: inst = 1, avgr = 2
      integer, parameter :: singl = 1, multi = 2
      integer, parameter :: symlen = 8

      integer ::   kpar, nchar, k, j, khold
      integer ::   time_ind, level_ind
      integer ::   retcod
      integer ::   kindex
      integer ::   tokcnt, cnt
      integer ::   parsw(20,2)
      integer ::   toklen(20)
      real    ::   time
      character (len=80) :: buff
      character (len=80) :: buffh
      character (len=20) :: parkey(20),     keywrd
      character (len=8)  :: tokens(20)
      character (len=8)  :: temp

!-----------------------------------------------------------------------
!	... Function declarations
!-----------------------------------------------------------------------
      integer  ::  INCLIST
      integer  ::  INILIST

      parkey(1) = 'RETENTIONTIME'
      parkey(2) = 'WRITEFREQUENCY'
      parkey(3) = 'STARTFILENUMBER'
      parkey(4) = 'DENSITY'
      parkey(5) = 'PASSWORD'

      parkey(6) = 'TRANSPORTEDSPECIES'
      parkey(7) = 'GROUPMEMBERS'
      parkey(8) = 'SURFACEFLUX'
      parkey(9) = 'DEPOSITIONVELOCITY'
      parkey(10) = 'TEMPERATURE'
      parkey(11) = 'WATERVAPOR'
      parkey(12) = 'SURFACEPRESSURE'
      parkey(13) = 'PHOTORATES'
      parkey(14) = 'REACTIONRATES'
      parkey(15) = 'WASHOUTRATES'
      parkey(16) = 'EXTERNALFORCING'

      parkey(17) = 'DEFAULTOUTPUTS'
      parkey(18) = 'USERDEFINED'
      parkey(19) = 'PRINTFREQUENCY'

      parsw  = 0
      histout_cnt = 0

!-----------------------------------------------------------------------
!   	... Scan for valid option keyword
!-----------------------------------------------------------------------
      do
         call CARDIN( lin, buff, nchar )
	 buffh = buff
	 call UPCASE ( buffh )
         if( buffh == 'ENDENT' ) then
            return
	 end if
	 k = INDEX( buffh(:nchar), '=' )
         if( k /= 0 ) then
	    keywrd = buffh(:k-1)
         else
	    keywrd = buffh(:nchar)
	 end if
         do kpar = 1,19
            if( keywrd == parkey(kpar) ) then
	       exit
	    end if
	 end do
	 if( kpar > 19 ) then
!-----------------------------------------------------------------------
!  	... Invalid parameter keyword; terminate the program
!-----------------------------------------------------------------------
            call ERRMES ( ' # is an invalid job control' &
                       // ' parameter keyword@', &
                          lout, &
                          keywrd, &
                          LEN_TRIM(keywrd), &
                          buffh )
         end if

!-----------------------------------------------------------------------
!     	... Check for instantaneous or averaged qualifier
!-----------------------------------------------------------------------
         if( kpar > 5 .and. kpar < 19 ) then
	    if( k /= 0 ) then
	       if( kpar == 18 ) then
	          j = INDEX( buffh(:nchar), ',' )
		  if( j == 0 ) then
		     j = nchar
		  else
		     j = j - 1
		  end if
	       else
		 j = nchar
	       end if
	       if( buffh(k+1:j) == 'INST' ) then
	          time_ind = inst
	       else if( buffh(k+1:j) == 'AVGR' ) then
	          time_ind = avgr
	       else
                  call ERRMES( '0 *** # is invalid time qualifier@', &
                               lout, buff(k+1:j), j-k, buff )
	       end if
	       if( kpar == 18 ) then
	          k = INDEX( buffh(:nchar), ',' )
	          if( k /= 0 ) then
	             if( buffh(k+1:nchar) == 'SINGLE' ) then
	                level_ind = singl
	             else if( buffh(k+1:nchar) == 'MULTI' ) then
	                level_ind = multi
	             else
                        call ERRMES( '0 *** # is invalid level qualifier@', &
                                     lout, buff(k+1:), nchar-k, buff )
	             end if
		  else
		     level_ind = multi
	          end if
	       end if
	    else
	       if( kpar == 18 ) then
                  call ERRMES( ' User defined type must have time qualifier@', &
                               lout, buff, nchar, buff )
	       end if
	       time_ind = inst
	    end if
	 else
	    time_ind = inst
	 end if
!-----------------------------------------------------------------------
!     	... Valid parameter keyword; now check for duplicate keyword
!-----------------------------------------------------------------------
	 if( kpar == 18 ) then
            if( parsw(kpar,time_ind) == level_ind ) then
               call ERRMES( '0 *** # has already been specified@', &
                             lout, parkey(kpar), k, ' ' )
            end if
	    parsw(kpar,time_ind) = level_ind
	 else
            if( parsw(kpar,time_ind) /= 0 ) then
               call ERRMES( '0 *** # has already been specified@', &
                             lout, parkey(kpar), k, ' ' )
            end if
	    parsw(kpar,time_ind) = 1
         end if

!-----------------------------------------------------------------------
!     	... Set individual options
!-----------------------------------------------------------------------
	 if( kpar >= 6 ) then
!-----------------------------------------------------------------------
!     	... The "default" option
!-----------------------------------------------------------------------
	    if( kpar == 17 ) then
	       histout_cnt(1:20,time_ind) = 0
	       histout_cnt(1,time_ind) = spccnt(6)
	       histout_cnt(2,time_ind) = spccnt(7)
	       histout_cnt(3,time_ind) = spccnt(6)
	       histout_cnt(4,time_ind) = spccnt(6)
	       if( time_ind == inst ) then
	          histout_cnt(5,inst) = 1
	          if( indexh2o /= 0 ) then
	             histout_cnt(6,inst) = 1
	          end if
	          histout_cnt(7,inst) = 1
	       end if
	       do k = 1,spccnt(6)
		  histout_map(k,1,time_ind) = k
		  histout_map(k,3,time_ind) = k
		  histout_map(k,4,time_ind) = k
	       end do
	       do k = 1,spccnt(7)
		  histout_map(k,2,time_ind) = k
	       end do
!-----------------------------------------------------------------------
!     	... The printout frequency option
!-----------------------------------------------------------------------
	    else if( kpar == 19 ) then
	       call TIMCON( buff(k+1:nchar), &
                            time, &
                            lout )
	       histout(6) = buff(k+1:nchar)
!-----------------------------------------------------------------------
!     	... The temp, water vapor, and surf press options
!-----------------------------------------------------------------------
            else if( kpar >= 10 .and. kpar <= 12 .and. time_ind == inst ) then
	       histout_cnt(kpar-5,inst) = 1
!-----------------------------------------------------------------------
!     	... All other options
!-----------------------------------------------------------------------
	    else
	       call CARDIN( lin, buff, nchar )
	       buffh = buff
	       call UPCASE( buffh )
	       khold = kpar
	       do while( buffh /= 'ENDLST' )
		  kpar = khold
		  call GETTOKENS( buff, &
                                  nchar, &
                                  ',', &
                                  symlen, &
                                  tokens, &
                                  toklen, &
                                  20, &
                                  tokcnt )
		  if( tokcnt == 0 ) then
		     call ERRMES( ' Hist tape output list in error@', &
                                  lout, &
                                  buff, &
                                  1, &
                                  buff )
		  end if
		  if( kpar == 18 .and. histout_cnt(11+level_ind,time_ind) + tokcnt > 64 ) then
		     call ERRMES( ' Hist tape output list > 64 elements@', &
                                  lout, &
                                  buff, &
                                  1, &
                                  buff )
		  else if( histout_cnt(kpar-5,time_ind) + tokcnt > 64 ) then
		     call ERRMES( ' Hist tape output list > 64 elements@', &
                                  lout, &
                                  buff, &
                                  1, &
                                  buff )
		  end if
	          if( kpar == 18 ) then
	             do j = 1,tokcnt
	                cnt = histout_cnt(11+level_ind,time_ind) + 1
	                histout_cnt(11+level_ind,time_ind) = cnt
	                user_hst_names(cnt,2*(time_ind-1)+level_ind) = tokens(j)
	             end do
		  else if( kpar <= 9 .or. kpar >= 13 ) then
		     temp = tokens(1)
	             if( kpar /= 7 ) then
			kindex = 6
	             else
			kindex = 7
		     end if
	             kpar = kpar - 5
		     call UPCASE( temp )
!-----------------------------------------------------------------------
!     	... Handle the "all" list specifier
!-----------------------------------------------------------------------
		     if( tokcnt == 1 .and. temp == 'ALL' ) then
			if( kpar <= 4 ) then
			   do j = 1,spccnt(kindex)
			      histout_map(j,kpar,time_ind) = j
			   end do
			   histout_cnt(kpar,time_ind) = spccnt(kindex)
			else if( kpar >= 10 ) then
			   if( kpar == 10 ) then
			      do j = 1,hetcnt
			         histout_map(j,kpar,time_ind) = j
			      end do
			      histout_cnt(kpar,time_ind) = hetcnt
			   else if( kpar == 11 ) then
			      do j = 1,usrcnt
			         histout_map(j,kpar,time_ind) = j
			      end do
			      histout_cnt(kpar,time_ind) = usrcnt
			   end if
			end if
!-----------------------------------------------------------------------
!     	... Handle individual list elements
!-----------------------------------------------------------------------
		     else
			do j = 1,tokcnt
			   if( kpar == 8 .or. kpar == 9 ) then
	                      call INTCON( tokens(j), &      ! input string to convert
                                           toklen(j), &      ! length of input string
                                           k, &              ! surrogate for converted number
                                           retcod )          ! return code
	                      if( retcod /= 0 ) then
	                         call ERRMES( ' # is not a valid integer@', &
                                              lout, &
                                              tokens(j), &
                                              toklen(j), &
                                              buff )
	                      end if
			      if( kpar == 8 .and. k > phtcnt) then
	                         call ERRMES( ' # out of photolysis rate numbering@', &
                                              lout, &
                                              tokens(j), &
                                              toklen(j), &
                                              buff )
			      else if( k > gascnt ) then
	                         call ERRMES( ' # out of reaction rate numbering@', &
                                              lout, &
                                              tokens(j), &
                                              toklen(j), &
                                              buff )
			      end if
			      histout_cnt(kpar,time_ind) = histout_cnt(kpar,time_ind) + 1
			      histout_map(histout_cnt(kpar,time_ind),kpar,time_ind) = k
			   else
			      k = INCLIST( tokens(j), &
                                           spcsym(1,kindex), &
                                           spccnt(kindex) )
			      if( k == 0 ) then
			         call ERRMES( '# not in list@', &
                                              lout, &
                                              tokens(j), &
                                              toklen(j), &
                                              buff )
			      end if
			      if( kpar >= 10 ) then
			         if( kpar == 10 ) then
			            k = INILIST( k, hetmap, hetcnt )
			         else if( kpar == 11 ) then
			            k = INILIST( k, usrmap, usrcnt )
			         end if
			         if( k == 0 ) then
			            call ERRMES( '# not in list@', &
                                                 lout, &
                                                 tokens(j), &
                                                 toklen(j), &
                                                 buff )
			         end if
			      end if
			      histout_cnt(kpar,time_ind) = histout_cnt(kpar,time_ind) + 1
			      histout_map(histout_cnt(kpar,time_ind),kpar,time_ind) = k
			   end if
			end do
		     end if
		  end if
		  call CARDIN( lin, buff, nchar )
		  buffh = buff
		  call UPCASE( buffh )
	       end do
	    end if
	 else if( kpar <= 2 ) then
	    call TIMCON( buff(k+1:nchar), time, lout )
	    histout(kpar) = buff(k+1:nchar)
	 else if( kpar == 3 ) then
	    call INTCON( buff(k+1:nchar), & ! input string to convert
                         nchar - k,       & ! length of input string
                         toklen(1),       & ! surrogate for converted number
                         toklen(2) )        ! surrogate for error code
	    if( toklen(2) /= 0 ) then
	       call ERRMES( ' # is not a valid integer@', &
                            lout, &
                            buff(k+1:nchar), &
                            nchar - k, &
                            buff )
	    end if
	    histout(kpar) = buff(k+1:nchar)
	 else
	    histout(kpar) = buff(k+1:nchar)
	 end if
      end do

      end subroutine HIST_OUT
