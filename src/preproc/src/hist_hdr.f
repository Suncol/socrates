! subversion Id for THIS file : $Id: hist_hdr.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/hist_hdr.f $
!-----------------------------------------------------------------------
      subroutine HIST_HDR( histout_cnt, &
                           histout_map, &
                           user_hst_names, &
                           hist_type, &
			   dyn_hst_fld_cnt, &
                           spcsym, &
                           spccnt, &
                           hetmap, &
                           usrmap, &
                           ptplen )
!-----------------------------------------------------------------------
!        ... Process all output history tape controls
!-----------------------------------------------------------------------

      implicit none

!-----------------------------------------------------------------------
!        ... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in)  ::      histout_cnt(20,2)    ! number of outputs in each catagory
      integer, intent(in)  ::      histout_map(64,20,2) ! map of actual outputs
      integer, intent(in)  ::      dyn_hst_fld_cnt(2)
      integer, intent(in)  ::      spccnt(*)        ! number of symbols in each catagory
      integer, intent(in)  ::      hetmap(*)        ! wet dep map
      integer, intent(in)  ::      usrmap(*)        ! ext frc map
      integer, intent(out) ::      ptplen           ! total hist tape fields
      character(len=64), intent(in) :: hist_type         ! type of dyn hist tape ( short/long )
      character(len=8), intent(in)  :: user_hst_names(64,4)
      character(len=8), intent(in)  :: spcsym(64,*)     ! list of symbols

!-----------------------------------------------------------------------
!        ... Local variables
!-----------------------------------------------------------------------
      integer, parameter :: inst = 1, avgr = 2

      integer  ::  i, j, m, typind
      integer  ::  summ(2), sums(2)
      character(len=72) :: comment
      character(len=8)  :: namtag(64)
      logical  ::  lexist
      
      INQUIRE( file = 'hist.h', exist = lexist )
      if( lexist ) then
	 call SYSTEM( 'rm hist.h' )
      end if
      CLOSE(30)
      OPEN( unit = 30, file = 'hist.h' )

      comment = '##-------------------------------------------------'
      write(30,'(a)') comment
      write(30,'(''##          Set the history tape options'')')
      write(30,'(a)') comment
      summ(1) = histout_cnt(1,inst) + histout_cnt(2,inst) + histout_cnt(5,inst) &
           + histout_cnt(6,inst) + histout_cnt(8,inst) + histout_cnt(9,inst) &
           + histout_cnt(10,inst) + histout_cnt(11,inst) + histout_cnt(13,inst)
      summ(2) = histout_cnt(1,avgr) + histout_cnt(2,avgr) + histout_cnt(5,avgr) &
           + histout_cnt(6,avgr) + histout_cnt(8,avgr) + histout_cnt(9,avgr) &
           + histout_cnt(10,avgr) + histout_cnt(11,avgr) + histout_cnt(13,avgr)
      write(30,'(''# set PMULTI '',i3)') summ(1) + summ(2)
      write(30,'(''# set PMULTA '',i3)') summ(2)
      sums(1) = histout_cnt(3,inst) + histout_cnt(4,inst) + histout_cnt(7,inst) &
				    + histout_cnt(12,inst)
      sums(2) = histout_cnt(3,avgr) + histout_cnt(4,avgr) + histout_cnt(12,avgr)
      write(30,'(''# set PSINGL '',i3)') sums(1) + sums(2)
      write(30,'(''# set PSINGLA '',i3)') sums(2)
      ptplen = summ(1) + sums(1) + summ(2) + sums(2)
      write(30,'(''# set PTPLEN '',i3)') ptplen
      i = histout_cnt(1,inst) + histout_cnt(2,inst) &
        + histout_cnt(3,inst) + histout_cnt(4,inst) &
        + histout_cnt(8,inst) + histout_cnt(9,inst) &
        + histout_cnt(10,inst) + histout_cnt(11,inst) &
        + histout_cnt(12,inst) + histout_cnt(13,inst) &
        + histout_cnt(1,avgr) + histout_cnt(2,avgr) &
        + histout_cnt(3,avgr) + histout_cnt(4,avgr) &
        + histout_cnt(8,avgr) + histout_cnt(9,avgr) &
        + histout_cnt(10,avgr) + histout_cnt(11,avgr) &
        + histout_cnt(12,avgr) + histout_cnt(13,avgr)
      write(30,'(''# set HSTLEN '',i3)') i
      write(30,'(''# set HSTOFFSET '',i3)') SUM( histout_cnt(5:7,inst) )
      write(30,'(''# set HSTINSCNT '',i3)') SUM( histout_cnt(1:13,inst) )
      write(30,'(''# set HSTINSCNTM '',i3)') &
              SUM( histout_cnt(1:4,inst) ) &
            + SUM( histout_cnt(8:13,inst) )
      write(30,'(''# set HSTPHTCNT '',i3)') histout_cnt(8,inst)
      write(30,'(''# set HSTRXTCNT '',i3)') histout_cnt(9,inst)
      write(30,'(''# set HSTPHTCNTA '',i3)') histout_cnt(8,avgr)
      write(30,'(''# set HSTRXTCNTA '',i3)') histout_cnt(9,avgr)
      write(30,'(''# set HSTRXTIND '',i3)') SUM( histout_cnt(1:4,inst) )
      write(30,'(''# set HSTUSRINDI '',i3)') SUM( histout_cnt(1:4,inst) ) &
					     + SUM( histout_cnt(8:11,inst) )
      write(30,'(''# set HSTRXTINDA '',i3)') SUM( histout_cnt(1:4,inst) ) &
                                            + SUM( histout_cnt(8:13,inst) ) &
                                            + SUM( histout_cnt(1:4,avgr) )
      write(30,'(''# set HSTUSRINDA '',i3)') SUM( histout_cnt(1:4,inst) ) &
					     + SUM( histout_cnt(8:13,inst) ) &
					     + SUM( histout_cnt(1:11,avgr) )
      write(30,'(''# set HSTAVGCNT '',i3)') SUM( histout_cnt(1:11,avgr) )
      write(30,'(''# set HSTXPTICNT '',i3)') histout_cnt(1,inst)
      write(30,'(''# set HSTXPTACNT '',i3)') histout_cnt(1,avgr)
      write(30,'(''# set HSTPCEICNT '',i3)') histout_cnt(2,inst)
      write(30,'(''# set HSTPCEACNT '',i3)') histout_cnt(2,avgr)
      write(30,'(''# set HSTSFLXICNT '',i3)') histout_cnt(3,inst)
      write(30,'(''# set HSTSFLXACNT '',i3)') histout_cnt(3,avgr)
      write(30,'(''# set HSTDVELICNT '',i3)') histout_cnt(4,inst)
      write(30,'(''# set HSTDVELACNT '',i3)') histout_cnt(4,avgr)
      write(30,'(''# set HSTWDEPICNT '',i3)') histout_cnt(10,inst)
      write(30,'(''# set HSTWDEPACNT '',i3)') histout_cnt(10,avgr)
      write(30,'(''# set HSTEXTICNT '',i3)') histout_cnt(11,inst)
      write(30,'(''# set HSTEXTACNT '',i3)') histout_cnt(11,avgr)
      write(30,'(''# set HSTUSRSICNT '',i3)') histout_cnt(12,inst)
      write(30,'(''# set HSTUSRMICNT '',i3)') histout_cnt(13,inst)
      write(30,'(''# set HSTUSRSACNT '',i3)') histout_cnt(12,avgr)
      write(30,'(''# set HSTUSRMACNT '',i3)') histout_cnt(13,avgr)
      if( SUM( histout_cnt(12:13,inst) ) + SUM( histout_cnt(12:13,avgr) ) /= 0 ) then
         write(30,'(''# set HSTUSR true'')')
      else
         write(30,'(''# set HSTUSR false'')')
      end if
!-----------------------------------------------------------------------
!	... The input dynamics history tape field counts
!-----------------------------------------------------------------------
      write(30,'(''# set DHMULTI '',i3)') dyn_hst_fld_cnt(1)
      write(30,'(''# set DHSINGL '',i3)') dyn_hst_fld_cnt(2)

!-----------------------------------------------------------------------
!	... Group history output flag
!-----------------------------------------------------------------------
      if( (histout_cnt(2,inst) + histout_cnt(2,avgr)) /= 0 ) then
	 write(30,'(''# set GRPHST true'')')
      else
	 write(30,'(''# set GRPHST false'')')
      end if

      CLOSE(30)

!-----------------------------------------------------------------------
!        ... Write the history output tape information
!	     1. item count by category
!	     2. xported species
!	     3. "pce" species
!	     4. surface emissions
!	     5. deposition velocities
!	     6. washout rates
!	     7. "extraneous" forcing rates
!-----------------------------------------------------------------------
      if( ptplen /= 0 ) then
         OPEN( unit   = 30, &
               file   = 'sim.dat', &
               status = 'old', &
	       position = 'append' )
	 write(30,'(10i4)') histout_cnt
	 do typind = 1,2
	    if( histout_cnt(1,typind) /= 0 ) then
	       if( typind == inst ) then
	          write(30,'(10a8)')  (spcsym(histout_map(j,1,inst),6), &
                                           j = 1,histout_cnt(1,inst))
	       else
	          do i = 1,histout_cnt(1,avgr)
	             if( histout_map(i,1,avgr) < 10 ) then
	                write(namtag(i),'(''TRA'',i1)') histout_map(i,1,avgr)
	             else
	                write(namtag(i),'(''TRA'',i2)') histout_map(i,1,avgr)
	             end if
	          end do
	          write(30,'(10a8)')  (namtag(j), j = 1,histout_cnt(1,avgr))
	       end if
	    end if

	    if( histout_cnt(2,typind) /= 0 ) then
	       if( typind == inst ) then
	          write(30,'(10a8)')  (spcsym(histout_map(j,2,inst),7), &
                                        j = 1,histout_cnt(2,inst))
	       else
	          do i = 1,histout_cnt(2,avgr)
	             if( histout_map(i,2,avgr) < 10 ) then
	                write(namtag(i),'(''CEQA'',i1)') histout_map(i,2,avgr)
	             else
	                write(namtag(i),'(''CEQA'',i2)') histout_map(i,2,avgr)
	             end if
	          end do
	          write(30,'(10a8)')  (namtag(j), j = 1,histout_cnt(2,avgr))
	       end if
	    end if

	    if( histout_cnt(3,typind) /= 0 ) then
	       do i = 1,histout_cnt(3,typind)
	          if( histout_map(i,3,typind) < 10 ) then
		     if( typind == inst ) then
	                write(namtag(i),'(''SFLXI'',i1)') histout_map(i,3,inst)
		     else
	                write(namtag(i),'(''SFLXA'',i1)') histout_map(i,3,avgr)
		     end if
	          else
		     if( typind == inst ) then
	                write(namtag(i),'(''SFLXI'',i2)') histout_map(i,3,inst)
		     else
	                write(namtag(i),'(''SFLXA'',i2)') histout_map(i,3,avgr)
		     end if
	          end if
	       end do
	       write(30,'(10a8)')  (namtag(j), j = 1,histout_cnt(3,typind))
	    end if

	    if( histout_cnt(4,typind) /= 0 ) then
	       do i = 1,histout_cnt(4,typind)
	          if( histout_map(i,4,typind) < 10 ) then
		     if( typind == inst ) then
	                write(namtag(i),'(''DVELI'',i1)') histout_map(i,4,inst)
		     else
	                write(namtag(i),'(''DVELA'',i1)') histout_map(i,4,avgr)
		     end if
	          else
		     if( typind == inst ) then
	                write(namtag(i),'(''DVELI'',i2)') histout_map(i,4,inst)
		     else
	                write(namtag(i),'(''DVELA'',i2)') histout_map(i,4,avgr)
		     end if
	          end if
	       end do
	       write(30,'(10a8)')  (namtag(j), j = 1,histout_cnt(4,typind))
	    end if

	    if( histout_cnt(8,typind) /= 0 ) then
	       do i = 1,histout_cnt(8,typind)
	          j = histout_map(i,8,typind)
		  if( typind == inst ) then
	             if( j < 10 ) then
	                write(namtag(i),'(''JI-'',i1)') j
	             else if( j < 100 ) then
	                write(namtag(i),'(''JI-'',i2)') j
	             else if( j < 1000 ) then
	                write(namtag(i),'(''JI-'',i3)') j
	             end if
		  else
	             if( j < 10 ) then
	                write(namtag(i),'(''JA-'',i1)') j
	             else if( j < 100 ) then
	                write(namtag(i),'(''JA-'',i2)') j
	             else if( j < 1000 ) then
	                write(namtag(i),'(''JA-'',i3)') j
	             end if
		  end if
	       end do
	       write(30,'(10a8)')  (namtag(j), j = 1,histout_cnt(8,typind))
	    end if

	    if( histout_cnt(9,typind) /= 0 ) then
	       do i = 1,histout_cnt(9,typind)
	          j = histout_map(i,9,typind)
		  if( typind == inst ) then
	             if( j < 10 ) then
	                write(namtag(i),'(''RI-'',i1)') j
	             else if( j < 100 ) then
	                write(namtag(i),'(''RI-'',i2)') j
	             else if( j < 1000 ) then
	                write(namtag(i),'(''RI-'',i3)') j
	             end if
		  else
	             if( j < 10 ) then
	                write(namtag(i),'(''RA-'',i1)') j
	             else if( j < 100 ) then
	                write(namtag(i),'(''RA-'',i2)') j
	             else if( j < 1000 ) then
	                write(namtag(i),'(''RA-'',i3)') j
	             end if
		  end if
	       end do
	       write(30,'(10a8)')  (namtag(j), j = 1,histout_cnt(9,typind))
	    end if

	    if( histout_cnt(10,typind) /= 0 ) then
	       do i = 1,histout_cnt(10,typind)
	          j = hetmap(histout_map(i,10,typind))
		  if( typind == inst ) then
	             if( j < 10 ) then
	                write(namtag(i),'(''WTDEPI'',i1)') j
	             else
	                write(namtag(i),'(''WTDEPI'',i2)') j
	             end if
		  else
	             if( j < 10 ) then
	                write(namtag(i),'(''WTDEPA'',i1)') j
	             else
	                write(namtag(i),'(''WTDEPA'',i2)') j
	             end if
		  end if
	       end do
	       write(30,'(10a8)')  (namtag(j), j = 1,histout_cnt(10,typind))
	    end if

	    if( histout_cnt(11,typind) /= 0 ) then
	       do i = 1,histout_cnt(11,typind)
	          j = usrmap(histout_map(i,11,typind))
	          if( typind == inst ) then
	             if( j < 10 ) then
	                write(namtag(i),'(''EXTFRI'',i1)') j
	             else
	                write(namtag(i),'(''EXTFRI'',i2)') j
	             end if
	          else
	             if( j < 10 ) then
	                write(namtag(i),'(''EXTFRA'',i1)') j
	             else
	                write(namtag(i),'(''EXTFRA'',i2)') j
	             end if
	          end if
	       end do
	       write(30,'(10a8)')  (namtag(j), j = 1,histout_cnt(11,typind))
	    end if

	    if( histout_cnt(12,typind) /= 0 ) then
	       write(30,'(10a8)')  (user_hst_names(j,2*(typind-1)+1), j = 1,histout_cnt(12,typind))
	    end if

	    if( histout_cnt(13,typind) /= 0 ) then
	       write(30,'(10a8)')  (user_hst_names(j,2*(typind-1)+2), j = 1,histout_cnt(13,typind))
	    end if

	    do i = 1,4
	       if( histout_cnt(i,typind) /= 0 ) then
	          write(30,'(20i4)') (histout_map(j,i,typind),j = 1,histout_cnt(i,typind))
	       end if
	    end do
	    do i = 8,13
	       if( histout_cnt(i,typind) /= 0 ) then
	          write(30,'(20i4)') (histout_map(j,i,typind),j = 1,histout_cnt(i,typind))
	       end if
	    end do
	 end do
	 CLOSE(30)
      end if

      end subroutine HIST_HDR
