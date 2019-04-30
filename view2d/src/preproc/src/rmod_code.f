! subversion Id for THIS file : $Id: rmod_code.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/rmod_code.f $
!-----------------------------------------------------------------------

      subroutine MAKE_RMOD( rxt2rel_pntr, &
                            rel_rxt_map, &
                            rxt2grp_pntr, &
                            grp_rxt_map, &
                            hetmap, &
                            hetcnt, &
                            rxntot, &
                            grp_mem_cnt, &
                            relcnt, &
			    f90 )
!-----------------------------------------------------------------------
!        ... Write the reaction rate adjustment for groups code
!-----------------------------------------------------------------------

      use LIMITS

      implicit none

!-----------------------------------------------------------------------
!        ... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in) ::    rxt2rel_pntr(rxt_lim,2)
      integer, intent(in) ::    rel_rxt_map(rxt_lim,3,2)
      integer, intent(in) ::    rxt2grp_pntr(rxt_lim,2)
      integer, intent(in) ::    grp_rxt_map(rxt_lim,3,2)
      integer, intent(in) ::    hetmap(*)
      integer, intent(in) ::    rxntot 
      integer, intent(in) ::    hetcnt
      integer, intent(in) ::    grp_mem_cnt
      integer, intent(in) ::    relcnt
      logical, intent(in) ::    f90

!-----------------------------------------------------------------------
!        ... Local variables
!-----------------------------------------------------------------------
      integer  ::   k, l, ll, rxno, row, index
      character(len=72) :: line
      character(len=5)  :: num
      logical  ::  first
      logical  ::  found
      logical  ::  lexist
      
      INQUIRE( file = 'rxtmod.F', exist = lexist )
      if( lexist ) then
	 call SYSTEM( 'rm rxtmod.F' )
      end if
      OPEN( unit = 30, file = 'rxtmod.F' )

      line = ' '
      write(30,100) line(:LEN_TRIM(line))
      line(7:) = 'subroutine RXT_MOD( rate'
      write(30,100) line(:LEN_TRIM(line))
      if( hetcnt /= 0 ) then
         line(6:) = '$                    ,het_rates'
         write(30,100) line(:LEN_TRIM(line))
      end if
      if( relcnt /= 0 ) then
         line(6:) = '$                    ,rel_ratios'
         write(30,100) line(:LEN_TRIM(line))
      end if
      if( grp_mem_cnt /= 0 ) then
         line(6:) = '$                    ,grp_ratios'
         write(30,100) line(:LEN_TRIM(line))
      end if
      line(6:) = '$                    )'
      write(30,100) line(:LEN_TRIM(line))
      line = ' '
      write(30,100) line(:LEN_TRIM(line))
      line(7:) = 'implicit none '
      write(30,100) line(:LEN_TRIM(line))
      line = ' '
      write(30,100) line(:LEN_TRIM(line))
      if( f90 ) then
         line = '!----------------------------------------------'
	 write(30,100) line(:LEN_TRIM(line))
         line = '!       ... Dummy args'
	 write(30,100) line(:LEN_TRIM(line))
         line = '!----------------------------------------------'
	 write(30,100) line(:LEN_TRIM(line))
         line = '      real, intent(inout) ::      rate(PLEV,RXNCNT)'
         write(30,100) line(:LEN_TRIM(line))
	 if( hetcnt /= 0 ) then
            line = '      real, intent(inout) ::      het_rates(PLEV,HETCNT)'
            write(30,100) line(:LEN_TRIM(line))
         end if
         if( relcnt /= 0 ) then
            line = '      real, intent(in)    ::      rel_ratios(PLEV,RELCNT)'
            write(30,100) line(:LEN_TRIM(line))
	 end if
	 if( grp_mem_cnt /= 0 ) then
            line = '      real, intent(in)    ::      grp_ratios(PLEV,GRPCNT)'
            write(30,100) line(:LEN_TRIM(line))
	 end if
         line = ' '
	 write(30,100) line(:LEN_TRIM(line))
      else
         line = '!----------------------------------------------'
	 write(30,100) line(:LEN_TRIM(line))
         line = '!       ... Input/Output args'
	 write(30,100) line(:LEN_TRIM(line))
         line = '!----------------------------------------------'
	 write(30,100) line(:LEN_TRIM(line))
         line = '      real      rate(PLEV,RXNCNT)'
         write(30,100) line(:LEN_TRIM(line))
         line = '# if HETCNT != 0 '
         write(30,100) line(:LEN_TRIM(line))
         line = '      real      het_rates(PLEV,HETCNT)'
         write(30,100) line(:LEN_TRIM(line))
         line = '# else'
         write(30,100) line(:LEN_TRIM(line))
         line = '      real      het_rates(1)'
         write(30,100) line(:LEN_TRIM(line))
         line = '# endif'
         write(30,100) line(:LEN_TRIM(line))
         line = ' '
	 write(30,100) line(:LEN_TRIM(line))
         line = '!----------------------------------------------'
	 write(30,100) line(:LEN_TRIM(line))
         line = '!       ... Input args'
	 write(30,100) line(:LEN_TRIM(line))
         line = '!----------------------------------------------'
	 write(30,100) line(:LEN_TRIM(line))
         line = '# if RELCNT != 0 '
         write(30,100) line(:LEN_TRIM(line))
         line = '      real      rel_ratios(PLEV,RELCNT)'
         write(30,100) line(:LEN_TRIM(line))
         line = '# else'
         write(30,100) line(:LEN_TRIM(line))
         line = '      real      rel_ratios(1)'
         write(30,100) line(:LEN_TRIM(line))
         line = '# endif'
         write(30,100) line(:LEN_TRIM(line))
         line = '# if GRPCNT != 0 '
         write(30,100) line(:LEN_TRIM(line))
         line = '      real      grp_ratios(PLEV,GRPCNT)'
         write(30,100) line(:LEN_TRIM(line))
         line = '# else'
         write(30,100) line(:LEN_TRIM(line))
         line = '      real      grp_ratios(1)'
         line = '# endif'
         write(30,100) line(:LEN_TRIM(line))
         line = ' '
	 write(30,100) line(:LEN_TRIM(line))
         line = '!----------------------------------------------'
	 write(30,100) line(:LEN_TRIM(line))
         line = '!       ... Local variables'
	 write(30,100) line(:LEN_TRIM(line))
         line = '!----------------------------------------------'
         write(30,100) line(:LEN_TRIM(line))
         line = '      integer   j'
         write(30,100) line(:LEN_TRIM(line))
      end if
      line = ' '
      write(30,100) line(:LEN_TRIM(line))
      
      first = .true.
      do k = 1,rxntot
!-----------------------------------------------------------------------
!        ... Scan the relationship map
!-----------------------------------------------------------------------
         found = .false.
         index = rxt2rel_pntr(k,1)
         row   = rxt2rel_pntr(k,2)
         do l = 1,index
            found = .true.
            if( first ) then
               line = ' '
	       if( .not. f90 ) then
                  line(7:) = 'do j = 1,PLEV'
                  write(30,100) line(:LEN_TRIM(line))
                  first = .false.
               end if
            end if
            rxno = rel_rxt_map(row,1,index)
            if( l == 1 ) then
	       line = ' '
	       write(num,'(i5)') rxno
	       num = ADJUSTL( num )
	       ll = LEN_TRIM( num )
	       if( f90 ) then
                  line(7:) = 'rate(:,' // num(:ll) // ') = rate(:,' // num(:ll) // ') * rel_ratios(:,'
	       else
                  line(10:) = 'rate(j,' // num(:ll) // ') = rate(j,' // num(:ll) // ') * rel_ratios(j,'
	       end if
	       write(num,'(i5)') rel_rxt_map(row,l+1,index)
	       num = ADJUSTL( num )
               line(LEN_TRIM(line)+1:) = num(:LEN_TRIM(num)) // ')'
	    else
	       write(30,100) line(:LEN_TRIM(line))
	       line(6:) = '$'
	       if( f90 ) then
                  line(33:) = ' * rel_ratios(:,'
	       else
                  line(36:) = ' * rel_ratios(j,'
	       end if
	       write(num,'(i5)') rel_rxt_map(row,l+1,index)
	       num = ADJUSTL( num )
               line(LEN_TRIM(line)+1:) = num(:LEN_TRIM(num)) // ')'
            end if
         end do

!-----------------------------------------------------------------------
!        ... Scan the group map
!-----------------------------------------------------------------------
         index = rxt2grp_pntr(k,1)
         row   = rxt2grp_pntr(k,2)
         do l = 1,index
            found = .true.
            if( first ) then
               line = ' '
	       if( .not. f90 ) then
                  line(7:) = 'do j = 1,PLEV'
                  write(30,100) line(:LEN_TRIM(line))
	       end if
               first = .false.
            end if
            rxno = grp_rxt_map(row,1,index)
            if( l == 1 ) then
	       line = ' '
	       write(num,'(i5)') rxno
	       num = ADJUSTL( num )
	       ll = LEN_TRIM( num )
	       if( f90 ) then
                  line(7:) = 'rate(:,' // num(:ll) // ') = rate(:,' // num(:ll) // ') * grp_ratios(:,'
	       else
                  line(10:) = 'rate(j,' // num(:ll) // ') = rate(j,' // num(:ll) // ') * grp_ratios(j,'
	       end if
	       write(num,'(i5)') grp_rxt_map(row,l+1,index)
	       num = ADJUSTL( num )
               line(LEN_TRIM(line)+1:) = num(:LEN_TRIM(num)) // ')'
	    else
	       write(30,100) line(:LEN_TRIM(line))
	       line(6:) = '$'
	       if( f90 ) then
                  line(33:) = ' * grp_ratios(:,'
	       else
                  line(36:) = ' * grp_ratios(j,'
	       end if
	       write(num,'(i5)') grp_rxt_map(row,l+1,index)
	       num = ADJUSTL( num )
               line(LEN_TRIM(line)+1:) = num(:LEN_TRIM(num)) // ')'
            end if
         end do
         if( found ) then
            write(30,100) line(:LEN_TRIM(line))
         end if
      end do

      do k = 1,hetcnt
	 if( hetmap(k) /= 0 ) then
            line = ' '
            if( first ) then
	       if( .not. f90 ) then
                  line(7:) = 'do j = 1,PLEV'
                  write(30,100) line(:LEN_TRIM(line))
	       end if
               first = .false.
            end if
	    write(num,'(i5)') k
	    num = ADJUSTL( num )
	    ll = LEN_TRIM( num )
	    if( f90 ) then
               line(7:) = 'het_rates(:,' // num(:ll) // ') = het_rates(:,' // num(:ll) // ') * grp_ratios(:,'
	    else
               line(10:) = 'het_rates(j,' // num(:ll) // ') = het_rates(j,' // num(:ll) // ') * grp_ratios(j,'
	    end if
	    write(num,'(i5)') hetmap(k)
	    num = ADJUSTL( num )
            line(LEN_TRIM(line)+1:) = num(:LEN_TRIM(num)) // ')'
            write(30,100) line(:LEN_TRIM(line))
         end if
      end do

      if( .not. first .and. .not. f90 ) then
         line = '      end do'
         write(30,100) line(:LEN_TRIM(line))
      end if

      line = ' '
      write(30,100) line(:LEN_TRIM(line))
      if( f90 ) then
         line = '      end subroutine RXT_MOD'
      else
         line = '      end'
      end if
      write(30,100) line(:LEN_TRIM(line))
      
      CLOSE(30)
      
100   format(a)      
      
      end subroutine MAKE_RMOD
