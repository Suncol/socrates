! subversion Id for THIS file : $Id: mak_grp_vmr.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/mak_grp_vmr.f $
!-----------------------------------------------------------------------
      
      subroutine MAK_GRP_VMR( grp_mem_cnt, &
                              mem2grp_map, &
			      f90 )

      implicit none

!-------------------------------------------------------------------
!	... Dummy args
!-------------------------------------------------------------------
      integer, intent(in) ::  grp_mem_cnt
      integer, intent(in) ::  mem2grp_map(*)
      logical, intent(in) ::  f90

!-------------------------------------------------------------------
!	... Local variables
!-------------------------------------------------------------------
      integer  ::  m, indx
      character(len=72) :: line
      logical  ::  lexist

      INQUIRE( file = 'mak_grp_vmr.F', exist = lexist )
      if( lexist ) then
	 call SYSTEM( 'rm mak_grp_vmr.F' )
      end if
      OPEN( unit = 30, file = 'mak_grp_vmr.F' )

      line = ' '
      write(30,100) line(:LEN_TRIM(line))
      line(7:) = 'subroutine MAK_GRP_VMR( sol,'
      write(30,100) line(:LEN_TRIM(line))
      line(6:) = '$                        group_ratios,'
      write(30,100) line(:LEN_TRIM(line))
      line(6:) = '$                        group_vmrs )'
      write(30,100) line(:LEN_TRIM(line))
      line = ' '
      write(30,100) line(:LEN_TRIM(line))
      line(7:) = 'implicit none '
      write(30,100) line(:LEN_TRIM(line))
      line = ' '
      write(30,100) line(:LEN_TRIM(line))
      if( f90 ) then
         line = '!------------------------------------------------'
         write(30,100) line(:LEN_TRIM(line))
         line = '!        ... Dummy args'
         write(30,100) line(:LEN_TRIM(line))
         line = '!------------------------------------------------'
         write(30,100) line(:LEN_TRIM(line))
         line = '      real, intent(in) ::      sol(PLEV,PCNST)'
         write(30,100) line(:LEN_TRIM(line))
         line(7:) = 'real, intent(in) ::      group_ratios(PLEV,GRPCNT)'
         write(30,100) line(:LEN_TRIM(line))
         line(7:) = 'real, intent(out) ::     group_vmrs(PLEV,GRPCNT)'
         write(30,100) line(:LEN_TRIM(line))
         line = ' '
         write(30,100) line(:LEN_TRIM(line))
         line = '!------------------------------------------------'
         write(30,100) line(:LEN_TRIM(line))
         line = '!        ... Local variables'
         write(30,100) line(:LEN_TRIM(line))
         line = '!------------------------------------------------'
         write(30,100) line(:LEN_TRIM(line))
         line = '      integer ::  k'
      else
         line = 'c------------------------------------------------'
         write(30,100) line(:LEN_TRIM(line))
         line = '!        ... Input arguments'
         write(30,100) line(:LEN_TRIM(line))
         line = 'c------------------------------------------------'
         write(30,100) line(:LEN_TRIM(line))
         line = '      real      sol(PLEV,PCNST)'
         write(30,100) line(:LEN_TRIM(line))
         line(7:) = 'real      group_ratios(PLEV,GRPCNT)'
         write(30,100) line(:LEN_TRIM(line))
         line = ' '
         write(30,100) line(:LEN_TRIM(line))
         line = 'c------------------------------------------------'
         write(30,100) line(:LEN_TRIM(line))
         line = '!        ... Output arguments'
         write(30,100) line(:LEN_TRIM(line))
         line = 'c------------------------------------------------'
         write(30,100) line(:LEN_TRIM(line))
         line = '      real      group_vmrs(PLEV,GRPCNT)'
         write(30,100) line(:LEN_TRIM(line))
         line = ' '
         write(30,100) line(:LEN_TRIM(line))
         line = 'c------------------------------------------------'
         write(30,100) line(:LEN_TRIM(line))
         line = '!        ... Local variables'
         write(30,100) line(:LEN_TRIM(line))
         line = 'c------------------------------------------------'
         write(30,100) line(:LEN_TRIM(line))
         line = '      integer  i, k'
      end if
      write(30,100) line(:LEN_TRIM(line))
      line = ' '
      write(30,100) line(:LEN_TRIM(line))
      if( .not. f90 ) then
         line(7:) = 'do k = 1,PLEV'
         write(30,100) line(:LEN_TRIM(line))
      end if
      do m = 1,grp_mem_cnt
	 line = ' '
	 if( f90 ) then
	    line(7:) = 'group_vmrs(:,  ) = group_ratios(:,  )'
	    indx = INDEX( line, ' ' )
	    write(line(indx:indx+1),'(i2)') m
	    indx = INDEX( line, ' ' )
	    write(line(indx:indx+1),'(i2)') m
	 else
	    line(10:) = 'group_vmrs(k,  ) = group_ratios(k,  )'
	    indx = INDEX( line, ' ' )
	    write(line(indx:indx+1),'(i2)') m
	    indx = INDEX( line, ' ' )
	    write(line(indx:indx+1),'(i2)') m
	 end if
         write(30,100) line(:LEN_TRIM(line))
	 if( f90 ) then
	    line(6:) = '$                        * sol(:,'
	 else
	    line(6:) = '$                           * sol(k,  )'
	 end if
	 write(line(LEN_TRIM(line)+1:),'(i2,'')'')') mem2grp_map(m)
         write(30,100) line(:LEN_TRIM(line))
      end do
      if( .not. f90 ) then
         line = ' '
         line(7:) = '   end do'
         write(30,100) line(:LEN_TRIM(line))
      end if
      line = ' '
      line(7:) = 'end do'
      write(30,100) line(:LEN_TRIM(line))
      line = ' '
      write(30,100) line(:LEN_TRIM(line))
      if( f90 ) then
         line(7:) = 'end subroutine MAK_GRP_VMR'
      else
         line(7:) = 'end'
      end if
      write(30,100) line(:LEN_TRIM(line))

100   format(a)

      end subroutine MAK_GRP_VMR
