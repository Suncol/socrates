! subversion Id for THIS file : $Id: rate_code.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/rate_code.f $
!-----------------------------------------------------------------------

      subroutine MAKE_RATE( sym_rates, &
                            rxt_alias, &
                            rxptab, &
                            rxpcnt, &
                            use_t0, &
			    f90 )
!-----------------------------------------------------------------------
!        ... Write fortran "internal" reaction rates
!-----------------------------------------------------------------------

      use LIMITS, only : rxt_lim

      implicit none

!-----------------------------------------------------------------------
!        ... Dummy arguments
!-----------------------------------------------------------------------
      integer, intent(in) ::    rxpcnt
      integer, intent(in) ::    rxptab(rxt_lim)
      character(len=8),  intent(in) :: rxt_alias(rxt_lim)
      character(len=16), intent(in) :: sym_rates(2,rxt_lim)
      logical, intent(in) ::    use_t0                  ! use temp rate form
      logical, intent(in) ::    f90

!-----------------------------------------------------------------------
!        ... Local variables
!-----------------------------------------------------------------------
      integer  ::   i, cnt, indp, inde, l, rxno
      character(len=72) :: line
      character(len=32) :: wrk
      character(len=5)  :: num
      logical  ::  lexist
      
      if( rxpcnt == 0 ) then
	 return
      end if

      INQUIRE( file = 'setrxt.F', exist = lexist )
      if( lexist ) then
	 call SYSTEM( 'rm setrxt.F' )
      end if
      OPEN( unit = 30, file = 'setrxt.F' )

!-----------------------------------------------------------------------
!        ... Check for temp dependent rates
!-----------------------------------------------------------------------
      cnt = COUNT( sym_rates(2,1:rxpcnt) /= ' ' )

      line = ' '
      write(30,100) line
      line(7:) = 'subroutine SETRXT( lat,'
      write(30,100) line
      line(6:) = '$                   temp )'
      write(30,100) line
      line = ' '
      write(30,100) line
      if( f90 ) then
         line(7:) = 'use RXT_NAMES'
         write(30,100) line
         line(7:) = 'use RATES_MODS, rate => rxt_rates'
         write(30,100) line
         line = ' '
         write(30,100) line
      end if
      line(7:) = 'implicit none '
      write(30,100) line
      line = ' '
      write(30,100) line
      if( f90 ) then
         line = '!----------------------------------------------'
	 write(30,100) line
         line = '!       ... Dummy args'
	 write(30,100) line
         line = '!----------------------------------------------'
	 write(30,100) line
         line = '      integer, intent(in)  ::     lat'
         write(30,100) line
         line = '      real, intent(in)  ::     temp(PLEV)'
         write(30,100) line
         line = ' '
         write(30,100) line
         line = '!----------------------------------------------'
	 write(30,100) line
         line = '!       ... Local variables'
	 write(30,100) line
         line = '!----------------------------------------------'
	 write(30,100) line
         if( cnt /= 0 ) then
	    line = ' '
            line(7:) = 'real    :: itemp(PLEV)'
            write(30,100) line
            line = ' '
            line(7:) = 'logical, save :: entered = .false.'
	    write(30,100) line
         end if
      else
         line = '!----------------------------------------------'
	 write(30,100) line
         line = '!       ... Input args'
	 write(30,100) line
         line = '!----------------------------------------------'
	 write(30,100) line
         line = '      real      temp(PLEV)'
         write(30,100) line
         line = ' '
         write(30,100) line
         line = '!----------------------------------------------'
	 write(30,100) line
         line = '!       ... Output args'
	 write(30,100) line
         line = '!----------------------------------------------'
	 write(30,100) line
         line = '      real      rate(PLEV,RXNCNT)'
         write(30,100) line
         line = ' '
         write(30,100) line
         line = '!----------------------------------------------'
	 write(30,100) line
         line = '!       ... Local variables'
	 write(30,100) line
         line = '!----------------------------------------------'
	 write(30,100) line
         line = '      integer   j'
         write(30,100) line
         if( cnt /= 0 ) then
            line(7:) = 'real      itemp(PLEV)'
            write(30,100) line
         end if
      end if
      line = ' '
      write(30,100) line
      
      if( .not. f90 ) then
         line(7:) = 'do j = 1,PLEV'
         write(30,100) line
      end if
      
!-----------------------------------------------------------------------
!        ... First do all temperature independent rates
!            This was done only if( .not. entered ) but gave an error
!            in ADJRXT (radj_code.f) for reactions with invariants &
!            T-indep rates. Commenting out the '.not. entered' part
!-----------------------------------------------------------------------
      if( cnt /= rxpcnt ) then
         line = ' '
!         line(7:) = 'if( .not. entered ) then'
!         write(30,100) line
!         line = ' '
!         line(10:) = 'entered = .true.'
!         write(30,100) line
         do i = 1,rxpcnt
            if( sym_rates(2,i) == ' ' ) then
	       if( f90 ) then
                  line(10:) = 'rate(:,'
	       else
                  line(13:) = 'rate(j,'
	       end if
	       rxno = rxptab(i)
	       l = LEN_TRIM( sym_rates(1,i) )
	       wrk = sym_rates(1,i)(:l)
	       indp = SCAN( sym_rates(1,i)(:l), '.' )
	       inde = SCAN( sym_rates(1,i)(:l), 'eE' )
	       if( inde == 0 .and. indp == 0 ) then
		  l = l + 1
		  wrk(l:l) = '.' 
	       end if
	       line(LEN_TRIM(line)+1:) = 'rid_' // rxt_alias(rxno)(:LEN_TRIM(rxt_alias(rxno))) // ',:) = ' // wrk(:l)
               write(30,100) line
            end if
         end do
!         line = ' '
!         line(7:) = 'end if'
!         write(30,100) line
      end if

!-----------------------------------------------------------------------
!        ... temperature dependent rates
!-----------------------------------------------------------------------
      if( cnt /= 0 ) then
	 if( f90 ) then
	    if( use_t0 ) then
               line(7:) = 'itemp(:) = 1./RATE_T0 - 1./temp(:)'
	    else
               line(7:) = 'itemp(:) = 1. / temp(:)'
	    end if
	 else
	    if( use_t0 ) then
               line(10:) = 'itemp(j) = 1./RATE_T0 - 1./temp(j)'
	    else
               line(10:) = 'itemp(j) = 1. / temp(j)'
	    end if
	 end if
         write(30,100) line
         line = ' '
         do i = 1,rxpcnt
            if( sym_rates(2,i) /= ' ' ) then
	       if( f90 ) then
                  line(7:) = 'rate(:,'
	       else
                  line(10:) = 'rate(j,'
	       end if
	       rxno = rxptab(i)
	       l = LEN_TRIM( sym_rates(1,i) )
	       wrk = sym_rates(1,i)(:l)
	       indp = SCAN( sym_rates(1,i)(:l), '.' )
	       inde = SCAN( sym_rates(1,i)(:l), 'eE' )
	       if( inde == 0 .and. indp == 0 ) then
		  l = l + 1
		  wrk(l:l) = '.'
	       end if
	       line(LEN_TRIM(line)+1:) = 'rid_' // rxt_alias(rxno)(:LEN_TRIM(rxt_alias(rxno))) // ',lat) = ' // wrk(:l)
	       l = LEN_TRIM( sym_rates(2,i) )
	       wrk = sym_rates(2,i)(:l)
	       indp = SCAN( sym_rates(2,i)(:l), '.' )
	       inde = SCAN( sym_rates(2,i)(:l), 'eE' )
	       if( inde == 0 .and. indp == 0 ) then
		  l = l + 1
		  wrk(l:l) = '.' 
	       end if
               line(LEN_TRIM(line)+1:) = ' * EXP( ' // wrk(:l)
	       if( f90 ) then
                  line(LEN_TRIM(line)+1:) = ' * itemp(:) )'
	       else
                  line(LEN_TRIM(line)+1:) = ' * itemp(j) )'
	       end if
               write(30,100) line
            end if
         end do
      end if

      if( .not. f90 ) then
         line = ' '
         line(7:) = 'end do'
         write(30,100) line
      end if
      line = ' '
      write(30,100) line
      if( f90 ) then
         line(7:) = 'end subroutine SETRXT'
      else
         line(7:) = 'end'
      end if
      write(30,100) line
      
      CLOSE(30)
      
100   format(a72)

      end subroutine MAKE_RATE
