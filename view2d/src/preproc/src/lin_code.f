! subversion Id for THIS file : $Id: lin_code.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/lin_code.f $
!-----------------------------------------------------------------------

      subroutine MAKE_LIN( clscnt, &
                           clsmap, &
                           cls_rxt_cnt, &
                           cls_rxt_map, &
                           pcoeff_ind, &
                           pcoeff, &
                           machine, &
			   permute, &
			   mat_map, &
			   class, &
			   hetcnt, &
			   rxt_alias, &
			   f90 )
!-----------------------------------------------------------------------
!        ... Write the fortran code for the linear components
!	     of the Jacobian matrix
!-----------------------------------------------------------------------

      use LIMITS
      use SYMBOLS, only : solsym => new_solsym

      implicit none
     
!-----------------------------------------------------------------------
!        ... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in) ::      clscnt, &
                                  hetcnt, &
                                  class, &
                                  clsmap(var_lim,5,2), &
                                  cls_rxt_map(rxt_lim,7), &
                                  cls_rxt_cnt(4)
      integer, intent(in) ::      mat_map(clscnt,clscnt)
      integer, intent(in) ::      permute(clscnt)
      integer, intent(in) ::      pcoeff_ind(*)
      real, intent(in) ::         pcoeff(4,*)
      character(len=8), intent(in) ::  machine                ! target machine
      character(len=8), intent(in) ::  rxt_alias(rxt_lim)     ! rxt aliases
      logical, intent(in) ::      f90                         ! Fortran 90
      
!-----------------------------------------------------------------------
!        ... Local variables
!-----------------------------------------------------------------------
      integer  ::   i, j, k, l, m
      integer  ::   length, index
      integer  ::   row, col
      integer  ::   line_pos, rxno, target
      integer  ::   base
      integer  ::   species, spndx, rxndx
      integer  ::   match_cnt
      integer  ::   list_cnt
      integer  ::   other_ind
      integer  ::   match_ind(var_lim)
      integer  ::   scan(var_lim,3)
      real     ::   rate
      character(len=72) :: line
      character(len=72) :: buff
      character(len= 5) :: num
      logical  ::  beg_line, flush
      logical  ::  lexist
      logical  ::  cache
      
      cache = .not. (machine == 'CRAY')
      INQUIRE( file = 'linmat.F', exist = lexist )
      if( lexist ) then
	 call SYSTEM( 'rm linmat.F' )
      end if
      OPEN( unit = 30, file = 'linmat.F' )

      line = ' '
      write(30,100) TRIM( line )
      line = '      subroutine LINMAT( mat'
      write(30,100) TRIM( line )
      line = '     $,                  y'
      write(30,100) TRIM( line )
      if( .not. cache ) then
         if( hetcnt /= 0 ) then
            line = '     $,                  rxt'
            write(30,100) TRIM( line )
            line = '     $,                  het_rates )'
            write(30,100) TRIM( line )
         else
            line = '     $,                  rxt )'
            write(30,100) TRIM( line )
         end if
      else
         line = '     $,                  rxt'
         write(30,100) TRIM( line )
         if( hetcnt /= 0 ) then
            line = '     $,                  het_rates'
            write(30,100) TRIM( line )
	 end if
         line = '     $,                  kl, ku )'
         write(30,100) TRIM( line )
      end if
      line = ' '
      write(30,100) TRIM( line )
      line = '      use RXT_NAMES'
      write(30,100) TRIM( line )
      line = '      use SPC_NAMES'
      write(30,100) TRIM( line )
      line = ' '
      write(30,100) TRIM( line )
      line = ' '
      write(30,100) line
      line = '      implicit none '
      write(30,100) line
      line = ' '
      write(30,100) line
      line = '!----------------------------------------------'
      write(30,100) line
      line = '!       ... Dummy arguments'
      write(30,100) line
      line = '!----------------------------------------------'
      write(30,100) line
      if( cache ) then
         line = '      integer, intent(in) ::   kl, ku'
         write(30,100) TRIM( line )
         line = '      real, intent(in) ::   y(4,PCNST)'
         write(30,100) TRIM( line )
         line = '      real, intent(in) ::   rxt(4,RXNCNT)'
         write(30,100) TRIM( line )
	 if( hetcnt /= 0 ) then
            line = '      real, intent(in) ::   het_rates(4,HETCNT)'
            write(30,100) TRIM( line )
	 end if
         line = '      real, intent(out) ::  mat(4,NZCNT)'
         write(30,100) TRIM( line )
      else
         line = '      real, intent(in) ::   y(PLEV,PCNST)'
         write(30,100) TRIM( line )
         line = '      real, intent(in) ::   rxt(PLEV,RXNCNT)'
         write(30,100) TRIM( line )
	 if( hetcnt /= 0 ) then
            line = '      real, intent(in) ::   het_rates(PLEV,HETCNT)'
            write(30,100) TRIM( line )
	 end if
         line = '      real, intent(out) ::  mat(PLEV,NZCNT)'
         write(30,100) TRIM( line )
      end if
      line = ' '
      write(30,100) TRIM( line )
      line = ' '
      write(30,100) TRIM( line )
      line = '!----------------------------------------------'
      write(30,100) TRIM( line )
      line = '!       ... Local variables'
      write(30,100) TRIM( line )
      line = '!----------------------------------------------'
      write(30,100) TRIM( line )
      line = '      integer :: k'
      write(30,100) TRIM( line )
      line = ' '
      write(30,100) TRIM( line )

      line = '!----------------------------------------------'
      write(30,100) TRIM( line )
      line = '!       ... Linear Matrix entries for'
      length = LEN_TRIM( line ) + 2
      line(length:) = 'Implicit species'
      write(30,100) line
      line = '!----------------------------------------------'
      write(30,100) TRIM( line )
      line = ' '
      write(30,100) TRIM( line )
      if( clscnt /= 0 ) then
	 if( .not. cache ) then
            line(7:) = 'do k = 1,PLEV'
	 else
            line(7:) = 'do k = kl,ku'
	 end if
         write(30,100) TRIM( line )
      end if

Species_loop : &
      do species = 1,clscnt
         target = clsmap(species,class,2)
         flush  = .false.
!-----------------------------------------------------------------------
!       ...Write code for linear loss entries
!-----------------------------------------------------------------------
	 row = permute(species)
         write(num,'(i5)') mat_map(row,row)
	 num = ADJUSTL( num )
	 l = LEN_TRIM( num )
         line = ' '
         line(10:) = 'mat(k,' // num(:l) // ') = -('
         line_pos = LEN_TRIM( line ) + 2
         base = cls_rxt_cnt(1)
         beg_line = .true.
         do k = base+1,base+cls_rxt_cnt(2)
            if( cls_rxt_map(k,2) == target ) then
               flush = .true.
               rxndx = cls_rxt_map(k,1)
               write(num,'(i5)') rxndx
	       num = ADJUSTL( num )
	       l = LEN_TRIM( num )
	       buff = 'rxt(k,rid_' // rxt_alias(rxndx)(:LEN_TRIM(rxt_alias(rxndx))) // ')'
               if( cls_rxt_map(k,3) > 0 ) then
                  spndx = cls_rxt_map(k,3)
                  write(num,'(i5)') spndx
	          num = ADJUSTL( num )
	          l = LEN_TRIM( num )
		  buff(LEN_TRIM(buff)+1:) = '*y(k,vid_' // solsym(spndx)(:LEN_TRIM(solsym(spndx))) // ')'
               end if
               length = LEN_TRIM(buff)
               if( (line_pos + length) <= 69 ) then
                  if( beg_line ) then
                     line(line_pos:) = buff(:length)
                     beg_line = .false.
                  else
                     line(line_pos:) = ' + ' // buff(:length)
                  end if
               else
                  write(30,100) TRIM( line )
                  line = '     $'
                  line(23:) = '+ ' // buff(:length)
               end if
               line_pos = LEN_TRIM( line ) + 1
            end if
         end do
	 base = base + cls_rxt_cnt(2) + cls_rxt_cnt(3)
	 do k = base+1,base+cls_rxt_cnt(4)
	    if( cls_rxt_map(k,2) == species ) then
	       flush = .true.
	       write(num,'(i5)') cls_rxt_map(k,1)
	       num = ADJUSTL( num )
	       l = LEN_TRIM( num )
	       buff = 'het_rates(k,' // num(:l) // ')'
	       length = LEN_TRIM(buff)
	       if( (line_pos + length) <= 69 ) then
	          if( beg_line ) then
	             line(line_pos:) = buff(:length)
		     beg_line = .false.
		  else
		     line(line_pos:) = ' + ' // buff(:length)
		  end if
	       else
		  write(30,100) TRIM( line )
		  line = '     $'
		  line(18:) = '+ ' // buff(:length)
	       end if
	       line_pos = LEN_TRIM( line ) + 1
	    end if
	 end do
         if( flush ) then
	    if( line_pos <= 71 ) then
               line(line_pos+1:) = ')'
            else
               write(30,100) TRIM( line )
               line = '     $ )'
	    end if
            write(30,100) TRIM( line )
         end if
               
!-----------------------------------------------------------------------
!       ... Scan for production matches
!-----------------------------------------------------------------------
         match_cnt = 0
         base = cls_rxt_cnt(1)
         do k = base+1,base+cls_rxt_cnt(2)
            other_ind = 0
            do l = 4,7
               if( cls_rxt_map(k,l) == species ) then
                  if( other_ind == 0 ) then
                     match_cnt = match_cnt + 1
                     scan(match_cnt,1) = k
                     scan(match_cnt,2) = ABS(cls_rxt_map(k,2))
                  end if
                  other_ind = other_ind + 1
               end if
            end do
            if( other_ind /= 0 ) then
               scan(match_cnt,3) = other_ind
            end if
         end do
	 list_cnt = match_cnt
         do while( list_cnt > 0 )
	    do j = 1,match_cnt
	       if( scan(j,2) /= 0 ) then
		  index = scan(j,2)
		  exit
	       end if
	    end do
            m = 0
	    do j = 1,match_cnt
	       if( scan(j,2) == index ) then
		  m = m + 1
		  match_ind(m) = j
		  scan(j,2)    = 0
		  list_cnt = list_cnt - 1
	       end if
	    end do
	    row = permute(species)
	    col = permute(clsmap(index,class,1))
            write(num,'(i5)') mat_map(row,col)
	    num = ADJUSTL( num )
	    l = LEN_TRIM( num )
            line = ' '
            line(10:) = 'mat(k,' // num(:l) // ') ='
            line_pos = LEN_TRIM( line )
	    if( clsmap(index,class,1) == species ) then
               line(LEN_TRIM(line)+2:) = 'mat(k,' // num(:l) // ') +'
	    end if
            line_pos = LEN_TRIM( line ) + 2
	    if( m > 0 ) then
	       beg_line = .true.
	    else
               line(line_pos:) = '0.'
	    end if
            do j = 1,m
	       l = match_ind(j)
	       rxno  = cls_rxt_map(scan(l,1),1)
               index = pcoeff_ind(rxno)
               rate = 0.
               if( index /= 0 ) then
                  do i = 4,7
                     if( cls_rxt_map(scan(l,1),i) == species ) then
                        rate = rate + pcoeff(i-3,index)
                     end if
                  end do
               else if( scan(l,3) /= 1 ) then
                  rate = REAL(scan(l,3))
               end if
	       buff = ' '
               if( rate /= 0. .and. rate /= 1. ) then
                  call R2C( buff, rate, 'l' )
                  buff(LEN_TRIM(buff)+1:) = '*'
               end if
               write(num,'(i5)') rxno
	       num = ADJUSTL( num )
	       buff(LEN_TRIM(buff)+1:) = 'rxt(k,rid_' // rxt_alias(rxno)(:LEN_TRIM(rxt_alias(rxno))) // ')'
	       if( cls_rxt_map(scan(l,1),3) > 0 ) then
                  spndx = cls_rxt_map(scan(l,1),3)
                  write(num,'(i5)') spndx
	          num = ADJUSTL( num )
	          buff(LEN_TRIM(buff)+1:) = '*y(k,vid_' // solsym(spndx)(:LEN_TRIM(solsym(spndx))) // ')'
	       end if
               length = LEN_TRIM(buff)
               if( (line_pos + length) <= 69 ) then
                  if( beg_line ) then
                     line(line_pos:) = buff(:length)
                     beg_line = .false.
                  else
                     line(line_pos:) = ' + ' // buff(:length)
                  end if
               else
                  write(30,100) TRIM( line )
                  line = '     $'
                  line(23:) = '+ ' // buff(:length)
               end if
               line_pos = LEN_TRIM( line ) + 1
            end do
            write(30,100) TRIM( line )
         end do       
         line = ' '
         write(30,100) TRIM( line )
      end do Species_loop

      if( clscnt /= 0 ) then
         line = '      end do'
         write(30,100) TRIM( line )
         line = ' '
         write(30,100) TRIM( line )
      end if

      line = '      end subroutine LINMAT'
      write(30,100) TRIM( line )

      CLOSE( 30 )

100   format(a)

      end subroutine MAKE_LIN
