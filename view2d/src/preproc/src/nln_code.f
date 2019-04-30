! subversion Id for THIS file : $Id: nln_code.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/nln_code.f $
!-----------------------------------------------------------------------

      subroutine MAKE_NLN( clscnt, &
                           clsmap, &
                           cls_rxt_cnt, &
                           cls_rxt_map, &
                           pcoeff_ind, &
                           pcoeff, &
                           machine, &
			   permute, &
			   mat_map, &
			   class, &
			   rxt_alias, &
			   f90 )
!-----------------------------------------------------------------------
!        ... Write the fortran code for the non-linear components
!	     of the Jacobian matrix
!-----------------------------------------------------------------------
     
      use LIMITS
      use SYMBOLS, only : solsym => new_solsym

      implicit none

!-----------------------------------------------------------------------
!        ... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in) ::      clscnt, &                ! count of class members
				  class, &                 ! class index
                                  clsmap(var_lim,5,2), &
                                  cls_rxt_map(rxt_lim,7), &
                                  cls_rxt_cnt(5)           ! class rxtns count
      integer, intent(in) ::      permute(clscnt)
      integer, intent(in) ::      mat_map(clscnt,clscnt)
      integer, intent(in) ::      pcoeff_ind(*)            ! map for nonunity prod
      real, intent(in) ::         pcoeff(4,*)
      character(len=8), intent(in) ::  machine             ! target machine
      character(len=8), intent(in) ::  rxt_alias(rxt_lim)  ! reaction aliases
      logical, intent(in) ::      f90                      ! Fortran 90
      
!-----------------------------------------------------------------------
!        ... Local variables
!-----------------------------------------------------------------------
      integer  ::   i, j, k, l, m, n
      integer  ::   length, index, pindx
      integer  ::   row, col
      integer  ::   line_pos, buf_pos, rxno, target
      integer  ::   base
      integer  ::   species, spndx, rxndx
      integer  ::   match_cnt
      integer  ::   list_cnt
      integer  ::   rxtnt_cnt
      integer  ::   other_ind
      integer  ::   match_ind(var_lim)
      integer  ::   scan(var_lim,4)
      integer  ::   rxtnt(2)
      real     ::   rate
      character(len=72) :: line
      character(len=72) :: buff
      character(len= 5) ::  num, num1
      logical  ::  beg_line
      logical  ::  lexist
      logical  ::  cache
      
      cache = .not. (machine == 'CRAY')
      INQUIRE( file = 'nlnmat.F', exist = lexist )
      if( lexist ) then
	 call SYSTEM( 'rm nlnmat.F' )
      end if
      OPEN( unit = 30, file = 'nlnmat.F' )

      line = ' '
      write(30,100) TRIM( line )
      line = '      subroutine NLNMAT( mat'
      write(30,100) TRIM( line )
      line = '     $,                  y'
      write(30,100) TRIM( line )
      if( .not. cache ) then
         line = '     $,                  rxt )'
      else
         line = '     $,                  rxt'
         write(30,100) TRIM( line )
         line = '     $,                  converged'
         write(30,100) TRIM( line )
         line = '     $,                  kl, ku )'
      end if
      write(30,100) TRIM( line )
      line = ' '
      write(30,100) TRIM( line )
      line = '      use RXT_NAMES'
      write(30,100) TRIM( line )
      line = '      use SPC_NAMES'
      write(30,100) TRIM( line )
      line = ' '
      write(30,100) TRIM( line )
      line = '      implicit none '
      write(30,100) TRIM( line )
      line = ' '
      write(30,100) TRIM( line )
      line = '!----------------------------------------------'
      write(30,100) TRIM( line )
      line = '!       ... Dummy args'
      write(30,100) TRIM( line )
      line = '!----------------------------------------------'
      write(30,100) TRIM( line )
      if( cache ) then
         line = '      integer, intent(in) ::   kl, ku'
         write(30,100) TRIM( line )
         line = '      real, intent(in) ::   y(4,PCNST)'
         write(30,100) TRIM( line )
         line = '      real, intent(in) ::   rxt(4,RXNCNT)'
         write(30,100) TRIM( line )
         line = '      real, intent(out) ::  mat(4,NZCNT)'
         write(30,100) TRIM( line )
         line = '      logical, intent(in) ::   converged(4)'
         write(30,100) TRIM( line )
      else
         line = '      real, intent(in) ::   y(PLEV,PCNST)'
         write(30,100) TRIM( line )
         line = '      real, intent(in) ::   rxt(PLEV,RXNCNT)'
         write(30,100) TRIM( line )
         line = '      real, intent(out) ::  mat(PLEV,NZCNT)'
         write(30,100) TRIM( line )
      end if
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
      line = '!       ... Nonlinear Matrix entries for'
      length = LEN_TRIM( line ) + 2
      line(length:) = 'Implicit species'
      write(30,100) TRIM( line )
      line = '!----------------------------------------------'
      write(30,100) TRIM( line )
      line = ' '
      write(30,100) TRIM( line )
      if( .not. cache ) then
         line(7:) = 'do k = 1,PLEV'
      else
         line(7:) = 'do k = kl,ku'
         write(30,100) TRIM( line )
         line(7:) = '   if( .not. converged(k) ) then'
      end if
      write(30,100) TRIM( line )

      base = SUM( cls_rxt_cnt(:2) )
Species_loop : &
      do species = 1,clscnt
         target = clsmap(species,class,2)
!-----------------------------------------------------------------------
!       ...Write code for nonlinear loss entries
!-----------------------------------------------------------------------
         match_cnt = 0
         do k = base+1,base+cls_rxt_cnt(3)
!-----------------------------------------------------------------------
!       ...Find all reactions with target reactant
!-----------------------------------------------------------------------
            other_ind = 0
            do l = 2,3
               if( cls_rxt_map(k,l) == target ) then
                  if( other_ind == 0 ) then
                     match_cnt = match_cnt + 1
                     scan(match_cnt,1) = k
                     if( l == 2 ) then
                        scan(match_cnt,2) = ABS(cls_rxt_map(k,3))
                     else
                        scan(match_cnt,2) = ABS(cls_rxt_map(k,2))
                     end if
                     scan(match_cnt,4) = l
                  end if
                  other_ind = other_ind + 1
               end if
            end do
         end do
!-----------------------------------------------------------------------
!       ...Write the diagonal loss entry
!-----------------------------------------------------------------------
         if( match_cnt > 0 ) then
            scan(:match_cnt,3) = scan(:match_cnt,2)
	    pindx = permute(species)
            write(num,'(i5)') mat_map(pindx,pindx)
	    num = ADJUSTL( num )
	    n = LEN_TRIM( num )
            line = ' '
            line(10:) = 'mat(k,' // num(:n) // ') = -('
            line_pos = LEN_TRIM( line ) + 2
            beg_line = .true.
         end if
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
            do j = 1,m
               l = match_ind(j)
               rxno = cls_rxt_map(scan(l,1),1)
               buff = ' '
	       buf_pos = 1
               if( j == 1 .and. m > 1 ) then
                  if( scan(l,3) == target ) then
                     buff(buf_pos:) =  '(4.*'
                  else
                     buff(buf_pos:) =  '('
                  end if
               else if( scan(l,3) == target ) then
                  buff(buf_pos:) =  '4.*'
               end if
               write(num,'(i5)') rxno
	       num = ADJUSTL( num )
	       n = LEN_TRIM( num )
	       buff(LEN_TRIM(buff)+1:) = 'rxt(k,rid_' // rxt_alias(rxno)(:LEN_TRIM(rxt_alias(rxno))) // ')'
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
            spndx = scan(l,3)
            write(num,'(i5)') spndx
	    num = ADJUSTL( num )
            if( m > 1 ) then
	       buff = ') * y(k,vid_' // solsym(spndx)(:LEN_TRIM(solsym(spndx))) // ')'
            else
	       buff = '*y(k,vid_' // solsym(spndx)(:LEN_TRIM(solsym(spndx))) // ')'
            end if
            length = LEN_TRIM(buff)
            if( (line_pos + length) <= 69 ) then
               line(line_pos:) = buff(:length)
            else
               write(30,100) TRIM( line )
               line = '     $'
               line(23:) = buff(:length)
            end if
            line_pos = LEN_TRIM( line ) + 1
         end do
	 if( match_cnt /= 0 ) then
	    line(line_pos+1:) = ')'
	 end if
         write(30,100) TRIM( line )
         
!-----------------------------------------------------------------------
!       ...Write nondiagonal loss entries
!-----------------------------------------------------------------------
         list_cnt = match_cnt
         do j = 1,match_cnt
            if( scan(j,3) == target ) then
               scan(j,2) = 0
               list_cnt = list_cnt - 1
            else
               scan(j,2) = scan(j,3)
            end if
         end do
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
	    pindx = permute(clsmap(index,class,1))
            write(num,'(i5)') mat_map(permute(species),pindx)
	    num = ADJUSTL( num )
	    n = LEN_TRIM( num )
            line = ' '
            line(10:) = 'mat(k,' // num(:n) // ') = -'
            line_pos = LEN_TRIM( line ) + 2
            if( m > 0 ) then
               beg_line = .true.
            else
               line(line_pos:) = '0.'
            end if
            do j = 1,m
               l = match_ind(j)
               rxno = cls_rxt_map(scan(l,1),1)
               buff = ' '
               if( j == 1 .and. m > 1 ) then
                  buff =  '('
                  buf_pos = 2
               else
                  buf_pos = 1
               end if
               write(num,'(i5)') rxno
	       num = ADJUSTL( num )
	       n = LEN_TRIM( num )
	       buff(buf_pos:) = 'rxt(k,rid_' // rxt_alias(rxno)(:LEN_TRIM(rxt_alias(rxno))) // ')'
               if( j == 1 ) then
                  if ( scan(l,4) == 2 ) then
                     index = 3
                  else
                     index = 2
                  end if
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
            write(num,'(i5)') target
	    num = ADJUSTL( num )
            if( m > 1 ) then
	       buff = ') * y(k,vid_' // solsym(target)(:LEN_TRIM(solsym(target))) // ')'
            else
	       buff = '*y(k,vid_' // solsym(target)(:LEN_TRIM(solsym(target))) // ')'
            end if
            length = LEN_TRIM(buff)
            if( (line_pos + length) <= 69 ) then
               line(line_pos:) = buff(:length)
            else
               write(30,100) TRIM( line )
               line = '     $'
               line(23:) = buff(:length)
            end if
            write(30,100) TRIM( line )
         end do               
         line = ' '
         write(30,100) TRIM( line )

!-----------------------------------------------------------------------
!       ...Scan for production matches
!-----------------------------------------------------------------------
         match_cnt = 0
         do k = base+1,base+cls_rxt_cnt(3)
            other_ind = 0
            do l = 4,7
               if( cls_rxt_map(k,l) == species ) then
                  if( other_ind == 0 ) then
                     match_cnt = match_cnt + 1
                     scan(match_cnt,1) = k
                     scan(match_cnt,2) = ABS(cls_rxt_map(k,2))
                     scan(match_cnt,4) = ABS(cls_rxt_map(k,3))
                  end if
                  other_ind = other_ind + 1
               end if
            end do
            if( other_ind /= 0 ) then
               scan(match_cnt,3) = other_ind
            end if
         end do
         list_cnt = match_cnt
!-----------------------------------------------------------------------
!       ..."Order" the match list reactants
!-----------------------------------------------------------------------
         do j = 1,match_cnt
            if( scan(j,2) > scan(j,4) ) then
               l = scan(j,2)
               scan(j,2) = scan(j,4)
               scan(j,4) = l
            end if
         end do
         do while( list_cnt > 0 )
            do j = 1,match_cnt
               if( scan(j,2) /= 0 ) then
                  rxtnt(1) = scan(j,2)
                  rxtnt(2) = scan(j,4)
                  exit
               end if
            end do
            m = 0
            do j = 1,match_cnt
               if( scan(j,2) == rxtnt(1) .and. scan(j,4) == rxtnt(2) ) then
                  m = m + 1
                  match_ind(m) = j
                  scan(j,2)    = 0
                  list_cnt = list_cnt - 1
               end if
            end do
            if( rxtnt(1) /= rxtnt(2) ) then
               rxtnt_cnt = 2
            else
               rxtnt_cnt = 1
            end if
            do n = 1,rxtnt_cnt
	       pindx = permute(clsmap(rxtnt(n),class,1))
               write(num,'(i5)') mat_map(permute(species),pindx)
	       num = ADJUSTL( num )
               line = ' '
               line(10:) = 'mat(k,' // num(:LEN_TRIM(num)) // ') ='
	       line_pos = LEN_TRIM(line) + 2
               line(line_pos:) = 'mat(k,' // num(:LEN_TRIM(num)) // ') +'
	       if( m > 1 ) then
		  line(LEN_TRIM(line)+2:) = '('
	       end if
               line_pos = LEN_TRIM( line ) + 2
               beg_line = .true.
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
                     rate = FLOAT(scan(l,3))
                  end if
                  if( rxtnt_cnt == 1 ) then
                     if( rate == 0. ) then
                        rate = 2.
                     else
                        rate = 2.*rate
                     end if
                  end if
                  buff = ' '
                  if( rate /= 0. .and. rate /= 1. ) then
                     call R2C( buff, rate, 'l' )
                     buff(LEN_TRIM(buff)+1:) = '*'
                  end if
                  write(num,'(i5)') rxno
		  num = ADJUSTL( num )
		  buff(LEN_TRIM(buff)+1:) = 'rxt(k,rid_' // rxt_alias(rxno)(:LEN_TRIM(rxt_alias(rxno))) // ')'
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
	       if( m > 1 ) then
		  if( line_pos+3 <= 72 ) then
                     line(line_pos+2:) = ')'
                     line_pos = line_pos + 3
		  else
                     write(30,100) TRIM( line )
                     line = '     $'
                     line(23:) = ')'
                     line_pos = LEN_TRIM( line ) + 1
		  end if
	       end if
               if( n == 1 ) then
                  spndx = rxtnt(2)
               else
                  spndx = rxtnt(1)
               end if
               write(num,'(i5)') spndx
	       num = ADJUSTL( num )
	       buff = '*y(k,vid_' // solsym(spndx)(:LEN_TRIM(solsym(spndx))) // ')'
               length = LEN_TRIM(buff)
               if( (line_pos + length) <= 69 ) then
                  line(line_pos:) = buff(:length)
               else
                  write(30,100) TRIM( line )
                  line = '     $'
                  line(23:) = buff(:length)
               end if
               write(30,100) TRIM( line )
            end do
         end do       
         line = ' '
         write(30,100) TRIM( line )
      end do Species_loop

      if( cache ) then
         line = '         end if'
         write(30,100) TRIM( line )
      end if
      line = '      end do'
      write(30,100) TRIM( line )

      line = ' '
      write(30,100) TRIM( line )
      if( f90 ) then
         line = '      end subroutine NLNMAT'
      else
         line = '      end'
      end if
      write(30,100) TRIM( line )

      CLOSE( 30 )

100   format(a)

      end subroutine MAKE_NLN
