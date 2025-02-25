! subversion Id for THIS file : $Id: eqrep.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/eqrep.f $
!-----------------------------------------------------------------------

      subroutine EQUATION_REP(  &
                   prdcnt, &
                   prdmap, &
                   rxntot, &
                   rxmcnt, &
                   rxmap, &
                   coeff_cnt, &
                   coeff_ind, &
                   coeffs, &
                   fxmcnt, &
                   fixmap, &
                   rxt_alias, &
                   phtcnt )
!-----------------------------------------------------------------------
!    	... The equation writing utility
!-----------------------------------------------------------------------
     
      use IO, only : lout
      use LIMITS
      use SYMBOLS, only : nq => new_nq, nfix => nfs, solsym, fixsym

      implicit none

!-----------------------------------------------------------------------
!    	... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in) ::  prdcnt, &
                              rxntot, &
			      coeff_cnt, &
			      phtcnt
     
      integer, intent(in) ::  prdmap(var_lim,5), &
			      rxmcnt(2), &
			      rxmap(rxt_lim,7,2), &
                              coeff_ind(rxt_lim), &
			      fxmcnt(2), &
			      fixmap(var_lim,3,2)
     
      real, intent(in)    ::  coeffs(4,rxt_lim)
      
      character(len=8), intent(in) ::  rxt_alias(rxt_lim)
      
!-----------------------------------------------------------------------
!    	... Local variables
!-----------------------------------------------------------------------
      integer  ::   i, j, k, l
      integer  ::   spc_num, length, beg_mark
      integer  ::   line_pos, line_num, buf_pos, rxno
      character(len=120) ::  lines(2)
      character(len=80)  ::  eq_piece
      character(len=8)   ::  symbol
      character(len=5)   ::  num
      logical  ::  production, destruction, blow_off, quadratic
      
      write(lout,'(''1 Equation Report'')')
      
      do spc_num = 1,nq
         production = .false.
         destruction = .false.
         line_num = 1
         lines = ' '
         lines(1) = '0   d('
         length = LEN_TRIM( solsym(spc_num) )
         lines(1)(7:) = solsym(spc_num)(:length)
         line_pos = 7 + length
         lines(1)(line_pos:) = ')/dt = '
         line_pos = line_pos + 7
         beg_mark = line_pos
!-----------------------------------------------------------------------
!    	... Scan the "independent" production map for product target
!-----------------------------------------------------------------------
         do i = 1,prdcnt
            do k = 2,5
               if( prdmap(i,k) == 0 ) then
                  exit
               else if( prdmap(i,k) /= spc_num ) then
                  cycle
               end if
               eq_piece = ' '
               if( production ) then
                  eq_piece = ' + '
                  buf_pos = 4
               else
                  buf_pos = 1
               end if
               production = .true.
               rxno = prdmap(i,1)
               if( coeff_ind(rxno) /= 0 .and. coeffs(k-1,coeff_ind(rxno)) /= 1.e0 ) then
                  call NUMCON( eq_piece(buf_pos:), coeffs(k-1,coeff_ind(rxno)), 'l' )
                  buf_pos = LEN_TRIM( eq_piece ) + 1
		  if( rxt_alias(rxno) /= ' ' ) then
                     eq_piece(buf_pos:) = '*' // rxt_alias(rxno)
                     buf_pos = buf_pos + LEN_TRIM( rxt_alias(rxno) )
		  else
                     if( rxno > phtcnt ) then
                        eq_piece(buf_pos:) = '*r'
                     else
                        eq_piece(buf_pos:) = '*j'
                     end if
                     buf_pos = buf_pos + 2
                  end if
               else
		  if( rxt_alias(rxno) /= ' ' ) then
                     eq_piece(buf_pos:) = rxt_alias(rxno)
                     buf_pos = buf_pos + LEN_TRIM( rxt_alias(rxno) )
		  else
                     if( rxno > phtcnt ) then
                        eq_piece(buf_pos:) = 'r'
                     else
                        eq_piece(buf_pos:) = 'j'
                     end if
                     buf_pos = buf_pos + 1
                  end if
               end if
	       if( rxt_alias(rxno) == ' ' ) then
                  if( rxno > phtcnt ) then
                     call NUMCON( eq_piece(buf_pos:), REAL(rxno-phtcnt), 'l' )
                  else
                     call NUMCON( eq_piece(buf_pos:), REAL(rxno), 'l' )
                  end if
               end if
               buf_pos = LEN_TRIM( eq_piece ) + 1
               call SET_FIXED_REACTANTS( fixmap, var_lim,       3, &
                             rxno,       eq_piece, buf_pos, &
                             phtcnt )
               length = buf_pos
               if( (line_pos+length) > 120 ) then  ! inc line count
                  line_pos = beg_mark
                  line_num = line_num + 1
                  if( line_num > 2 ) then         ! write out the buffer
                     write(lout,'(a120)') lines
                     line_num = 1
                     lines = ' '
                  end if
               end if
               lines(line_num)(line_pos:) = eq_piece(:length)
               line_pos = line_pos + length
            end do
         end do

!-----------------------------------------------------------------------
!    	... Scan the "regular" reaction map for product target
!-----------------------------------------------------------------------
         do i = 1,2
            do j = 1,rxmcnt(i)
               do k = i+2,i+5
                  if( rxmap(j,k,i) == 0 ) then
                     exit
                  else if( rxmap(j,k,i) /= spc_num ) then
                     cycle
                  end if
                  eq_piece = ' '
                  if( production ) then
                     eq_piece = ' + '
                     buf_pos = 4
                  else
                     buf_pos = 1
                  end if
                  production = .true.
                  rxno = rxmap(j,1,i)
                  if( coeff_ind(rxno) /= 0 .and. coeffs(k-(i+1),coeff_ind(rxno)) /= 1.e0 ) then
                     call NUMCON( eq_piece(buf_pos:), coeffs(k-(i+1),coeff_ind(rxno)), 'l' )
                     buf_pos = LEN_TRIM( eq_piece ) + 1
		     if( rxt_alias(rxno) /= ' ' ) then
                        eq_piece(buf_pos:) = '*' // rxt_alias(rxno)
                        buf_pos = buf_pos + LEN_TRIM( rxt_alias(rxno) )
		     else
                        if( rxno > phtcnt ) then
                           eq_piece(buf_pos:) = '*r'
                        else
                           eq_piece(buf_pos:) = '*j'
                        end if
                        buf_pos = buf_pos + 2
                     end if
                  else
		     if( rxt_alias(rxno) /= ' ' ) then
                        eq_piece(buf_pos:) = rxt_alias(rxno)
                        buf_pos = buf_pos + LEN_TRIM( rxt_alias(rxno) )
		     else
                        if( rxno > phtcnt ) then
                           eq_piece(buf_pos:) = 'r'
                        else
                           eq_piece(buf_pos:) = 'j'
                        end if
                        buf_pos = buf_pos + 1
                     end if
                  end if
		  if( rxt_alias(rxno) == ' ' ) then
                     if( rxno > phtcnt ) then
                        call NUMCON( eq_piece(buf_pos:), REAL(rxno-phtcnt), 'l' )
                     else
                        call NUMCON( eq_piece(buf_pos:), REAL(rxno), 'l' )
                     end if
                  end if
                  buf_pos = LEN_TRIM( eq_piece ) + 1
                  call SET_FIXED_REACTANTS( fixmap, var_lim,       3, &
                             rxno,          eq_piece, buf_pos, &
                             phtcnt )
                  do l = 2,i+1
                     if( rxmap(j,l,i) == 0 ) then
                        exit
                     end if
                     symbol = solsym(ABS(rxmap(j,l,i)))
                     length = LEN_TRIM( symbol )
                     eq_piece(buf_pos:) = '*' // symbol(:length)
                     buf_pos = buf_pos + length + 1
                  end do
                  length = buf_pos
                  if( (line_pos+length) > 120 ) then  !inc line count
                     line_pos = beg_mark
                     line_num = line_num + 1
                     if( line_num > 2 ) then   !write out the buffer
                        write(lout,'(a120)') lines
                        line_num = 1
                        lines = ' '
                     end if
                  end if
                  lines(line_num)(line_pos:) = eq_piece(:length)
                  line_pos = line_pos + length
               end do
            end do
         end do
!-----------------------------------------------------------------------
!    	... If buffer has unprinted lines flush it
!-----------------------------------------------------------------------
         if( production ) then
            do l = 1,line_num
               if( lines(l) /= ' ' ) then
                  write(lout,'(a120)') lines(l)
               end if
            end do
            lines(1:line_num) = ' '
            line_num = 1
            line_pos = beg_mark
         end if

!-----------------------------------------------------------------------
!    	... Scan the "regular" reaction map for reactant target
!-----------------------------------------------------------------------
         do i = 1,2
            do j = 1,rxmcnt(i)
               blow_off = .false.
               do k = 2,i+1
                  eq_piece = ' '
                  if( rxmap(j,k,i) /= spc_num ) then
                     cycle
                  end if
                  eq_piece = ' - '
                  buf_pos = 4
                  if( i == 2 .and. k == 2 ) then
                     if( rxmap(j,3,i) == spc_num ) then
                        eq_piece(buf_pos:) = '2*'
                        buf_pos   = buf_pos + 2
                        blow_off  = .true.
                        quadratic = .true.
                     else
                        quadratic = .false.
                     end if
                  end if
                  destruction = .true.
                  rxno = rxmap(j,1,i)
		  if( rxt_alias(rxno) /= ' ' ) then
                     eq_piece(buf_pos:) = rxt_alias(rxno)
                     buf_pos = buf_pos + LEN_TRIM( rxt_alias(rxno) )
		  else
                     if( rxno > phtcnt ) then
                        eq_piece(buf_pos:) = 'r'
                     else
                        eq_piece(buf_pos:) = 'j'
                     end if
                     buf_pos = buf_pos + 1
                  end if
		  if( rxt_alias(rxno) == ' ' ) then
                     if( rxno > phtcnt ) then
		        write(num,'(i5)') rxno - phtcnt
                     else
		        write(num,'(i5)') rxno
                     end if
		     num = ADJUSTL( num )
		     eq_piece(buf_pos:) = num(:LEN_TRIM(num))
                  end if
                  buf_pos = LEN_TRIM( eq_piece ) + 1
                  if( i == 1 ) then
                     blow_off = .true.
                  end if
                  call SET_FIXED_REACTANTS( fixmap, var_lim,       3, &
                             rxno,          eq_piece, buf_pos, &
                             phtcnt )
                  if( blow_off ) then
                     symbol = solsym(spc_num)
                  else
                     do l = 2,i+1
                        if( ABS(rxmap(j,l,i)) == spc_num ) then
                           cycle
                        end if
                        symbol = solsym(ABS(rxmap(j,l,i)))
                     end do
                  end if
                  length = LEN_TRIM( symbol )
                  eq_piece(buf_pos:) = '*' // symbol(:length)
                  buf_pos = buf_pos + length + 1
                  if( .not. blow_off .or. quadratic ) then
                     symbol = solsym(spc_num)
                     length = LEN_TRIM( symbol )
                     eq_piece(buf_pos:) = '*' // symbol(:length)
                     buf_pos = buf_pos + length + 1
                  end if
                  length = buf_pos
                  if( (line_pos+length) > 120 ) then  ! inc line count
                     line_pos = beg_mark
                     line_num = line_num + 1
                     if( line_num > 2 ) then          ! write out the buffer
                        write(lout,'(a120)') lines
                        line_num = 1
                        lines = ' '
                     end if
                  end if
                  lines(line_num)(line_pos:) = eq_piece(:length)
                  line_pos = line_pos + length
                  if( blow_off ) then
                     exit
                  end if
               end do
            end do
         end do

         if( .not. production .and. .not. destruction ) then
            lines(line_num)(line_pos:) = '0'
         end if

!-----------------------------------------------------------------------
!     	... If buffer has unprinted lines flush it
!-----------------------------------------------------------------------
         do l = 1,line_num
            if( lines(l) /= ' ' ) then
               write(lout,'(a120)') lines(l)
            end if
         end do
         
      end do
      
      end subroutine EQUATION_REP
      
      integer function GET_INDEX( array, rdim, cdim, scol, key )
      
      implicit none

!-----------------------------------------------------------------------
!     	... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in) ::      rdim,     cdim,     scol,     key
      integer, intent(in) ::      array(rdim,cdim)
      
!-----------------------------------------------------------------------
!     	... Local variables
!-----------------------------------------------------------------------
      integer  ::  i
      
      do i = 1,rdim
         if( array(i,scol) == key ) then
            GET_INDEX = i
            return
         end if
      end do
      
      GET_INDEX = 0
      
      end function GET_INDEX
      
      subroutine SET_FIXED_REACTANTS( fixmap, &
                                      rowdim, &
                                      coldim, &
                                      rxno, &
                                      eq_piece, &
                                      buf_pos, &
                                      phtcnt )
     
      use LIMITS
      use SYMBOLS, only : fixsym

      implicit none

!-----------------------------------------------------------------------
!     	... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in) ::  rowdim,        coldim,        phtcnt
      integer, intent(in) ::  fixmap(rowdim,coldim,1)
      integer, intent(inout) :: rxno
      integer, intent(inout) :: buf_pos
      
      character(len=80), intent(out) ::  eq_piece
      
!-----------------------------------------------------------------------
!     	... Local variables
!-----------------------------------------------------------------------
      integer  ::  j, l, index, length
      character(len=8) ::  symbol

      integer  ::  GET_INDEX
      
      if( rxno < phtcnt ) then
         rxno = - rxno
      end if
      do j = 1,2
         index = GET_INDEX( fixmap(1,1,j), var_lim, 3, 1, rxno )
         if( index /= 0 ) then
            exit
         end if
      end do

      do l = 2,3
         if( fixmap(index,l,j) == 0 ) then
            return
         end if
         symbol = fixsym(fixmap(index,l,j))
         length = LEN_TRIM( symbol )
         eq_piece(buf_pos:) = '*' // symbol(:length)
         buf_pos = buf_pos + length + 1
      end do

      end subroutine SET_FIXED_REACTANTS
