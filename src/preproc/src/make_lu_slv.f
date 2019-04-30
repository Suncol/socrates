! subversion Id for THIS file : $Id: make_lu_slv.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/make_lu_slv.f $
!-----------------------------------------------------------------------

      subroutine MAKE_LU_SLV( n, lu_sp_pat, machine )
!-----------------------------------------------------------------------
!        ... Write the fortran code for the sparse matrix solver
!-----------------------------------------------------------------------

      implicit none
     
!-----------------------------------------------------------------------
!        ... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in) :: n
      character(len=8), intent(in) :: machine
      logical, intent(in), dimension(n,n) :: lu_sp_pat
      
!-----------------------------------------------------------------------
!        ... Local variables
!-----------------------------------------------------------------------
      integer  ::   i, ip1, j, k, l, row, col
      integer  ::   indx, pos
      integer  ::   sp_map(n,n)
      character(len=72) :: code, comment, blank, buff
      character(len= 5) :: num
      logical  ::  lexist
      logical  ::  cache
      
      cache = .not. (machine == 'CRAY')
!-----------------------------------------------------------------------
!        ... Create and open code file; if it exists remove first
!-----------------------------------------------------------------------
      INQUIRE( file = 'lu_slv.F', exist = lexist )
      if( lexist ) then
	 call SYSTEM( 'rm lu_slv.F' )
      end if
      OPEN( unit = 30, file = 'lu_slv.F' )

!-----------------------------------------------------------------------
!        ... Form the lu matrix map
!-----------------------------------------------------------------------
      k = 0 ; sp_map = 0
      do i = 1,n
         do j = 1,n
	    if( lu_sp_pat(j,i) ) then
	       k = k + 1
	       sp_map(j,i) = k
	    end if
         end do
      end do

      code = ' ' ; blank = ' '
      comment = '!------------------------------------------------------------------------'

      write(30,100) blank
      if( .not. cache ) then
         code(7:) = 'subroutine LU_SLV( lu, b )'
      else
         code(7:) = 'subroutine LU_SLV( lu, b, kl, ku, converged )'
      end if
      write(30,100) TRIM( code )
      write(30,100) blank
      code(7:) = 'implicit none '
      write(30,100) TRIM( code )
      write(30,100) blank
      write(30,100) comment
      code = '!       ... Dummy arguments'
      write(30,100) TRIM( code )
      write(30,100) comment
      code = ' '
      write(num,'(i5)') n
      num = ADJUSTL( num )
      if( cache ) then
         code(7:) = 'integer, intent(in) ::   kl, ku'
         write(30,100) TRIM( code )
         code(7:) = 'real, intent(in)    ::   lu(4,NZCNT)'
         write(30,100) TRIM( code )
         code(7:) = 'real, intent(inout) ::   b(4,' // num(:LEN_TRIM(num)) // ')'
         write(30,100) TRIM( code )
         code(7:) = 'logical, intent(in) ::   converged(4)'
      else
         code(7:) = 'real, intent(in)    ::   lu(PLEV,NZCNT)'
         write(30,100) TRIM( code )
         code(7:) = 'real, intent(inout) ::   b(PLEV,' // num(:LEN_TRIM(num)) // ')'
      end if
      write(30,100) TRIM( code )
      write(30,100) blank
      write(30,100) comment
      code = '!       ... Local variables'
      write(30,100) code
      write(30,100) comment
      code = ' '
      code(7:) = 'integer :: k'
      write(30,100) code
      write(30,100) blank
      if( .not. cache ) then
         code = 'CDIR$ IVDEP '
         write(30,100) TRIM( code )
      end if
      write(30,100) blank
      write(30,100) comment
      code = '!       ... Solve L * y = b'
      write(30,100) code
      write(30,100) comment
      code = ' '
      if( cache ) then
         code(7:) = 'do k = kl,ku'
         write(30,100) TRIM( code )
         code(7:) = '   if( .not. converged(k) ) then'
      else
         code(7:) = 'do k = 1,PLEV'
      end if
      write(30,100) TRIM( code )
      code = ' '

!-----------------------------------------------------------------------
!        ... Solve L * y = b
!-----------------------------------------------------------------------
Forward_loop : &
      do col = 1,n-1
         write(num,'(i5)') col
	 num = ADJUSTL( num )
	 l = LEN_TRIM( num )
         buff = ' * b(k,' // num(:l) // ')'
	 do row = col+1,n
	    if( lu_sp_pat(row,col) ) then
               write(num,'(i5)') row
	       num = ADJUSTL( num )
	       l = LEN_TRIM( num )
               code(10:) = 'b(k,' // num(:l) // ') = b(k,' // num(:l) // ')'
               indx = sp_map(row,col)
               write(num,'(i5)') indx
	       num = ADJUSTL( num )
	       l = LEN_TRIM( num )
               code(LEN_TRIM(code)+2:) = '- lu(k,' // num(:l) // ')' // buff(:LEN_TRIM(buff))
               write(30,100) TRIM( code )
	    end if
	 end do
         write(30,100) blank
      end do Forward_loop

      write(30,100) blank
      write(30,100) comment
      code = '!       ... Solve U * x = y'
      write(30,100) TRIM( code )
      write(30,100) comment
      code = ' '

!-----------------------------------------------------------------------
!        ... Solve U * x = y
!-----------------------------------------------------------------------
Backward_loop : &
      do col = n,1,-1
         write(num,'(i5)') col
	 num = ADJUSTL( num )
	 l = LEN_TRIM( num )
         code(10:) = 'b(k,' // num(:l) // ') = b(k,' // num(:l) // ')'
         buff = ' * b(k,' // num(:l) // ')'
         write(num,'(i5)') sp_map(col,col)
	 num = ADJUSTL( num )
	 l = LEN_TRIM( num )
         code(LEN_TRIM(code)+2:) = '* lu(k,' // num(:l) // ')'
	 write(30,100) TRIM( code )
	 do row = col-1,1,-1
	    if( lu_sp_pat(row,col) ) then
               write(num,'(i5)') row
	       num = ADJUSTL( num )
	       l = LEN_TRIM( num )
               code(10:) = 'b(k,' // num(:l) // ') = b(k,' // num(:l) // ')'
               indx = sp_map(row,col)
               write(num,'(i5)') indx
	       num = ADJUSTL( num )
	       l = LEN_TRIM( num )
               code(LEN_TRIM(code)+2:) = '- lu(k,' // num(:l) // ')' // buff(:LEN_TRIM(buff))
               write(30,100) TRIM( code )
	    end if
	 end do
         write(30,100) blank
      end do Backward_loop

      if( cache ) then
         code(7:) = '   end if'
         write(30,100) TRIM( code )
      end if
      code(7:) = 'end do'
      write(30,100) TRIM( code )
      write(30,100) blank

      code(7:) = 'end subroutine LU_SLV'
      write(30,100) TRIM( code )

      CLOSE( 30 )

100   format(a)

      end subroutine MAKE_LU_SLV
