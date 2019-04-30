! subversion Id for THIS file : $Id: make_lu_fac.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/make_lu_fac.f $
!-----------------------------------------------------------------------

      subroutine MAKE_LU_FAC( n, lu_sp_pat, mat_sp_pat, sp_map, machine )
!-----------------------------------------------------------------------
!        ... Write the fortran code for the sparse matrix decomposition
!-----------------------------------------------------------------------

      implicit none
     
!-----------------------------------------------------------------------
!        ... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in) :: n
      integer, intent(in) :: sp_map(n,n)
      character(len=8), intent(in) :: machine
      logical, intent(in), dimension(n,n) :: lu_sp_pat, mat_sp_pat
      
!-----------------------------------------------------------------------
!        ... Local variables
!-----------------------------------------------------------------------
      integer  ::   i, ip1, j, k, l, row, col
      integer  ::   indx, pos
      character(len=72) :: code, comment, blank, buff
      character(len= 5) :: num
      logical  ::  lexist
      logical  ::  cache
      logical  ::  sp_pat(n,n)
      
     cache = .not. (machine == 'CRAY')
!-----------------------------------------------------------------------
!        ... Create and open code file; if it exists remove first
!-----------------------------------------------------------------------
      INQUIRE( file = 'lu_fac.F', exist = lexist )
      if( lexist ) then
	 call SYSTEM( 'rm lu_fac.F' )
      end if
      OPEN( unit = 30, file = 'lu_fac.F' )

      sp_pat = mat_sp_pat
      code = ' ' ; blank = ' '
      comment = '!------------------------------------------------------------------------'

      write(30,100) blank
      if( .not. cache ) then
         code(7:) = 'subroutine LU_FAC( lu )'
      else
         code(7:) = 'subroutine LU_FAC( lu, kl, ku, converged )'
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
      if( cache ) then
         code(7:) = 'integer, intent(in) ::   kl, ku'
         write(30,100) TRIM( code )
         code(7:) = 'real, intent(inout) ::   lu(4,NZCNT)'
         write(30,100) TRIM( code )
         code(7:) = 'logical, intent(in) ::   converged(4)'
      else
         code(7:) = 'real, intent(inout) ::   lu(PLEV,NZCNT)'
      end if
      write(30,100) TRIM( code )
      if( cache ) then
      end if
      write(30,100) blank
      write(30,100) comment
      code = '!       ... Local variables'
      write(30,100) TRIM( code )
      write(30,100) comment
      code = ' '
      code(7:) = 'integer :: k'
      write(30,100) TRIM( code )
      write(30,100) blank
      if( .not. cache ) then
         code = 'CDIR$ IVDEP'
         write(30,100) TRIM( code )
      end if
      code = ' '
      if( .not. cache ) then
         code(7:) = 'do k = 1,PLEV'
      else
         code(7:) = 'do k = kl,ku'
         write(30,100) TRIM( code )
         code(7:) = '   if( .not. converged(k) ) then'
      end if
      write(30,100) TRIM( code )
      code = ' '

Column_loop : &
      do i = 1,n
!-----------------------------------------------------------------------
!        ... Form diagonal inverse
!-----------------------------------------------------------------------
         indx = sp_map(i,i)
         write(num,'(i5)') indx
	 num = ADJUSTL( num )
	 l = LEN_TRIM( num )
         code(10:) = 'lu(k,' // num(:l) // ') = 1. / lu(k,' // num(:l) // ')'
         write(30,100) TRIM( code )
         buff = ' * lu(k,' // num(:l) // ')'
	 ip1 = i + 1
!-----------------------------------------------------------------------
!        ... Multiply column below diagonal
!-----------------------------------------------------------------------
	 do row = ip1,n
	    if( sp_pat(row,i) ) then
               indx = sp_map(row,i)
               write(num,'(i5)') indx
	       num = ADJUSTL( num )
	       l = LEN_TRIM( num )
               code(10:) = 'lu(k,' // num(:l) // ') = lu(k,' // num(:l) // ')' &
				   // buff(:LEN_TRIM(buff))
               write(30,100) TRIM( code )
	    end if
	 end do
!-----------------------------------------------------------------------
!        ... Modify sub-matrix
!-----------------------------------------------------------------------
	 do col = ip1,n
	    if( sp_pat(i,col) ) then
               indx = sp_map(i,col)
               write(num,'(i5)') indx
	       num = ADJUSTL( num )
	       l = LEN_TRIM( num )
               buff = ' * lu(k,' // num(:l) // ')'
	       do row = ip1,n
	          if( sp_pat(row,i) ) then
                     indx = sp_map(row,col)
                     write(num,'(i5)') indx
	             num = ADJUSTL( num )
	             l = LEN_TRIM( num )
	             if( sp_pat(row,col) ) then
                        code(10:) = 'lu(k,' // num(:l) // ') = lu(k,' // num(:l) // ')'
                        indx = sp_map(row,i)
                        write(num,'(i5)') indx
	                num = ADJUSTL( num )
	                l = LEN_TRIM( num )
                        code(LEN_TRIM(code)+2:) = '- lu(k,' // num(:l) // ')' // buff(:LEN_TRIM(buff))
                        write(30,100) TRIM( code )
		        code(6:) = ' '
		     else
			sp_pat(row,col) = .true.
                        code(10:) = 'lu(k,' // num(:l) // ') = '
                        indx = sp_map(row,i)
                        write(num,'(i5)') indx
	                num = ADJUSTL( num )
	                l = LEN_TRIM( num )
		        pos = INDEX( code,'=' ) + 2
                        code(pos:) = '- lu(k,' // num(:l) // ')' // buff(:LEN_TRIM(buff))
                        write(30,100) TRIM( code )
	             end if
	          end if
	       end do
	    end if
	 end do
         write(30,100) blank
      end do Column_loop

      if( cache ) then
         code(7:) = '   end if'
         write(30,100) TRIM( code )
      end if
      code(7:) = 'end do'
      write(30,100) TRIM( code )
      write(30,100) blank

      code(7:) = 'end subroutine LU_FAC'
      write(30,100) TRIM( code )

      CLOSE( 30 )

100   format(a)

      end subroutine MAKE_LU_FAC
