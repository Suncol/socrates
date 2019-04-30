! subversion Id for THIS file : $Id: mat_anal.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/mat_anal.f $
!-----------------------------------------------------------------------

      subroutine MAT_ANAL( xaction_matrix )
!--------------------------------------------------------------------
!	... Analyze the implicit solutions interactions
!--------------------------------------------------------------------
      use DFS

      implicit none

!--------------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------------
      logical, intent(in) :: xaction_matrix(:,:)

!--------------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------------
      integer :: m, row, col, vertex
      integer :: lower, upper
      integer, allocatable, dimension(:) :: rowcnt, colcnt
      logical :: lexist

      n = SIZE( xaction_matrix, dim = 1 )
      ALLOCATE( rp(n+1) )
      nz = COUNT( xaction_matrix(:,:) )
      ALLOCATE( ci(nz) )
      ALLOCATE( rowcnt(n), colcnt(n) )
      rp(1) = 1
      m = 0
      do row = 1,n
	 rowcnt(row) = COUNT( xaction_matrix(row,:) ) 
	 colcnt(row) = COUNT( xaction_matrix(:,row) ) 
	 rp(row+1) = rp(row) + rowcnt(row)
	 do col = 1,n
	    if( xaction_matrix(row,col) ) then
	       m = m + 1
	       ci(m) = col
	    end if
	 end do
      end do

!---------------------------------------------------------------------------
!	... Write the non-zero row indicies to file
!---------------------------------------------------------------------------
      INQUIRE( file = 'ctm.dfs.inp', &
	       exist = lexist )
      if( lexist ) then
	 call SYSTEM( 'rm ctm.dfs.inp' )
      end if
      OPEN( unit = 77, &
	    file = 'ctm.dfs.inp' )
      upper = 0
      do row = 1,n
         write(77,*) ci(rp(row):rp(row+1)-1)
      end do
      CLOSE( 77 )

      ALLOCATE( vstack(n), number(n), lowlink(n), perm(n), stcoblk(n), blkmemcnt(n) )
      vstack = 0 ; number = 0 ; lowlink = 0 ; stcoblk = 0 ; blkmemcnt = 0
      do vertex = 1,n
	 if( number(vertex) == 0 ) then
	    call STCO( vertex )
	 end if
      end do

      DEALLOCATE( vstack, number, lowlink, perm, stcoblk, blkmemcnt )
      DEALLOCATE( rowcnt, colcnt )

      end subroutine MAT_ANAL
