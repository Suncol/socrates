! subversion Id for THIS file : $Id: sp_mods.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/sp_mods.f $
!-----------------------------------------------------------------------

      module SP_MODS
	 integer :: n  = 0        ! order of matrix
	 integer :: nz = 0        ! # of non=zero elements
	 integer :: sp = 0        ! stack pointer
	 integer :: nb = 0        ! search counter
	 integer :: pp = 0        ! perm vector position
	 integer :: blkcnt = 0    ! strongly connected blk count
	 integer, allocatable :: number(:)
	 integer, allocatable :: lowlink(:)
	 integer, allocatable :: vstack(:)
	 integer, allocatable :: perm(:)
	 integer, allocatable :: rp(:)
	 integer, allocatable :: ci(:)
	 integer, allocatable :: stcoblk(:)
	 integer, allocatable :: blkmemcnt(:)
	 logical, allocatable :: matrix(:,:)
      end module SP_MODS

