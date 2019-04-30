! subversion Id for THIS file : $Id: soc_mods.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/soc_mods.f $
!-----------------------------------------------------------------------
      module IO

      implicit none

      integer :: lin = 5                ! input unit number
      integer :: lout = 6               ! output unit number
      character(len=120) :: buff        ! primary line input buffer
      character(len=120) :: buffh       ! upcase xform of buff

      end module IO

      module LIMITS
!-------------------------------------------------------------------
!	... Simulation limits
!-------------------------------------------------------------------

      implicit none

      integer, parameter :: var_lim = 1000
      integer, parameter :: rxt_lim = 5000

      end module LIMITS

      module DFS

      implicit none

      integer :: n             ! order of matrix
      integer :: nz            ! # of non=zero elements
      integer :: sp = 0        ! stack pointer
      integer :: nb = 0        ! search counter
      integer :: pp = 0        ! perm vector position
      integer :: blkcnt = 0    ! strongly connected blk count
      integer, allocatable, dimension(:) :: &
                 number, &
	         lowlink, &
	         vstack, &
	         perm, &
	         rp, &
	         ci, &
	         stcoblk, &
	         blkmemcnt

      end module DFS

      module SYMBOLS

      use LIMITS

      implicit none

      integer, target  :: spccnt(7)
      integer, pointer :: new_nq, grp_mem_cnt, nq, relcnt, nfs, ngrp, ncol
      character(len=8), target  :: spcsym(var_lim,7)
      character(len=8), pointer, dimension(:) :: pcesym, solsym, fixsym, grpsym, colsym, &
                                                 new_solsym, grp_mem_sym
      character(len=8) :: phtsym(rxt_lim)

      end module SYMBOLS
