! subversion Id for THIS file : $Id: socpp-soc_inti.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/socpp-soc_inti.f $
!-----------------------------------------------------------------------
! DO NOT EDIT THIS FILE!! It was automatically written by
! the socpp program. Use that program and its *.in input file
! (see directory preproc/ ) to overwrite it
!============================================================

      subroutine SOC_INTI( )
!--------------------------------------------------------
!    	... Initialize chemistry modules
!--------------------------------------------------------

      use TRACNM
      use CHEM_MODS
      use SIM_CONTROLS, only : data_dir
      use ASCII_UTILS, only : NAVU

      implicit none

!--------------------------------------------------------
!    	... Local variables
!--------------------------------------------------------
      integer  ::  i, k, ios, unit
      character (len=64) :: filename

!--------------------------------------------------------
!    	... Open chem input unit
!--------------------------------------------------------
      unit = NAVU()
      filename = data_dir(:LEN_TRIM(data_dir)) // 'preprocessed.dat'
      OPEN( unit = unit
     $,     file = filename
     $,     status = 'old'
     $,     recl   = 1024
     $,     iostat = ios )
      if( ios /= 0 ) then
         write(*,*) ' SOC_INTI: Failed to open preprocessed.dat file'
         write(*,*) ' Error code = ',ios
         stop 'Opn error'
      end if

!--------------------------------------------------------
!        ... Read map info from data file
!--------------------------------------------------------
      read(unit,'(5i4)',iostat=ios) clscnt
      if( ios /= 0 ) goto 1000
      read(unit,'(4i4)',iostat=ios) cls_rxt_cnt
      if( ios /= 0 ) goto 1000
      do k = 1,5
	 if( clscnt(k) /= 0 ) then
	    read(unit,'(20i4)',iostat=ios) base2cls(:56,k)
	    read(unit,'(20i4)',iostat=ios) clsmap(:clscnt(k),k)
            if( ios /= 0 ) goto 1000
         end if
      end do
      read(unit,'(10a8)',iostat=ios) solsym(:56)
      if( ios /= 0 ) goto 1000
      do i = 2,5
	 if( clscnt(i) /= 0 ) then
	    read(unit,'(20i4)',iostat=ios) permute(:clscnt(i),i)
            if( ios /= 0 ) goto 1000

	    if( i > 3 ) then
	       read(unit,'(20i4)',iostat=ios) permute_orig(:clscnt(i),i)
               if( ios /= 0 ) goto 1000
	       read(unit,'(20i4)',iostat=ios) diag_map(:clscnt(i))
               if( ios /= 0 ) goto 1000
	       exit
	    end if

	 end if
      end do

      CLOSE( unit )
!     call IMP_SLV_INTI() ! this routine deleted from preproc/in/vector/imp_slv.F at v6s08

      return

 1000 CLOSE( unit )
      write(*,*) ' SOC_INTI: Failed to read preprocessed.dat file'
      write(*,*) ' Error code = ',ios
      stop 'Read error'

      end subroutine SOC_INTI

