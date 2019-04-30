! subversion Id for THIS file : $Id: ver_opts.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/ver_opts.f $
!-----------------------------------------------------------------------

      subroutine VER_OPTS( options, &
                           machine, &
                           wrk_dir, &
                           subfile, &
			   diagprnt, &
			   tavgprnt, &
                           cpucnt, &
			   half_precision )
!-----------------------------------------------------------------------
!   	... Set the simulation options
!-----------------------------------------------------------------------

      use IO

      implicit none

!-----------------------------------------------------------------------
!   	... Dummy args
!-----------------------------------------------------------------------
      integer, intent(inout) ::      cpucnt
      character(len=8), intent(out)  ::  machine
      character(len=64), intent(out) ::  wrk_dir
      character(len=64), intent(out) ::  subfile
      logical, intent(out) ::      diagprnt
      logical, intent(out) ::      tavgprnt
      logical, intent(out) ::      half_precision
      logical, intent(out) ::      options(*)

!-----------------------------------------------------------------------
!   	... Local variables
!-----------------------------------------------------------------------
      integer, parameter :: maxparm = 21

      integer      :: kpar, nchar, k
      integer      :: err
      logical      :: entered(maxparm)

      character(len=20) :: keywrd
      character(len=20) :: parkey(maxparm)

      logical      :: found

      parkey(1) = 'MACHINE'
      parkey(2) = 'DIFFUSION'
      parkey(3) = 'CONVECTION'
      parkey(4) = 'NORMS'
      parkey(5) = 'CONSERVATION'
      parkey(6) = 'SOURCECODE'
      parkey(7) = 'CPUS'
      parkey(8) = 'MULTITASK'
      parkey(9) = 'FIXER'
      parkey(10) = 'DIAGPRNT'
      parkey(11) = 'RXTNLOOKUP'
      parkey(12) = 'RELHUM'
      parkey(13) = 'F90'
      parkey(14) = 'GEOHEIGHT'
      parkey(15) = 'USERHOOK'
      parkey(16) = 'MODULES'
      parkey(17) = 'WORKDIR'
      parkey(18) = 'NAMEMOD'
      parkey(19) = 'SUBFILE'
      parkey(20) = 'TAVGPRNT'
      parkey(21) = 'HALFPRECISION'

      entered = .false.

!-----------------------------------------------------------------------
!   	... Scan for valid option keyword
!-----------------------------------------------------------------------
      do
         call CARDIN( lin, buff, nchar )
	 buffh = buff
	 call UPCASE ( buffh )
         if( buffh == 'ENDENT' ) then
	    if( .not. options(1) ) then       ! not a cray target
	       if( cpucnt > 1 ) then
	          options(10) = .true.        ! "distributed" processing
	       else
	          options(10) = .false.       ! no dist processing
	       end if
	    end if
	    if( .not. options(13) ) then      ! if not fortran90 then no modules
	       options(16) = .false.
	       options(17) = .false.
	    end if
            return
	 end if
	 k = INDEX( buffh(:nchar), '=' )
         if( k /= 0 ) then
	    keywrd = buffh(:k-1)
	    found = .false.
            do kpar = 1,maxparm
               if( keywrd == parkey(kpar) ) then
		  found = .true.
	          exit
	       end if
	    end do
	 else
            call ERRMES ( ' option specification has no = operator@', &
                          lout, buff, 1, buff )

         end if
	 if( .not. found ) then
!-----------------------------------------------------------------------
!  	... Invalid parameter keyword; terminate the program
!-----------------------------------------------------------------------
            call ERRMES ( ' # is an invalid options' &
                       // ' parameter keyword@', lout, keywrd, &
                          LEN_TRIM(keywrd), buff )
         end if

!-----------------------------------------------------------------------
!     	... Valid parameter keyword; now check for duplicate keyword
!-----------------------------------------------------------------------
         if( entered(kpar) ) then
            call ERRMES( '0 *** # has already been specified@', &
                          lout, parkey(kpar), k, ' ' )
         end if

!-----------------------------------------------------------------------
!     	... Set individual options
!-----------------------------------------------------------------------
         if( kpar == 1 ) then
	    machine = buffh(k+1:nchar)
	    if( machine /= 'CRAY' .and. machine /= 'CRAYYMP' .and. &
		machine /= 'CRAY2' .and. machine /= 'CRAY3' ) then
	       options(1) = .false.
	    end if
         else if( kpar == 6 ) then
	    if( buffh(k+1:nchar) /= 'FULL' ) then
	       options(6) = .false.
	    end if
         else if( kpar == 7 ) then
	    call INTCON( buffh(k+1:), &
                         nchar - k, &
                         cpucnt, &
                         err )
	    if( err /= 0 ) then
	       call ERRMES( ' # is not a valid number@', &
                            lout, &
                            buffh(k+1:), &
                            nchar - k, &
                            buff )
	    end if
         else if( kpar == 21 ) then
	    if( buffh(k+1:nchar) == 'ON' .or. buffh(k+1:nchar) == 'YES' ) then
	       half_precision = .true.
	    end if
	 else
	    if( buffh(k+1:nchar) == 'ON' .or. buffh(k+1:nchar) == 'YES' ) then
	       if( kpar == 10) then
		  diagprnt = .true.
	       else if( kpar == 20) then
		  tavgprnt = .true.
	       else if( kpar == 18 ) then
	          options(17) = .true.
	       else if( kpar /= 8 .and. kpar /= 9 ) then
	          options(kpar) = .true.
	       else if( kpar == 8 ) then
	          options(10) = .true.
	       else if( kpar == 9 ) then
	          options(9) = .true.
	       end if
	    else
	       if( kpar == 17) then
		  wrk_dir = buff(k+1:nchar)
	       else if( kpar == 19) then
		  subfile = buff(k+1:nchar)
	       else if( kpar == 10) then
		  diagprnt = .false.
	       else if( kpar == 20) then
		  tavgprnt = .false.
	       else if( kpar /= 8 .and. kpar /= 9 ) then
	          options(kpar) = .false.
	       else if( kpar == 8 ) then
	          options(10) = .false.
	       else if( kpar == 9 ) then
	          options(9) = .false.
	       end if
	    end if
	 end if
	 entered(kpar) = .true.
      end do

      end subroutine VER_OPTS
