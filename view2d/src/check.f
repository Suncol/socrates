! subversion Id for THIS file : $Id: check.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/check.f $
!-----------------------------------------------------------------------
      subroutine CHECK_CTRLS()
!-----------------------------------------------------------------------
!     	... Check basic simulation control parms
!-----------------------------------------------------------------------
      use GRID_DIMS, only : lmax
      use TIME_CONTROLS
      use SIM_CONTROLS
      use DIAG_CONTROLS, only : diags
      use ALLCO, only : omp_lats   ! output
      use DATE_UTILS, only : DATE2TIME, TIME2DATE

      implicit none

!-----------------------------------------------------------------------
!     	... Local variables
!-----------------------------------------------------------------------
      integer :: mode, delta_days, i, j, l
      type( TIMING ) :: ran_times

!-----------------------------------------------------------------------
!     	... Check model_type and start_from (initial conditions mode)
!-----------------------------------------------------------------------
      if( model_type == 'two_d' ) then
         if( start_from == 'ascii' ) then
            write(*,*) 'CHECK_CTRLS: error in input namelist ' //
     $       'CONTROL_PARMS : model_type="two_d" but start_from="ascii"'
            write(*,*) '   set model_type to "one_d" or "zerod", *or* '
     $       // 'set start_from to "newnc"'
            stop 'CHECK_CTRLS: conflict between model_type & start_from'
         else if(start_from == 'newnc' .and. rstrt_filespec == ' ') then
            write(*,*) 'CHECK_CTRLS: error in input namelist '
            write(*,*) '    CONTROL_PARMS : start_from="newnc" but ' //
     $       'rstrt_filespec not set'
            write(*,*) '   set rstrt_filespec to a valid netCDF file'
            stop 'CHECK_CTRLS: input var rstrt_filespec not set'
         else if( start_from /= 'newnc' ) then
            write(*,*) 'CHECK_CTRLS: error in input namelist '
            write(*,*) '    CONTROL_PARMS : start_from can be set only '
     $       // 'to "ascii" or "newnc"'
            stop 'CHECK_CTRLS: input var start_from has illegal value'
         end if
       else if( model_type == 'one_d' .or. model_type == 'zerod' ) then
         mainsw(1) = 2
         mainsw(2) = 1
       else if( model_type /= 'tests' ) then
         write(*,*)'CHECK_CTRLS: error in input namelist CONTROL_PARMS:'
         write(*,*) '    model_type set to "',model_type,'"'
         write(*,*) '    should be "two_d", "one_d", "tests" or "zerod"'
         stop 'CHECK_CTRLS: input var model_type has illegal value'
      end if

!-----------------------------------------------------------------------
!     	... Check simulation start & end dates, duration, time step
!-----------------------------------------------------------------------
      if( sim_start_time%month == -1 ) then
	 write(*,*) 'CHECK_CTRLS : No simulation start date'
	 stop 'CHECK_CTRLS: No simulation start date'
      end if
      call DATE2TIME( sim_start_time )
      if( sim_stop_time%month == -1 ) then
	 write(*,*) 'CHECK_CTRLS : No simulation ending date'
	 stop 'CHECK_CTRLS: No simulation ending date'
      end if
      call DATE2TIME ( sim_stop_time )
      delta_days = sim_stop_time%days0 - sim_start_time%days0     ! simulation duration in days
      if( delta_days <= 0 ) then
         write(*,'('' Simulation start date = '',i2,''/'',i2,
     $          ''/'',i4)') sim_start_time%month,
     $                      sim_start_time%day,
     $                      sim_start_time%year
         write(*,*) ' sim_start_time%days0 = ',sim_start_time%days0
         write(*,'('' Simulation stop date  = '',i2,''/'',i2,
     $          ''/'',i4)') sim_stop_time%month,
     $                      sim_stop_time%day,
     $                      sim_stop_time%year
         write(*,*) ' sim_stop_time%days0 = ',sim_stop_time%days0
	 write(*,*) 'CHECK_CTRLS : Simulation duration <= 0'
	 stop 'CHECK_CTRLS : Simulation duration <= 0'
       else
	 delta_days = delta_days + 1
	 sim_stop_time%days0 = sim_start_time%days0 + delta_days
	 call TIME2DATE( sim_stop_time )
      end if
      ntstep = sim_stop_time%days0 - sim_start_time%days0
      if( daypas < 1 .or. daypas > 30 ) then
	 write(*,*) 'CHECK_CTRLS : daypas= ',daypas,' is illegal value'
	 stop 'CHECK_CTRLS : daypas has illegal value'
      end if

!-----------------------------------------------------------------------
!     	... Check ncpus, the nb of CPUs, and prepare the optimal 
!          latitude order for OpenMP parallelization across lat
!-----------------------------------------------------------------------
      if( MOD(lmax,ncpus) /= 0 ) then
         omp_lats(:) = (/ ( l, l=1, lmax ) /)
       else
         do i = 1, ncpus
            do j = 0, lmax/ncpus - 1
               l = 1+j + (lmax/ncpus) * (i-1)
               if( l>lmax ) stop 'CHECK_CTRLS: omp_lats, error 1'
               omp_lats(l) = i + j*ncpus
            end do
         end do
         if( ANY( omp_lats>lmax ) ) stop'CHECK_CTRLS: omp_lats error2'
         if( ANY( omp_lats< 1 ) ) stop 'CHECK_CTRLS: omp_lats error 3'
         if( diags ) then
            write(*,*)'CHECK_CTRLS: omp_lats set for ncpus=',ncpus
            write(*,'(a12,35i3)') 'l= ',( l, l=1, lmax )
            write(*,'(a12,35i3)')'omp_lats= ',( omp_lats(l),l=1,lmax )
         end if
      end if

!-----------------------------------------------------------------------
!     	... SIM_CONTROLS variables
!-----------------------------------------------------------------------      
      if( mainsw(11) > 0 .and. ncpus > 1 ) then
	 write(*,*) 'CHECK_CTRLS : error setting mainsw(11)= ',mainsw(11)
     $     ,mainsw(11),' and ncpus= ',ncpus
         write(*,*) '     mainsw(11)/=0 allowed only for ncpus=1'
	 stop 'CHECK_CTRLS: mainsw(11)/=0 allowed only for ncpus=1'
      end if
      if( ANY( het_sw(1:6) /= 0 ) ) then
         write(*,*) 'CHECK_CTRLS: error setting het_sw(1:6) /= 0'
         write(*,*) ' All chem effects of PSC, PMC, aerosols removed',
     $                     'in v6s* versions'
        stop 'CHECK_CTRLS: error setting het_sw(1:6) /= 0'
      end if
      
      end subroutine CHECK_CTRLS

!=======================================================================
       
      subroutine CHECK_TOTAL_MOLEC( postr, time, vid )
!--------------------------------------------------------------
!	... Compute and print the total number of molecules 
!           of species vid
!--------------------------------------------------------------
      use SPECIES, only : qn2noon                              ! input
      use TRACNM, only : solsym
      use SIM_CONTROLS, only : label_short
      use TIME_CONTROLS, only : TIMING
      use ASCII_UTILS, only : NAVU

      implicit none
!--------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------
      character(len=*), intent(in) :: postr
      type( TIMING ), intent(in) :: time
      integer, intent(in) :: vid
      
!--------------------------------------------------------------
!	... local variables
!--------------------------------------------------------------
      real :: nitot                             ! molec
      integer, save :: ou
      logical, save :: entered = .false.
      real, external :: TOTAL_MOLEC       ! function declaration
      
      if( .not. entered ) then
         ou = NAVU()
         OPEN( ou, file=label_short(:LEN_TRIM(label_short)) // 
     $         '.total_molec.dat', form='FORMATTED' )
         entered = .true.
      end if
      
      nitot = TOTAL_MOLEC( qn2noon(:,:,vid) )
      
      write(ou,'(a37,i2,''/'',i2,''/'',i4,a25,es11.5,a)') 
     $   'CHECK_TOTAL_MOLEC '//postr//',',time%month,time%day,time%year,
     $    ', species '//solsym(vid)(:3)//' noon: ', nitot,' molecules'
     
      end subroutine CHECK_TOTAL_MOLEC
      
!=======================================================================
       
      subroutine CHECK_VALS( vals, valmin, valmax, valname, 
     $                       dyn_step, date, header )
!--------------------------------------------------------------------
!	... Report abnormal temperatures
!--------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use TIME_CONTROLS
      use DIAG_CONTROLS, only : debug
      use ARCH_WHAT, only : arch

      implicit none

!--------------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------------
      real, dimension(lmax,niz), intent(in) :: vals
      real, intent(in) :: valmin, valmax
      character(len=*), intent(in) :: valname, header
      integer, intent(in)          :: dyn_step
      type( TIMING), intent(in)    :: date

!--------------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------------
      integer :: lat, badvals, inds(1)
      logical :: found

      found = .false.
      do lat = 1, lmax
	 badvals = COUNT( vals(lat,:) < valmin )
	 if( badvals > 0 ) then
            write(*,'(a,es12.5,2(a,i3),a)')'CHECK_VALS warning: '
     $          //valname//' < ',valmin,' at lat index= ',lat,' for '
     $           ,badvals,' altitudes'
	    inds(:) = MINLOC( vals(lat,:) )
            write(*,*) '    Min value = ',MINVAL( vals(lat,:) )
            write(*,*) '    Min alt index = ',inds
	    found = .true.
	 end if
	 badvals = COUNT( vals(lat,:) > valmax )
	 if( badvals > 0 ) then
            write(*,'(a,es12.5,2(a,i3),a)')'CHECK_VALS warning: '
     $         //valname//' > ',valmax,' at lat index= ',lat,' for '
     $         ,badvals,' altitudes'
	    inds(:) = MAXLOC( vals(lat,:) )
            write(*,*) '    Max value = ',MAXVAL( vals(lat,:) )
            write(*,*) '    Max alt index = ',inds
	    found = .true.
	 end if
      end do

      if( found ) then
         write(*,'(2a,i2,''/'',i2,''/'',i4,a,i3)')
     $      header(:LEN_TRIM(header)),' ; sim time= ',
     $      date%month,date%day,date%year,' ; dyn_step= ',dyn_step
      end if
      
      if( found .and. debug ) then
         write(*,*)'CHECK_VALS called after ',header(:LEN_TRIM(header))
         write(*,*)' Fatal error, archiving and aborting...'
         arch(2)%active = .true.           ! arch(2)%mode = 'heat '
         arch(:)%days0do(1) = date%days0
         call ARCH_PLUG( date )
         stop 'CHECK_VALS: a variable is out of range, see stdout'
      end if

      end subroutine CHECK_VALS
      
!=======================================================================
       
      subroutine CHECK_NEG_DA( header, lat, current_time )
!--------------------------------------------------------------------
!	... Report negative mixing ratios
!--------------------------------------------------------------------
      use SPECIES_DIMS, only : nbcon
      use TRACNM
      use SPECIES, only : qn2da
      use TIME_CONTROLS
      use DIAG_CONTROLS, only : debug

      implicit none

!--------------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------------
      character(len=*), intent(in) :: header
      integer, intent(in) :: lat
      type( TIMING), intent(in)    :: current_time

!--------------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------------
      integer :: m, badvals, inds(1)
      logical :: found

      found = .false.
      do m = 1, nbcon
	 badvals = COUNT( qn2da(lat,:,m) < -1.e-16 )
	 if( badvals > 0 ) then
	    if( .not. found ) then
               write(*,*)'CHECK_NEG_DA warning: neg values, lat index= ',lat
               write(*,'(a,'', sim time = '',i2,''/'',i2,''/'',i4)')
     $                     header(:LEN_TRIM(header)),current_time%month,
     $                     current_time%day,current_time%year
	    end if
	    write(*,*) '  Species ',solsym(m),' has ',badvals
     $                                            ,' neg values'
	    inds(:) = MINLOC( qn2da(lat,:,m) )
            write(*,*) '    Min value = ',MINVAL( qn2da(lat,:,m) )
            write(*,*) '    Min alt index = ',inds
	    found = .true.
	 end if
	 badvals = COUNT( qn2da(lat,:,m) > 1. )
	 if( badvals > 0 ) then
	    if( .not. found ) then
               write(*,*)'CHECK_NEG_DA warning: vals>1, lat index= ',lat
               write(*,'(a,'', sim time = '',i2,''/'',i2,''/'',i4)')
     $                     header(:LEN_TRIM(header)),current_time%month,
     $                     current_time%day,current_time%year
	    end if
	    write(*,*) '  Species ',solsym(m),' has ',badvals
     $                                            ,'  values > 1'
	    inds(:) = MAXLOC( qn2da(lat,:,m) )
            write(*,*) '    Max value = ',MAXVAL( qn2da(lat,:,m) )
            write(*,*) '    Max alt index = ',inds
	    found = .true.
	 end if
      end do

      if( found .and. debug ) then
         stop 'CHECK_NEG_DA: wrong vals for chem species, see stdout'
      end if

      end subroutine CHECK_NEG_DA
