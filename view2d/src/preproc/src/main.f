! subversion Id for THIS file : $Id: main.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/main.f $
!-----------------------------------------------------------------------

      program SOC_CHEMPP
!-----------------------------------------------------------------------
!        ... Socrates pre-processor
!            This program was written by Stacy Walters 
!            (NCAR/ACD, stacy@acd.ucar.edu) and downloaded from
!            acd.ucar.edu://ur/stacy/socrates/ftend by simonc@oma.be
!            on April 5, 2000. It was then simplified to write only
!            preprocessed.mod.f and preprocessed.f on April 13, 2000.
!            The original name of this file is soc_chprm.new.f
!-----------------------------------------------------------------------

      use IO
      use LIMITS
      use SYMBOLS

      implicit none

!-----------------------------------------------------------------------
!        ... Local variables
!-----------------------------------------------------------------------
      integer  ::  fixmap(var_lim,3,2), &
                   prdmap(var_lim,5), &
                   fixcnt(2), &                     ! count of reactions with fixed rxtnts
                   rxmcnt(2), &                     ! count of reactions with sol rxtnts
                   ipcel(2), &                      ! not used
                   ipcep(3)                         ! not used
      integer  ::  prdcnt = 0, &                    ! entries in prdmap matrix
                   rxpcnt = 0                       ! entries in rxparm matrix

      integer  ::  rxmap(rxt_lim,7,2), &
                   pcel(var_lim,6,2), &                  ! not used
                   pcep(var_lim,5,3)                     ! not used

      integer  ::  rxptab(rxt_lim), &
                   grpflg(var_lim), &
                   mem2grp_map(var_lim), &
                   newind(var_lim), &
                   hetmap(var_lim,2), &
                   usrmap(var_lim), &
                   relmap(var_lim,2), &
                   grpmap(20,20), &
                   grpcnt(20), &
                   colmap(var_lim), &
                   rel_flg(var_lim)
      
      integer  ::  srf_flx_map(var_lim)             ! surface flux flag
      integer  ::  srf_flx_cnt = 0                  ! count of soln species with surface emissions
      integer  ::  dvel_map(var_lim)                ! deposition flux flag
      integer  ::  dvel_cnt = 0                     ! count of soln species with deposition flux

!-----------------------------------------------------------------------
!        ... The group and relationship counters and maps
!-----------------------------------------------------------------------
      integer  ::  grp_rat_ind(var_lim), &
                   grp_rat_map(rxt_lim,3,2), &
                   grp_rat_cnt(2), &
                   rxt_to_grp_map(rxt_lim,2), &
                   rel_rxt_cnt(2), &
                   rel_rxt_pntr(rxt_lim,2), &
                   rel_rxt_map(rxt_lim,3,2)
      integer  ::  extcnt(5)                        ! class counter for ext frcing

      integer  ::  pcoeff_cnt                       ! count of reactions with non-unity product coefficients
      integer  ::  pcoeff_ind(rxt_lim)                  ! map of reactions with non_unity product coefficients

      integer, target  ::  nind(200)
      integer, pointer ::  nbeg(:),     nend(:)
      integer  ::  dimensions(5) = (/ 128, 64, 18, 1, 1 /)

      integer  ::  plon, plat, plev                 ! spatial dimensions of simulation
      integer  ::  jintmx, nxpt                     ! slt parameters for bounds and array "padding"
      integer  ::  indexm = 0, &                    ! index for fixed species denoting total atm density
                   indexh2o = 0, &                  ! index for fixed species denoting water vapor density
                   phtcnt = 0, &                    ! count of photolysis reactions
                   hetcnt = 0, &                    ! count of heterogeneous processes
                   usrcnt = 0, &                    ! count of "extraneous" forcing processes
                   rxntot = 0, &                    ! count of photo and gas phase reactions
                   gascnt = 0, &                    ! count of gas phase reactions
                   sub_cnt = 0                      ! count of user subroutines

      integer  ::  ptplen = 0                       ! total hist tape fields

!-----------------------------------------------------------------------
!        ... The solution class variables
!-----------------------------------------------------------------------
      integer  ::  class
      integer  ::  cls_ind_prdcnt
      integer  ::  clscnt(5), &                     ! count of solution species in each numerical "class"
                   clsmap(var_lim,5,2), &
                   cls_rxt_map(rxt_lim,7,5), &
                   cls_rxt_cnt(4,5)                 ! count of reactions types per solution class

      integer  ::  grp_rows, rel_rows
      integer  ::  histout_cnt(20,2)
      integer  ::  histout_map(var_lim,20,2)
!-----------------------------------------------------------------------
!        ... Iteration counts are as follows:
!            (1) == "hov" iteration count
!            (2) == "implicit" iteration count
!            (3) == "implicit" jacobian update count( first count iterations)
!            (4) == "ebi" iteration count
!-----------------------------------------------------------------------
      integer  ::  iter_counts(4) = (/ 7, 4, 2, 10 /)

      real     ::  rxparm(2,rxt_lim), &             ! gas phase reaction rate parameters
                   colub(var_lim), &                ! upper boundary column integral
                   grpcof(20,20), &                 ! multiplier for group members
                   pcoeff(4,rxt_lim)                ! reaction product coefficients

      character (len=120) :: command
      character (len=80) :: errcom, filout, filin
      character (len=80) :: iout(100)
      character (len=64), dimension(100) :: mod_names, mod_paths
      character (len=64), dimension(100) :: mod_alias_names
      character (len=64), dimension(500) :: filename, filepath, sub_names, lib_names
      character (len=64) :: oper_flpth
      character (len=64) :: wrk_dir, src_dir
      character (len=64) :: subfile
      character (len=64) :: histinp(4)
      character (len=64) :: histout(6)
      character (len=64) :: mod_src(100)
      character (len=64) :: lib_src(500)
      character (len=16) :: sym_rates(2,rxt_lim)
      character (len=16) :: rate_t0
      character (len=16) :: param
      character (len=16) :: hostname
      character (len=16) :: jobname
      character (len=16) :: jobctl(8)
      character (len=10) :: clshdr(5)
      character (len=8)  :: user_hst_names(var_lim,4)
      character (len=8)  :: rxt_alias(rxt_lim)
    
      character (len=8) ::  machine = 'CRAYYMP'
      character (len=8) ::  char
      character (len=4) ::  ftunit = 'ft'
      character (len=1) ::  errflg
      character (len=1), parameter :: on = 'y', off = 'n'

      integer ::  entry(10)
      integer ::  filelines(5)
      integer ::  file_cnt
      integer ::  dyn_hst_fld_cnt(2)                        ! multi and single level field count
      integer ::  ratind(2), mask(7)
      integer ::  nchar, k, noff, m, j, l, il, iu, &
                  npce, ipl, i, indx, ntab
      integer ::  spcno, counter, rxno, col, retcod, length, place
      integer ::  fixrows, rxmrows, pcelrows, pceprows
      integer ::  cpucnt = 1                        ! number of cpu's
      integer ::  ios
      integer ::  nzcnt  = 0                        ! number of nonzero entries in lu
      integer, dimension(2) ::  additions, multiplications
      integer              :: permute(var_lim,5)         ! permutation vector
      integer              :: permutation(var_lim), permute_orig(var_lim,5)
      integer, allocatable :: diag_map(:)           ! map of jacobian diagonals
      integer, allocatable :: mat_sp_map(:,:)       ! matrix sparsity "map"
      
      logical ::  mod_alias(100) = .false.
      logical ::  mod_promote(100) = .false.
      logical ::  lib_alias(500) = .false.
      logical ::  promote(500) = .false.
      logical, target  ::  options(20)
      logical, allocatable, dimension(:,:) :: mat_sp_pat, lu_sp_pat
      logical, pointer :: usemods
      logical ::  quad_loss(var_lim)
      logical ::  null_flag
      logical ::  found
      logical ::  lexist
      logical ::  half_precision = .false.      ! half precision flag
      logical ::  radj_flag
      logical ::  ohstflag                      ! output history tape flag
      logical ::  diagprnt = .false.            ! chktrc or negtrc diagnostics printout
      logical ::  tavgprnt = .false.            ! time averaged printout
      logical ::  use_t0   = .false.            ! use a t0 in gas phase reaction rates

!----------------------------------------------------------------------------------
!       ... The options array has the following mapping:
!
!       (1) Chemistry (on/off)            (2) Target machine == cray (yes/no)
!       (3) Diffusion (on/off)            (4) Convection (on/off)
!       (5) Iter norms (on/off)           (6) Conservation (on/off)
!       (7) Source code (yes/no)          (8) Submission files (yes/no)
!       (9) Execution (yes/no)           (10) SLT fixer (on/off)
!      (11) Multitasking (yes/no)        (12) Rxt rate lookup table (on/off)
!      (13) Relative humidity (yes/no)   (14) New compiler (yes/no)
!      (15) Height field (yes/no )       (16) User "hook" (yes/no)
!      (17) Use f90 modules (yes/no)     (18) Make and use f90 names module (yes/no)
! (19 - 20) Unused
!
!           Iter norms, Execution, and Rxt rate lookup default to off
!----------------------------------------------------------------------------------
      data options / 4*.true., .false., 3*.true., .false., 2*.true., 2*.false., .true., 6*.false. /
      data clshdr / 'Explicit', 'Ebi', 'Hov', 'Implicit', 'Rodas' /

!-----------------------------------------------------------------------
!        ... Initialize pointers and data
!-----------------------------------------------------------------------
      nq     => spccnt(1)
      relcnt => spccnt(2)
      nfs    => spccnt(3)
      ngrp   => spccnt(4)
      ncol   => spccnt(5)
      new_nq => spccnt(6)
      grp_mem_cnt => spccnt(7)
      solsym => spcsym(:,1)
      pcesym => spcsym(:,2)
      fixsym => spcsym(:,3)
      grpsym => spcsym(:,4)
      colsym => spcsym(:,5)
      new_solsym => spcsym(:,6)
      grp_mem_sym => spcsym(:,7)
      usemods => options(17)
      nbeg => nind(1:100)
      nend => nind(101:200)
      spcsym = ' '
      jobctl = ' '
      histout = ' '
      histinp = ' '
      histinp(4) = 'LONG'
      wrk_dir = '$TMPDIR'
      subfile = ' '
      spccnt = 0
      fixcnt = 0
      rxmcnt = 0
      ipcel  = 0
      ipcep  = 0
      grpcof = 1.
      entry  = 0
      filelines(:) = 0
      histout_cnt = 0
      rate_t0 = '298.'
      src_dir = './'

!-----------------------------------------------------------------------
!        ... Get arguments - NOTICE! implicit routine GETARG nonstandard
!            On DEC, using call GETARG( 1, filin ) ; call GETARG( 2, filout )
!-----------------------------------------------------------------------
      filin  = ' '
      filout = ' '
      call GETARG( 2, filin )
      call GETARG( 3, filout )
      write(*,*) 'Simon-s diags, MAIN: filin= >',filin,'<'
      write(*,*) 'Simon-s diags, MAIN: filout= >',filout,'<'

!-----------------------------------------------------------------------
!        ... Assign input unit and scan for override
!-----------------------------------------------------------------------
      if( filin == ' ' ) then
         write(*,'('' Enter filespec of input file'')')
         read(*,'(a80)') filin
         if( filin == ' ' ) then
            filin = 'ctm.inp'
         end if
      end if
      OPEN( unit = 5, &
            file = filin, &
            status = 'old', &
            iostat = ios )
      if( ios /= 0 ) then
	 write(lout,*) ' Failed to open input file : ',filin(:LEN_TRIM(filin))
	 stop 'Opn err'
      end if

      call CARDIN( lin, &
                   buff, &
                   nchar )
      buffh = buff
      call UPCASE( buffh )

!-----------------------------------------------------------------------
!        ... Check for input overide and process if present
!            if input unit 5 is overriden take 
!            all simulation input from lin
!-----------------------------------------------------------------------

      if( buffh(:11) == 'INPUTUNIT=' ) then
         errflg = on
         call INTCON( buff(12:nchar), &
                      nchar - 11, &
                      lin, &
                      retcod )
         if( retcod /= 0 ) then
            errcom = buff(12:nchar) // ' is an invalid unit number@'
         else
            if( lin <= 0 ) then
               errcom = buff(12:nchar) // ' is an invalid unit number@'
            else if( lin <= 3 ) then
               errcom = buff(12:nchar) // ' is a reserved unit number@'
            else if( lin == 6 ) then
               errcom = buff(12:nchar) // ' is a reserved unit number@'
            else if( lin >= 100 ) then
               errcom = buff(12:nchar) // ' is an invalid unit number@'
            else
               errflg = off
            end if
         end if

         if( errflg == on ) then
            call ERRMES ( errcom, &
                          6, &
                          param, &
                          8, &
                          buff )
         end if

         call CARDIN ( 5, &
                       buff, &
                       nchar )
         buffh = buff
         call UPCASE( buffh )
         if( buffh(:5) == 'FILE=' ) then
            filin = buff(6:nchar)
            if( lin <= 10 ) then
               write (ftunit(3:4),'(''0'',i1)') lin
            else
               write (ftunit(3:4),'(i2)') lin
            end if
            CLOSE( unit = 5 )
            OPEN( unit = lin, &
                  file = filin, &
                  status = 'old', &
                  iostat = ios )
            if( ios /= 0 ) then
	       write(lout,*) ' Failed to open input file : ', filin(:LEN_TRIM(filin))
	       stop 'Opn err'
            end if
            call CARDIN( lin, &
                         buff, &
                         nchar )
            buffh = buff
            call UPCASE( buffh )
         else
            call ERRMES( ' ** input reassignment requires both a unit' &
                       // ' number and filename@', &
                         lout, &
                         param, &
                         8, &
                         buff )
         end if
      end if

!-----------------------------------------------------------------------
!        ... Check for simulation start card (begsim)
!-----------------------------------------------------------------------
      if( buffh /= 'BEGSIM' ) then
         call ERRMES ( ' ** first card not begsim **@', &
                       lout, &
                       param, &
                       8, &
                       buff )
      end if
      call CARDIN ( lin, &
                    buff, &
                    nchar )
      buffh = buff
      call UPCASE( buffh )

!-----------------------------------------------------------------------
!        ... Check for output overide and process if present
!-----------------------------------------------------------------------
      if( buffh(:11) == 'OUTPUTUNIT=' ) then
         errflg = on
         call INTCON( buff(12:nchar), &
                      nchar - 11, &
                      lout, &
                      retcod )
         if( retcod /= 0 ) then
            errcom = buff(12:nchar) // ' is an invalid unit number@'
         else
            if( lout <= 0 ) then
               errcom = buff(12:nchar) // ' is an invalid unit number@'
            else if( lout <= 3 ) then
               errcom = buff(12:nchar) // ' is a reserved unit number@'
            else if( lout == lin .or. lout == 6 ) then
               errcom = buff(12:nchar) // ' is a reserved unit number@'
            else if( lout >= 100 ) then
               errcom = buff(12:nchar) // ' is an invalid unit number@'
            else
               errflg = off
            end if
         end if

!-----------------------------------------------------------------------
!        ... Error in assigning output unit number
!-----------------------------------------------------------------------
         if( errflg == on ) then
            call ERRMES ( errcom, &
                          6, &
                          param, &
                          8, &
                          buff )
         end if

!-----------------------------------------------------------------------
!        ... Set the output unit number
!-----------------------------------------------------------------------
         if( lout <= 10 ) then
            write (ftunit(3:4),'(''0'',i1)') lout
         else
            write (ftunit(3:4),'(i2)') lout
         end if

         call CARDIN ( lin, &
                       buff, &
                       nchar )
         buffh = buff
         call UPCASE( buffh )

         if( buffh(:5) == 'FILE=' ) then
            filout = buff(6:nchar)
            INQUIRE( file = filout, exist = lexist )
            iout(1) = 'rm ' // filout
            if( lexist ) then
               call SYSTEM( iout(1) )
            end if
            OPEN( unit   = lout, &
                  file   = filout, &
                  status = 'new', &
                  iostat = ios )
            if( ios /= 0 ) then
	       write(lout,*) ' Failed to open output file : ', filout(:LEN_TRIM(filout))
	       stop 'Opn err'
            end if
            call CARDIN( lin, &
                         buff, &
                         nchar )
            buffh = buff
            call UPCASE( buffh )
         else if( filout /= ' ' ) then
            filout = buff(6:nchar)
            INQUIRE( file = filout, exist = lexist )
            iout(1) = 'rm' // filout
            if( lexist ) then
               call SYSTEM( iout(1) )
            end if
            OPEN( unit   = lout, &
                  file   = filout, &
                  status = 'new', &
                  iostat = ios )
            if( ios /= 0 ) then
	       write(lout,*) ' Failed to open output file : ', filout(:LEN_TRIM(filout))
	       stop 'Opn err'
            end if
         else
            call ERRMES( ' ** output reassignment requires both a unit' &
                       // ' number and filename@', &
                         lout, &
                         param, &
                         8, &
                         buff )
         end if
      else
!-----------------------------------------------------------------------
!        ... Assign output unit
!-----------------------------------------------------------------------
         if( filout == ' ' ) then
            write(*,'('' Enter filespec of output file'')')
            read(*,'(a80)') filout
            if( filout == ' ' ) then
               filout = 'ctm.out'
            end if
         end if
         OPEN( unit   = lout, &
               file   = filout, &
               status = 'new', &
               iostat = ios )
         if( ios /= 0 ) then
	    write(lout,*) ' Failed to open output file : ', filout(:LEN_TRIM(filout))
	    stop 'Opn err'
         end if
      end if

!-----------------------------------------------------------------------
!        ... Check for comments and process if present
!-----------------------------------------------------------------------
      if( buffh == 'COMMENTS' ) then
         k = 1
         do 
            read(lin,'(a80)') iout(k)
            buffh = iout(k)
            call UPCASE( buffh )
            if( buffh == 'ENDPAR' ) then
               exit
            end if
            k = k + 1
         end do

         k = k - 1
         noff = 100

         do m = 1,k
            buff = iout(m)
            do j = 1,80
               if( buff(j:j) /= ' ' ) then
                  exit
               end if
            end do
            l = j
            do j = 80,l,-1
               if( buff(j:j) /= ' ' ) then
                  nchar = j - l + 1
                  nchar = 40 - nchar/2
                  nbeg(m) = l
                  nend(m) = j
                  noff = MIN( nchar, noff )
                  exit
               end if
            end do
         end do

         do m = 1,k
            buff = iout(m)
            iout(m) = ' '
            il = nbeg(m)
            if( il /= 0 ) then
               iu = nend(m)
               iout(m)(noff:) = buff(il:iu)
            end if
         end do

!-----------------------------------------------------------------------
!        ... Write out the comments
!-----------------------------------------------------------------------
         write(lout,1565)
         write(lout,1569)
         write(lout,1571)
         write(lout,1571)
         write(lout,1567) (iout(m),m = 1,k)
         write(lout,1571)
         write(lout,1571)
         write(lout,1569)
         write(lout,1569)
         do m = 1,k
            iout(m) = ' '
         end do
         call CARDIN ( lin, &
                       buff, &
                       nchar )
         buffh = buff
         call UPCASE( buffh )
      end if

!-----------------------------------------------------------------------
!        ... The species symbol list processing
!-----------------------------------------------------------------------
      call SYMBOL( indexh2o, &
                   indexm, &
                   iout, &
                   grpcof, &
                   grpmap, &
                   grpcnt, &
                   colmap, &
                   colub, &
                   relmap )

      ntab = MAXVAL( spccnt(1:5) )

!-----------------------------------------------------------------------
!       ... Form individual group members
!-----------------------------------------------------------------------
      i = 0
      do l = 1,ngrp
         do k = 1,grpcnt(l)
            j = grpmap(k,l)
            i = i + 1
	    grp_mem_sym(i) = solsym(j)
         end do
      end do
      grp_mem_cnt = i

!-----------------------------------------------------------------------
!        ... Now begin group modification process by making
!            new species numbering and group association tables
!-----------------------------------------------------------------------
      counter = 0
      do i = 1,ngrp
         do j = 1,grpcnt(i)
            indx = grpmap(j,i)
            grpflg(indx) = i
            counter = counter + 1
            mem2grp_map(counter) = i
            grp_rat_ind(indx) = counter
         end do
         new_solsym(i) = grpsym(i)
      end do

      do i = 1,relcnt
         indx = relmap(i,1)
         rel_flg(indx) = i
      end do
      
      indx = ngrp
      do i = 1,nq
         if( grpflg(i) /= 0 .or. rel_flg(i) /= 0 ) then
            cycle
         else
            indx = indx + 1
            newind(i) = indx
            new_solsym(indx) = solsym(i)
         end if
      end do
      new_nq = indx
      
      write (lout,230)
      write (lout,231) (j, solsym(j), j = 1,nq)
      if( relcnt /= 0 ) then
         write(lout,235)
         do j = 1,relcnt
            length = LEN_TRIM( solsym(relmap(j,1)) )
            buff = ' '
            write(buff,'(6x,''('',i4,'')'')') j
            buff(LEN_TRIM(buff)+2:) = solsym(relmap(j,1))(:length) // ' ~ ' // solsym(relmap(j,2))
            write(lout,'(a)') buff(:LEN_TRIM(buff))
         end do
      end if
      if( nfs /= 0 ) then
         write(lout,232)
         write(lout,231) (j, fixsym(j), j = 1,nfs)
      end if
      if( ncol /= 0 ) then
         write(lout,236)
         write(lout,238) (j, colsym(j), colub(j), j = 1,ncol)
      end if
      if( ngrp /= 0 ) then
         write (lout,237)
         write (lout,'(2x,''('',i4,'')'',2x,a80)') ( j, iout(j), j = 1,ngrp )
         do j = 1,ngrp
           iout(j) = ' '
         end do
      end if

!-----------------------------------------------------------------------
!        ... Write out group modified species list
!-----------------------------------------------------------------------
      if( ngrp /= 0 ) then
         write(lout,'(''0Modified Solution Species List'')')
         write(lout,231) (j, new_solsym(j), j = 1,new_nq)
      end if

!-----------------------------------------------------------------------
!        ... Define the solution classes
!-----------------------------------------------------------------------
      call SOL_CLS( new_nq, &
                    new_solsym, &
                    clscnt, &
                    clsmap )

     
!-----------------------------------------------------------------------
!        ... Write out class lists
!-----------------------------------------------------------------------
      write(lout,'(''0Class List'')')
      write(lout,'('' ----- ----'')')
      do k = 1,5
         if( clscnt(k) /= 0 ) then
            write(lout,'(''0'',a10)') clshdr(k)
            write(lout,'('' --------'')')
            write(lout,231) (j, new_solsym(clsmap(j,k,2)), j = 1,clscnt(k) )
         end if
      end do
!-----------------------------------------------------------------------
!        End of the variable list processing
!------------------------------------------------------------------------

!=======================================================================
!        ... Chemistry processing
!=======================================================================
      call CARDIN( lin, buff, nchar )
      call UPCASE( buff )
      if( buff == 'CHEMISTRY' ) then
!-----------------------------------------------------------------------
!        ... Set the reactions and rates
!-----------------------------------------------------------------------
      ntab = MAXVAL( spccnt(1:5) )
         call CHEM( rxntot, &
                    ipl, &
                    rxmcnt, &
                    prdcnt, &
                    ipcep, &
                    ipcel, &
                    fixcnt, &
                    prdmap, &
                    fixmap, &
                    pcep, &
                    pcel, &
                    rxmap, &
                    hetcnt, &
                    hetmap, &
                    usrcnt, &
                    usrmap, &
                    rxparm, &
                    rxptab, &
                    rxpcnt, &
                    phtcnt, &
                    pcoeff_cnt, &
                    pcoeff_ind, &
                    pcoeff, &
		    sym_rates, &
		    rxt_alias, &
		    use_t0, &
		    rate_t0 )
         options(1) = .true.
         gascnt = rxntot - phtcnt
         call CARDIN( lin, &
                      buff, &
                      nchar )
         call UPCASE( buff )
      else
         options(1) = .false.
      end if

!-----------------------------------------------------------------------
!        ... Transform the "hetero" reaction map
!            The 1st column is the new species number
!            if the species is a group member then the second column
!            indicates the species number within the group ( the 1st col)
!-----------------------------------------------------------------------
      do j = 1,hetcnt
         spcno = hetmap(j,1)
         if( grpflg(spcno) /= 0 ) then
            hetmap(j,1) = grpflg(spcno)
            hetmap(j,2) = grp_rat_ind(spcno)
         else
            hetmap(j,1) = newind(spcno)
            hetmap(j,2) = 0
         end if
      end do

!-----------------------------------------------------------------------
!        ... Then the "extraneous" reaction map
!-----------------------------------------------------------------------
      do j = 1,usrcnt
         spcno = usrmap(j)
         if( grpflg(spcno) /= 0 ) then
            usrmap(j) = grpflg(spcno)
         else
            usrmap(j) = newind(spcno)
         end if
      end do
     
!=======================================================================
!        ... The run parameters processing section
!=======================================================================
      if( buff == 'ENDSIM' ) then
         go to 292
      else if( buff == 'SIMULATIONPARAMETERS' ) then
         do
            call CARDIN( lin, &
                         buff, &
                         nchar )
            call UPCASE( buff )
            if( buff == 'SPATIALDIMENSIONS' ) then
               if( entry(1) /= 0 ) then
                  call ERRMES( ' spatial dimensions already' &
                            // '  prescribed@', &
                               lout, &
                               char, &
                               1, &
                               buff )
               else
                  entry(1) = 1
                  call SPAT_DIMS( dimensions )
		  plon = dimensions(1)
                  plev = dimensions(3)
		  plat = dimensions(2)
               end if
            else if( buff == 'VERSIONOPTIONS' ) then
               if( entry(2) /= 0 ) then
                  call ERRMES( ' Version options already prescribed@', &
                               lout, &
                               char, &
                               1, &
                               buff )
               else
                  entry(2) = 1
                  call VER_OPTS( options(2), &
                                 machine, &
                                 wrk_dir, &
                                 subfile, &
                                 diagprnt, &
                                 tavgprnt, &
                                 cpucnt, &
				 half_precision )
!-----------------------------------------------------------------------
!        ... Write out the species names parameter file
!-----------------------------------------------------------------------
		  if( options(18) ) then
                     call MAKE_NAME_MOD( new_nq, new_solsym )
                     call MAKE_RXT_NAME_MOD( rxntot, gascnt, phtcnt, rxt_alias )
                  end if
               end if
            else if( buff == 'EXECUTIONOPTIONS' ) then
               if( entry(4) /= 0 ) then
                  call ERRMES( ' Exec options already prescribed@', &
                               lout, &
                               char, &
                               1, &
                               buff )
               else
                  entry(4) = 1
                  call EXE_OPTS( options(8) )
               end if
            else if( buff == 'USERSUBROUTINES' ) then
               if( entry(3) /= 0 ) then
                  call ERRMES( ' Subroutines already specified@', &
                               lout, &
                               char, &
                               1, &
                               buff )
               else
                  entry(3) = 1
                  call USRSUBS( sub_names, &
				lib_names, &
				lib_alias, &
				promote, &
                                sub_cnt )
!-----------------------------------------------------------------------
!       ... Parse user file pathnames
!-----------------------------------------------------------------------
                  if( sub_cnt /= 0 ) then
                     do i = 1,sub_cnt
                        call PARSE_FLPTH( sub_names(i), &
                                          filename(i), &
                                          filepath(i) )
                     end do
                     do i = 1,sub_cnt
	                if( INDEX( filename(i), 'mod', back = .true. ) == LEN_TRIM(filename(i))-2 ) then
			   options(17) = .true.                 ! force fortran90
			   usemods = .true.
			   exit
			end if
                     end do
                  end if
               end if
            else if( buff == 'JOBCONTROL' ) then
               if( entry(5) /= 0 ) then
                  call ERRMES( ' Job control already specified@', &
                               lout, &
                               char, &
                               1, &
                               buff )
               else
                  entry(5) = 1
                  call JOB_CTL( jobctl )
               end if
            else if( buff == 'NUMERICALCONTROL' ) then
               if( entry(10) /= 0 ) then
                  call ERRMES( ' Numerical control already specified@', &
                               lout, &
                               char, &
                               1, &
                               buff )
               else
                  entry(10) = 1
                  call NUM_CTL( iter_counts )
               end if
            else if( buff == 'INPUTS' ) then
               if( entry(6) /= 0 ) then
                  call ERRMES( ' Inputs already specified@', &
                               lout, &
                               char, &
                               1, &
                               buff )
               else
                  entry(6) = 1
                  call HIST_INP( lin, &
                                 lout, &
                                 histinp, &
				 dyn_hst_fld_cnt )
               end if
            else if( buff == 'OUTPUTS' ) then
               if( entry(7) /= 0 ) then
                  call ERRMES( ' Outputs already specified@', &
                               lout, &
                               char, &
                               1, &
                               buff )
               else
                  entry(6) = 1
                  call HIST_OUT( lin, &
                                 lout, &
                                 histout, &
                                 histout_cnt, &
                                 histout_map, &
                                 user_hst_names, &
                                 spcsym, &
                                 spccnt, &
                                 indexh2o, &
                                 srf_flx_cnt, &
                                 dvel_cnt, &
                                 hetcnt, &
                                 hetmap, &
                                 usrcnt, &
                                 usrmap, &
                                 gascnt, &
                                 phtcnt )
               end if
            else if( buff == 'ENDSIM' ) then
               go to 292
            else if( buff /= 'ENDPAR' ) then
               call ERRMES ( ' endsim card missing@', &
                             lout, &
                             char, &
                             1, &
                             buff )
            end if
         end do
      else
         call ERRMES ( ' endsim card missing@', &
                       lout, &
                       char, &
                       1, &
                       buff )
      end if

292   continue
Chemistry_test: &
      if( options(1) ) then         ! do only if there is chemistry
!=======================================================================
!        ... Weed out the proportional products in all reaction maps
!=======================================================================
!-----------------------------------------------------------------------
!        ... First the "independent" production map
!-----------------------------------------------------------------------
         do j = 1,prdcnt
            do k = 2,5
               spcno = prdmap(j,k)
               if( spcno == 0 ) then
                  mask(k) = -1
                  exit
               else if( rel_flg(spcno) == 0 ) then
                  mask(k) = 1
               else
                  mask(k) = 0
               end if
            end do
            place = 1
            do k = 2,5
               if( mask(k) == -1 ) then
                  prdmap(j,place+1:5) = 0
                  exit
               else if( mask(k) == 1 ) then
                  place = place + 1
                  prdmap(j,place) = prdmap(j,k)
               end if
            end do
         end do

!-----------------------------------------------------------------------
!        ... Then the "regular" reaction map
!-----------------------------------------------------------------------
         do i = 1,2
            do j = 1,rxmcnt(i)
               do k = i+2,i+5
                  spcno = rxmap(j,k,i)
                  if( spcno == 0 ) then
                     mask(k) = -1
                     exit
                  else if( rel_flg(spcno) == 0 ) then
                     mask(k) = 1
                  else
                     mask(k) = 0
                  end if
               end do
               place = i + 1
               do k = i+2,i+5
                  if( mask(k) == -1 ) then
                     rxmap(j,place+1:i+5,i) = 0
                     exit
                  else if( mask(k) == 1 ) then
                     place = place + 1
                     rxmap(j,place,i) = rxmap(j,k,i)
                  end if
               end do
            end do
         end do

!-----------------------------------------------------------------------
!        ... Now xform all "proportional" reactants to proportional species
!        NOTE! The proportional reactants are replaced by the NEGATIVE
!              index of the species they are proportional to
!-----------------------------------------------------------------------
         do i = 1,2
            do j = 1,rxmcnt(i)
               counter = 0
               do k = 2,i+1               !do only the reactants
                  spcno = rxmap(j,k,i)
                  if( rel_flg(spcno) /= 0 ) then
                     counter = counter + 1
                     rxmap(j,k,i) = -relmap(rel_flg(spcno),2)
                     ratind(counter) = spcno
                  end if
               end do
               if( counter /= 0 ) then
                  rxno = rxmap(j,1,i)
                  rel_rxt_cnt(counter) = rel_rxt_cnt(counter) + 1
                  indx = rel_rxt_cnt(counter)
                  rel_rxt_map(indx,1,counter) = rxno   !the reaction number
                  do l = 1,counter
                     rel_rxt_map(indx,l+1,counter) = rel_flg(ratind(l))
                  end do
                  rel_rxt_pntr(rxno,1) = counter
                  rel_rxt_pntr(rxno,2) = indx
               end if
            end do
         end do

!-----------------------------------------------------------------------
!        Now do the actual reaction matrix transforms
!        The first phase just does the basic x-form.
!        The second phase scans resultant maps to "eliminate"
!        matching product and reactant species in the
!        same reaction.
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!        ... First the "independent" production map
!-----------------------------------------------------------------------
         do j = 1,prdcnt
            do k = 2,5
               spcno = prdmap(j,k)
               if( spcno == 0 ) then
                  exit
               else if( grpflg(spcno) /= 0 ) then
                  prdmap(j,k) = grpflg(spcno)
               else
                  prdmap(j,k) = newind(spcno)
               end if
            end do
         end do

!-----------------------------------------------------------------------
!        ... Then the "regular" reaction map
!-----------------------------------------------------------------------
         do i = 1,2
            do j = 1,rxmcnt(i)
               counter = 0
               do k = 2,i+5
                  spcno = ABS( rxmap(j,k,i) )
                  if( spcno == 0 ) then
                     exit
                  else if( grpflg(spcno) /= 0 ) then
                     rxmap(j,k,i) = SIGN( grpflg(spcno), rxmap(j,k,i) )
                     if( i == 1 ) then
                        if( k == 2 ) then
                           counter = counter + 1
                           ratind(counter) = spcno
                        end if
                     else
                        if( k <= 3 ) then
                           counter = counter + 1
                           ratind(counter) = spcno
                        end if
                     end if
                  else
                     rxmap(j,k,i) = SIGN( newind(spcno), rxmap(j,k,i) )
                  end if
               end do
               if( counter /= 0 ) then
                  rxno = rxmap(j,1,i)
                  grp_rat_cnt(counter) = grp_rat_cnt(counter) + 1
                  indx = grp_rat_cnt(counter)
                  grp_rat_map(indx,1,counter) = rxno
                  do l = 1,counter
                     grp_rat_map(indx,l+1,counter) = grp_rat_ind(ratind(l))
                  end do
                  rxt_to_grp_map(rxno,1) = counter
                  rxt_to_grp_map(rxno,2) = indx
               end if
            end do
         end do

!-----------------------------------------------------------------------
!        Scan reaction matrix to eliminate equally weigthed reactants
!        and products by setting the index = -index
!-----------------------------------------------------------------------
         do i = 1,2
            do j = 1,rxmcnt(i)
               do k = i+2,i+5
                  spcno = rxmap(j,k,i)
                  if( spcno == 0 ) then
                     exit
                  else
                     col = pcoeff_ind(rxmap(j,1,i))
                     if( col /= 0 .and. pcoeff(k-(i+1),col) /= 1.e0 ) then
                        cycle
                     end if
                     do l = 2,i+1
                        if( spcno == rxmap(j,l,i) ) then
                           rxmap(j,l,i) = -rxmap(j,l,i)
                           rxmap(j,k,i) = -spcno
                           exit
                        end if
                     end do
                  end if
               end do
            end do
         end do

!-----------------------------------------------------------------------
!        Scan reaction matrix to detect "null" reactions
!        and eliminate such reactions from the following maps:
!           1. groups
!           2. relationships
!           3. reactions
!-----------------------------------------------------------------------
         do i = 1,2
            place = 1
            do j = 1,rxmcnt(i)
               null_flag = .true.         ! assume a null reaction
               do k = 2,i+5
                  if( rxmap(j,k,i) > 0 ) then
                     null_flag = .false.  ! not a null reaction
                     exit
                  end if
               end do
               if( null_flag ) then       ! remove from lists if null
                  rxno = rxmap(j,1,i)
                  rxt_to_grp_map(rxno,1:2) = 0
                  rel_rxt_pntr(rxno,1:2)   = 0
               else                       ! a non-null reaction; keep it
                  rxmap(place,1:i+5,i) = rxmap(j,1:i+5,i)
                  place = place + 1
               end if
            end do
            rxmcnt(i) = place - 1
         end do

!-----------------------------------------------------------------------
!        ... Form the solution class reaction maps
!-----------------------------------------------------------------------
         call CLS_MAPS( cls_rxt_map, &
                        cls_rxt_cnt, &
                        clsmap, &
                        rxmap, &
                        rxmcnt, &
                        prdmap, &
                        prdcnt, &
                        hetmap, &
                        hetcnt, &
                        usrmap, &
                        usrcnt, &
                        extcnt )
     
!-----------------------------------------------------------------------
!        ... Order class reaction map reactants for the nonlinear reactions
!-----------------------------------------------------------------------
         do i = 1,5
	    if( cls_rxt_cnt(3,i) /= 0 ) then
	       do k = SUM( cls_rxt_cnt(1:2,i) )+1,SUM( cls_rxt_cnt(1:3,i) )
		  if( (ABS(cls_rxt_map(k,2,i)) > ABS(cls_rxt_map(k,3,i)) .and. &
		       cls_rxt_map(k,3,i) > 0 ) .or. cls_rxt_map(k,2,i) <= 0 ) then
		     m = cls_rxt_map(k,3,i)
		     cls_rxt_map(k,3,i) = cls_rxt_map(k,2,i)
		     cls_rxt_map(k,2,i) = m
		  end if
	       end do
	    end if
	 end do
!=======================================================================
!        ... Call the code writing utilities
!=======================================================================
!-----------------------------------------------------------------------
!        ... Force permutation for explicit method
!-----------------------------------------------------------------------
         if( clscnt(1) /= 0 ) then
	    permute(:clscnt(1),1) = (/ (i,i=1,clscnt(1)) /)
	 end if
!-----------------------------------------------------------------------
!        ... The iterated Euler backward and "Hov" methods
!-----------------------------------------------------------------------
	 do class = 2,3
            if( clscnt(class) /= 0 ) then
	       ALLOCATE( mat_sp_pat(clscnt(class),clscnt(class)) )
	       ALLOCATE( lu_sp_pat(clscnt(class),clscnt(class)) )
	       call SPARSITY_PAT( clscnt(class), &
			          clsmap(1,class,2), &
			          cls_rxt_cnt(1,class), &
			          cls_rxt_map(1,1,class), &
			          mat_sp_pat )
	       lu_sp_pat(:,:) = mat_sp_pat(:,:)
	       call DIAG_MARK( clscnt(class), lu_sp_pat, permute(1,class) )
	       permute_orig(:clscnt(class),class) = permute(:clscnt(class),class)
	       DEALLOCATE( mat_sp_pat )
	       DEALLOCATE( lu_sp_pat )
	    end if
	 end do
!-----------------------------------------------------------------------
!        ... The sparse matrix backward Euler method
!-----------------------------------------------------------------------
	 do class = 4,5
            if( clscnt(class) /= 0 ) then
	       ALLOCATE( mat_sp_pat(clscnt(class),clscnt(class)) )
	       ALLOCATE( lu_sp_pat(clscnt(class),clscnt(class)) )
	       ALLOCATE( mat_sp_map(clscnt(class),clscnt(class)) )
	       ALLOCATE( diag_map(clscnt(class)) )
!-----------------------------------------------------------------------
!        ... Determine original jacobian sparsity
!-----------------------------------------------------------------------
	       call SPARSITY_PAT( clscnt(class), &
			          clsmap(1,class,2), &
			          cls_rxt_cnt(1,class), &
			          cls_rxt_map(1,1,class), &
			          mat_sp_pat )
!              call DRAW_MAT( clscnt(class), mat_sp_pat )
	       lu_sp_pat(:,:) = mat_sp_pat(:,:)
!-----------------------------------------------------------------------
!        ... Reorder according to diagonal Markowitz
!-----------------------------------------------------------------------
	       call DIAG_MARK( clscnt(class), lu_sp_pat, permute(1,class) )
	       permute_orig(:clscnt(class),class) = permute(:clscnt(class),class)
!-----------------------------------------------------------------------
!        ... Permute the original sparsity pattern
!-----------------------------------------------------------------------
	       call PERM_MAT( clscnt(class), lu_sp_pat, permute(1,class) )
	       mat_sp_pat = lu_sp_pat
!              call DRAW_MAT( clscnt(class), lu_sp_pat )
!-----------------------------------------------------------------------
!        ... Symbolic factorization; includes fillin
!-----------------------------------------------------------------------
	       call SYM_FAC( clscnt(class), lu_sp_pat, additions, multiplications )
!-----------------------------------------------------------------------
!        ... Make column oriented non-zero "map"
!-----------------------------------------------------------------------
               nzcnt = COUNT( lu_sp_pat )
	       mat_sp_map = 0
	       k = 0
	       do j = 1,clscnt(class)
	          do i = 1,clscnt(class)
                     if( lu_sp_pat(i,j) ) then
		        k = k + 1
		        mat_sp_map(i,j) = k
		        if( i == j ) then
		           diag_map(j) = k
		        end if
		     end if
	          end do
	       end do
!-----------------------------------------------------------------------
!        ... Write the factorization code
!-----------------------------------------------------------------------
               call MAKE_LU_FAC( clscnt(class), lu_sp_pat, mat_sp_pat, mat_sp_map, machine )
!-----------------------------------------------------------------------
!        ... Write the solver code
!-----------------------------------------------------------------------
               call MAKE_LU_SLV( clscnt(class), lu_sp_pat, machine )
	       DEALLOCATE( mat_sp_pat )
	       DEALLOCATE( lu_sp_pat )
	       exit
            end if
         end do
!-----------------------------------------------------------------------
!        ... Make "from-to" permutation
!-----------------------------------------------------------------------
	 do class = 2,5
	    do j = 1,clscnt(class)
	       do i = 1,clscnt(class)
	          if( permute(i,class) == j ) then
	             permutation(j) = i
		  end if
	       end do
	    end do
	    if( clscnt(class) /= 0 ) then
	       permute(:clscnt(class),class) = permutation(:clscnt(class))
	    end if
	 end do
!-----------------------------------------------------------------------
!        ... Make reaction scheme dependent prod & loss code
!-----------------------------------------------------------------------
	 quad_loss(:new_nq) = .false.
         call PL_CODE( new_nq, &
		       clscnt, &
                       clsmap, &
                       cls_rxt_cnt, &
                       cls_rxt_map, &
                       pcoeff_ind, &
                       pcoeff, &
                       permute, &
                       hetcnt, &
                       rxt_alias, &
                       machine, &
		       options(14) )
         cls_ind_prdcnt = SUM( cls_rxt_cnt(1,1:5) )
         if( usrcnt /= 0 .or. cls_ind_prdcnt /= 0 ) then
!-----------------------------------------------------------------------
!        ... Make reaction scheme independent prod & loss code
!-----------------------------------------------------------------------
            call IPD_CODE( new_nq, &
			   clscnt, &
                           clsmap, &
                           cls_rxt_cnt, &
                           extcnt, &
                           cls_rxt_map, &
                           pcoeff_ind, &
                           pcoeff, &
                           permute, &
                           rxt_alias, &
                           solsym, &
			   options(14) )
         end if
!-----------------------------------------------------------------------
!        ... Make tabular reaction rates
!-----------------------------------------------------------------------
         if( rxpcnt /= 0 ) then
            if( options(12) ) then
               call MAKE_RATE_TAB( rxparm, &
                                   rxptab, &
                                   rxpcnt, &
				   options(14) )
            else
               call MAKE_RATE( sym_rates, &
                               rxt_alias, &
                               rxptab, &
                               rxpcnt, &
                               use_t0, &
			       options(14) )
            end if
         end if
         if( fixcnt(2) /= 0 ) then
            radj_flag = .true.
         else
            radj_flag = .false.
            do i = 1,fixcnt(1)
               if( ABS(fixmap(i,1,1)) > phtcnt ) then
                  radj_flag = .true.
               end if
            end do
         end if
         if( radj_flag ) then
            call MAKE_RADJ( fixmap, &
                            fixcnt, &
                            rxmap(1,1,2), &
                            rxmcnt(2), &
                            phtcnt, &
                            rxt_alias, &
			    options(14) )
         end if
         if( phtcnt /= 0 ) then
            call MAKE_PADJ( fixmap, &
                            fixcnt, &
                            phtcnt, &
                            rxt_alias, &
			    options(14) )
         end if
         if( grp_mem_cnt /= 0 .or. relcnt /= 0 ) then
            call MAKE_RMOD( rel_rxt_pntr, &
                            rel_rxt_map, &
                            rxt_to_grp_map, &
                            grp_rat_map, &
                            hetmap(1,2), &
                            hetcnt, &
                            rxntot, &
                            grp_mem_cnt, &
                            relcnt, &
			    options(14) )
         end if
	 do class = 4,5
            if( clscnt(class) /= 0 ) then
               call MAKE_LIN( clscnt(class), &
                              clsmap, &
                              cls_rxt_cnt(1,class), &
                              cls_rxt_map(1,1,class), &
                              pcoeff_ind, &
                              pcoeff, &
                              machine, &
                              permute(1,class), &
                              mat_sp_map, &
			      class, &
			      hetcnt, &
                              rxt_alias, &
			      options(14) )
               call MAKE_NLN( clscnt(class), &
                              clsmap, &
                              cls_rxt_cnt(1,class), &
                              cls_rxt_map(1,1,class), &
                              pcoeff_ind, &
                              pcoeff, &
                              machine, &
                              permute(1,class), &
                              mat_sp_map, &
			      class, &
                              rxt_alias, &
			      options(14) )
	       exit
            end if
         end do
!-----------------------------------------------------------------------
!        ... Make group members vmr subroutine
!-----------------------------------------------------------------------
         if( grp_mem_cnt /= 0 .and. SUM( histout_cnt(2,1:2) ) /= 0 ) then
            call MAK_GRP_VMR( grp_mem_cnt, &
                              mem2grp_map, &
			      options(14) )
         end if
!-----------------------------------------------------------------------
!        ... Writeout the surface flux and depos vel info
!-----------------------------------------------------------------------
         if( srf_flx_cnt /= 0 ) then
            write(lout,'(''0 Species with non-zero surface flux'')')
            do i = 1,srf_flx_cnt
               write(lout,'(1x,''('',i2,'')'',3x,a8)') i, new_solsym(srf_flx_map(i))
            end do
         end if
         if( dvel_cnt /= 0 ) then
            write(lout,'(''0 Species with non-zero deposition flux'')')
            do i = 1,dvel_cnt
               write(lout,'(1x,''('',i2,'')'',3x,a8)') i, new_solsym(dvel_map(i))
            end do
         end if
!-----------------------------------------------------------------------
!        ... Call the equation reporting utility
!-----------------------------------------------------------------------
         call EQUATION_REP( prdcnt, &
                            prdmap, &
                            rxntot, &
                            rxmcnt, &
                            rxmap, &
                            pcoeff_cnt, &
                            pcoeff_ind, &
                            pcoeff, &
                            fixcnt, &
                            fixmap, &
                            rxt_alias, &
                            phtcnt )
      end if Chemistry_test                ! end of if chemistry

!=======================================================================
!     ... This is for the new CTM interface; the old driver
!         data file can still be output for potential diagnostics
!=======================================================================
      INQUIRE( file = 'preprocessed.dat', exist = lexist )
      if( lexist ) then
         call SYSTEM( 'rm preprocessed.dat' )
      end if
      OPEN( unit = 20, &
            file = 'preprocessed.dat', &
            status = 'new', &
            iostat = ios )
      if( ios /= 0 ) then
	 write(lout,*) ' Failed to open preprocessed.dat file'
	 stop 'Opn err'
      end if
      write(20,510) clscnt
      write(20,508) cls_rxt_cnt
!-----------------------------------------------------------------------
!        ... Write the "class" maps & species symbols
!-----------------------------------------------------------------------
      do k = 1,5
         if( clscnt(k) /= 0 ) then
            write(20,522) clsmap(:new_nq,k,1)
            write(20,522) clsmap(:clscnt(k),k,2)
         end if
      end do
      write(20,'(10a8)') new_solsym(:new_nq)
      if( grp_mem_cnt /= 0 ) then
         write(20,'(10a8)') grp_mem_sym(:grp_mem_cnt)
      end if
      do class = 2,5
         if( clscnt(class) /= 0 ) then
	    if( class <= 3 ) then
               write(20,522) permute_orig(:clscnt(class),class)
	    else
               write(20,522) permute(:clscnt(class),class)
	    end if
	    if( class > 3 ) then
               write(20,522) permute_orig(:clscnt(class),class)
               write(20,522) diag_map(:clscnt(class))
	       exit
	    end if
         end if
      end do
      write(20,'(40l)') quad_loss(:new_nq)
      CLOSE( 20 )


!-----------------------------------------------------------------------
!        ... Write the chemistry header file
!-----------------------------------------------------------------------
      call CHM_HDR( hetcnt, &
                    usrcnt, &
                    cls_rxt_cnt, &
                    radj_flag, &
                    phtcnt, &
                    rxpcnt, &
                    rxparm, &
                    rxntot, &
                    ncol, &
                    nfs, &
                    indexm, &
                    indexh2o, &
                    new_nq, &
                    relcnt, &
                    grp_mem_cnt, &
                    clscnt, &
                    iter_counts, &
                    nzcnt, &
                    half_precision, &
                    machine, &
		    use_t0, &
		    rate_t0 )

!-----------------------------------------------------------------------
!        ... Write the resolution header file
!-----------------------------------------------------------------------
      call RES_HDR( plon, &
                    plat, &
                    plev, &
                    jintmx, &
                    nxpt, &
                    cpucnt )

!-----------------------------------------------------------------------
!        ... Write the version header file
!-----------------------------------------------------------------------
      ptplen = histout_cnt(1,1) + histout_cnt(2,1) + histout_cnt(5,1) &
             + histout_cnt(3,1) + histout_cnt(4,1) + histout_cnt(7,1) &
             + histout_cnt(1,2) + histout_cnt(2,2) + histout_cnt(5,2) &
             + histout_cnt(3,2) + histout_cnt(4,2) + histout_cnt(7,2)
      if( ptplen /= 0 .and. histout(2) /= ' ' ) then
         ohstflag = .true.
      else
         ohstflag = .false.
      end if
      call VER_HDR( options, &
                    ohstflag, &
                    diagprnt, &
                    tavgprnt, &
                    srf_flx_cnt, &
                    dvel_cnt, &
		    machine )

!-----------------------------------------------------------------------
!        ... Write the history tape header file
!-----------------------------------------------------------------------
      call HIST_HDR( histout_cnt, &
                     histout_map, &
                     user_hst_names, &
                     histinp(4), &
		     dyn_hst_fld_cnt, &
                     spcsym, &
                     spccnt, &
                     hetmap, &
                     usrmap, &
                     ptplen )

!-----------------------------------------------------------------------
!       ... Form "include" files stub file
!-----------------------------------------------------------------------
      INQUIRE( file = 'wrk.stub.F', exist = lexist )
      if( lexist ) then
	 call SYSTEM( 'rm -f wrk.stub.F' )
      end if
      OPEN( unit = 3, &
            file = 'wrk.stub.F', &
            iostat = ios )
      if( ios /= 0 ) then
	 write(*,*) ' Failed to open wrk file; terminating'
	 stop 'Opn err'
      end if
      write(3,'(''# include <version.h>'')') 
      write(3,'(''# include <res.h>'')') 
      write(3,'(''# include <chem.h>'')') 
      CLOSE( 3 )
      call SYSTEM( 'rm -f wrk.F' )

!-----------------------------------------------------------------------
!       ... Check for fortran 90 modules
!-----------------------------------------------------------------------
      if( usemods .and. options(8) ) then
         INQUIRE( file = TRIM( src_dir ) // 'in/mod_to_preproc.PP', exist = lexist )
         if( .not. lexist ) then
            call ERRMES( ' ** file in/mod_to_preproc.PP missing@', &
                         6, &
                         param, &
                         8, &
                         buff )
         end if
         INQUIRE( file = 'mod.src.files.PP', exist = lexist )
         if( lexist ) then
	    call SYSTEM( 'rm -f mod.src.files.PP' )
         end if
         call SYSTEM( 'cat ' // TRIM( src_dir ) // 'in/mod_to_preproc.PP'  &
                         // ' > mod.src.files.PP' )
         call SYSTEM( '/lib/cpp -P -C -I. mod.src.files.PP > mod.src.files' )
         OPEN( unit = 2, &
               file = 'mod.src.files', &
               status = 'old', &
	       position = 'rewind', &
	       iostat = ios )
         if( ios /= 0 ) then
	    write(lout,*) ' Failed to open mod.src.files file'
	    stop 'Opn err'
         end if
	 file_cnt = 1
         do k = 1,100
            read(2,'(a64)',end=1105) mod_src(file_cnt)
	    if( mod_src(file_cnt) /= ' ' ) then
	       filelines(5) = filelines(5) + 1
	       file_cnt = file_cnt + 1
	    end if
         end do
1105     CLOSE( 2 )
!-----------------------------------------------------------------------
!       ... Check user files for any module files
!-----------------------------------------------------------------------
         k = 0
!-----------------------------------------------------------------------
!       ... Check for species names module
!-----------------------------------------------------------------------
	 if( options(18) ) then
	    k = k + 1
	    mod_paths(k) = './'
	    mod_names(k) = 'spc_names.mod'
	    mod_promote(k) = .true.
	    k = k + 1
	    mod_paths(k) = './'
	    mod_names(k) = 'rxt_names.mod'
	    mod_promote(k) = .true.
	 end if
         do i = 1,sub_cnt
	    if( lib_alias(i) .and. &
	        (INDEX( lib_names(i), 'mod', back = .true. ) == LEN_TRIM(lib_names(i))-2) ) then
	       k = k + 1
	       mod_alias(k) = .true.
	       mod_alias_names(k) = lib_names(i)
	       mod_paths(k) = TRIM( filepath(i) )
	       mod_names(k) = TRIM( filename(i) )
	       nend(i) = 0
	       write(*,*) 'Removing file: ',TRIM(filename(i))
	    else if( INDEX( filename(i), 'mod', back = .true. ) == LEN_TRIM(filename(i))-2 ) then
	       k = k + 1
	       mod_paths(k) = TRIM( filepath(i) )
	       mod_names(k) = TRIM( filename(i) )
	       mod_promote(k) = promote(i)
	       nend(i) = 0
	    else
	       nend(i) = 1
	    end if
         end do
         filelines(2) = k
!-----------------------------------------------------------------------
!       ... Remove .mod files from user subroutine lists
!-----------------------------------------------------------------------
	 k = 0
         do i = 1,sub_cnt
	    if( nend(i) == 1 ) then
	       k = k + 1
	       filepath(k) = filepath(i)
	       filename(k) = filename(i)
	       lib_names(k) = lib_names(i)
	       lib_alias(k) = lib_alias(i)
	    end if
         end do
	 sub_cnt = k
!-----------------------------------------------------------------------
!       ... Check for user file "overrides"
!-----------------------------------------------------------------------
         if( filelines(2) /= 0 ) then
            call SUB_SCAN( filelines(5), &
                           mod_src, &
                           mod_paths, &
                           mod_names, &
                           mod_alias_names, &
                           mod_alias, &
                           filelines(2) )
         end if
!-----------------------------------------------------------------------
!       ... Form and preprocess the modules
!-----------------------------------------------------------------------
	 call SYSTEM( 'cat wrk.stub.F > wrk.F' )
	 if( ANY( mod_promote(:filelines(2)) ) ) then
            do i = 1,filelines(2)
	       if( mod_promote(i) ) then
	          command = 'cat ' // TRIM( mod_paths(i) ) // TRIM( mod_names(i) ) // ' >> wrk.F'
	          call SYSTEM( command(:LEN_TRIM(command)) )
               end if
            end do
	 end if
         do i = 1,filelines(5)
	    command = 'cat ' // TRIM( mod_src(i) ) // ' >> wrk.F'
	    call SYSTEM( command(:LEN_TRIM(command)) )
         end do
         if( filelines(2) > 0 ) then
            do i = 1,filelines(2)
	       if( .not. mod_promote(i) ) then
	          command = 'cat ' // TRIM( mod_paths(i) ) // TRIM( mod_names(i) ) // ' >> wrk.F'
	          call SYSTEM( command(:LEN_TRIM(command)) )
               end if
            end do
         end if
	 if( options(8) ) then
            INQUIRE( file = 'preprocessed.mod.f', exist = lexist )
            if( lexist ) then
               call SYSTEM( 'rm preprocessed.mod.f' )
            end if
            call SYSTEM( '/lib/cpp -P -C -I. wrk.F > preprocessed.mod.f' )
	    call SYSTEM( 'rm -f wrk.F' )
	 end if
      end if

!-----------------------------------------------------------------------
!       ... Get all source files
!-----------------------------------------------------------------------
      if( options(7) .and. options(8) ) then
!-----------------------------------------------------------------------
!       ... Get "main" library files
!-----------------------------------------------------------------------
         INQUIRE( file = 'lib.src.files.PP', exist = lexist )
         if( lexist ) then
	    call SYSTEM( 'rm -f lib.src.files.PP' )
         end if
!-----------------------------------------------------------------------
!       ... Get "chemistry" library files
!-----------------------------------------------------------------------
         if( options(1) ) then
            INQUIRE( file = TRIM( src_dir ) // 'in/rout_to_preproc.PP', exist = lexist )
            if( .not. lexist ) then
               call ERRMES( ' ** file in/rout_to_preproc.PP missing@', &
                            6, &
                            param, &
                            8, &
                            buff )
            end if
            call SYSTEM( 'cat ' // TRIM( src_dir ) // 'in/rout_to_preproc.PP ' &
                         // ' > lib.src.files.PP' )
	 end if
         call SYSTEM( '/lib/cpp -P -C -I. lib.src.files.PP > lib.src.files' )
         CLOSE(2)
         OPEN( unit = 2, &
               file = 'lib.src.files', &
               status = 'old', &
	       iostat = ios )
         if( ios /= 0 ) then
	    write(lout,*) ' Failed to open lib.src.files file'
	    stop 'Opn err'
         end if
	 file_cnt = 1
         do
            read(2,'(a64)',end=1005) lib_src(file_cnt)
	    if( lib_src(file_cnt) /= ' ' ) then
	       if( filelines(1) >= 500 ) then
                  call ERRMES( ' ** Exceeded lib file count of 500', &
                               lout, &
                               buff, &
                               1, &
                               buff )
	       end if
	       filelines(1) = filelines(1) + 1
	       file_cnt = file_cnt + 1
	    end if
         end do
!-----------------------------------------------------------------------
!       ... Check for iterative convergence norms
!-----------------------------------------------------------------------
1005     if( options(5) ) then
            lib_src(file_cnt)   = 'del_norm.F'
            lib_src(file_cnt+1) = 'it_norm.F'
            filelines(1) = filelines(1) + 2
         end if
!-----------------------------------------------------------------------
!       ... Check for user file "overrides"
!-----------------------------------------------------------------------
         if( sub_cnt /= 0 ) then
            call SUB_SCAN( filelines(1), &
                           lib_src, &
                           filepath, &
                           filename, &
                           lib_names, &
                           lib_alias, &
                           sub_cnt )
         end if

!-----------------------------------------------------------------------
!       ... Form main lib portion of ccmpp file
!-----------------------------------------------------------------------
	 call SYSTEM( 'cat wrk.stub.F > wrk.F' )
         if( filelines(1) /= 0 ) then
            do i = 1,filelines(1)
	       command = 'cat '// lib_src(i)(:LEN_TRIM(lib_src(i))) // ' >> wrk.F'
	       call SYSTEM( command(:LEN_TRIM(command)) )
            end do
         end if

!-----------------------------------------------------------------------
!       ... Get user specified files
!-----------------------------------------------------------------------
         if( sub_cnt > 0 ) then
            do i = 1,sub_cnt
	       command = 'cat ' // filepath(i)(:LEN_TRIM(filepath(i))) &
	                        // filename(i)(:LEN_TRIM(filename(i))) &
                                // ' >> wrk.F'
	       call SYSTEM( command(:LEN_TRIM(command)) )
            end do
         end if
      end if
      call SYSTEM( '/lib/cpp -P -C -I. wrk.F > preprocessed.f' )
      call SYSTEM( 'rm -f wrk.F' )

!-----------------------------------------------------------------------
!       ... Move to the "wrk" directory all intermediate files
!-----------------------------------------------------------------------
      call SYSTEM( 'mv *.F wrk/.' )
      call SYSTEM( 'mv *.mod wrk/.' )
      call SYSTEM( 'mv *.files wrk/.' )
      call SYSTEM( 'mv *.PP wrk/.' )
      call SYSTEM( 'mv *.h wrk/.' )

!-----------------------------------------------------------------------
!	... Diagnostic stop point
!-----------------------------------------------------------------------
      if( options(1) ) then
	 stop 'SOCRATES preprocessing successful'
      end if
      call JCL( jobctl, &
                histinp, &
                histout, &
                options, &
                sub_cnt, &
                clscnt(4), &
                hostname, &
                jobname, &
                machine, &
                wrk_dir, &
                subfile, &
                cpucnt )
      stop 'Ok'

!-----------------------------------------------------------------------
!       ... Format statements
!-----------------------------------------------------------------------
101   format('0 *****Species header must be first card *****')
102   format('0 *****Solution must follow species card *****')

202   format(6x,i3,2x,i3)
204   format(6x,i3,2x,i3,2x,i3)
206   format(6x,i3,2x,i3,2x,i3,2x,i3)
208   format(6x,i3,2x,i3,2x,i3,2x,i3,2x,i3)
201   format('0     the unimolecular fixed map'/6x,'rxn',2x,'fsn')
203   format('0     the bimolecular fixed map'/6x,'rxn',2(2x,'fsn'))
205   format('0     the production map'/6x,'rxn',2(2x,'psn'))
209   format('0     the unimolecular reaction map'/6x,'rxn',2x,'rsn',2(2x,'psn'))
210   format(6x,i3,2x,i3,2x,i3,2x,i3,2x,i3,2x,i3)
211   format('0     the bimolecualr reaction map'/6x,'rxn',2(2x,'rsn'),2(2x,'psn'))
213   format('0     the pce loss map'/6x,'pcn',2x,'rxn',2(2x,'psn'))
215   format('0     the pce,sol map'/6x,'pcn',2x,'rxn',2x,'rsn',2(2x,'psn'))
217   format('0     pure prod map for pces'/6x,'pcn',2x,'rxn',2x,'ind')
219   format('0     the linear prod map for pces'/6x,'pcn',2x,'rxn',2x,'rsn',2x,'ind')
221   format('0     the quadratic prod map for pces'/6x,'pcn',2x,'rxn',2x,'rsn',2x,'rsn',2x,'ind')
230   format('0',5x,'***Solution species***')
231   format(6x,'(',i4,')',2x,a8)
232   format('0',5x,'***Invariant species***')
234   format('0',5x,'***Pce species***')
235   format('0',5x,'***Relationships***')
236   format('0',5x,'***Column integrals***')
237   format('0',5x,'***Groups***')
238   format(3x,'(',i4,')',2x,a8,' - ',1pe10.3)

502   format(10i4)
504   format(2i4)
506   format(3i4)
508   format(4i4)
510   format(5i4)
512   format(6i4)
514   format(i4)
516   format(1x,2e10.4)
519   format(5e16.8)
522   format(20i4)

600   format('0  upper bndy conds'/2x,'species  d  n')
602   format(1x,a8,2i3)
604   format('0  lower bndy conds'/2x,'species  d  n')
606   format('0  upper bndy flux'/2x,'species     day        night   ')
608   format(1x,a8,1p,2e12.4)
610   format('0  lower bndy flux'/2x,'species     day        night   ')
612   format('0  upper bndy dir constants'/2x,'species',5x,'day',8x,'night   ')
614   format('0  lower bndy dir constants'/2x,'species',5x,'day',8x,'night   ')
616   format('0  aust coefficients'/2x,'species',5x,'day',8x,'night   ')
618   format('0  the time increments')
620   format(1x,'(',i2,')',2x,1pe12.4)

1565  format('1',10x,'*************************************************' &
                     /'+',59x,'***************************************************')
1571  format(11x,'**',96x,'**')
1567  format(11x,'**',8x,a80,8x,'**')
1569  format(11x,'****************************************************************' &
                 /'+',73x,'*************************************')

2502  format('0  rxn      a0          b0')
2504  format(3x,i3,1p,2e12.4)
2506  format('1',14x,'boundary conditions'/'+',14x,'________ __________' &
      /'0',12x,'upper boundary',6x,'lower boundary'/'+',12x, &
      '_____ ________',6x,'_____ ________'/'  species',5x,'day',6x, &
      'night',6x,'day',6x,'night'/'+ _______',5x,'___',6x,'_____', &
      6x,'___',6x,'_____')
2508  format(1x,10a8)
2270  format('0',10a8)

4000  format('0*** group table ***')
4002  format('0   group no',i4)
4004  format(1x,f3.1,i4)
4010  format('0*** column table ***')
4012  format(1x,2i4)
4020  format('0*** printout table ***')
4022  format('0 index',i4,' type',i4,' ic flag',i4)
4024  format('0 st fine prt',1pe12.4,' st course prt',e12.4)
4026  format('0 fine prt grid'/(1x,1pe12.4))
4028  format('0 course prt grid'/(1x,1pe12.4))
4030  format('0 column count'/(1x,i4))
4032  format('0 print directory'/(1x,i4))

      end program SOC_CHEMPP
