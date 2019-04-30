! subversion Id for THIS file : $Id: ascii_output.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/ascii_output.f $
!-----------------------------------------------------------------------

      subroutine DVOUT_ASCII( dvout, time, slt, sza, newvmr )
!-----------------------------------------------------------------------
!	... ASCII_OUTPUT of diurnal variations, new v6s38 version done
!           mainly to prototype netCDF output (DVOUT_ARCH)
!-----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use SPECIES_DIMS, only : nbcon
      use RATES_MODS, only : rxt_rates
      use SIM_CONTROLS, only : chemdtm
      use DIAG_CONTROLS, only : ldiag, zdiag
      use VEN1, only : t2d
      use CONC, only : hm2d
      use RXT_NAMES
      use TIME_CONTROLS, only : TIMING
      use ARCH_WHAT, only : max_dvout_dates, DVOUTING

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      type( DVOUTING ),dimension(max_dvout_dates),intent(inout) :: dvout
      type( TIMING), intent(in)  ::  time
      real, intent(in) :: slt, sza(lmax)
      real, dimension(niz,nbcon), intent(in) :: newvmr

!----------------------------------------------------------------------- 
!	... Local variables
!-----------------------------------------------------------------------
      real :: sltdiag, jdiag
      integer :: i, j, n
      integer, save :: ou, mapqn2dv(nbcon), ndv
      
!----------------------------------------------------------------------- 
!	... Find the dvout file corresponding to the present time
!-----------------------------------------------------------------------
      j = 0
      do i = 1, max_dvout_dates
         if( dvout(i)%active .and. dvout(i)%days0do == time%days0 ) then
            j = i
            exit
         end if
      end do
      if( j == 0 ) return
      
!----------------------------------------------------------------------- 
!	... Get a unit number for the ASCII file and write its header
!-----------------------------------------------------------------------
      if( .not. dvout(j)%ascii_def ) then
         call DVOUT_ASCII_INIT( dvout(j), ou, mapqn2dv, ndv )
         dvout(j)%ascii_def = .true.
      end if

!----------------------------------------------------------------------- 
!	... Write the data
!-----------------------------------------------------------------------
      sltdiag = MOD( slt, 24. )
      jdiag = rxt_rates(zdiag,rid_jo3_op,ldiag)
     $      + rxt_rates(zdiag,rid_jo3_od,ldiag)
      write(ou,'(6x,2(i2,a1),i4,2f16.8,es16.5,2f16.8,60es16.5)') 
     $ time%month,'/',time%day,'/',time%year, time%cal_day+slt/24.,
     $ t2d(ldiag,zdiag), hm2d(ldiag,zdiag), sltdiag, 
     $ sza(ldiag), MAX( jdiag, 9.e-99 ),
     $  ( MAX( newvmr(zdiag,mapqn2dv(n)), 9.e-99 ), n=1,ndv )
      
      contains
      
!=======================================================================      

      subroutine DVOUT_ASCII_INIT( dvoutj, ou, mapqn2dv, ndv ) 
!----------------------------------------------------------------------
!	... Setup the ASCII output file for diurnal variations
!----------------------------------------------------------------------
      use SIM_CONTROLS
      use ARCH_WHAT, only : nldesc, desc_text
      use ALLCO, only : phi, zkm
      use TRACNM, only : solsym
      use ASCII_UTILS, only : NAVU
        
      implicit none

!-----------------------------------------------------------------------
!         ... Dummy args
!-----------------------------------------------------------------------
      type( DVOUTING ), intent(in) :: dvoutj
      integer, intent(out) :: ou         ! logical unit for ASCII output
      integer, intent(out) :: ndv        ! nb of species to output
      integer, dimension(nbcon), intent(out) :: mapqn2dv

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer :: i, slen
      character(len=68) :: flnm
      
      ndv = 0
      do i = 1, nbcon
         if( dvoutj%vid_qn2dv(i) ) then
            ndv = ndv + 1
            mapqn2dv(ndv) = i
         end if
      end do

!-----------------------------------------------------------------------
!	... Open the file and write the file header
!-----------------------------------------------------------------------
      ou = NAVU()
      flnm = dvoutj%flnm(:LEN_TRIM(dvoutj%flnm)) // '.dat'
      slen = LEN_TRIM( flnm )
      OPEN( ou, file=flnm(:slen), form='FORMATTED' )
      write(ou,'(2a16)') 'Diurnal',' variations'
      write(ou,'(2(a16,f16.0,a16),2(a16,i16))') 'latitude= ',
     $ phi(ldiag),'(degrees)','altitude= ',zkm(zdiag),'(km)',
     $ 'i.e. lat index=',ldiag,'alt index= ',zdiag
      write(ou,'(a16,f16.2,a16,16f16.2)') 'chemdtm= ',chemdtm
      write(ou,'(9a16)') 'Header for','data below', 'description:'

!-----------------------------------------------------------------------
!	... Write the data header
!-----------------------------------------------------------------------
      write(ou,*)
      write(ou,'(60a16)') 'date', 'cal_day', 'Temperature', 'totdens',
     $ 'slt', 'sza','Jtot(O3)',
     $  (TRIM(ADJUSTL(solsym(mapqn2dv(i)))), i=1,ndv)
      write(ou,'(60a16)')'(MM/DD/YYYYY)','(days)', '(K)', '(molec/cm3)',
     $ '(hours)','(degrees)', '(1/s)',    ( '(vmr)', i=1,ndv )
      write(ou,*)
  
!-----------------------------------------------------------------------
!	... Write the simulation description
!-----------------------------------------------------------------------
      write(ou,*)
      write(ou,*) ( '================', i=1,5 )
      do i = 1, 50
         if( LEN_TRIM(desc_text(i)) /= 0 ) write(ou,*) desc_text(i)
      end do
      write(ou,*)
      write(ou,*) ( '================', i=1,5 )

!-----------------------------------------------------------------------
!	... Write the data header again
!-----------------------------------------------------------------------
      write(ou,*)
      write(ou,'(60a16)') 'date', 'cal_day', 'Temperature', 'totdens',
     $ 'slt', 'sza','Jtot(O3)',
     $  (TRIM(ADJUSTL(solsym(mapqn2dv(i)))), i=1,ndv)
      write(ou,'(60a16)')'(MM/DD/YYYYY)','(days)', '(K)', '(molec/cm3)',
     $ '(hours)','(degrees)', '(1/s)',    ( '(vmr)', i=1,ndv )
      write(ou,*)
      
      end subroutine DVOUT_ASCII_INIT

      end subroutine DVOUT_ASCII
