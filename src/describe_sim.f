! subversion Id for THIS file : $Id: describe_sim.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/describe_sim.f $
!-----------------------------------------------------------------------
      subroutine DESCRIBE_SIM( text )
!-----------------------------------------------------------------------
!    ... Describe the simulation
!-----------------------------------------------------------------------
      use SIM_CONTROLS
      use DIAG_CONTROLS
      use ARCH_WHAT, only : n_arch_modes, max_dvout_dates, arch, dvout,
     $                      max_nldesc, nldesc

      implicit none
!-----------------------------------------------------------------------
!    ... Dummy arguments
!-----------------------------------------------------------------------
      character(len=80), dimension(50), intent(out) :: text

!-----------------------------------------------------------------------
!    ... local variables
!-----------------------------------------------------------------------
      integer :: i, j, k, l
      character(len=80) :: c

      text(:) = ' '
      l = 1
      text(l) = '     label_short= ' // label_short
      l = l + 1
      if( label_long /= ' ' ) then
         text(l) = '     label_long= ' // label_long
         l = l + 1
      end if
      write(c,'(2a)') 'model_type= ',model_type
      i = LEN_TRIM(c)
      if( model_type == 'two_d' ) then
         text(l) = c(:i) // ' : sim performs full 2-D calculations'
       else
         if( model_type == 'one_d' ) then
            text(l) = c(:i) // ' : 1D run, only photochem & vdiff'
          else if( model_type == 'zerod' ) then
            text(l) = c(:i) // ' : stack of BOX model, only photochem'
         end if
         l = l + 1
         write(text(l),*) 'Simulation at lat index ldiag= ',ldiag
      end if
      l = l + 1
      write(c,'(2a)') 'start_from= ',start_from
      i = LEN_TRIM(c)
      if( start_from == 'ascii' ) then
         text(l) = c(:i) // ' : all initvals=0 except a few read in ' //
     $                                                     'ASCII files'
       else if( start_from == 'oldnc' ) then
         text(l) = c(:i) // ' : initvals read in default netCDF file'
       else
         text(l) = c(:i) // ' : sim will start from netCDF file...'
         l = l + 1
         text(l) = 'rstrt_filespec= ' // rstrt_filespec
      end if
      l = l + 1
      write(text(l),'('' Simulation start date = '',i2,''/'',i2,
     $          ''/'',i4)') sim_start_time%month, sim_start_time%day,
     $                      sim_start_time%year
      l = l + 1
      write(text(l),'('' Simulation stop date  = '',i2,''/'',i2,
     $          ''/'',i4)') sim_stop_time%month, sim_stop_time%day,
     $                      sim_stop_time%year
      l = l + 1
      write(text(l),'('' -> Simulation duration   = '', i6, '' days'')')
     $     sim_stop_time%days0 - sim_start_time%days0
      l = l + 1
      if( model_type == 'two_d' ) then
         l = l + 1
         write(text(l),'(a,i3,a)')'dyn_steps= ',dyn_steps,
     $                             ' (per day) iters of dyn-T loop'
      end if
      if( static_sim ) then
         l = l + 1
         text(l) = ' static_sim= T : This is a STEADY-STATE simulation'
         l = l + 1
         text(l) = '    All forcings for a cst date = sim start date'
      end if
      l = l + 1
      if( debug ) then
         text(l) = ' debug= T : detection of errors aborts simulation'
       else
         text(l) = ' debug= F : some errors create "warning:" in stdout'
     $                  // ' but do NOT abort simulation!!!'
      end if
      l = l + 1
      if( jump_chem ) then
         text(l) = ' jump_chem= T : Sim will NOT calculate chemistry!!!'
       else
         write(text(l),'(a,i4,a)')'chem_itermax= ',chem_itermax,
     $                        ' is max nb of iter for chem solver'
         l = l + 1
         write(text(l),'(a,f5.1,a)') 'chemdtm= ',chemdtm,' (minutes) '
     $             // 'is timestep for chem solver'
      end if
      l = l + 1

!-----------------------------------------------------------------------
!    ... Main switches
!-----------------------------------------------------------------------
      write(c,'(a,i2,a)') 'mainsw(1)= ',mainsw(1),' : '
      i = LEN_TRIM(c)
      if( mainsw(1) == 0 ) then
         text(l) = c(:i) // ' sim has interactive calculation of T'
       else if( mainsw(1) == 1 ) then
          text(l) = c(:i) // ' sim T relaxed to MSIS T, from 0 to 30km'
       else
          text(l) = c(:i) // ' sim T relaxed to MSIS T everywhere !!'
      end if
      l = l + 1
      write(c,'(a,i2,a)') 'mainsw(2)= ',mainsw(2),' : '
      i = LEN_TRIM(c)
      if( mainsw(2) == 0 ) then
         text(l) = c(:i) //' ATMCOND uses interactive vals for O,O2,T,n'
       else if ( mainsw(2) == 1 ) then
         text(l) = c(:i) // ' ATMCOND uses n (=totdens) by MSIS'
       else if ( mainsw(2) == 2 ) then
         text(l) = c(:i) // ' ATMCOND uses O,O2,T,n by MSIS'
      end if
      if( model_type /= 'two_d' ) goto 150
      l = l + 1
      write(c,'(a,i2,a)') 'mainsw(3)= ',mainsw(3),' : '
      i = LEN_TRIM(c)
      if( mainsw(3) == 0 ) then
         text(l) = c(:i) // ' simulation uses zonal wind u from HWM'
       else if( mainsw(3) == 1 ) then
         text(l) = c(:i) // ' sim uses HWM u to calc interactive u'
       else if( mainsw(3) == 2 ) then
         text(l) = c(:i) // ' sim calc u completetely interactive'
      end if
      l = l + 1
      write(c,'(a,i2,a)') 'mainsw(4)= ',mainsw(4),' : '
      i = LEN_TRIM(c)
      if( mainsw(4) == -1 ) then
         text(l) = c(:i) // ' Hines (using HWM) Kzz and ZERO GW drag !!'
       else if( mainsw(4) == 0 ) then
         text(l) = c(:i) // ' sim has Rayleigh friction & specified'
     $                     // ' Kzz i.o. GW param'
       else if( mainsw(4) == 1 ) then
         text(l) = c(:i) //' sim has Kzz by Hines (using HWM) & '
     $                     // '"arbitrary" GW drag'
       else if( mainsw(4) == 2 ) then
         text(l) = c(:i) //' sim has Lindzen param for GW drag & Kzz'
       else if( mainsw(4) == 3 ) then
         text(l) = c(:i) //' sim has Hines param for GW drag & Kzz '
      end if
      l = l + 1
      write(c,'(a,i2,a)') 'mainsw(5)= ',mainsw(5),' : '
      i = LEN_TRIM(c)
      if( mainsw(5) == 0 ) then
         text(l) = c(:i) // ' PW param: no drag, specified kyy'
       else if( mainsw(5) == 2 ) then
         text(l) = c(:i) // '  PW param is coupled wave model'
       else
         stop 'DESCRIBE_SIM: mainsw(5) has forbidden value'
      end if
      l = l + 1
      write(c,'(a,i2,a)') 'mainsw(6)= ',mainsw(6),' : '
      i = LEN_TRIM(c)
      if( mainsw(6) == 2 ) then
         text(l) = c(:i) // ' simulation has Chemical Heating'
       else if( mainsw(6) == 1 ) then
         text(l) = c(:i) // ' ChemHeat: O2, O, H by MSIS'
       else if( mainsw(6) == 0 ) then
         text(l) = c(:i) // ' ChemHeat set to zero above 70km'
       else if( mainsw(6) == -1 ) then
         text(l) = c(:i) // ' NO ChemHeat, but SolHeat efficiencies = 1'
      end if
150   l = l + 1
      write(c,'(a,i2,a)') 'mainsw(7)= ',mainsw(7),' : '
      i = LEN_TRIM(c)
      if( mainsw(7) == 1 ) then
      text(l) = c(:i) // ' sim has param for transport by '
     $                     // 'conv/fronts in chem solver'
      else if( mainsw(7) == 0 ) then
      text(l) = c(:i) // ' sim has NO transport by conv/fronts'
      end if
      if( model_type /= 'two_d' ) goto 200
      l = l + 1
      write(c,'(a,i2,a)') 'mainsw(8)= ',mainsw(8),' : '
      i = LEN_TRIM(c)
      if( mainsw(8) == 0 ) then
         text(l) = c(:i) // 'Q sim has NO Quasi Biennal Oscillation'
      else if( mainsw(8) == 1 ) then
         text(l) = c(:i) // 'sim has QBO with diabatic forcing'
      else if( mainsw(8) == 2 ) then
         text(l) = c(:i) // 'sim has QBO with effective forcing'
      else if( mainsw(8) == 3 ) then
         text(l) = c(:i) // 'sim has QBO with tropical wave forcing'
      end if
      l = l + 1
      write(c,'(a,i2,a)') 'mainsw(9)= ',mainsw(9),' : '
      i = LEN_TRIM(c)
      if( mainsw(9) == 0 ) then
        text(l) = c(:i) // ' Solar flux & var by Rottmann (old values)'
       else if( mainsw(9) == 1 ) then
        text(l) = c(:i) // ' Solar flux by Lean et al. (1997,122-417nm)'
      end if
      l = l + 1
      write(c,'(a,i2,a)') 'mainsw(10)= ',mainsw(10),' : '
      i = LEN_TRIM(c)
      if( mainsw(10) == 0 ) then
        text(l) = c(:i) // ' LBC on CHI is SIN_VAR(month) of some fct'
       else if( mainsw(10) == 1 ) then
        text(l) = c(:i) // ' LBC on CHI by downward control principle'
      end if
      l = l + 1
      write(c,'(a,i2,a)') 'mainsw(11)= ',mainsw(11),' : '
      i = LEN_TRIM(c)
      if( mainsw(11) == 0 ) then
        text(l) = c(:i) // 'PHO_CHEM interpolated from TABLES_PHO...'
        l = l + 1
        write(c,'(i2)') daypas
        i = LEN_TRIM(c)
        text(l) = '   ... which is calc every daypas= '//c(:i)//' days'
       else if( mainsw(11) == 1 ) then
        text(l) = c(:i) // 'PHO called at each chem tstep (vmr at noon)'
       else if( mainsw(11) == 2 ) then
        text(l) = c(:i) // 'PHO called at each chem tstep (t-dep vmr)'
      end if
      
!-----------------------------------------------------------------------
!    ... Mesosphere/lower thermosphere switches
!-----------------------------------------------------------------------
      l = l + 1
      write(c,'(a,i2,a)') 'mlt_sw(1)= ',mlt_sw(1),' : '
      i = LEN_TRIM(c)
      if( mlt_sw(1) == 1 ) then
         text(l) = c(:i) // ' simulation has molecular viscous damping'
       else if( mlt_sw(1) == 0 ) then
         text(l) = c(:i) // ' sim has NO molecular viscous damping'
      end if
      l = l + 1
      write(c,'(a,i2,a)') 'mlt_sw(2)= ',mlt_sw(2),' : '
      i = LEN_TRIM(c)
      if( mlt_sw(2) == 1 ) then
         text(l) = c(:i) // ' simulation has tidal wave breaking'
       else if( mlt_sw(2) == 0 ) then
         text(l) = c(:i) // ' simulation has NO tidal wave breaking'
      end if
      l = l + 1
      write(c,'(a,i2,a)') 'mlt_sw(3)= ',mlt_sw(3),' : '
      i = LEN_TRIM(c)
      if( mlt_sw(3) == 1 ) then
        text(l) = c(:i) // ' Simulation has thermal conductivity'
      else if( mlt_sw(3) == 0 ) then
        text(l) = c(:i) // ' Simulation has NO thermal conductivity'
      end if
200   l = l + 1
      write(c,'(a,i2,a)') 'mlt_sw(4)= ',mlt_sw(4),' : '
      i = LEN_TRIM(c)
      if( mlt_sw(4) == -2 ) then
        c = c(:i) // ' Sim has ZERO special productions for NO'
       else 
        if( mlt_sw(4) >=-1 ) c=c(:i)//' prod of NO by lightning'
        if( mlt_sw(4) >= 0 ) c = c(:LEN_TRIM(c)) // '+cosmic rays'
        if( mlt_sw(4) >= 1 ) c = c(:LEN_TRIM(c)) // ' + soft X-rays'
        if( mlt_sw(4) == 2 ) c = c(:LEN_TRIM(c)) // ' + aurorae'
      end if
      if( mlt_sw(4) <= 0 ) c = c(:LEN_TRIM(c)) // ' ; flxub(NO)=0'
      text(l) = c
      l = l + 1
      write(c,'(a,i2,a)') 'mlt_sw(5)= ',mlt_sw(5),' : '
      i = LEN_TRIM(c)
      if( mlt_sw(5) == 1 ) then
         text(l) = c(:i) // ' Simulation uses Lyman-a param'
      else 
        text(l) = c(:i) // ' Sim uses crs(O2)=1e-20 cm2 at Lyman-a'
      end if
      l = l + 1
      write(c,'(a,i2,a)') 'mlt_sw(6)= ',mlt_sw(6),' : '
      i = LEN_TRIM(c)
      if( mlt_sw(6) == 0 ) then
        text(l) = c(:i) // ' Solar activity at minimum (solmin)'
       else if( mlt_sw(6) == 1 ) then
        text(l) = c(:i) // ' Solar activity at maximum (solmax)'
       else if( mlt_sw(6) == 2 ) then
        text(l) = c(:i) // ' Solar activity has SIN var, 10.8-yr period'
       else if( mlt_sw(6) == 3 ) then
        text(l) = c(:i) // ' Solar activity follows daily SOLAR2000' 
     $                  // ' (in 19470214-20020731, else sin)'
      end if
      l = l + 1
      write(c,'(a,i2,a)') 'mlt_sw(7)= ',mlt_sw(7),' : '
      i = LEN_TRIM(c)
      if( mlt_sw(7) == 0 ) then
        text(l) = c(:i) // ' E10.7 proxy is cst = 70 (solmin)'
       else if( mlt_sw(7) == 1 ) then
        text(l) = c(:i) // ' E10.7 proxy depends on mlt_sw(6)'
      end if
      l = l + 1
      write(c,'(a,i2,a)') 'mlt_sw(8)= ',mlt_sw(8),' : '
      i = LEN_TRIM(c)
      if( mlt_sw(8) == 0 ) then
        text(l) = c(:i) // ' non-Lya solar flux at solmin'
       else if( mlt_sw(8) == 1 ) then
        text(l) = c(:i) // ' non-Lya solar flux depends on mlt_sw(6)'
      end if
      l = l + 1
      write(c,'(a,i2,a)') 'mlt_sw(9)= ',mlt_sw(9),' : '
      i = LEN_TRIM(c)
      if( mlt_sw(9) == 0 ) then
        text(l) = c(:i) // ' Lya solar flux = 3e11, at solmin'
       else if( mlt_sw(9) == 1 ) then
        text(l) = c(:i) // ' Lya solar flux depends on mlt_sw(6)'
      end if
      l = l + 1
      write(c,'(a,i2,a)') 'mlt_sw(10)= ',mlt_sw(10),': '
      i = LEN_TRIM(c)
      if( mlt_sw(10) == 0 ) then
        text(l) = c(:i) // ' Special prods & flxub for MLT NO, cf ' //
     $                     'mlt_sw(4), at solmin'
       else if( mlt_sw(8) == 1 ) then
        text(l) = c(:i) // ' Special prods & flxub for MLT NO depend on'
     $                  // ' mlt_sw(4,6)'
      end if

!-----------------------------------------------------------------------
!    ... Heterogeneous processes switches
!        No het_sw(1:6) can be active in v6s series, see CHECK_CTRLS
!-----------------------------------------------------------------------
      if( model_type /= '' ) goto 300   
      l = l + 1
      write(c,'(a,i2,a)') 'het_sw(1)= ',het_sw(1),' : '
      i = LEN_TRIM(c)
      if (het_sw(1) == 1) then
         text(l) = c(:i) // ' Simulation has polar stratospheric chem'
         l = l + 1
         write(c,'(a,i2,a)') 'het_sw(2)= ',het_sw(2),' : '
         i = LEN_TRIM(c)
         if( het_sw(2) == 0 ) then
            text(l) = c(:i) // ' Temperature fluctuation for PSCs is 0.'
          else if( het_sw(2) == 1 ) then
            text(l) = c(:i) // ' T fluctuation for PSCs is RANDOM' 
          else if( het_sw(2) == 2 ) then
            text(l) = c(:i) // ' T fluctuation for PSCs is  wave' 
         end if
         l = l + 1
         write(c,'(a,i2,a)') 'het_sw(3)= ',het_sw(3),' : '
         i = LEN_TRIM(c)
         if( het_sw(3) == 1) then
            write(text(l),*) ' Simulation uses NMC temp for PSC '
          else if (het_sw(3) == 0) then
            write(text(l),*) ' Simulation uses model temp. for PSC '
         end if
       else if( het_sw(1) == 0 ) then
        text(l) = c(:i) // ' sim has NO polar stratospheric chemistry'
      end if
      l = l + 1
      write(c,'(a,i2,a)') 'het_sw(4)= ',het_sw(4),' : '
      i = LEN_TRIM(c)
      if( het_sw(4) == 1 ) then
         text(l) = c(:i) // ' Simulation has Polar Mesospheric Chem'
         l = l + 1
         write(c,'(a,i2,a)') 'het_sw(5)= ',het_sw(5),' : '
         i = LEN_TRIM(c)
         if( het_sw(5) == 0 ) then
            text(l) = c(:i) // ' Temperature fluctuation for PMCs is 0.'
          else if( het_sw(5) == 1 ) then
            text(l) = c(:i) // ' T fluctuation for PMCs is  RANDOM' 
          else if( het_sw(5) == 2 ) then
            text(l) = c(:i) // ' T fluctuation for PMCs is  wave' 
        end if
       else if (het_sw(4) == 0) then
        text(l) = c(:i) // ' sim has NO Polar Mesospheric Chemistry'
      end if
      l = l + 1
      write(c,'(a,i2,a)') 'het_sw(6)= ',het_sw(6),' : '
      i = LEN_TRIM(c)
      if( het_sw(6) == 1 ) then
        text(l) = c(:i) // ' Simulation has sulfate aerosol'
     $                  // ' heterogeneous chemistry'
      else if( het_sw(6) == 0 ) then
        text(l) = c(:i) // ' Simulation has NO sulfate aerosol'
     $              // ' heterogeneous chemistry'
      end if
 300  l = l + 1
      write(c,'(a,i2,a)') 'het_sw(7)= ',het_sw(7),' : '
      i = LEN_TRIM(c)
      if( het_sw(7) == 1 ) then
         text(l) = c(:i) // ' Simulation has aerosol effect on J and'
     $              // ' solar heating calc'
       else 
         text(l) = c(:i) // ' Simulation has NO aerosol effect on J and'
     $              // ' solar heating calc'
      end if
      l = l + 1
      write(c,'(a,i2,a)') 'het_sw(8)= ',het_sw(8),' : '
      i = LEN_TRIM(c)
      if( het_sw(8) == 1 ) then
         text(l) = c(:i) // ' Simulation has washout of soluble species'
       else 
         text(l) = c(:i) // ' Simulation has NO soluble species washout'
      end if

!-----------------------------------------------------------------------
!    ... Global chemistry/climate change switches
!-----------------------------------------------------------------------
      if( gcc_sw(1) == 1 ) then
         l = l + 1
         write(c,'(a,i2,a)') 'gcc_sw(1)= ',gcc_sw(1),' : '
         i = LEN_TRIM(c)
         text(l) = c(:i) // ' IC for CO2,CH4,H2O adjusted by initial'
     $              // ' TRENDS_BOUNDY'
      end if
      if( gcc_sw(2) /= 0 ) then
         l = l + 1
         write(c,'(a,i2,a)') 'gcc_sw(2)= ',gcc_sw(2),' : '
         i = LEN_TRIM(c)
         text(l) = c(:i) // ' LBC for CO2 follows TRENDS_BOUNDY'
      end if
      if( gcc_sw(3) /= 0 ) then
         l = l + 1
         write(c,'(a,i2,a)') 'gcc_sw(3)= ',gcc_sw(3),' : '
         i = LEN_TRIM(c)
         text(l) = c(:i) // ' LBC for CH4 follows TRENDS_BOUNDY'
      end if
      if( gcc_sw(4) /= 0 ) then
         l = l + 1
         write(c,'(a,i2,a)') 'gcc_sw(4)= ',gcc_sw(4),' : '
         i = LEN_TRIM(c)
         text(l) = c(:i) // ' IBC for H2O follows TRENDS_BOUNDY'
      end if

!-----------------------------------------------------------------------
!    ... Output parameters
!-----------------------------------------------------------------------
      if( ANY( arch(:)%active ) .or. ANY( dvout(:)%active ) ) then
         l = l + 1
         text(l) = 'Filenames of active output archives (netCDF):'
         do j = 1, n_arch_modes
            if( arch(j)%active ) then
               l = l + 1
               i = LEN_TRIM( arch(j)%flnm )
               text(l) = '"' // arch(j)%flnm(:i) // '"'
            end if
         end do
         do j = 1, max_dvout_dates
            if( dvout(j)%active ) then
               l = l + 1
               if( l == max_nldesc ) then
                  text(l) = '   ...and other "dvout" archive(s)'
                  exit
               end if
               i = LEN_TRIM( dvout(j)%flnm )
               text(l) = '"' // dvout(j)%flnm(:i) // '"'
            end if
         end do
      end if

!-----------------------------------------------------------------------
!    ... Write this description to standard output. It will also be set
!        as a text variable in the netCDF archives (grep NETCDF_MULTITEXT)
!-----------------------------------------------------------------------
      nldesc = l
      write(*,*) '----------------------------------------------------'
      write(*,*) ' '
      do k = 1, nldesc
         write(*,*) TRIM ( ADJUSTL( text(k) ) )
      end do
      write(*,*) '----------------------------------------------------'
      
!      do j = 1, n_arch_modes
!         write(*,*) j, arch(j)%mode, arch(j)%flnm, arch(j)%active
!      end do
!      stop 'test in DESCRIBE_SIM'

      end subroutine DESCRIBE_SIM
