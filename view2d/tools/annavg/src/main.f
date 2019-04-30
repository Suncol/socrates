
      program ANNAVG
!--------------------------------------------------------------------- 
!	... Read variables archived in SOCRATES output file ('in_file')
!           (netCDF), calculate the annual averages, and write these
!           in a new netcdf file ('out_file')
! TO ENHANCE: FIT COMPLETE (INPUT) TIME-SERIES -> OPTIMIZE MEMORY
!    -> MAKE A BIG EXTERNAL LOOP ON VARS AND ARCHIVE ONE VAR AT A TIME.
!--------------------------------------------------------------------- 
      use GRID_DIMS, only : lmax, niz
      use SIM_CONTROLS, only : missval
     $                       , label_short, label_long, model_name
      use ALLCO, only : phi, zkm
      use ARCH_WHAT, only : desc_text, nldesc
      use TYPE_DEF, only: TIMING, VAR2D_TYPE
      use NETCDF_UTILS
      use NETCDF, only : NF_INQ_LIBVERS, NF_NOWRITE
      use DATE_UTILS, only : TIME2DATE, DATE2TIME
      use FIT_UTILS, only : FIT_GET_LIN, FIT_GET_EXP

      implicit none
 
!--------------------------------------------------------------------- 
!	... parameters
!--------------------------------------------------------------------- 
      integer, parameter :: nvars_max = 40
     $,                     ndates_max = 999
     $,                     nymax = 100
      real, parameter :: dlat = 5. ! latitude step, degrees 

!--------------------------------------------------------------------- 
!	... Local variables
!--------------------------------------------------------------------- 
      integer :: nvars = 0   ! nb of variables to process
     $,          ndates = 0  ! nb of archived dates in in_file
      integer, dimension(nvars_max) :: ndpy  = 0   ! nb of dates in current year
     
      integer :: i, iz, iy, lat, idate, prev_year, year1, slen
      real :: a, b
      real, dimension(ndates_max) :: days0in = -999.
      real, dimension(nymax) :: years, linseries
      real, dimension(lmax,niz,nvars_max,nymax) :: ydepvals   ! 130 megabytes !
      character(len=80) :: in_file, out_file
      character(len=20), dimension(nvars_max) :: varname = ''
      type( TIMING) :: date_in, date_out
      type( VAR2D_TYPE ), dimension(nvars_max) :: var2din, var2dout

!-----------------------------------------------------------------------
!         ... Declare the namelist inputs
!-----------------------------------------------------------------------
      namelist / io_parms / in_file, varname, out_file
 
!----------------------------------------------------------------------- 
!	... Initialize & read input namelist
!----------------------------------------------------------------------- 
      write(*,*) 'NetCDF library version ',NF_INQ_LIBVERS()
      read(*,io_parms)
      if( LEN_TRIM( out_file ) == 0 ) stop 'ANNAVG: set "out_file"'
      iy = 0
      nldesc = 3
      desc_text(1:nldesc) =
     $  (/ 'ANNUAL AVERAGES computed from file... '
     $   , in_file
     $   , '... by program ANNAVG (socrates/tools/annavg)' /)
 
!----------------------------------------------------------------------- 
!	... Set up axes coords (should be read in input file)
!----------------------------------------------------------------------- 
      zkm(:) = (/ (REAL(iz), iz=0, niz-1) /) ! log-p alt, module ALLCO
      phi(:) = (/ ( dlat * REAL(lat) - 90., lat=1, lmax) /)

!----------------------------------------------------------------------- 
!	... Open the file, read the archived dates & global attributes
!----------------------------------------------------------------------- 
      call OPEN_FILE( in_file, NF_NOWRITE )
      call NETCDF_DIM_READ( 'time', ndates )
      write(*,*) 'ANNAVG: nb of dates in in_file is ndates= ',ndates
      if( ndates < 8 ) stop 'ANNAVG: ndates is < 8'
      if( ndates > ndates_max ) stop 'ANNAVG: ndates is > ndates_max'
      call NETCDF_READ( 'time', vector = days0in(:ndates) )
      call GET_ATTRIBUTE( 'model_name', 'global', model_name )
      call GET_ATTRIBUTE( 'run_label', 'global', label_short )
      label_short = TRIM(ADJUSTL(label_short)) // '_aa'
      call GET_ATTRIBUTE( 'label_long', 'global', label_long )
      label_long = 'Annual avg ; ' // TRIM(ADJUSTL(label_long))

!----------------------------------------------------------------------- 
!	... Prepare the variables
!----------------------------------------------------------------------- 
      do i = 1, nvars_max
         slen = LEN_TRIM( varname(i) )
         if( slen == 0 ) exit
         var2din(i)%name = varname(i)(:slen)
         write(*,*) 'ANNAVG: will process var >'//varname(i)(:slen)//'<'
         call GET_ATTRIBUTE( 'units', varname(i), var2din(i)%units )
         var2din(i)%vals = missval
         var2dout(i) = var2din(i)
         var2dout(i)%vals = 0.
      end do
      nvars = i - 1
      write(*,*) 'ANNAVG: nb of vars to process is nvars= ',nvars
      if( nvars == 0 ) stop 'ANNAVG: set some "varname"'

      date_in%days0 = INT( days0in(1) )
      call TIME2DATE( date_in )
      year1 = date_in%year
      prev_year = year1
!      ndates = 100
      
!-----------------------------------------------------------------------
!	... Loop on archived dates in input file
!-----------------------------------------------------------------------
      do idate = 1, ndates
         date_in%days0 = INT( days0in(idate) )
         call TIME2DATE( date_in )

!-----------------------------------------------------------------------
!	... If new input year, first arch ann avg of prev year
!-----------------------------------------------------------------------
         if( date_in%year > prev_year ) then
            if( prev_year == year1 .or. ANY( ndpy(:nvars) < 4 ) .or.
     $                                  ANY( ndpy(:nvars) > 13) ) then
               write(*,*) 'ANNAVG, WARNING: ',prev_year,' NOT ARCHIVED'
             else
               write(*,*)'NEW YEAR BEGINS. Arch ann avg of prev year...'
               iy = iy + 1
               if( iy > nymax ) stop'ANNAVG: too many years, iy > nymax'
               do i = 1, nvars
                  if( ndpy(i) > 0 ) then
                     var2dout(i)%vals = var2dout(i)%vals / REAL(ndpy(i))
                   else
                     var2dout(i)%vals = missval
                  end if
                  ydepvals(:,:,i,iy) = var2dout(i)%vals
               end do
               years(iy) = REAL( prev_year )
               date_out = TIMING( prev_year, 7, 2, 0, 0. ) ! Middle of year is 2 July
               call DATE2TIME( date_out )
               call ARCHIVE( out_file, date_out, nvars, .true.
     $                     , var2dout(:nvars) )
               call SET_FILE( in_file )
            end if
            do i = 1, nvars
               var2dout(i)%vals = 0.
            end do
            ndpy(:) = 0
         end if

!-----------------------------------------------------------------------
!	... Read values at next date in input file
!           Notice: last input year not averaged and not output
!-----------------------------------------------------------------------
         do i = 1, nvars
            call NETCDF_READ( varname(i), time=days0in(idate)
     $                      , matrix=var2din(i)%vals )
            if( ALL( var2din(i)%vals > missval ) ) then
               var2dout(i)%vals = var2dout(i)%vals + var2din(i)%vals
               ndpy(i) = ndpy(i) + 1
            end if
         end do
         slen = LEN_TRIM( varname(2) )
         write(*,'(a,i4,3(a,i2),a,es12.5)') 'date_in= ',date_in%year
     $    ,'/',date_in%month,'/',date_in%day,' ; ndpy= ',ndpy(2)
     $    ,' ; '//varname(2)(:slen)//'(18,61)= ', var2din(2)%vals(18,61)
         prev_year = date_in%year
      end do

!-----------------------------------------------------------------------
!	... Find & archive the time-indep linear trends (units/year)
!-----------------------------------------------------------------------
      if( iy < 2 ) stop 'ANNAVG: less than 2 years where averaged'
 1000 write(*,*) 'Finished reading input file. Finding linear fit...'
      do i = 1, nvars
         slen = LEN_TRIM( varname(i) )
         var2dout(i)%name = varname(i)(:slen) // '_ltrend'
         if( TRIM(ADJUSTL( varname(i) )) == 'temperature' ) 
     $       var2dout(i)%name = 'T_ltrend'
         slen = LEN_TRIM( var2din(i)%units )
         var2dout(i)%units = var2din(i)%units(:slen) // '/yr'
         do lat = 1, lmax
            do iz = 1, niz
               call FIT_GET_LIN( iy, years(:iy), ydepvals(lat,iz,i,:iy)
     $                         , a, b )
               var2dout(i)%vals(lat,iz) = a
            end do
         end do
         if( var2din(i)%units(:slen) == 'vmr' ) then
            var2dout(i)%vals = 1.e9 * var2dout(i)%vals
            var2dout(i)%units = 'ppb/yr'
         end if
      end do
      call ARCHIVE( out_file, date_out, nvars, .false.
     $            , var2dout(:nvars) )

!-----------------------------------------------------------------------
!	... Find & archive the time-indep exponential trends c (%/year)
!-----------------------------------------------------------------------
      do i = 1, nvars
         slen = LEN_TRIM( varname(i) )
         var2dout(i)%name = varname(i)(:slen) // '_etrend'
         if( TRIM(ADJUSTL( varname(i) )) == 'temperature' ) 
     $       var2dout(i)%name = 'T_etrend'
         slen = LEN_TRIM( var2din(i)%units )
         var2dout(i)%units = '%/yr'
         write(*,*) 'Finding Exponential fit for ',varname(i)
         do lat = 1, lmax
            do iz = 1, niz
               call FIT_GET_EXP( iy, years(:iy), ydepvals(lat,iz,i,:iy)
     $                         , a, b, missval )
               var2dout(i)%vals(lat,iz) = a
            end do
         end do
      end do
      call ARCHIVE( out_file, date_out, nvars, .false.
     $            , var2dout(:nvars) )

      call CLOSE_ALL_FILES()

      end program ANNAVG
