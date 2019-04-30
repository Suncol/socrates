! subversion Id for THIS file : $Id: archive.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/archive.f $
!-----------------------------------------------------------------------

      subroutine ARCHIVE( arch, times )
!-----------------------------------------------------------------------
!	... main archiving routine, depends on mode set in SET_ARCH
!           uses ARCH_VAR2D, ARCH_CHEM & ARCHIVE_INIT *contained* here
!           Beware! The CPU time to define the archive can become *very*
!                   long (~1 min) when there are lots of vars, eg for 
!                   modes 'chem ' or 'full '
!                                       simonc@oma.be  - v6s20, Feb 2001
!-----------------------------------------------------------------------
      use SPECIES_DIMS, only : nbcon
      use GRID_DIMS, only : lmax, niz
      use SIM_CONTROLS, only : jump_chem, model_type, mainsw
      use CONC, only  : ro_s, hm2d
      use VEN1, only  : t2d
      use VEN2, only  : u
      use VEN5, only  : fx, ftot, cf
      use VEN6, only  : x, v, w
      use VEN9, only  : xky, xkz
      use VEN10, only : xtz
      use VEN12, only : ftrop
      use VEN13, only : fbnd, fmvisc
      use VEN14, only : f_e
      use ROSS, only  : fr
      use TIDE1, only : ftx
      use TAU0, only  : q, qsum, qcorr
      use TAU1, only  : pch
      use TAU3, only  : qcool
      use TAU4, only  : q2, q3, q4, q5
      use TROPIC, only : clh
      use SOLTEST, only : srheat3 
      use HEAT_TERMS, only : nch, heatreac, solheat, chemheat, gwheat
      use SPECIES, only : qn2da, qn2noon, qn2night, qn2sr, qn2ss
      use DIAGS_CHEM_VARS
      use BACKATM, only : Xmsis, N2
      use RXT_NAMES
      use SOLDAY, only : logjk
      use TIME_CONTROLS, only : TIMING
      use ARCH_WHAT, only : ARCHIVING
      use NETCDF, only : nf_write
      use NETCDF_UTILS, only : OPEN_FILE,CLOSE_FILE,SET_TIME,NETCDF_ARCH

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      type( ARCHIVING ), intent(inout) :: arch
      type( TIMING), intent(in)  ::  times

!----------------------------------------------------------------------- 
!	... Local variables
!-----------------------------------------------------------------------
      integer :: i, l
      real, dimension(lmax,niz) :: jprov, xm
      character(len=16) :: vname
      
      if( .not. arch%defined ) then
         call ARCHIVE_INIT( arch )             ! contained at end of this file
       else                                    ! Open the file for writing
         call OPEN_FILE( arch%flnm, nf_write )
      end if

!----------------------------------------------------------------------- 
!	... Set the time & Output the next time
!----------------------------------------------------------------------- 
      call SET_TIME( REAL(times%days0) )
      call NETCDF_ARCH( 'time' )

!-----------------------------------------------------------------------
!	... Output temperature & total density
!----------------------------------------------------------------------- 
      call ARCH_VAR2D( arch, 'temperature', 'K', t2d )
      call ARCH_VAR2D( arch, 'totdens', 'molec/cm3', hm2d )
      
!-----------------------------------------------------------------------
!	... Output the dynamical variables
!----------------------------------------------------------------------- 
      if( model_type == 'zerod' .or. arch%mode == 'ubaim' .or. 
     $   arch%mode == 'chem2' ) goto 400
      call ARCH_VAR2D( arch, 'kzz', 'm2/s', xkz )
      if( model_type /= 'two_d' ) goto 400
      call ARCH_VAR2D( arch, 'w', 'm/s', w )
      if( arch%mode == 'basic' .or. arch%mode == 'chem ' ) goto 300
      call ARCH_VAR2D( arch, 'u', 'm/s', u )
!      call ARCH_VAR2D( arch, 'fmvisc', 'm/s/day', 86400.*fmvisc )
!      call ARCH_VAR2D( arch, 'ftx', 'm/s/day', 86400.*ftx )
      call ARCH_VAR2D( arch, 'fx', 'm/s/day', 86400.*fx )
      call ARCH_VAR2D( arch, 'fr', 'm/s/day', 86400.*fr )
!      call ARCH_VAR2D( arch, 'ftrop', 'm/s/day', 86400.*ftrop )
!      call ARCH_VAR2D( arch, 'fbnd', 'm/s/day', 86400.*fbnd )
      call ARCH_VAR2D( arch, 'ftot', 'm/s/day', 86400.*ftot )
      call ARCH_VAR2d( arch, 'f_e', 'm/s/day', 86400.*f_e )
      call ARCH_VAR2D( arch, 'cf', 's-3', cf )
      call ARCH_VAR2D( arch, 'chi',  'm2/s', x )
      do l = 1, lmax
         xm(l,:) = ro_s(:) * x(l,:)
      end do
      call ARCH_VAR2D( arch, 'chiM', 'kg/m/s', xm )
      call ARCH_VAR2D( arch, 'v', 'm/s', v )
      call ARCH_VAR2D( arch, 'kyy', 'm2/s', xky )

!-----------------------------------------------------------------------
!	... Output the heat variables
!----------------------------------------------------------------------- 
300   call ARCH_VAR2D( arch, 'SolHeat', 'K/day', pch )
      call ARCH_VAR2D( arch, 'ChemHeat', 'K/day', srheat3)
      call ARCH_VAR2D( arch, 'gwheat', 'K/day', gwheat )
      call ARCH_VAR2D( arch, 'qcool', 'K/day', -qcool )
      if( arch%mode == 'basic' .or. arch%mode == 'chem ' ) goto 400
      call ARCH_VAR2D( arch, 'therm_diff', 'm2/s', xtz )
      call ARCH_VAR2D( arch, 'clh', 'K/day', clh )
      call ARCH_VAR2D( arch, 'qsum', 'K/day', qsum )
      call ARCH_VAR2D( arch, 'q', 'K/day', 86400.*q )
      if( mainsw(1)/=0 ) 
     $    call ARCH_VAR2D( arch, 'qcorr', 'K/day', 86400.*qcorr )
      call ARCH_VAR2D( arch, 'UVSol_O3', 'K/day', solheat(:,:,2) )
      call ARCH_VAR2D( arch, 'UVSol_O2', 'K/day', solheat(:,:,1) )
      if( .not. jump_chem ) then
         do i = 1, nch
            call ARCH_VAR2D(arch, heatreac(i), 'K/day', chemheat(:,:,i))
         end do
      end if
      call ARCH_VAR2D( arch, 'IRcool_H2O', 'K/day', -q2 )
      call ARCH_VAR2D( arch, 'IRcool_O3', 'K/day', -q3 )
      call ARCH_VAR2D( arch, 'IRcool_CO2', 'K/day', -q4 )
      call ARCH_VAR2D( arch, 'IRcool_NO', 'K/day', -q5 )

!-----------------------------------------------------------------------
!	... Output the chemical variables
!----------------------------------------------------------------------- 
400   continue
      do i = 1, nbcon
         if( arch%vid_qn2da(i) ) 
     $      call ARCH_CHEM( arch, i, '', 'vmr', qn2da )
         if( arch%vid_qn2dv(i) ) then
            call ARCH_CHEM( arch, i, 'noon', 'vmr', qn2noon )
            call ARCH_CHEM( arch, i, 'night', 'vmr', qn2night )
            if( arch%mode == 'chem2' ) then
               call ARCH_CHEM( arch, i, 'rise', 'vmr', qn2sr )
               call ARCH_CHEM( arch, i, 'sets', 'vmr', qn2ss )
               call ARCH_CHEM( arch, i, 'lt12', 's', tau_noon )
               call ARCH_CHEM( arch, i, 'lt24', 's', tau_night )
            end if
         end if
      end do
      if( jump_chem ) goto 500
      if( arch%mode == 'ubaim' ) goto 500
      call ARCH_VAR2D( arch, 'n2', 'vmr', Xmsis(:,:,N2) )
      if( arch%mode == 'heat ' ) goto 500
      if( arch%mode == 'chem2' ) then
         do i = 1, nsfam
            vname = TRIM(ADJUSTL( famname(i) )) // '-loss12'
            call ARCH_VAR2D( arch, vname, '1/s', FamLoss_noon(:,:,i) )
            vname = TRIM(ADJUSTL( famname(i) )) // '-loss24'
            call ARCH_VAR2D( arch, vname, '1/s', FamLoss_night(:,:,i) )
         end do
         call ARCH_VAR2D( arch, 'Ox-lossA12', '1/s', lossOxAnoon(:,:) )
         call ARCH_VAR2D( arch, 'Ox-lossA24', '1/s', lossOxAnight(:,:) )
         call ARCH_VAR2D( arch, 'Ox-lossB12', '1/s', lossOxBnoon(:,:) )
         call ARCH_VAR2D( arch, 'Ox-lossB24', '1/s', lossOxBnight(:,:) )
         call ARCH_VAR2D( arch, 'Ox-lossC12', '1/s', lossOxCnoon(:,:) )
         call ARCH_VAR2D( arch, 'Ox-lossC24', '1/s', lossOxCnight(:,:) )
         call ARCH_VAR2D( arch, 'O3prod12', '1/s', prodO3noon(:,:) )
         call ARCH_VAR2D( arch, 'O3prod24', '1/s', prodO3night(:,:) )
         goto 500
      end if
      do i = 1, nfam
         call ARCH_VAR2D( arch, famname(i), 'vmr', famvmrda(:,:,i) )
      end do

!-----------------------------------------------------------------------
!	... Output a few selected J's
!----------------------------------------------------------------------- 
      if( arch%mode == 'basic' ) goto 500
      jprov = EXP( TRANSPOSE( logjk(:,rid_jo3_od,1,:) ) )
      call ARCH_VAR2D( arch, 'jo3_o1d_1', '1/s', jprov )
      jprov = EXP( TRANSPOSE( logjk(:,rid_jo3_op,1,:) ) )
      call ARCH_VAR2D( arch, 'jo3_o3p_1', '1/s', jprov )
      jprov = EXP( TRANSPOSE( logjk(:,rid_jo2_op,1,:) ) )
      call ARCH_VAR2D( arch, 'jo2_o3p_1', '1/s', jprov )
      jprov = EXP( TRANSPOSE( logjk(:,rid_jo2_od,1,:) ) )
      call ARCH_VAR2D( arch, 'jo2_o1d_1', '1/s', jprov )
      jprov = EXP( TRANSPOSE( logjk(:,rid_jch4,1,:) ) )
      call ARCH_VAR2D( arch, 'jch4_1', '1/s', jprov )
      jprov = EXP( TRANSPOSE( logjk(:,rid_jh2o,1,:) ) )
      call ARCH_VAR2D( arch, 'jh2o_1', '1/s', jprov )
      jprov = EXP( TRANSPOSE( logjk(:,rid_jh2o,2,:) ) )
      call ARCH_VAR2D( arch, 'jh2o_2', '1/s', jprov )

500   call CLOSE_FILE()
      arch%defined = .true.

      end subroutine ARCHIVE
                  
!=======================================================================      

      subroutine ARCHIVE_INIT( arch )
!-----------------------------------------------------------------------
!	... Setup the common parts of the archive netcdf files
!                                       simonc@oma.be  - v6s20, Feb 2001
!-----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use ALLCO, only : phi, zkm
      use ARCH_WHAT, only : vdims, desc_text, nldesc
      use SIM_CONTROLS, only : missval, label_short, label_long
      use NETCDF, only : nf_float
      use ARCH_WHAT, only : ARCHIVING
      use NETCDF_UTILS             ! contains all subroutines called here

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      type( ARCHIVING ), intent(in) :: arch

!----------------------------------------------------------------------- 
!	... Local variables
!-----------------------------------------------------------------------
      integer :: genid
      character(len=8) :: zname
      real :: junk(1)

!----------------------------------------------------------------------
!	... Initialize archive netcdf file
!----------------------------------------------------------------------
      call NETCDF_INIT( arch%flnm )

!----------------------------------------------------------------------
!	... Define the dimensions, vdims(3)='time'=0 for unlimited dim
!----------------------------------------------------------------------
      call NETCDF_DIM_DEF( TRIM( ADJUSTL( vdims(1) ) ), lmax )
      call NETCDF_DIM_DEF( TRIM( ADJUSTL( vdims(2) ) ), niz )
      call NETCDF_DIM_DEF( TRIM( ADJUSTL( vdims(3) ) ), 0 )

!--------------------------------------------------------------------- 
!	... Add attributes for dimensions
!--------------------------------------------------------------------- 
      call SET_ATTRIBUTE( 'lat', 'latitudes', 'global' )
      call SET_ATTRIBUTE( 'lev', 'levels', 'global' )

!--------------------------------------------------------------------- 
!	... Add global attributes to label the run and say it's a d.a.
!--------------------------------------------------------------------- 
      call SET_ATTRIBUTE( 'model_name', 'SOCRATES', 'global' )
      if( arch%mode == 'ma   ' ) then
         call SET_ATTRIBUTE( 'run_label', 'MONTH AVG ; '//label_short
     $                     , 'global' )
       else 
         call SET_ATTRIBUTE( 'run_label', label_short, 'global' )
      end if
      call SET_ATTRIBUTE( 'label_long', label_long, 'global' )
      junk(:) = missval
      call SET_ATTRIBUTE( 'missing_value', var_name='global', 
     $                    attr_val=junk(:) )

!--------------------------------------------------------------------------
!	... Define the spatial variables and attributes
!--------------------------------------------------------------------------
      zname = 'levels  '
      if( arch%mode == 'ma   ' ) zname = 'altitude'
      call NETCDF_VAR_DEF( 'latitudes', nf_float, .false.,
     $                      vdims(1:1), genid )
      call NETCDF_VAR_DEF( TRIM(ADJUSTL(zname)), nf_float, .false.,
     $                      vdims(2:2), genid )
      call SET_ATTRIBUTE( 'units', 'angular degrees', 'latitudes' )
      call SET_ATTRIBUTE( 'units', 'kilometers', TRIM(ADJUSTL(zname)) )

!-----------------------------------------------------------------------
!	... Write the spatial variables
!-----------------------------------------------------------------------
      call NETCDF_ARCH( 'latitudes', vector = phi )
      call NETCDF_ARCH( TRIM(ADJUSTL(zname)), vector = zkm )

!--------------------------------------------------------------------------
!	... Define the time variable and attribute
!--------------------------------------------------------------------------
      call NETCDF_VAR_DEF( 'time', nf_float, .true.,
     $                      vdims(3:3), genid )
      call SET_ATTRIBUTE( 'units', 'days since 0/0/0, no leap years', 
     $                    'time' )

!--------------------------------------------------------------------------
!	... Define and set the description text variable
!--------------------------------------------------------------------------
      call NETCDF_MULTITEXT( 'description', nldesc, desc_text(:nldesc) )

      end subroutine ARCHIVE_INIT

!=======================================================================      

      subroutine ARCH_VAR2D( arch, name, units, vals )
!-----------------------------------------------------------------------
!	... Small utility to define & archive any lat/time variable
!           Interpolate vals to zgeo grid for monthly averaged ('ma')
!           archives   - simonc - v7s15, Feb 2003
!-----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use ALLCO, only : zkm
      use MONTHLY_VAR, only : zgeo_ma
      use SIM_CONTROLS, only : missval
      use DIAG_CONTROLS, only : diags
      use ARCH_WHAT, only : vdims
      use NETCDF, only : nf_float
      use ARCH_WHAT, only : ARCHIVING
      use NUMERICAL, only : INTERP
      use NETCDF_UTILS, only : NETCDF_VAR_DEF, SET_ATTRIBUTE,NETCDF_ARCH

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      type( ARCHIVING ), intent(in) :: arch
      character(len=*), intent(in) :: name, units
      real, dimension(lmax,niz), intent(in) :: vals
      
!----------------------------------------------------------------------- 
!	... Local variables
!-----------------------------------------------------------------------
      logical :: ok
      integer :: genid, l
      real, dimension(lmax,niz) :: archvals

      if( .not. arch%defined ) then
        if( diags ) write(*,*) 'ARCH_VAR2D in ARCHIVE: arch%mode= ',
     $                 arch%mode,' ; defining var >' // name // '<'
        call NETCDF_VAR_DEF( TRIM(ADJUSTL( name )), nf_float, .true., 
     $                       vdims(1:2), genid )
        call SET_ATTRIBUTE( 'units', units, TRIM(ADJUSTL( name )) )
      end if
      
      archvals = vals
      if( arch%mode == 'ma   ' ) then
         archvals = missval
         do l = 1, lmax
            call INTERP( niz, zkm, archvals(l,:)
     $                 , niz, zgeo_ma(l,:), vals(l,:)
     $                 , ext_vals = 'notouch', ok=ok )
            if( .not. ok ) then
               write(*,*)'ARCH_VAR2D fatal error archiving var ' // name
               write(*,*)'   to file '//arch%flnm
               write(*,*)'INTERP from zgeo to zkm failed, lat= ',l
               stop 'ARCH_VAR2D fatal error:  INTERP from zgeo failed'
           end if
         end do
      end if
      
      call NETCDF_ARCH( TRIM(ADJUSTL( name )), matrix = archvals )
      
      end subroutine ARCH_VAR2D

!=======================================================================      

      subroutine ARCH_CHEM( arch, i, str, units, vals )
!-----------------------------------------------------------------------
!	... small utility to define & archive chem variables
!-----------------------------------------------------------------------
      use SPECIES_DIMS, only : nbcon
      use GRID_DIMS, only : lmax, niz
      use TRACNM, only : solsym
      use ARCH_WHAT, only : ARCHIVING

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      type( ARCHIVING ), intent(in) :: arch
      integer, intent(in) :: i
      character(len=*), intent(in) :: str, units
      real, dimension(lmax,niz,nbcon) :: vals
           
!----------------------------------------------------------------------- 
!	... Local variables
!-----------------------------------------------------------------------
      character(len=16) :: vnaf

      vnaf = TRIM( ADJUSTL( solsym(i) ) ) 
      if( LEN_TRIM(str) /= 0 ) then
         vnaf = TRIM(ADJUSTL( solsym(i) )) //'-'// TRIM(ADJUSTL( str ))
      end if
      call ARCH_VAR2D( arch, vnaf, units, vals(:,:,i) )
      
      end subroutine ARCH_CHEM
