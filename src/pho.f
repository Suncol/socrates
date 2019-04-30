! subversion Id for THIS file : $Id: pho.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/pho.f $
!-----------------------------------------------------------------------

      subroutine PHO( write_diags, rayli, aero, drop, albedo, 
     $                dens, temper, levlo, secchap, zs, drat, wrat )
!---------------------------------------------------------------------
!    ... Calculates photodissociation and warming/cooling rates in a 
!           multilayered plane parallel medium for wavenumbers from 
!           116.3 nm to 730 nm using the two-stream approximation radiative 
!           transfer algorithm twostr.
!---------------------------------------------------------------------
! DATA FILES
!      *** solflux.dat    : wavelength intervals and the solar flux
!      *** solflux.dat    : absorption cross-sections at same wavelength
!                           intervals. The data are from 116.3 nm to 730 nm.
!
!      *** solar.ssalb :  Data files for water cloud parameterization.
!          solar.b_ext
!          solar.asymm
!
!---------------------------------------------------------------------
!            >>>>>>>>>>  Subroutines and purposes  <<<<<<<<<<<<<
!
!     Routine PHO_INIT & AERO_INIT must be called before this one (PHO),
!     which calls:
!     ------------
!
!       ABSDEP_PHO:  Calculates absorption coefficients due to o2, o3 and n2o
!       AEROSOL_PHO: Calculate absorption and scattering optical
!                    depth and asymmetry factor due to aerosol
!       COLDEN_PHO:  Calculates column densities
!       CRSCOR_PHO:  Corrects cross-sections for temperature and
!                    pressure dependence
!       LYA_SRB_PHO: Parameterizations by reduction factors for Lyman-alpha 
!                    line & Schumann-Runge bands
!       LINPOL_PHO:  Linear interpolation between two points
!       LSKIP :      Skips n lines while reading from file
!       OPTICP_PHO:  Calculates phase function, optical depth and single
!                    scattering albedo
!       RATES_PHO:   Calculates the photodissociation rate
!       SCADEP_PHO:  Calculates scattering coefficients due to Rayleigh
!                    and Mie scattering
!
!---------------------------------------------------------------------
!
!     Index conventions ( for all variables described below ) :
!
!     iproc: For photodissociation process indexing                      
!
!------------------------------------------------------------------
!     photodissociation rates numbering
!------------------------------------------------------------------
!      1..........O2 -> O(3P) + O(3P)
!      2..........O3 -> O(3P) + O2
!      3..........H2O
!      4..........N2O
!      5..........CO2
!      6..........CH4
!      7..........NO2
!      8..........HNO3
!      9..........CFC-12
!     10..........CFC-11
!     11..........CCl4
!     12..........HOCl
!     13..........CH3CCl3
!     14..........HO2NO2 -> HO2 + NO2
!     15..........CH3Cl
!     16..........ClONO2 -> ClO + NO2
!     17..........N2O5
!     18..........O3 -> O(1D) + O2
!     19..........CFC-113
!     20..........HCFC-22
!     21..........Ha-1211
!     22..........Ha-1301
!     23..........H2O2
!     24..........CH2O -> CHO + H
!     25..........BrONO2
!     26..........HOBr
!     27..........CH3Br
!     28..........OClO
!     29..........Cl2O2
!     30..........Cl2   
!     31..........CCl2O
!     32..........CClFO
!     33..........CF2O
!     34..........CFC-114
!     35..........CFC-115
!     36..........HCl
!     37..........CH2O -> CO + H2
!     38..........CH3OOH
!     39..........CH3CO3 (cross section to be specified)
!     40..........PAN
!     41..........ClNO2 
!     42..........NO3  -> NO2 + O
!     43..........NO
!     44..........BrCl  ( presently ajbrcl in chemical code)
!     45..........BrO   ( presently ajbro in chemical code )
!     46..........HO2NO2 -> OH + HNO3
!     47..........ClONO2 -> Cl + NO3
!     48..........NO3  -> NO + O2
!     49..........ClOO -> ClO + O ( Unused )
!     50..........O2   -> O(3P) + O(1D) ( Unused )
!
!---------------------------------------------------------------------
!               I N P U T    V A R I A B L E S
!---------------------------------------------------------------------
!
!   ====================================================================
!   ==         NOTE: mxcly = 120   niz = 121                          ==
!   == Level-dependant variables are numbered from 0 (at 120km)       == 
!   ==                                        to mxcly (at 0km)       ==
!   == Layer-dependant variables are numbered     from 1 (120-119km)  ==
!   ==                             to mxcly (1-0km)      ==
!   == Upward-numbered variables are numbered     from 1 (at 0km)       ==
!   ==                             to niz (at 120km)     ==
!   ==  ...and come from the 2d-model (see common / airglow)           ==
!   ==                                             ==
!   == Only the aerosol-related variables in common / ae do not       ==
!   == respect this convention (to correct ASAP)             ==
!   ====================================================================
!
!     albedo      : bottom-boundary albedo for Lambertian reflecting
!                   surface.
!
!     dens(lev,idens): density of species idens (molecules/cm**3)
!                    idens = 1            air
!                    idens = 2            o3
!                    idens = 3            o2
!                    idens = 4            no
!                    idens = 5            no2
!
!
!    drop     = true, absorption and scattering of drops (water clouds)
!                     are included, in this case must also specify:
!
!                     wceffr(lev) : equivalent radius of drop size
!                                   spectrum (um)
!                     wccon(lev)  : liquid water content for cloud (g/m**3)
!
!             = false, not included
!
!    rayli    = true, rayleigh scattering is included
!             = false, rayleigh scattering is not included
!
!
!    temper(lev) lev = 0 to mxcly, temperatures (K) at levels.
!                (note that temperature is specified at levels
!                rather than for layers.)  don't forget to put top
!                temperature in 'temper(0)', not in 'temper(1)'. 
!                Used to calculate the temperature dependence in
!                cross-sections etc.
!    
!    levlo       integer between lev=0 and lev=mxcly, all calc done
!                for lev=0,levlo because sun below horizon at lower levels
!
!    secchap(lev) lev = 0 to mxcly, Secant or Chapman of zenith angle of 
!                incident beam (positive).
!                ** warning **  if the corresponding cosine is close to one 
!                of the computational polar angle cosines, serious ill-
!                conditioning and a possible crash of 'twostr' might 
!                result;  hence this is flagged as a fatal error.    
!
!    zs(lev)      lev = 0 to mxcly, *geometrical* altitude of level (km) : 
!                 zs(0) = +-120km , zs(120) = 0km
!
!    aero         true, absorption and scattering by aerosols included
!                 false, absorption and scattering by aerosols not included
!
!---------------------------------------------------------------------
!               O U T P U T    V A R I A B L E S
!---------------------------------------------------------------------
!
!    == note on units == fluxes and average intensities comes in 
!                        units of (no. of photons/(cm**2 s) 
!
!    drat(iproc,lev):  photodissociation rate (1/s) of process iproc
!    wrat(1,0:mxcly) : O2 heating (J/s ?)
!    wrat(2,0:mxcly) : O3 heating (J/s ?)
!    wrat(3,0:mxcly) : aerosol and cloud heating (J/s)
!
!---------------------------------------------------------------------
      use GRID_DIMS, only : niz            ! for diags
      use PHYS_CST, only : pi, R, g0, R0
      use PHO_PARMS
      use PHO_VARS
      use PHO_AERO, only : exabs
      use SIM_CONTROLS, only : mlt_sw, missval
      use DIAG_CONTROLS, only : zdiag

      implicit none

!---------------------------------------------------------------------
!   local symbolic dimensions:
!
!       mxcly  = max no. of computational layers (mxcly+1=no. of levels)
!       mxwvn  = no. of wavelength intervals
!       mxwvwc = max no. wavelength points for water cloud
!       mxsiz  = max no. sizes for water clouds
!       mxcof  = no. of coefficients for water cloud parameterization
!       phtmax = max no. of photolysis process
!---------------------------------------------------------------------
      integer, parameter :: jno = 43
      integer, parameter :: lya_iv = 8   ! Lyman-alpha wavelength interval

!---------------------------------------------------------------------
!    ... Dummy args
!---------------------------------------------------------------------
      logical, intent(in) ::  write_diags, rayli, aero, drop
      integer, intent(in) ::  levlo
      real, intent(in)    ::  albedo
      real, intent(in)    ::  dens(0:mxcly,maxden)
      real, dimension(0:mxcly), intent(in) ::  temper, secchap, zs
      real, intent(out)   ::  drat(phtmax,0:mxcly)
      real, intent(out)   ::  wrat(3,0:mxcly)

!---------------------------------------------------------------------
!               I n t e r n a l    v a r i a b l e s
!
!    babsx(lev)   lev = 1 to mxcly,
!                absorption coefficients for o2 and o3 (x = o),    
!                drops (e.g. clouds, x = d),
!                beta abs in s3 (dimensionless)   
!
!    bscax(lev)   lev = 1 to mxcly,
!                scattering coefficient for rayleigh scattering (x = r),    
!                drops (e.g. clouds, x = d),
!                beta sca in s3 (dimensionless)   
!
!    cdenox(lev)  lev = 1 to mxcly,
!                column density for oxygen (x=2) and ozone (x=3),
!                (molecules/cm**2)
!
!    cdento(lev)  lev = 1 to mxcly,
!                total column density for air,
!                (molecules/cm**2)
!
!    crs(iproc,iv) iproc = 1, phtmax, iv = ivbegin, ivend
!                reaction cross-sections for the different photo-
!                chemical processes at wavelength interval iv as read
!                from datafiles crs97.dat
!
!    xsect(iproc,lev,iv) iproc = 1, phtmax ; iv = ivbegin, ivend
!                reaction cross-sections for the different photo-
!                chemical processes at wavelength interval iv ,
!                temperature(lev) and pressure adjusted, ( cm**2 )
!
!    dtauc(lev,iv)   lev = 1 to mxcly ; iv =1 to mxwvn ,
!                optical depths of computational layers
!
!    fbeamr(iv)   intensity of incident solar  beam at  
!                top boundary. ( no. of photons/( cm**2 s) )            
!
!    gd(lev)      lev = 1 to mxcly,
!                asymmetry factor for drop particles
!
!    gg(lev,iv)  lev = 1 to mxcly, iv = 1 to mxwvn
!                1st coeff. in Legendre polynomial expansions of phase
!                functions for computational layers => single scattering
!                albedo asymmetry factor. Previously named pmom(1,lev) .
!
!    iv  :  For wavelength interval indexing                      
!
!    iwcsiz(lev)  index for which size interval to use
!                for water cloud parameterization
!
!    lev     Index of vertical layer (1 at the top to mxcly at the bottom)
!            or of vertical level (0 at the top to mxcly at the bottom)
!
!    sigra(iv)   iv = 1 to mxwvn (?)
!                rayleigh scattering cross sections (cm**2) 
!
!    ssalb(lev,iv)   lev = 1 to mxcly ; iv =1 to mxwvn ,
!                single-scatter albedos of computational layers
!
!    uavg(lev,iv)   lev = 0 to mxcly ; iv =1 to mxwvn ,
!                mean intensity (including the direct beam)
!
!    wcwvn(ivwc) wavelengths (nm) for water cloud parameterization
!
!    wc_asy(isiz,ivwc,icof),   Water cloud parameters for different
!      wc_ssa(isiz,ivwc,icof), effective drop sizes (isiz=1,3), and
!      wc_ext(isiz,ivwc,icof), wavelengths (ivwc). icof=1,3 represents (i.e.)
!                                  b_ext= a*(r_eff**b) + c
!                              where icof=1 -> a, icof=2 -> b and
!                              icof=3 -> c.
!
!    wvnwcz(isiz+1)  contains the different sizes for water clouds (microns)
!
!    wk()        work area
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!    ... Local variables
!---------------------------------------------------------------------
      integer ::  iv, lev, isize, iok
      integer ::  iwcsiz(0:mxcly)
      real    ::  sho2, gtop, solid_anglei
      real, dimension(0:mxcly) ::  fdr, fup, fdn, jjno
      real, dimension(mxcly)   ::  bscae, babsae, gaer, tautotal,
     $                             delsc, colo2d, dtauc2, babso3,
     $                             babso, bscar, babsd, bscad, gd
      real, dimension(mxcly,mxwvn) ::  dtauc, ssalb, gg
      real    ::  uavg(mxwvn,0:mxcly)
      real    ::  exabscl(0:mxcly,mxwvn)         ! drop (cloud) absorption coefficient
      real    ::  xsect(phtmax,0:mxcly,mxwvn)    ! working cross section array
      real    ::  qy_o3(mxwvn,0:mxcly),
     $            qy_o3_o1d(mxwvn,0:mxcly)
      real, dimension(mxcly)   ::  cdento, cdeno2
      real, dimension(0:mxcly) ::  colo2, wccon, wceffr
 
         if ( niz /= mxcly+1 ) then
           print *,'! FATAL ERROR : nb of levels in 2-d model : niz = ',
     $       niz,' not equal to mxcly+1 where mxcly = nb of layers in',
     $        'phodis model = ',mxcly
            stop 'PHODIS: fatal error dimension mismatch'
         end if    
         wccon = 0.
         wceffr = 0.
         solid_anglei = 1. / (4.*pi)

      if( levlo < 0 .or. levlo > mxcly ) stop'PHO: levlo out of range'
      if( ANY(secchap(0:levlo)==missval) ) stop'PHO: some sechap misses'
      
      drat(:,:) = 0.
      wrat(:,:) = 0.
      
!-----------------------------------------------------------------------
!         ... Integrate over single layers to obtain column densities
!-----------------------------------------------------------------------
      call COLDEN_PHO( cdeno2, cdento, dens, zs )

!----------------------------------------------------------------------
!    ... Find the size-interval to be used for water clouds
!----------------------------------------------------------------------
      if( drop ) then
         do lev = 0,mxcly
            if( wccon(lev) > 0. .and. wceffr(lev) > 0. ) then
               if( wceffr(lev) < wvnwcz(1) ) then
                  iwcsiz(lev) = 1
                else if( wceffr(lev) >= wvnwcz(mxsiz+1) ) then
                  iwcsiz(lev) = mxsiz
                else
                  do isize = 1,mxsiz
                    if( wceffr(lev) >= wvnwcz(isize) .and.
     $                  wceffr(lev) < wvnwcz(isize+1) ) then
                        iwcsiz(lev) = isize
                  exit
                    end if
                  end do
               end if
            end if
         end do
      end if

!----------------------------------------------------------------------
!    ... Correct absorption cross-sections for T and P dependence 
!----------------------------------------------------------------------
      call CRSCOR_PHO( xsect, qy_o3, qy_o3_o1d, dens, temper )

!---------------------------------------------------------------------
!    ... Calculate O2 overhead column amount (cm-2) for Kockart SR approx.
!---------------------------------------------------------------------
      colo2d(1:mxcly) = 5.e4 * (zs(0:mxcly-1) - zs(1:mxcly))
     $                       * (dens(0:mxcly-1,3) + dens(1:mxcly,3))
      gtop = g0 * ( R0 / (R0+zs(0)) )**2       ! m2/s
      sho2 = R / (.032*gtop)*100.*temper(0)
      colo2(0) = dens(0,3) * sho2
      do lev = 1,mxcly
         colo2(lev) = colo2(lev-1) + colo2d(lev)
      end do

!---------------------------------------------------------------------
!    ... Calculate reduction factors ro2lya and rmlya for
!           Chabrillat/Kockarts Lyman-alpha line parametrization.
!    ... Calculate reduction factors ro2 and rm for Kockarts
!           Schumann-Runge bands parametrization.
!---------------------------------------------------------------------
      call LYA_SRB_PHO( (mlt_sw(5)==1), levlo, colo2, secchap )
      
!---------------------------------------------------------------------
!    ... Diagnostics for altitude where optical depth=1 - STOPS if called
!---------------------------------------------------------------------
!      mu2(:) = 1. ! solar zenith angle = 0 degrees - mu2=1/secchap
!      call TAU1_DIAGS_PHO( colo2(0), mu2, zs, dens, xsect )

!---------------------------------------------------------------------
!    ... Loop on wavelength intervals
!---------------------------------------------------------------------
      do iv = 1,mxwvn
        
         gg(:,iv) = 0.
!---------------------------------------------------------------------
!       ... Calculate the absorption optical depth due to molec abs.
!---------------------------------------------------------------------
         call ABSDEP_PHO( xsect(:,:,iv), dens, zs, babso, babso3 )
         
!---------------------------------------------------------------------
!         ... Calculate absorption and scattering optical
!           depth and asymmetry factors due to water 
!           clouds and Rayleigh scattering
!---------------------------------------------------------------------
         call SCADEP_PHO( babsd, bscad, bscar, cdento, drop, gd, 
     $                    exabscl, iwcsiz, rayli, sigra(:,iv), 
     $                    wccon, wceffr, zs, iv )
     
!---------------------------------------------------------------------
!    ... Calculate absorption and scattering optical
!           depth and asymmetry factor due to aerosol
!---------------------------------------------------------------------
         babsae = 0.
         bscae = 0.
         gaer = 0.
         exabs(:,iv) = 0.
         if( aero ) call AEROSOL_PHO( zs, babsae, bscae, gaer, iv )

!---------------------------------------------------------------------
!    ... Calculate total absorption and scattering optical 
!           depth, single scattering albedo and phase function
!---------------------------------------------------------------------
         call OPTICP_PHO( babso, bscar, babsd, bscad, 
     $                    gd, dtauc(:,iv), gg(:,iv), ssalb(:,iv),
     $                    babsae, bscae, gaer )
     
!---------------------------------------------------------------------
!    ... Solve the one-dimensional radiative transfer equation
!           to obtain mean intensities for each wavelength.
!---------------------------------------------------------------------
         uavg(iv,:) = 0. 
         if( iv > 61 ) then

!---------------------------------------------------------------------
!    ... wl > 205 nm : Delta eddington two-str method from Sasha Mandronich
!---------------------------------------------------------------------
            call TWSTR_PHO( levlo, secchap(:), albedo, dtauc(:,iv),
     $                      ssalb(:,iv), gg(:,iv), fdr, fup, fdn )
     
!---------------------------------------------------------------------
!    ... Total actinic flux= fdr+dfup+fdn
!           Need to multiply by fbeamr
!           mean Intensity = actinic flux/4*pi
!---------------------------------------------------------------------
            uavg(iv,0:levlo) = fbeamr(iv) * solid_anglei *
     $                  ( fdr(0:levlo) + fup(0:levlo) + fdn(0:levlo) )
         else
!---------------------------------------------------------------------
!    ... For wl < 205 nm
!           Direct absorption and single scattering
!        delta scaling for total optical depth
!        dtauc2 is the O3 optical depth for Kockarts SR approximations
!---------------------------------------------------------------------
            uavg(iv,0) = 1.
            delsc(:) = (1. - ssalb(:,iv) * gg(:,iv) * gg(:,iv))
            if( iv < 46 .and. ( mlt_sw(5)==0 .or. iv /= lya_iv) )  then
               tautotal(:levlo) = dtauc(:levlo,iv) * delsc(:levlo) 
     $                                             * secchap(1:levlo)
               do lev = 1, levlo
                  uavg(iv,lev) = uavg(iv,lev-1)
     $                           * EXP( -tautotal(lev) )
               end do 
             else
               dtauc2(:levlo) = babso3(:levlo) * delsc(:levlo) 
     $                                         * secchap(1:levlo)
               do lev = 1, levlo
                  uavg(iv,lev) = uavg(iv,lev-1)
     $                           * EXP( -dtauc2(lev) )
               end do 
            end if 
            if( write_diags .and. iv==61 ) then
               lev = niz - zdiag
               write(*,*)
               write(*,'(3(a,i3),a,es11.3)') 'PHO: levlo= ',levlo,
     $          ' ; lev= ',lev,' ; iv= ',iv,' ; secchap= ',secchap(lev)
               write(*,'(8x,3(a,es11.3))') 'babso: ',babso(1+lev),
     $          ' ; babsd: ',babsd(1+lev),' ; bscar: ',bscar(1+lev)
               write(*,'(8x,3(a,es11.3))') 'babso3: ',babso3(1+lev),
     $          ' ; delsc: ',delsc(1+lev)
               write(*,'(8x,3(a,es11.3))') 'dtauc2: ',dtauc2(1+lev),
     $          ' ; fbeamr: ',fbeamr(iv),' ; uavg: ',uavg(iv,lev)          
            end if
            uavg(iv,:levlo) = uavg(iv,:levlo) * fbeamr(iv)* solid_anglei
                    !  drat will be multiplied by fourpi in PHORAT
         end if
      end do             ! of wavelength loop

      if( levlo < mxcly ) uavg(:,levlo+1:mxcly) = 0.

!---------------------------------------------------------------------
!    ... Integrate over wavelength to obtain photodissociation
!        and heating rates.
!---------------------------------------------------------------------
      call RATES_PHO( levlo, xsect, qy_o3, qy_o3_o1d, exabscl, 
     $                uavg, aero, drat, wrat )

!---------------------------------------------------------------------
!    ... J(no) parameterization, file minsch93.pho.f
!---------------------------------------------------------------------
      call MINSCH93_PHO( levlo, zs, colo2, uavg, secchap, dens, jjno )
      drat(jno,:) = jjno(:)
      
      if( levlo < mxcly ) then
         drat(:,levlo+1:mxcly) = 0.
         wrat(:,levlo+1:mxcly) = 0.
      end if
      
      if( write_diags ) then
         lev = niz - zdiag
         iv = 61
         write(*,'(8x,3(a,es11.3))') 'uavg: ',uavg(iv,lev),
     $    ' ; drat(O3->O1D): ',drat(18,lev),' ; wrat(O3): ',wrat(2,lev)
         write(*,*)
      end if

      end subroutine PHO                          
