! subversion Id for THIS file : $Id: atmcond.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/atmcond.f $
!-----------------------------------------------------------------------

      subroutine ATMCOND( step, date )
!-----------------------------------------------------------------------
!         ... Calculate the new atmospheric density by hydrostatic
!        adjustment, the geometric altitude grid and the corresponding
!        atmospheric scale height
!-----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use PHYS_CST, only : R, g0, R0, Nav, k
      use ALLCO, only : zkm
      use SIM_CONTROLS, only : mainsw, model_type, jump_chem
      use DIAG_CONTROLS, only : diags, ldiag, zdiag
      use VEN8, only : p           ! cst pressure grid (Pa)
      use VEN1                     ! input : t2d
      use VEN10                    ! output :: xtz, xtzz, mu
      use SPECIES, only : qn2da
      use SPC_NAMES, only : vid_o3p, vid_o2, vid_h2o
      use BACKATM
      use CONC                                          ! output : hm2d
      use ZGRID, only : Hair, zgeo                 ! output : Hair, zgeo
      use TIME_CONTROLS, only : TIMING
      use TRANSFORM, only : VDERIV

      implicit none

!----------------------------------------------------------------------
!    ... Dummy arguments (for error reporting)
!----------------------------------------------------------------------
      integer, intent(in) :: step
      type( TIMING ), intent(in) :: date

!----------------------------------------------------------------------
!    ... Local variables
!----------------------------------------------------------------------
      integer :: l, iz
      real, dimension(niz), save :: g, p_k
      real, dimension(lmax,niz) :: T, O_mr, O2_mr, t69
      logical, save :: entered = .false.
      
      if( .not. entered ) then
         p_k(:) = 1.e-6 * p(:) / k              ! K molec/cm3
         g(:) = g0 * ( R0 / (R0+zkm(:)) )**2    ! m2/s, use zkm io unknown zgeo
         entered = .true.
      end if
            
      if( mainsw(2) < 2 .and. .not. jump_chem ) then     ! default case
         O2_mr = qn2da(:,:,vid_o2)
         O_mr = qn2da(:,:,vid_o3p)
         T = t2d
       else 
         O2_mr = Xmsis(:,:,O2)
         O_mr = Xmsis(:,:,O1)
         T = Tmsis
      end if
      if( mainsw(2) >= 1 ) hm2d(:,:) = Dmsis(:,:,tot)
      
!-----------------------------------------------------------------------
!    ...  mass of air (a.m.u./molec. = g/mole) wmole
!-----------------------------------------------------------------------
      wmole(:,:) = 28.*Xmsis(:,:,N2) + 40.*Xmsis(:,:,Ar)
     $           + 32.*O2_mr + 16.*O_mr ! + 18.*qn2da(:,:,vid_h2o)

!-----------------------------------------------------------------------
!    ... Specific heat at constant pressure (J/g/K)
!   Cp = k*Nav*( [n2]*(1+5/2)/28 + [o2]*(1+5/2)/32 + [o]*(1+3/2)/16 ) + ...
!-----------------------------------------------------------------------
      Cp(:,:) = R *
     $          ( 2.5*O_mr/16. + 3.5*O2_mr/32. + 3.5*Xmsis(:,:,N2)/28. )
      
!-----------------------------------------------------------------------
!    ... Calc atm scale height Hair, total nb density hm2d, geometric
!           altitude grid zgeo
!-----------------------------------------------------------------------
      do l = 1, lmax
         Hair(l,:) = R * T(l,:) / ( wmole(l,:) * g(:) )           ! km
         if( mainsw(2) == 0 ) hm2d(l,:) = p_k(:) / T(l,:)          ! molec/cm3
         zgeo(l,1) = 0.
         do iz = 2, niz
            zgeo(l,iz) = zgeo(l,iz-1)                  ! trapezoidal integration
     $                + .5 * (Hair(l,iz)+Hair(l,iz-1)) / 7.
     $                  *(zkm(iz) - zkm(iz-1))                    ! km
         end do
      end do

!-------------------------------------------------------------------
!    ... Calculate mean mass density of air (g/m3), 
!           *should* be same as 1e3 * ro_s (module CONC)
!-------------------------------------------------------------------
      ro(:,:) = 1.e6 * hm2d(:,:) * wmole(:,:) / Nav 

!-------------------------------------------------------------------
!    ... Calculate thermal conductivity, see Banks/Kockarts:1973
!           eqs (14.45) & (14.48). 1.e-5 converts from erg/cm/K/s to J/m/K/s
!           Then, Convert to thermal diffusivity (m2/s) and get dxtz/dz
!-------------------------------------------------------------------
      t69 = T**0.69
      xtz = 1.e-5 * ( O_mr * 75.9 + (Xmsis(:,:,N2)+O2_mr)*56. ) * t69 ! J/m/K/s
      xtz = xtz / ( ro * Cp )                                         ! m2/s
      call VDERIV( xtz, basedz = xtzz )

!-----------------------------------------------------------------------
!         ... Forcing by molecular viscosity. First, calculate the dynamic
!           coeff of viscosity (g/m/s), see Banks/Kockarts:1973 vol2 p10
!           Then, Convert to kinematic coeff (m2/s). To be used in DYNF_TOTAL
!-----------------------------------------------------------------------
      mu(:,:) = 1.e-4 * t69 * ( 3.43*Xmsis(:,:,N2) 
     $                         + 4.03*O2_mr + 3.9*O_mr )            ! g/m/s
      mu(:,:) = mu(:,:) / ro(:,:)                                   ! m2/s

!-------------------------------------------------------------------
!    ... Everything below is error reporting and diagnostics
!-------------------------------------------------------------------
      if( diags ) then
         write(*,'(2(a,i3),2(a,f7.3),a,es9.3)') 'ATMCOND @ (',ldiag,',',
     $      zdiag,'): T= ',T(ldiag,zdiag),' ; O2= ',O2_mr(ldiag,zdiag),
     $                      ' ; O= ',O_mr(ldiag,zdiag)
         write(*,'(4(a,f7.3))') '    wmole= ',wmole(ldiag,zdiag),
     $         ' ; Cp= ',Cp(ldiag,zdiag), ' ; Hair= ',Hair(ldiag,zdiag),
     $          ' ; zgeo= ',zgeo(ldiag,zdiag)
         write(*,'(4(a,es9.3))') '   hm2d= ',hm2d(ldiag,zdiag),
     $      ' ; ro= ',ro(ldiag,zdiag),' ; xtz= ',xtz(ldiag,zdiag),
     $        ' ; mu= ',mu(ldiag,zdiag)
      end if
      
      if( model_type == 'two_d' ) then
      call CHECK_VALS( wmole, 20., 30., 'wmole', step, date, 'ATMCOND' )
      call CHECK_VALS( Hair,   3., 15.,  'Hair', step, date, 'ATMCOND' )
      call CHECK_VALS( hm2d,1.e11,1.e21, 'hm2d', step, date, 'ATMCOND' )
      end if
      
      end subroutine ATMCOND
