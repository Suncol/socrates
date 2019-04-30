! subversion Id for THIS file : $Id: scadep_pho.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/scadep_pho.f $
!-----------------------------------------------------------------------

      subroutine SCADEP_PHO( babsd, bscad, bscar, cdento, drop, gd, 
     $                       exabscl, iwcsiz, rayli, sigra, 
     $                       wccon, wceffr, zs, iv )
!--------------------------------------------------------------------
!     SUBROUTINE SCAttering optical DEPth
!
!     Calculates optical depth due to scattering. Rayleigh scattering
!     only is included at the present moment
!
!     Input variable cdento: Column density for air
!     other are 'PHO' input variableS
!
!     Output variables:
!
!    babsx(lc)   lc = 1 to mxcly,
!                absorption coefficients drops (e.g. clouds, x = d),
!                beta abs in s3 (dimensionless)   
!    bscax(lc)   lc = 1 to mxcly,
!                scattering coefficient for rayleigh scattering (x = r),    
!                drops (e.g. clouds, x = d), beta sca in s3 (dimensionless)   
!    gd(lc)      lc = 1 to mxcly, asymmetry factor for drop particles
!--------------------------------------------------------------------
      use PHO_PARMS, only : mxwvwc, mxsiz, mxcly, mxwvn
      use PHO_VARS, only : wc_asy, wc_ext, wc_ssa, wcwvn, wvn
      use NUMERICAL, only : LINPOL

      implicit none

!--------------------------------------------------------------------
!	... Dummy args
!--------------------------------------------------------------------
      integer, intent(in)   ::  iv
      integer, intent(in)   ::  iwcsiz(0:mxcly)
      real, intent(in)      ::  cdento(mxcly)
      real, dimension(0:mxcly), intent(in) :: sigra, wccon, wceffr, zs
      real, dimension(mxcly), intent(out)  :: bscar, babsd, bscad, gd
      real, intent(out)     ::  exabscl(0:mxcly,mxwvn)
      logical, intent(in)   ::  drop, rayli

!--------------------------------------------------------------------
!	... Local variables
!--------------------------------------------------------------------
      integer :: lc, ivwc
      real :: deltaz
      real :: sprec
      real :: wvniv
      real :: basy_a1, basy_a2, basy_b1, basy_b2, basy_c1,
     $        basy_c2, basyd, basyd1, basyd2
      real :: bssa_a1, bssa_a2, bssa_b1, bssa_b2, bssa_c1,
     $        bssa_c2, bssad, bssad1, bssad2
      real :: bext_a1, bext_a2, bext_b1, bext_b2, bext_c1,
     $        bext_c2, bextd, bextd1, bextd2, ssalbd

      wvniv = wvn(iv)
      ssalbd = 0.
      sprec = TINY( sprec )

!--------------------------------------------------------------------
!   	... Rayleigh scattering
!--------------------------------------------------------------------
      if( rayli ) then
         bscar(1:mxcly) = cdento(1:mxcly) * sigra(1:mxcly)
      else
         bscar(1:mxcly) = 0.
      end if

!--------------------------------------------------------------------
!    	... Mie scattering from drops (clouds)
!--------------------------------------------------------------------
      exabscl = 0.

      if( drop ) then
         do lc = 1,mxcly
            deltaz = (zs(lc-1) - zs(lc))     ! Must be in kilometers for water clouds
            if( wccon(lc-1) > 0. .and. wccon(lc) > 0. ) then
               do ivwc = 1,mxwvwc-1
                  if( wvniv <= wcwvn(ivwc) .and.
     $                wvniv > wcwvn(ivwc+1)) then
!--------------------------------------------------------------------
!   	... Linearly interpolate coefficients to wvniv extinction coefficient
!--------------------------------------------------------------------
                     call LINPOL( wcwvn(ivwc),
     $                            wc_ext(iwcsiz(lc-1),ivwc,1),
     $                            wcwvn(ivwc+1),
     $                            wc_ext(iwcsiz(lc-1),ivwc+1,1),
     $                            wvniv, bext_a1  )
                     call LINPOL( wcwvn(ivwc),
     $                            wc_ext(iwcsiz(lc),ivwc,1),
     $                            wcwvn(ivwc+1),
     $                            wc_ext(iwcsiz(lc),ivwc+1,1),
     $                            wvniv, bext_a2  )
                     call LINPOL( wcwvn(ivwc),
     $                            wc_ext(iwcsiz(lc-1),ivwc,2),
     $                            wcwvn(ivwc+1),
     $                            wc_ext(iwcsiz(lc-1),ivwc+1,2),
     $                            wvniv, bext_b1  )
                     call LINPOL( wcwvn(ivwc),
     $                            wc_ext(iwcsiz(lc),ivwc,2),
     $                            wcwvn(ivwc+1),
     $                            wc_ext(iwcsiz(lc),ivwc+1,2),
     $                            wvniv, bext_b2  )
                     call LINPOL( wcwvn(ivwc),
     $                            wc_ext(iwcsiz(lc-1),ivwc,3),
     $                            wcwvn(ivwc+1),
     $                            wc_ext(iwcsiz(lc-1),ivwc+1,3),
     $                            wvniv, bext_c1  )
                     call LINPOL( wcwvn(ivwc),
     $                            wc_ext(iwcsiz(lc),ivwc,3),
     $                            wcwvn(ivwc+1),
     $                            wc_ext(iwcsiz(lc),ivwc+1,3),
     $                            wvniv, bext_c2  )
!--------------------------------------------------------------------
!   	... Asymmetry factor
!--------------------------------------------------------------------
                     call LINPOL( wcwvn(ivwc),
     $                            wc_asy(iwcsiz(lc-1),ivwc,1),
     $                            wcwvn(ivwc+1),
     $                            wc_asy(iwcsiz(lc-1),ivwc+1,1),
     $                            wvniv, basy_a1  )
                     call LINPOL( wcwvn(ivwc),
     $                            wc_asy(iwcsiz(lc),ivwc,1),
     $                            wcwvn(ivwc+1),
     $                            wc_asy(iwcsiz(lc),ivwc+1,1),
     $                            wvniv, basy_a2  )
                     call LINPOL( wcwvn(ivwc),
     $                            wc_asy(iwcsiz(lc-1),ivwc,2),
     $                            wcwvn(ivwc+1),
     $                            wc_asy(iwcsiz(lc-1),ivwc+1,2),
     $                            wvniv, basy_b1  )
                     call LINPOL( wcwvn(ivwc),
     $                            wc_asy(iwcsiz(lc),ivwc,2),
     $                            wcwvn(ivwc+1),
     $                            wc_asy(iwcsiz(lc),ivwc+1,2),
     $                            wvniv, basy_b2  )
                     call LINPOL( wcwvn(ivwc),
     $                            wc_asy(iwcsiz(lc-1),ivwc,3),
     $                            wcwvn(ivwc+1),
     $                            wc_asy(iwcsiz(lc-1),ivwc+1,3),
     $                            wvniv, basy_c1  )
                     call LINPOL( wcwvn(ivwc),
     $                            wc_asy(iwcsiz(lc),ivwc,3),
     $                            wcwvn(ivwc+1),
     $                            wc_asy(iwcsiz(lc),ivwc+1,3),
     $                            wvniv, basy_c2  )
!--------------------------------------------------------------------
!    	... Single scattering albedo
!--------------------------------------------------------------------
                     call LINPOL( wcwvn(ivwc),
     $                            wc_ssa(iwcsiz(lc-1),ivwc,1),
     $                            wcwvn(ivwc+1),
     $                            wc_ssa(iwcsiz(lc-1),ivwc+1,1),
     $                            wvniv, bssa_a1  )
                     call LINPOL( wcwvn(ivwc),
     $                            wc_ssa(iwcsiz(lc),ivwc,1),
     $                            wcwvn(ivwc+1),
     $                            wc_ssa(iwcsiz(lc),ivwc+1,1),
     $                            wvniv, bssa_a2  )
                     call LINPOL( wcwvn(ivwc),
     $                            wc_ssa(iwcsiz(lc-1),ivwc,2),
     $                            wcwvn(ivwc+1),
     $                            wc_ssa(iwcsiz(lc-1),ivwc+1,2),
     $                            wvniv, bssa_b1  )
                     call LINPOL( wcwvn(ivwc),
     $                            wc_ssa(iwcsiz(lc),ivwc,2),
     $                            wcwvn(ivwc+1),
     $                            wc_ssa(iwcsiz(lc),ivwc+1,2),
     $                            wvniv, bssa_b2  )
                     call LINPOL( wcwvn(ivwc),
     $                            wc_ssa(iwcsiz(lc-1),ivwc,3),
     $                            wcwvn(ivwc+1),
     $                            wc_ssa(iwcsiz(lc-1),ivwc+1,3),
     $                            wvniv, bssa_c1  )
                     call LINPOL( wcwvn(ivwc),
     $                            wc_ssa(iwcsiz(lc),ivwc,3),
     $                            wcwvn(ivwc+1),
     $                            wc_ssa(iwcsiz(lc),ivwc+1,3),
     $                            wvniv, bssa_c2  )
!--------------------------------------------------------------------
!   	... Calculate asymmetry factor, single scattering
!           albedo and extinction factor.
!--------------------------------------------------------------------
                     basyd1 = basy_a1*(wceffr(lc-1)**basy_b1) + basy_c1
                     basyd2 = basy_a2*(wceffr(lc)**basy_b2) + basy_c2
                     basyd = .5*(basyd1 + basyd2)

                     bssad1 = bssa_a1*(wceffr(lc-1)**bssa_b1) + bssa_c1
                     bssad2 = bssa_a2*(wceffr(lc)**bssa_b2) + bssa_c2
                     bssad = .5*(bssad1 + bssad2)

                     bextd1 = bext_a1*(wceffr(lc-1)**bext_b1) + bext_c1
                     bextd2 = bext_a2*(wceffr(lc)**bext_b2) + bext_c2
                     bextd = .5*deltaz
     $                         *(bextd1*wccon(lc-1) + bextd2*wccon(lc))
                     if( lc /= mxcly ) then
                        exabscl(lc-1,iv) = bextd1*wccon(lc-1)
     $                                        *(1. - bssad1)*1.e-5   !cm-1
                     else
                        exabscl(lc,iv) = bextd2*wccon(lc)
     $                                         *(1. - bssad2)*1.e-5
                     end if
                  end if
               end do
!--------------------------------------------------------------------
!*???? These two lines I added. (This is from T. Huang)
!--------------------------------------------------------------------
	       bextd = MAX( sprec,bextd )
	       ssalbd = MIN( 1.,bssad )
	       gd(lc) = MAX( 0.,basyd )
               bscad(lc) = ssalbd * bextd
               babsd(lc) = ( 1. - ssalbd ) * bextd
            end if
         end do
      else
	 babsd(1:mxcly) = 0.
	 bscad(1:mxcly) = 0.
	 gd(1:mxcly) = 0.
      end if

      end subroutine SCADEP_PHO
