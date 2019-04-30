! subversion Id for THIS file : $Id: twstr_pho.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/twstr_pho.f $
!-----------------------------------------------------------------------

      subroutine TWSTR_PHO( levlo, secchap, albedo, dtauciv, ssalbiv, 
     $                      giv, fdr, fup, fdn )
!------------------------------------------------------------------
! 	... Two-stream equations for multiple layers based on 
!           equations from Toon et al., JGR, vol. 94, #d13, 1989
!------------------------------------------------------------------
      use PHO_PARMS, only : mxcly
      use NUMERICAL, only : TRIDLA

      implicit none

!------------------------------------------------------------------
!	... Parameters
!------------------------------------------------------------------
      real, parameter :: eps = 1.e-3, precis = 1.e-7
      real, parameter :: pifs = 1.
      real, parameter :: fdn0 = 0.

!------------------------------------------------------------------
!	... Dummy args
!------------------------------------------------------------------
! 	secchap = secant or Chapman cosine of solar zenith angle
!	      ( secchap(lev) is then lev-dependent )
! 	albedo = surface albedo
! 	dtauciv =  unscaled optical depth of each layer
! 	ssalbiv  =  unscaled single scattering albedo
! 	giv   =  unscaled asymmetry factor
!------------------------------------------------------------------
      integer, intent(in) :: levlo
      real, intent(in)    :: albedo
      real, dimension(0:mxcly), intent(in)  :: secchap
      real, dimension(mxcly), intent(in)    :: dtauciv, ssalbiv, giv
      real, dimension(0:mxcly), intent(out) :: fup, fdn, fdr

!------------------------------------------------------------------
!	... Local variables
!------------------------------------------------------------------
      integer :: lev, i
      integer :: row, nrows
      real  :: tauc
      real  :: tempg
      real  :: ssfc
      real  :: taug
      real  :: expon, expon0, expon1
      real  :: divisr, up, dn, temp
      real  :: gam1, gam2, gam3, gam4
      real  :: om, tau, f, g
      real, dimension(0:mxcly) :: mu2
      real, dimension(mxcly) :: lam, taun, bgam, e1, e2, e3, e4,
     $                          cup, cdn, cuptn, cdntn, mu1
      real, dimension(2*mxcly) :: a, b, d, y

      mu2(0:levlo) = 1. / secchap(0:levlo)
!------------------------------------------------------------------
! 	... Initial conditions:  pi*solar flux = 1;
!                                diffuse incidence = 0
!------------------------------------------------------------------
!	... Compute coefficients for each layer:
! 	    gam1 - gam4 = 2-stream coefficients,
!           different for different approximations
! 	    expon0 = calculation of e when tau is zero
! 	    expon1 = calculation of e when tau is taun
! 	    cup and cdn = calculation when tau is zero
! 	    cuptn and cdntn = calc. when tau is taun
! 	    divisr = prevents division by zero
!------------------------------------------------------------------
      tauc = 0.
      do i = 1, levlo
         g = giv(i)
         tau = dtauciv(i)
         om = ssalbiv(i)
!------------------------------------------------------------------
! 	... Stay away from 1 by precision.  for g, also stay away from -1
!------------------------------------------------------------------
         tempg = MIN( ABS(g),1. - precis )
         g = SIGN( tempg,g )
         om = MIN( om,1. - precis )
!------------------------------------------------------------------
! 	... Delta-scaling
!------------------------------------------------------------------
         f = g*g
         g = (g - f) / (1. - f)
         taun(i) = (1. - om*f)*tau
         om = (1. - f)*om / (1. - om*f)       
!------------------------------------------------------------------
! 	... The following gamma equations are from pg 16,289, table 1
!------------------------------------------------------------------
! 	... Eddington approximation
!------------------------------------------------------------------
         gam1 = .25 * (7. - om*(4. + 3.*g))
         gam2 = -.25 * (1. - om*(4. - 3.*g))
         gam3 = .25 * (2. - 3.*g*mu2(i))
         gam4 = 1. - gam3
!------------------------------------------------------------------
! 	... Hemispheric mean; quadrature
!  	    save mu1 for use in converting irradiance to actinic flux
!------------------------------------------------------------------
         mu1(i) = (1. - om) / (gam1 - gam2)
!------------------------------------------------------------------
! 	... lambda = pg 16,290 equation 21
!           big gamma = pg 16,290 equation 22
!------------------------------------------------------------------
         lam(i) = SQRT(gam1*gam1 - gam2*gam2)
         bgam(i) = (gam1 - lam(i)) / gam2
         expon = EXP( -lam(i)*taun(i) )
!------------------------------------------------------------------
! 	... e1 - e4 = pg 16,292 equation 44
!------------------------------------------------------------------
         e1(i) = 1. + bgam(i)*expon
         e2(i) = 1. - bgam(i)*expon
         e3(i) = bgam(i) + expon
         e4(i) = bgam(i) - expon

!------------------------------------------------------------------
! 	... The following sets up for the c equations 23, and 24
!           found on page 16,290
! 	    prevent division by zero 
!	    (if lambda=1/mu, shift 1/mu^2 by eps = 1.e-3)
!           which is approx equiv to shifting mu by 0.5*eps* (mu)**3
!------------------------------------------------------------------
         expon0 = EXP( -tauc/mu2(i) )
         expon1 = EXP( -(tauc + taun(i))/mu2(i) )
         divisr = lam(i)*lam(i) - 1./(mu2(i)*mu2(i))
         temp = MAX( eps,ABS(divisr) )
         divisr = SIGN( temp,divisr )

         up = om*pifs*((gam1 - 1./mu2(i))*gam3 + gam4*gam2)/divisr
         dn = om*pifs*((gam1 + 1./mu2(i))*gam4 + gam2*gam3)/divisr
         
!------------------------------------------------------------------
! 	... cup and cdn are when tau is equal to zero
!           cuptn and cdntn are when tau is equal to taun
!------------------------------------------------------------------
         cup(i) = up*expon0
         cdn(i) = dn*expon0
         cuptn(i) = up*expon1
         cdntn(i) = dn*expon1
         tauc = tauc + taun(i)
      end do

!------------------------------------------------------------------
!	... Set up matrix
! 	    ssfc = pg 16,292 equation 37  where pifs is one (unity).
!------------------------------------------------------------------
      ssfc = albedo*mu2(levlo)*EXP( -tauc/mu2(levlo) )*pifs

!------------------------------------------------------------------
! 	... The following are from pg 16,292  equations 39 - 43.
! 	    set up first row of matrix:
!------------------------------------------------------------------
      a(1) = 0.
      b(1) = e1(1)
      d(1) = -e2(1)
      y(1) = fdn0 - cdn(1)

!------------------------------------------------------------------
! 	... Set up odd rows 3 thru (nrows - 1):
!------------------------------------------------------------------
      i = 0
      nrows = 2*levlo		! number of rows in the matrix
      do row = 3,nrows-1,2
         i = i + 1
         a(row) = e2(i)*e3(i) - e4(i)*e1(i)
         b(row) = e1(i)*e1(i + 1) - e3(i)*e3(i + 1)
         d(row) = e3(i)*e4(i + 1) - e1(i)*e2(i + 1)
         y(row) = e3(i)*(cup(i + 1) - cuptn(i))
     $          + e1(i)*(cdntn(i) - cdn(i + 1))
      end do

!------------------------------------------------------------------
! 	... Set up even rows 2 thru (nrows - 2): 
!------------------------------------------------------------------
      i = 0
      do row = 2,nrows-2,2
         i = i + 1
         a(row) = e2(i + 1)*e1(i) - e3(i)*e4(i + 1)
         b(row) = e2(i)*e2(i + 1) - e4(i)*e4(i + 1)
         d(row) = e1(i + 1)*e4(i + 1) - e2(i + 1)*e3(i + 1)
         y(row) = (cup(i + 1) - cuptn(i))*e2(i + 1)
     $          - (cdn(i + 1) - cdntn(i))*e4(i + 1)
      end do

!------------------------------------------------------------------
! 	... Set up last row of matrix at nrows:
!------------------------------------------------------------------
      a(nrows) = e1(levlo) - albedo*e3(levlo)
      b(nrows) = e2(levlo) - albedo*e4(levlo)
      d(nrows) = 0.
      y(nrows) = ssfc - cuptn(levlo) + albedo*cdntn(levlo)

!------------------------------------------------------------------
! 	... Solve tri-diagonal matrix:
!------------------------------------------------------------------
      call TRIDLA( nrows, a(1:nrows), b(1:nrows), d(1:nrows), 
     $             y(1:nrows) )

!------------------------------------------------------------------
! 	... Unfold solution of matrix, compute output fluxes
! 	... The following equations are from pg 16,291  equations 31 & 32
!------------------------------------------------------------------
      fdr(0) = 1.
      fdn(0) = fdn0 / mu1(1)
      fup(0) =  (y(1)*e3(1) - y(2)*e4(1) + cup(1)) / mu1(1)

      row = 1 
      taug = 0.
      do lev = 1, levlo
         taug = taug + taun(lev)
         fdr(lev) = EXP( -taug/mu2(lev) )
         fdn(lev) =  (y(row)*e3(lev) + y(row+1)*e4(lev) + cdntn(lev))
     $               / mu1(lev)
         fup(lev) =  (y(row)*e1(lev) + y(row+1)*e2(lev) + cuptn(lev))
     $               / mu1(lev)
         row = row + 2
      end do

      end subroutine TWSTR_PHO
