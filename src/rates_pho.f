! subversion Id for THIS file : $Id: rates_pho.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/rates_pho.f $
!-----------------------------------------------------------------------

      subroutine RATES_PHO( levlo, xsect, qy_o3, qy_o3_o1d, exabscl, 
     $                      uavg, aero, drat, wrat )
!---------------------------------------------------------------------
!     Calculates the photodissociation and the heating rates from 
!     layer 0 (top layer) to layer mxcly (surface layer)
!
!     drat(rate,0:mxcly) : photodissociation rate (1/s) of process rate
!     wrat(1,0:mxcly) : O2 heating (K/s)
!     wrat(2,0:mxcly) : O3 heating (K/s)
!     wrat(3,0:mxcly) : aerosol and cloud heating (K/s)
!---------------------------------------------------------------------
      use PHYS_CST, only : pi
      use GRID_DIMS, only : niz   ! necessary because of efsho2, efsho3
      use PHO_PARMS, only : mxcly, mxwvn, phtmax
      use PHO_VARS, only : rm, ro2, rmlya, ro2lya, qy, hv, ivbegin,ivend
      use PHO_AERO, only : exabs
      use AIRGLOW
      use SIM_CONTROLS, only : mlt_sw

      implicit none

!---------------------------------------------------------------------
!	... parameters
!---------------------------------------------------------------------
      integer, parameter :: jo2 = 1, jo2_o1d = 50
     $,                     jo3 = 2, jo3_o1d = 18
     $,                     jch3co3 = 39, jno = 43
      integer, parameter :: lya_iv = 8   ! Lyman-alpha wavelength interval

!---------------------------------------------------------------------
!	... Dummy args
!---------------------------------------------------------------------
      integer, intent(in)  :: levlo
      real, intent(inout)  ::  xsect(phtmax,0:mxcly,mxwvn)
      real, dimension(mxwvn,0:mxcly), intent(in) ::  qy_o3, qy_o3_o1d,
     $                                               uavg
      real, intent(in)     ::  exabscl(0:mxcly,mxwvn)
      logical, intent(in)  ::  aero
      real, intent(out)    ::  drat(phtmax,0:mxcly)        ! photorates
      real, intent(out)    ::  wrat(3,0:mxcly)             ! heating rates

!---------------------------------------------------------------------
!	... Local variables
!---------------------------------------------------------------------
      integer :: k, rate, wb, we, wave, izlo
      real    :: qyo3(mxwvn,0:mxcly)

!---------------------------------------------------------------------
!    	... Calculate photodissociation rates, eq. (18), ISAK
!	    used in the 2d code. Skip JNO (43) calculation since 
!           J(no) is calculated in subroutine MINSCH.
!	    First modify the cross section array xsect to account for :
!           (1) - Schumann Runge band alterations
!           (2) - General wavelength quantum yields
!           (3) - o3 z dependent quantum yields
!---------------------------------------------------------------------
!     	... Lyman-alpha line alterations
!---------------------------------------------------------------------
      if( mlt_sw(5) == 1 ) then
	 do rate = 1,phtmax
	    if( rate == jno .or. rate == jch3co3 ) then
	       cycle
	    end if
            if( ivbegin(rate) <= lya_iv .and. 
     $                               ivend(rate) >= lya_iv ) then
	       if( rate == jo2_o1d ) then
	          xsect(jo2_o1d,0:levlo,lya_iv) = ro2lya(0:levlo)
	       else
	          xsect(rate,0:levlo,lya_iv) = xsect(rate,0:levlo,lya_iv) 
     $                                       * rmlya(0:levlo)
	       end if
	    end if
	 end do
      end if
      
!---------------------------------------------------------------------
!     	... Schumann Runge band alterations
!---------------------------------------------------------------------
      do rate = 1,phtmax
	 if( rate == jno .or. rate == jch3co3 ) then
	    cycle
	 end if
	 wb = MAX( ivbegin(rate),46 )
	 we = MIN( ivend(rate),61 )
         if( wb <= 61 .and. we >= 46 ) then
	    if( rate == jo2 ) then
               do wave = wb,we
	          xsect(jo2,0:levlo,wave) = ro2(0:levlo,wave-45)
               end do
	    else
               do wave = wb,we
	          xsect(rate,0:levlo,wave) = xsect(rate,0:levlo,wave)
     $                                     * rm(0:levlo,wave-45)
               end do
	    end if
	 end if
      end do
!---------------------------------------------------------------------
! 	... General wavelength quantum yields
!---------------------------------------------------------------------
      do rate = 1,phtmax
	 if( rate == jno. or. rate == jch3co3 .or.
     $       rate == jo3 .or. rate == jo3_o1d ) then
	    cycle
	 end if
	 wb = ivbegin(rate)
	 we = ivend(rate)
         do wave = wb,we
	    xsect(rate,0:levlo,wave) = xsect(rate,0:levlo,wave) 
     $                               * qy(rate,wave)
         end do
      end do
!---------------------------------------------------------------------
!	... o3 z dependent quantum yields
!---------------------------------------------------------------------
      do wave = ivbegin(jo3),ivend(jo3)
         xsect(jo3,0:levlo,wave) = xsect(jo3,0:levlo,wave) 
     $                           * qy_o3(wave,0:levlo)
      end do
      do wave = ivbegin(jo3_o1d),ivend(jo3_o1d)
         xsect(jo3_o1d,0:levlo,wave) = xsect(jo3_o1d,0:levlo,wave)
     $                               * qy_o3_o1d(wave,0:levlo)
      end do

!---------------------------------------------------------------------
!	... Now compute the photorates
!---------------------------------------------------------------------
      do rate = 1,phtmax
	 if( rate == jno .or. rate == jch3co3 ) then
	    cycle
	 end if
	 wb = ivbegin(rate)
	 we = ivend(rate)
         do k = 0, levlo
	    drat(rate,k) = DOT_PRODUCT( xsect(rate,k,wb:we),
     $                                  uavg(wb:we,k) ) 
	 end do
      end do
      drat(:,0:levlo) = 4.*pi * drat(:,0:levlo)
      
      
!---------------------------------------------------------------------
!   	... Calculation of residual solar energy for solar heating
!  	... o2 heating
!---------------------------------------------------------------------
      wb = ivbegin(jo2)
      we = ivend(jo2)
      izlo = niz - levlo
      do wave = wb, we
         xsect(jo2,0:levlo,wave) = xsect(jo2,0:levlo,wave)
     $                        * denerg1(wave) * efsho2(niz:izlo:-1,wave)
      end do
      do k = 0,mxcly
	 wrat(1,k) = DOT_PRODUCT( xsect(jo2,k,wb:we), uavg(wb:we,k) )
      end do
      wb = ivbegin(jo2_o1d)
      we = ivend(jo2_o1d)
      do wave = wb, we
         xsect(jo2_o1d,0:levlo,wave) = xsect(jo2_o1d,0:levlo,wave) 
     $                        * denerg1(wave) * efsho2(niz:izlo:-1,wave)
      end do
      do k = 0, levlo
	 wrat(1,k) = DOT_PRODUCT( xsect(jo2_o1d,k,wb:we),
     $                            uavg(wb:we,k) )
     $             + wrat(1,k)
      end do

!---------------------------------------------------------------------
!  	... o3 heating
!---------------------------------------------------------------------
      wb = ivbegin(jo3)
      we = ivend(jo3)
      do wave = wb, we
         xsect(jo3,0:levlo,wave) = xsect(jo3,0:levlo,wave) 
     $                        * denerg2(wave) * efsho3(niz:izlo:-1,wave)
      end do
      do k = 0, levlo
	 wrat(2,k) = DOT_PRODUCT( xsect(jo3,k,wb:we), uavg(wb:we,k) )
      end do
      wb = ivbegin(jo3_o1d)
      we = ivend(jo3_o1d)
      do wave = wb, we
         xsect(jo3_o1d,0:levlo,wave) = xsect(jo3_o1d,0:levlo,wave)
     $                        * denerg2(wave) * efsho3(niz:izlo:-1,wave)
      end do
      do k = 0, levlo
	 wrat(2,k) = DOT_PRODUCT( xsect(jo3_o1d,k,wb:we),
     $                            uavg(wb:we,k) )
     $             + wrat(2,k)
      end do

!-----------------------------------------------------------------------
!  	... Aerosol heating, uses available crs(NO) since param for J(NO)
!-----------------------------------------------------------------------
      if( aero ) then
         do wave = 1,45
            xsect(jno,0:levlo,wave) = hv(wave) *
     $                   ( exabs(0:levlo,wave) + exabscl(0:levlo,wave) )
         end do
         do wave = 46,61
            xsect(jno,0:levlo,wave) = hv(wave) * rm(0:levlo,wave-45) *
     $                   ( exabs(0:levlo,wave) + exabscl(0:levlo,wave) )
         end do
         do wave = 62,mxwvn
            xsect(jno,0:levlo,wave) = hv(wave) *
     $                   ( exabs(0:levlo,wave) + exabscl(0:levlo,wave) )
         end do
         do k = 0, levlo
	    wrat(3,k) = DOT_PRODUCT( xsect(jno,k,:),uavg(:,k) )
         end do
      end if

!---------------------------------------------------------------------
! 	... Solar heating efficiency factor from Mlynczak and Solomon
!           Multiply by 4pi because of mean intensity -> actinic flux
!---------------------------------------------------------------------
      wrat(:,0:levlo) = 4.*pi * wrat(:,0:levlo)

      end subroutine RATES_PHO
