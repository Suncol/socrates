! subversion Id for THIS file : $Id: crscor_pho.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/crscor_pho.f $
!-----------------------------------------------------------------------

      subroutine CRSCOR_PHO( crst, qy_o3, qy_o3_o1d, dens, temper )
!-----------------------------------------------------------------------
!	... Correct absorption cross-sections and quantum yields
!           qy_o3 & qy_o3_1d for T dependence.
!----------------------------------------------------------------------
      use PHO_PARMS
      use PHO_VARS
      use DIAG_CONTROLS, only : debug

      implicit none

!-----------------------------------------------------------------------
!	... Parameters
!-----------------------------------------------------------------------
      integer, parameter :: jo3 = 2, jo3_o1d = 18, jn2o = 4, jco2 = 5
     $,                     jhno3 = 8, jcfc12 = 9, jcfc11 = 10
     $,                     jclono2 = 16, jch2o = 24, jpan = 40
     $,                     jn2o5 = 17, jh2o2 = 23, jcfc10 = 11
     $,                     jchbr3 = 51, jno2 = 7, jclono2_cl = 47
     $,                     jch2o_co = 37
      real, parameter :: kwn = 0.695  ! cm-1 K-1, see JPL2000 p.32
      real, parameter :: an2o(0:4) = (/ 68.21023, -4.071805, 
     $                   4.301146e-02, -1.777846e-04, 2.520672e-07 /)
      real, parameter :: bn2o(0:3) = (/ 123.4014, -2.116255, 
     $                            1.111572e-02, -1.881058e-05 /)
      real, parameter :: ah2o2(0:7) = 
     $         (/ 6.4761E+04, -9.2170972E+02, 4.535649,
     $           -4.4589016e-03, -4.035101e-05, 1.6878206e-07,
     $           -2.652014e-10, 1.5534675e-13 /)
      real, parameter :: bh2o2(0:4) = (/ 6.8123e+03, -5.1351e+01, 
     $                        1.1522e-01, -3.0493e-05, -1.0924e-07 /)
      real, parameter :: nu2 = 820., nu3 = 1190.
      
!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      real, intent(in)    :: dens(0:mxcly,maxden)
      real, intent(in)    :: temper(0:mxcly)
      real, intent(out)   :: crst(phtmax,0:mxcly,mxwvn)
      real, dimension(mxwvn,0:mxcly), intent(out) :: qy_o3, qy_o3_o1d

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer :: lc, iproc
      integer :: n, iv, ii,ivv
      real :: asum, bsum, sigma
      real, dimension(0:mxcly) :: log_y0, log_y1, tdiff, temp, tau, 
     $                            work, qyo3tdep2, qyo3tdep3

!-----------------------------------------------------------------------
! 	... Only the cross-sections recomputed below are T-dependent
!-----------------------------------------------------------------------
      do  iv = 1,mxwvn
        do iproc = 1,phtmax
          crst(iproc,:,iv) = crs(iproc,iv)
        end do
      end do
      
!-----------------------------------------------------------------------
! 	... correct o3 absorption cross-sections
!-----------------------------------------------------------------------
      tdiff = temper - 230.
      do iv = ivbegin(jo3),ivend(jo3)
        crst(jo3,:,iv) = crs(jo3,iv)
     $                 + 1.e-20 * tdiff * (tb_o3(iv) + tc_o3(iv)*tdiff)
        crst(jo3_o1d,:,iv) = crst(jo3,:,iv)
      end do
      
!-----------------------------------------------------------------------
!       ... Calc the only T-dependent quantum yields : qy_o3 & qy_o3_1d
!           New param from JPL2000 update - http://jpldataeval.jpl.nasa.gov
!           Temperature range is 200 - 300K
!--------------------------------------------------------------------
      temp(:) = MAX( 200., temper(:) )
      temp(:) = MIN( 300., temp(:) )
      work(:) = - 1. / ( kwn * temp(:) )
      qyo3tdep2(:) = ( temp / 300. )**4 * EXP( nu2 * work(:) )
      qyo3tdep3(:) = EXP( nu3 * work(:) )

      do lc = 0, mxcly
         where( wvn(:) <= 240. )
            qy_o3_o1d(:,lc) = 1.
          else where( wvn(:) < 300. )
            qy_o3_o1d(:,lc) = 0.95
          else where( wvn(:) <= 330. )
            qy_o3_o1d(:,lc) = qyo3cst(:) + qyo3coeff2(:) * qyo3tdep2(lc)
     $                                   + qyo3coeff3(:) * qyo3tdep3(lc)
          else where( wvn(:) < 345. )
            qy_o3_o1d(:,lc) = 0.06
          else where
            qy_o3_o1d(:,lc) = 0.
         end where
      end do
      
      qy_o3(:,:) = 1. - qy_o3_o1d(:,:)    ! O3 + hv -> O2 + O(3P)
      if( ANY( qy_o3(:,:) < 0. ) .or. ANY( qy_o3(:,:) > 1. ) ) then
         write(*,*) 'CRSCOR warning: error calculating qy_o3'
         if( debug ) stop 'CRSCOR: Fatal error calculating qy_o3'
      end if

!-----------------------------------------------------------------------
! 	... Calculate CO2 absorption cross-sections from values at 300K
!  (in var crs), 200K and 370K (Dumb linear interpolation) from values in 
!  Lewis & Carver, J Quant Spectros Radiat Transfer, vol30, p297, 1983
!-----------------------------------------------------------------------
      do iv = ivbegin(jco2),ivend(jco2)
        where( temper(:) <= 200. )
           crst(jco2,:,iv) = crs200co2(iv)
        end where
        where( temper(:) >= 370. )
           crst(jco2,:,iv) = crs370co2(iv)
        end where
        where( (temper(:) < 300.) .and. (temper(:) > 200.) )
           crst(jco2,:,iv) = crs200co2(iv) 
     $          + 1.e-2 * (temper(:)-200.) *(crs(jco2,iv)-crs200co2(iv))
        end where
        where( (temper(:) < 370.) .and. (temper(:) >= 300.) )
           crst(jco2,:,iv) = crs370co2(iv) 
     $            - (temper(:)-370.) *(crs(jco2,iv)-crs370co2(iv)) / 70.
        end where
      end do
      
!-----------------------------------------------------------------------
! 	... Calculate n2o absorption cross-sections including T dependence
!	Ref : JPL94,table 16, p.125 : Selwyn & al., 1977,GRL,vol.4,p.427
!-----------------------------------------------------------------------
      crst(jn2o,:,:) = 0.
      do iv = ivbegin(jn2o),ivend(jn2o)
        sigma = an2o(4)
        do n = 3,0,-1
           sigma = an2o(n) + wvn(iv)*sigma
        end do
        bsum = bn2o(3)
        do n = 2,0,-1
           bsum = bn2o(n) + wvn(iv)*bsum
        end do
	bsum = EXP( bsum )
        crst(jn2o,:,iv) = EXP( sigma + (temper - 300.) * bsum )
      end do
      
!-----------------------------------------------------------------------
! 	... Correct following absorption cross-sections for T dependence
!           See crs97.dat for references
!-----------------------------------------------------------------------
      temp = temper - 273.15  ! Temperature in Celsius
      
      do iv = ivbegin(jno2),ivend(jno2)
         crst(jno2,:,iv) = crs(jno2,iv) + ta_no2(iv)*temp
      end do
         
      do iv = ivbegin(jhno3),ivend(jhno3)
        crst(jhno3,:,iv) = crs(jhno3,iv)
     $                     * EXP( tb_hno3(iv) * (temper-298.) )
      end do
     
      do iv = ivbegin(jcfc12),ivend(jcfc12)
        crst(jcfc12,:,iv) = crs(jcfc12,iv)
     $                      * EXP( 4.1e-4*(wvn(iv) - 184.9)
     $                                   *(temper - 298.) )
      end do
     
      do iv = ivbegin(jcfc11),ivend(jcfc11)
        crst(jcfc11,:,iv) = crs(jcfc11,iv)
     $                      * EXP( 1.e-4*(wvn(iv) - 184.9)
     $                                  *(temper - 298.) )
      end do
       
      tdiff = temper - 296.
      do iv = ivbegin(jclono2),ivend(jclono2)
        crst(jclono2,:,iv) = crs(jclono2,iv)
     $                       * (1. + tdiff*(ta1_clono2(iv)
     $                             + ta2_clono2(iv)*tdiff))
         crst(jclono2_cl,:,iv) = crst(jclono2,:,iv)                 
      end do

      work = MAX( 223.15, temper )
      work = MIN( temper, 293.15 )
      work = work - 273.15
      do iv = ivbegin(jch2o),ivend(jch2o)
         crst(jch2o,:,iv) = 1.e-20
     $                      * (ta_ch2o(iv) + 1.e-3*tb_ch2o(iv)*work) 
         crst(jch2o_co,:,iv) = crst(jch2o,:,iv)
      end do
         
      work = MAX( 250., temper )
      work = MIN( work, 298. )
      do iv = ivbegin(jpan),ivend(jpan)
         crst(jpan,:,iv) = crs(jpan,iv)
     $                     * EXP( tb_pan(iv)*(work - 298.) )
      end do

!-----------------------------------------------------------------------
!    	... Calculate n2o5 absorption cross-sections
!           including T dependence between 281 and 380 nm
!-----------------------------------------------------------------------
      do iv = ivbegin(jn2o5),ivend(jn2o5)
         crst(jn2o5,:,iv) = crs(jn2o5,iv)
         if( wvn(iv) >= 281. .and. wvn(iv) <= 380. )  then
           crst(jn2o5,:,iv) =
     $        1.e-20 * EXP( 2.735 + ((4728.5 - 17.127*wvn(iv))/temper) )
         end if
      end do
      
!-----------------------------------------------------------------------
!    	... Calculate h2o2 absorption cross-sections
!           including T dependence between 259.7 and 352.5 nm
!           See JPL1994, Table 12, p.119
!-----------------------------------------------------------------------
      work = 1. / (1. + EXP( -1265./temper ))
      do iv = ivbegin(jh2o2),ivend(jh2o2)
        crst(jh2o2,:,iv) = crs(jh2o2,iv)
        if( wvn(iv) >= 259.7 .and. wvn(iv) <= 352.5 ) then
            asum = ah2o2(7)
            do n = 6,0,-1
               asum = ah2o2(n) + wvn(iv)*asum
            end do
            bsum = bh2o2(4)
            do n = 3,0,-1
               bsum = bh2o2(n) + wvn(iv)*bsum
            end do
            crst(jh2o2,:,iv) = (work*asum + (1.-work)*bsum) * 1.e-21
         endif
      end do

!-----------------------------------------------------------------------
!	... Correct temperature dependence for the 9 following cfc species :
!                 jcfc10, jch3ccl3, jch3cl, jcfc113, 
!                 jhcfc22, jha1211, jha1301, jch3br, jcfc114
!-----------------------------------------------------------------------
      call SIGCFC_PHO( crst, temper, crs )
      
!------------------------------------------------------------------
!	...Special linear-logarithmic interpolation for CCl4=CFC10
!          at wl>250nm (iv>lamax(1)=79) using values found at 
!	   iv=76 & iv=79		- S.Chabrillat
!------------------------------------------------------------------
      log_y0(:) = LOG( crst(jcfc10,:,76) )
      log_y1(:) = LOG( crst(jcfc10,:,79) )
      
      do iv = 80,ivend(jcfc10)
        crst(jcfc10,:,iv) = EXP( (log_y1 - log_y0)*(wvn(iv) - wvn(76))
     $                           / (wvn(79) - wvn(76)) + log_y0 )
      end do
!-----------------------------------------------------------------------
!       ... Calculate CHBr3 absorption cross-sections
!           including T dependence between 281 and 380 nm
!-----------------------------------------------------------------------
      work = MAX( 210., temper )
      work = MIN( work, 300. )
      do iv = ivbegin(jchbr3),ivend(jchbr3)
         crst(jchbr3,:,iv) = crs(jchbr3,iv)
         if( wvn(iv) >= 290. .and. wvn(iv) <= 340. )  then
            crst(jchbr3,:,iv) =
     $       EXP(  (0.06183-0.000241*wvn(iv))*(273.-work)
     $                -(2.376+0.14757*wvn(iv))   )
         end if
      end do

!-----------------------------------------------------------------------
!	... All the other cross-sections are temp-independent
!	    as set at beginning of subroutine
!-----------------------------------------------------------------------

      end subroutine CRSCOR_PHO
