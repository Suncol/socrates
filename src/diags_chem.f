! subversion Id for THIS file : $Id: diags_chem.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/diags_chem.f $
!-----------------------------------------------------------------------
      subroutine DIAGS_CHEM( lat,date,noon,midnight,hnmi, ck,loss_rate )
!-----------------------------------------------------------------------
!         ... Calculate various diagnostics for the chemistry, especially
!             the photochemical lifetimes and the diurnally averaged 
!             mixing ratios of the families. These vars will be archived
!             in *.chem2.nc
!                              simonc@oma.be       v6s33, July 2001
!-----------------------------------------------------------------------
      use GRID_DIMS, only : lmax, niz
      use SPECIES_DIMS, only : nbcon
      use CHEM_MODS, only : rxncnt
      use DIAGS_CHEM_VARS          ! output: tau_*, famloss*, lossOx*
      use RXT_NAMES
      use SPC_NAMES
      use RATES_MODS, only : rxt_rates
      use ARCH_WHAT, only : arch
      use TIME_CONTROLS, only : TIMING
      
!-----------------------------------------------------------------------
!    ... Dummy args: ck is ni (molec/cm3); loss_rate is Li*ni (molec/cm3/s)
!-----------------------------------------------------------------------
      integer, intent(in) :: lat              ! latitude & chem tstep indexes
      type( TIMING ), intent(in) :: date
      logical, intent(in) :: noon, midnight
      real, intent(in), dimension(niz) :: hnmi   ! 1/totdens  (cm3/molec)
      real, intent(in), dimension(niz,nbcon) :: ck, loss_rate
      
!-----------------------------------------------------------------------
!    ... Local variables
!-----------------------------------------------------------------------
      integer :: iz
      real, dimension(niz) :: f1, f2, f7, lossOx_A, lossOx_B, lossOx_C, 
     $                        prodO3
      real, dimension(niz,rxncnt) :: k
      real, dimension(niz,nbcon) :: tau
      real, dimension(niz,nsfam) :: fam_dens, famloss

!-----------------------------------------------------------------------
!    ... Execute only if arch(4)%mode='chem2' will happen (see arch_plug.f)
!-----------------------------------------------------------------------
      if( .not. arch(4)%active ) return
      if( .not. ANY( arch(4)%days0do(:) == date%days0+1 ) ) return
      
!-----------------------------------------------------------------------
!    ... Photochemical lifetimes of the species i is  ni/(ni*Li)
!-----------------------------------------------------------------------
      tau(:,:) = 9.e19
      where( loss_rate(:,:) > 1.e-99 )
         tau(:,:) = ck(:,:) / loss_rate(:,:)
      end where
      tau(:,:) = MIN( tau(:,:), 9.e19 )

      k(:,:) = rxt_rates(:,:,lat)
      famloss(:,:) = 0.

!-----------------------------------------------------------------------
!    ... Photochemical lifetimes of the families (molec/cm3/s)
!-----------------------------------------------------------------------
      fam_dens(:,ox) = ck(:,vid_o3p) + ck(:,vid_o3)      ! *NOT* O(1D) !!
      famloss(:,ox) = k(:,rid_jo3_od) *  ck(:,vid_o3)
     $              + k(:,rid_a2) *   ck(:,vid_h) * ck(:,vid_o3)
     $              + k(:,rid_a5) *   ck(:,vid_o3p) * ck(:,vid_oh)
     $              + k(:,rid_a6) *   ck(:,vid_oh) * ck(:,vid_o3)
     $              + k(:,rid_a6b) *  ck(:,vid_ho2) * ck(:,vid_o3)
     $              + k(:,rid_a7) *   ck(:,vid_o3p) * ck(:,vid_ho2)
     $              + k(:,rid_a24) *  ck(:,vid_h2) * ck(:,vid_o3p)
     $              + k(:,rid_a81) *  ck(:,vid_o3p) * ck(:,vid_h2o2)
     $              + k(:,rid_b3) *   ck(:,vid_o3p) * ck(:,vid_no2)
     $              + k(:,rid_b4) *   ck(:,vid_o3) * ck(:,vid_no)
     $              + k(:,rid_b9) *   ck(:,vid_o3) * ck(:,vid_no2)
     $              + k(:,rid_b71) *  ck(:,vid_no3) * ck(:,vid_o3p)
     $              + k(:,rid_b82) *  ck(:,vid_no) * ck(:,vid_o3p)
     $              + k(:,rid_c3) *   ck(:,vid_co) * ck(:,vid_o3p)
     $              + k(:,rid_c9) *   ck(:,vid_ch2o) * ck(:,vid_o3p)
     $              + k(:,rid_d2) *   ck(:,vid_cl) * ck(:,vid_o3)
     $              + k(:,rid_d3) *   ck(:,vid_clo) * ck(:,vid_o3p)
     $              + k(:,rid_d32) *  ck(:,vid_o3p) * ck(:,vid_clono2)
     $              + k(:,rid_d35) *  ck(:,vid_o3p) * ck(:,vid_hocl)
     $              + k(:,rid_d64) *  ck(:,vid_oclo) * ck(:,vid_o3p)
     $              + k(:,rid_d85) *  ck(:,vid_o3p) * ck(:,vid_hcl)
     $              + k(:,rid_e2) *   ck(:,vid_br) * ck(:,vid_o3)
     $              + k(:,rid_e3) *   ck(:,vid_bro) * ck(:,vid_o3p)
     $              + k(:,rid_e81) *  ck(:,vid_o3p) * ck(:,vid_hbr)
     $              + k(:,rid_hk7a) * ck(:,vid_o1d) * ck(:,vid_o3)
     $              + 2. * k(:,rid_hk1) *  ck(:,vid_o3p) * ck(:,vid_o3p)
     $              + 2. * k(:,rid_hk3) *  ck(:,vid_o3p) * ck(:,vid_o3)

      fam_dens(:,nox) = ck(:,vid_n) + ck(:,vid_no) + ck(:,vid_no2)
      famloss(:,nox) = 2. * k(:,rid_b6) * ck(:,vid_n) * ck(:,vid_no)
     $               + k(:,rid_b9) * ck(:,vid_o3) * ck(:,vid_no2)
     $               + k(:,rid_b12) * ck(:,vid_no2) * ck(:,vid_no3)
     $               + k(:,rid_b22) * ck(:,vid_oh) * ck(:,vid_no2) 
     $               + k(:,rid_b23) * ck(:,vid_ho2) * ck(:,vid_no2) 
     $               + k(:,rid_d31) * ck(:,vid_clo) * ck(:,vid_no2) 
     $               + k(:,rid_d36) * ck(:,vid_cl) * ck(:,vid_no2) 
     $               + k(:,rid_e13) * ck(:,vid_bro) * ck(:,vid_no2) 

      where( fam_dens(:,:) > 1.e-99 )
         famloss(:,:) = famloss(:,:) / fam_dens(:,:)     ! 1/s
      end where

!-----------------------------------------------------------------------
!    ... Special diags for Ox losses by Hox: cycle A (a6+a7) vs. B (a5+a1+a7)
!-----------------------------------------------------------------------
      f1(:) = k(:,rid_a1) * ck(:,vid_o2) * ck(:,vid_h) 
     $      / loss_rate(:,vid_h)
      f2(:) = k(:,rid_a2) * ck(:,vid_o3) * ck(:,vid_h) 
     $      / loss_rate(:,vid_h)
      f7(:) = k(:,rid_a7) * ck(:,vid_o3p) * ck(:,vid_ho2) 
     $      / loss_rate(:,vid_ho2)
      do iz = 1, niz
         if( f1(iz)>1.01 .or. f2(iz)>1.01 .or. f7(iz)>1.01 ) then
            write(*,*) 'DIAGS_CHEM: iz= ',iz,' ; f1= ',f1(iz),
     $         ' ; f2= ',f2(iz),' ; f7= ',f2(iz),' ; noon= ',noon
            stop 'DIAGS_CHEM: f1, f2 or f7 > 1.01'
         end if
      end do
      lossOx_A = 2. * k(:,rid_a5) * ck(:,vid_o3p) * ck(:,vid_oh) * f2(:)   ! 1/s
     $         / fam_dens(:,ox)
      lossOx_B = 2. *k(:,rid_a5) *ck(:,vid_o3p)*ck(:,vid_oh)*f1(:)*f7(:)  ! 1/s
     $         / fam_dens(:,ox)
      lossOx_C = 2. * k(:,rid_hk1) * ck(:,vid_o3p) * ck(:,vid_o3p)
     $         / fam_dens(:,ox)
      prodO3 = k(:,rid_hk2) * ck(:,vid_o3p) * ck(:,vid_o2) * hnmi(:)      ! 1/s

!-----------------------------------------------------------------------
!    ... Commit output at noon or midnight
!-----------------------------------------------------------------------
      if( noon ) then
          tau_noon(lat,:,:) = tau(:,:)
          famloss_noon(lat,:,:) = famloss(:,:)
          lossOxAnoon(lat,:) = lossOx_A(:)
          lossOxBnoon(lat,:) = lossOx_B(:)
          lossOxCnoon(lat,:) = lossOx_C(:)
          prodO3noon(lat,:) = prodO3(:)
       else if( midnight ) then
          tau_night(lat,:,:) = tau(:,:)
          famloss_night(lat,:,:) = famloss(:,:)
          lossOxAnight(lat,:) = lossOx_A(:)
          lossOxBnight(lat,:) = lossOx_B(:)
          lossOxCnight(lat,:) = lossOx_C(:)
          prodO3night(lat,:) = prodO3(:)
      end if
     
      end subroutine DIAGS_CHEM

!=======================================================================

      subroutine FAMILIES_DIAGS_CHEM(  )
!-----------------------------------------------------------------------
!	... Calculate diurnal avg of vmr of chem families, for arch only
!-----------------------------------------------------------------------
      use SPECIES, only : qn2da
      use DIAGS_CHEM_VARS              ! output: famvmrda
      use SPC_NAMES

      implicit none

      famvmrda(:,:,ox) = qn2da(:,:,vid_o3p) + qn2da(:,:,vid_o3)

      famvmrda(:,:,clox) = qn2da(:,:,vid_cl) + qn2da(:,:,vid_clo) + 
     &            qn2da(:,:,vid_hocl) + qn2da(:,:,vid_oclo) +
     &            2.*qn2da(:,:,vid_cl2o2) + 2.*qn2da(:,:,vid_cl2)

      famvmrda(:,:,cly) = famvmrda(:,:,clox) 
     $            + qn2da(:,:,vid_hcl) + qn2da(:,:,vid_clono2) 
     &           + qn2da(:,:,vid_clno2) + 3.*qn2da(:,:,vid_ch3ccl3) 
     $           + qn2da(:,:,vid_ch3cl) + qn2da(:,:,vid_brcl)
     $           + 2.*qn2da(:,:,vid_cfc12) + 3.*qn2da(:,:,vid_cfc11)
     $           + 4.*qn2da(:,:,vid_cfc10) + 3.*qn2da(:,:,vid_cfc113)
     $           + qn2da(:,:,vid_hcfc22) + qn2da(:,:,vid_ha1211)
     $           + 2.*qn2da(:,:,vid_cfc114) + qn2da(:,:,vid_cfc115)

      famvmrda(:,:,bry) = qn2da(:,:,vid_br) + qn2da(:,:,vid_bro) + 
     &           qn2da(:,:,vid_hobr) + qn2da(:,:,vid_brono2) + 
     &           qn2da(:,:,vid_brcl) + qn2da(:,:,vid_hbr) 
     $         + qn2da(:,:,vid_ch3br) + 3.*qn2da(:,:,vid_chbr3)
     $         + qn2da(:,:,vid_ha1211) + qn2da(:,:,vid_ha1301)

      famvmrda(:,:,nox) = qn2da(:,:,vid_n) + qn2da(:,:,vid_no)
     $                  + qn2da(:,:,vid_no2)

      famvmrda(:,:,noy) = famvmrda(:,:,nox)
     $          + qn2da(:,:,vid_hno3) + 2.*qn2da(:,:,vid_n2o5)
     &          + qn2da(:,:,vid_ho2no2) + qn2da(:,:,vid_clono2)
     &          + qn2da(:,:,vid_brono2) + qn2da(:,:,vid_no3) 

      famvmrda(:,:,hox) = qn2da(:,:,vid_h) 
     &          + qn2da(:,:,vid_oh) + qn2da(:,:,vid_ho2)
     
      famvmrda(:,:,toth) = famvmrda(:,:,hox)
     $          + 2.*qn2da(:,:,vid_h2o) + 4.*qn2da(:,:,vid_ch4) 
     &          + 2.*qn2da(:,:,vid_h2)
     $          + qn2da(:,:,vid_hcl) + qn2da(:,:,vid_hbr)
     &          + 2.*qn2da(:,:,vid_ch2o)
     &          + 3.*qn2da(:,:,vid_ch3cl) + 3.*qn2da(:,:,vid_ch3br) 
     $          + 2.*qn2da(:,:,vid_h2o2)
     $          + qn2da(:,:,vid_hocl) + qn2da(:,:,vid_hno3)
     $          + qn2da(:,:,vid_ho2no2) + qn2da(:,:,vid_hobr)
     &          + 3.*qn2da(:,:,vid_ch3o2) + 4.*qn2da(:,:,vid_ch3ooh) 

      end subroutine FAMILIES_DIAGS_CHEM
