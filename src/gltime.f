! subversion Id for THIS file : $Id: gltime.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/gltime.f $
!-----------------------------------------------------------------------

      subroutine GLTIME( start, cpusecs )

      use SPECIES_DIMS, only : nbcon
      use SIM_CONTROLS, only : ncpus
      use TRACNM
      use CHEMLIFE
      use ALLCO, only : phir
      use ELAPSED_TIMES

      implicit none
!------------------------------------------------------------------
!	... Dummy args
!------------------------------------------------------------------
      real, intent(in), dimension(11) :: cpusecs
      real, intent(in) :: start

!------------------------------------------------------------------
!       ... Local variables
!------------------------------------------------------------------
      integer ::  m
      real    ::  c1, c2, glt, glt1, glt2, c, t,s, sumlat(10)

!-----------------------------------------------------------------------
!	... Function declarations
!-----------------------------------------------------------------------
      real, external    :: SECOND

      goto 10
!----------------------------------------------------------------------
!     	... Calculate the global lifetimes of the long-lived species
!	    at each diurnal timestep. Diurnal averaging of the global
!	    lifetime is done later.
!----------------------------------------------------------------------
      do m = 1, nbcon
         c1 = DOT_PRODUCT( dens_acc(:,m),COS(phir(:)) )
         c2 = DOT_PRODUCT( loss_acc(:,m),COS(phir(:)) )
         if( c2 /= 0. ) then
            glt = c1/c2
         else
            glt = 0.
         end if
         glt1 = glt / (86400.*365.)
         glt2 = glt1 * 12.
         write(6,50) solsym(m), glt, glt1, glt2
      end do

  50  format (' Global lifetime of  ',a8,4x,
     $  1pe12.4 ,' sec',e12.4,' yr',e12.4,' mo')

!------------------------------------------------------------------
!       ... Printout timing subroutine breakdown
!------------------------------------------------------------------
 10   t = SECOND() - start
      c = 100. / t
      write(6,*)
      write(*,*) 'TIMING SUMMARY FOR THE MAIN'
      write(*,*) '---------------------------'
      write(6,30)'CPUt','%CPUt'
      write(6,20)'        Init :',cpusecs(1),' sec ; ',c*cpusecs(1),' %'
      write(6,20)'MSIS-ATMCOND :',cpusecs(2),' sec ; ',c*cpusecs(2),' %'
      write(6,20)'        heat :',cpusecs(4),' sec ; ',c*cpusecs(4),' %'
      write(6,20)'       Winds :',cpusecs(5),' sec ; ',c*cpusecs(5),' %'
      write(6,20)'        Temp :',cpusecs(6),' sec ; ',c*cpusecs(6),' %'
      write(6,20)'  BOUNDY-GCC :',cpusecs(7),' sec ; ',c*cpusecs(7),' %'
      write(6,20)'        CHEM :',cpusecs(8),' sec ; ',c*cpusecs(8),' %'
      write(6,20)'other & arch :',cpusecs(9),' sec ; ',c*cpusecs(9),' %'
      write(6,*)'--------------------------------------------------'
      write(6,20) '  TOTAL :',t,' sec ; ',c*SUM(cpusecs),' %'
      write(6,*)
      write(6,*)
      write(*,*) 'TIMING FOR CHEM'
      write(*,*) '---------------'
      write(6,30)'CPUt','%CPUt','REALt','%OMPeffic'
      t = cpusecs1(10)
      s = SUM( cpusecs1(1:9) )
      write(6,20) '    Begin :',cpusecs1(1),' sec ; ',c*cpusecs1(1),' %'
      write(6,20)'TABLES_PHO :',cpusecs1(2),' sec ; ',c*cpusecs1(2),' %'
      write(6,20)'      *RXT :',cpusecs1(3),' sec ; ',c*cpusecs1(3),' %'
      write(6,20) 'ADVECT_CHEM (omp):',cpusecs1(4),' sec ; '
     $    ,c*cpusecs1(4),' % ; ',realsecs1(4),' sec ; '
     $    ,100.*cpusecs1(4)/realsecs1(4)/REAL(ncpus),' %'
      write(6,20) '     CHEMDR (omp):',cpusecs1(5),' sec ; '
     $    ,c*cpusecs1(5),' % ; ',realsecs1(5),' sec ; '
     $    ,100.*cpusecs1(5)/realsecs1(5)/REAL(ncpus),' %'
      write(6,20) '    HDIFF :',cpusecs1(6),' sec ; ',c*cpusecs1(6),' %'
      write(6,*) '--------------------------------------------------'
      write(6,20) '  SUM :',s,' sec ; ',c*s,' %'
      write(6,20) ' Check: true sum :',t,' sec ; ',c*cpusecs(8),' %'
      write(6,*)
      write(6,*)
      write(*,*) 'TIMING FOR CHEMDR'
      write(*,*) '-----------------'
      write(6,30)'CPUt','%CPUt'
      do m = 1, 10
         sumlat(m) = SUM( realsecs2(m,:) )
      end do
      t = sumlat(10)
      s = SUM( sumlat(1:9) )
      write(6,20)'Init+WASHOUT :',sumlat(1),' sec ; ',c*sumlat(1),' %'
      write(6,20)'    PHO_CHEM :',sumlat(2),' sec ; ',c*sumlat(2),' %'
      write(6,20)'special prods:',sumlat(3),' sec ; ',c*sumlat(3),' %'
      write(6,20)'     IMP_SLV :',sumlat(4),' sec ; ',c*sumlat(4),' %'
      write(6,20)'  DIAGS_CHEM :',sumlat(5),' sec ; ',c*sumlat(5),' %'
      write(6,20)'       VDIFF :',sumlat(7),' sec ; ',c*sumlat(7),' %'
      write(6,20)'  HEAT_RATES :',sumlat(8),' sec ; ',c*sumlat(8),' %'
      write(6,20)'         end :',sumlat(9),' sec ; ',c*sumlat(9),' %'
      write(6,*) '----------------------------------------------------'
      write(6,20) '  SUM :',s,' sec ; ',c*s,' %'
      write(6,20) ' Check: true sum :',t,' sec ; ',c*cpusecs1(5),' %'
      write(6,*)

  20  format(a20,es9.2,a,f5.1,a,es9.2,a,f5.1,a)
  30  format(   23x,a6,    5x,a,   6x,a,  3x,a )
      
!      print *,' Below time for subroutine IMP_SLV : '
!      print *,'   LU_FAC              ',cpusecs2(2) 
!      print *,'   LU_SLV              ',cpusecs2(3) 
!      print *,'   LINMAT              ',cpusecs2(4) 
!      print *,'   NLNMAT              ',cpusecs2(5) 
!      print *,'   INDPRD & PROD_LOSS  ',cpusecs2(8) 

      end subroutine GLTIME
