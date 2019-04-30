! subversion Id for THIS file : $Id: chem.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/chem.f $
!-----------------------------------------------------------------------

      subroutine CHEM( rxno, &
                       ipl, &
                       irc, &
                       prdcnt, &
                       ipcep, &
                       ipcel, &
                       fixcnt, &
                       prdmap, &
                       fixmap, &
                       pcep, &
                       pcel, &
                       rxmap, &
                       hetcnt, &
                       hetmap, &
                       usrcnt, &
                       usrmap, &
                       rates, &
                       rattab, &
                       rateno, &
                       phtcnt, &
                       pcoeff_cnt, &
                       pcoeff_ind, &
                       pcoeff, &
		       sym_rates, &
		       rxt_alias, &
		       use_t0, &
		       rate_t0 )
!-----------------------------------------------------------------------
!	... Scan chemical reactions and produce base chemistry maps
!-----------------------------------------------------------------------

      use IO
      use LIMITS
      use SYMBOLS

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      integer, intent(out) :: rxno, &
                              ipl, &
                              prdcnt, &
                              hetcnt, &
                              usrcnt, &
                              rateno, &
                              phtcnt, &
                              pcoeff_cnt
      integer, intent(out) :: irc(*), &
                              fixcnt(*), &
                              ipcel(*), &
                              ipcep(*), &
                              rxmap(rxt_lim,7,2), &
                              pcel(var_lim,6,2), &
                              pcep(var_lim,5,3), &
                              fixmap(var_lim,3,2), &
                              prdmap(var_lim,5), &
                              hetmap(*), &
                              usrmap(*), &
                              rattab(*), &
                              pcoeff_ind(*)
      real, intent(out) ::    rates(2,*), &
                              pcoeff(4,*)
      logical, intent(out) :: use_t0
      character(len=16), intent(out) ::  rate_t0
      character(len=16), intent(out) ::  sym_rates(2,rxt_lim)
      character(len=8), intent(out)  ::  rxt_alias(rxt_lim)

!-----------------------------------------------------------------------
!	... Local variables
!           nsr = number of solution reactants
!           nsp = number of solution products
!           nf  = number of fixed reactants
!           npr = number of pce reactants
!           npp = number of pce products
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      integer, parameter :: photolysis = 1, gas_phase = 2
      integer, parameter :: heterogeneous = 3, extraneous = 4

      character(len=16) :: param, rxparms(5), sym_rate(2)
      character(len=16) :: keywords(4) = (/ 'PHOTOLYSIS      ', &
	                                    'REACTIONS       ', &
	                                    'HETEROGENEOUS   ', &
	                                    'EXTFORCING      ' /)
      integer  ::  rxtlim, hetlim, nchar, k, nr, np, nsr, nsp, nf, &
                   npr, npp, ic, kc, i, npl, l, j, m, il, ipp, im, &
                   photo, rxtn
      integer  ::  npce = 0
      integer  ::  rxttab(5,4)
      integer  ::  parsw(4)
      integer, allocatable :: toklen(:)
      integer  ::  tokcnt
      character(len=8) ::  loc_rxt_alias
      character(len=8) ::  rxtsym(3), prdsym(4)
      character(len=8), allocatable ::  tokens(:)
      character(len=1) ::  char
      real     ::  temp_fac
      real     ::  rate(2), pcoeffs(4)
      logical  ::  coeff_flg
      logical  ::  found
       
      rxtlim = rxt_lim
      hetlim = rxt_lim
      photo  = 0
      usrcnt = 0
      rxtn   = 1
      rxno   = 0
      ipl    = 0
      rateno = 0
      parsw  = 0

keyword_loop : &
      do
         call CARDIN( lin, buff, nchar )
         buffh = buff
         call UPCASE( buffh )
         if( buffh == 'ENDPAR' ) then
	    exit
         end if
         found = .false.
         do i = 1,4
	    if( INDEX( buffh, keywords(i)(:LEN_TRIM(keywords(i)) )) /= 0 ) then
	       if( parsw(i) /= 0 ) then
                  call ERRMES ( ' # Keyword already used@', &
                                lout, &
                                keywords(i), &
                                LEN_TRIM(keywords(i)), &
                                buff )
	       else if( i == 1 .and. parsw(2) /= 0 ) then
                  call ERRMES ( 'Must specify Photolysis before Reactions@', &
                                lout, char, 1, buff )
	       end if
	       parsw(i) = 1
	       found = .true.
	       exit
	    end if
         end do
         if( .not. found ) then
            call ERRMES( 'CHEM: # is not a keyword@', &
                          lout, &
                          buff, &
                          LEN_TRIM(buff), &
                          buff )
	 end if
         select case( i )
	    case( photolysis )
!=======================================================================
!     	... The photolysis chemistry processing
!=======================================================================
               write(lout,240)
	       do
                  call CARDIN( lin, buff, nchar )
                  buffh = buff
                  call UPCASE( buffh )
                  if( buffh == 'ENDENT' ) then
                     phtcnt = rxno
                     cycle keyword_loop
                  end if
                  rxtsym(1:3) = ' '
                  prdsym(1:4) = ' '
!-----------------------------------------------------------------------
!        ... Reaction parsing routine
!-----------------------------------------------------------------------
                  call RXTPRS( buff,     nchar,    nr,       np,       rxtsym, &
                               prdsym,   rate,     pcoeffs,  coeff_flg, &
                               rxparms,  sym_rate, loc_rxt_alias )
!-----------------------------------------------------------------------
!        ... Check for reaction string errors from parsing routine
!-----------------------------------------------------------------------
                  if( nr < 0 ) then
                     call ERRMES ( 'gross syntax errors in reaction string@', lout, char, 1, buff )
                  end if

!-----------------------------------------------------------------------
!        ... Reaction mapping routine
!-----------------------------------------------------------------------
                  call MAPPER( nq,       nfs,      npce,     nr,       np, &
                               rxtsym,   prdsym, &
                               nsr,      nsp,      nf,       npr,      npp, &
                               rxttab,   photo,    lout,     buff,     coeff_flg, &
                               pcoeffs )

!-----------------------------------------------------------------------
!        ... Check for logic errors in reaction
!-----------------------------------------------------------------------
                  if( (nf + nsr + npr) == 0 ) then
                     call ERRMES ( 'Photo-reaction has no reactants@', lout, char, 1, buff )
                  else if( (nf+nsr+npr) >= 2 ) then
                     call ERRMES ( 'Photo-reaction has two or more reactants@', lout, char, 1, buff )
                  end if
                  if( nf == 1 .and. nsp == 0 .and. npp == 0 ) then
                     cycle
                  end if
                  rxno = rxno + 1
                  if( rxno > rxtlim ) then
                     call ERRMES( ' Reaction count exceeds limit@', lout, buff, 1, buff )
                  end if
                  if( nf == 1 ) then
!-----------------------------------------------------------------------
!        ... Photolysis of an invariant species; check for products
!-----------------------------------------------------------------------
                     if( nsp /= 0 ) then
!-----------------------------------------------------------------------
!        ... Solution species production from fixed species photolysis
!-----------------------------------------------------------------------
                       prdcnt = prdcnt + 1
                       prdmap(prdcnt,1) = rxno
                       prdmap(prdcnt,2:nsp+1) = rxttab(3,1:nsp)
                     end if
                     if( npp /= 0 ) then
!-----------------------------------------------------------------------
!        ... Pce species production from fixed species photolysis
!-----------------------------------------------------------------------
                       ipl = ipl + 1
                       do k = 1,npp
                         ipcep(1) = ipcep(1) + 1
                         ic = ipcep(1)
                         pcep(ic,1,1) = rxttab(5,k)
                         pcep(ic,2,1) = rxno
                         pcep(ic,3,1) = ipl
                       end do
                     end if
!-----------------------------------------------------------------------
!        ... Set the fixed reactants map
!-----------------------------------------------------------------------
                     fixcnt(1) = fixcnt(1) + 1
                     ic = fixcnt(1)
                     fixmap(ic,1,1) = -rxno
                     fixmap(ic,2,1) = rxttab(1,1)
                     kc = rxttab(1,1)
                     phtsym(rxno) = fixsym(kc)
                  else if( npr == 1 ) then
!-----------------------------------------------------------------------
!        ... Pce species photolysis; put into pce loss map
!-----------------------------------------------------------------------
                     if( npp /= 0 ) then
!-----------------------------------------------------------------------
!        ... Check to see that a pce is not a product species
!-----------------------------------------------------------------------
                         call ERRMES ( 'pce reactants and products in same reaction@', lout, char, 1, buff )
                     end if
                     ipcel(1) = ipcel(1) + 1
                     ic = ipcel(1)
                     pcel(ic,1,1) = rxttab(4,1)
                     pcel(ic,2,1) = rxno
                     kc = rxttab(4,1)
                     phtsym(rxno) = pcesym(kc)
                     if( nsp /= 0 ) then
                        pcel(ic,3:nsp+2,1) = rxttab(3,1:nsp)
                     end if
!-----------------------------------------------------------------------
!        ... Solution species photolysis
!-----------------------------------------------------------------------
                  else
                     irc(1) = irc(1) + 1
                     ic = irc(1)
                     rxmap(ic,2,1) = rxttab(2,1)
                     rxmap(ic,1,1) = rxno
                     kc = rxttab(2,1)
                     phtsym(rxno) = solsym(kc)
                     if( nsp /= 0 ) then
!---------------------------------------------------------------------
!        ... Solution species production from solution photolysis
!---------------------------------------------------------------------
                        rxmap(ic,3:2+nsp,1) = rxttab(3,1:nsp)
                     end if
                     if( npp /= 0 ) then
!----------------------------------------------------------------------
!        ... Pce species production from solution photolysis
!----------------------------------------------------------------------
                        ipl = ipl + 1
                        do k = 1,npp
                          ipcep(2) = ipcep(2)+1
                          ic = ipcep(2)
                          pcep(ic,1,2) = rxttab(5,k)
                          pcep(ic,2,2) = rxno
                          pcep(ic,3,2) = rxttab(2,1)
                          pcep(ic,4,2) = ipl
                        end do
                     end if
                  end if
!-----------------------------------------------------------------------
!        ... Check for non-unity product coefficients
!-----------------------------------------------------------------------
                  if( coeff_flg ) then
                    pcoeff_cnt = pcoeff_cnt + 1
                    pcoeff_ind(rxno) = pcoeff_cnt
                    pcoeff(1:nsp,pcoeff_cnt) = pcoeffs(1:nsp)
                  end if
		  rxt_alias(rxno) = loc_rxt_alias
!-----------------------------------------------------------------------
!        ... Print the reaction on unit lout
!-----------------------------------------------------------------------
                  call OUTP( rxparms, nr, np, rxtsym, prdsym, &
                             rxno, rate, use_t0, rate_t0, loc_rxt_alias )
	       end do
      
	    case( gas_phase )
!=======================================================================
!    	... The chemical reactions
!=======================================================================
               write(lout,260)
	       k = INDEX( buffh, ':' )
	       if( k /= 0 ) then
		  use_t0 = .true.
		  k = k + 1
		  l = INDEX( buffh, '=' )
		  if( l /= 0 .and. buffh(k:l) == 'T0=' ) then
		     rate_t0 = buff(l+1:nchar)
		     call RELCON( rate_t0, nchar-l, temp_fac, l )
		     if( l /= 0 ) then
			rate_t0 = '298.'
		     end if
		  end if
	       end if
	       do
                  call CARDIN( lin, buff, nchar )
                  buffh = buff
                  call UPCASE( buffh )
                  if( buffh == 'ENDENT' ) then
	             cycle keyword_loop
                  end if

                  rxtsym(1:3) = ' '
                  prdsym(1:4) = ' '
                  call RXTPRS( buff,     nchar,    nr,       np,       rxtsym, &
                               prdsym,   rate,     pcoeffs,  coeff_flg, &
                               rxparms,  sym_rate, loc_rxt_alias )

                  if( nr < 0 ) then
                     call ERRMES ( 'there are no reactants@', lout, char, 1, buff )
                  end if

                  call MAPPER( nq,       nfs,      npce,     nr,       np, &
                               rxtsym,   prdsym, &
                               nsr,      nsp,      nf,       npr,      npp, &
                               rxttab,   rxtn,     lout,     buff,     coeff_flg, &
                               pcoeffs )

                  if( nsr == 3 ) then
                     call ERRMES ( ' three solution species reactants@', lout, char, 1, buff )
                  end if
                  if( npr >= 2 ) then
                     call ERRMES ( ' there are two or more pce reactants@', lout, char, 1, buff )
                  end if
                  if( nsr == 2 .and. npr /= 0 ) then
                        call ERRMES ( ' there are two solution and one pce reactants@', lout, char, 1, buff )
                  end if
                  if( (nf+nsr+npr) /= nr ) then
                     call ERRMES ( ' reaction parsing algorithm error@', lout, char, 1, buff )
                  end if
                  rxno = rxno + 1  
                  if( rxno > rxtlim ) then
                     call ERRMES( ' Reaction count exceeds limit@', lout, buff, 1, buff )
                  end if
                  if( rate(1) /= 0. ) then
                     rateno = rateno + 1
                     rattab(rateno) = rxno
                     rates(:,rateno) = rate(:)
		     sym_rates(:,rateno) = sym_rate(:)
                  end if
		  rxt_alias(rxno) = loc_rxt_alias
                  call OUTP( rxparms, nr, np, rxtsym, prdsym, &
                             rxno-phtcnt, rate, use_t0, rate_t0, loc_rxt_alias )

                  if( nf /= 0 ) then
                     fixcnt(nf) = fixcnt(nf) + 1  
                     ic = fixcnt(nf)
                     fixmap(ic,1,nf) = rxno
                     fixmap(ic,2:1+nf,nf) = rxttab(1,1:nf)
                     if( nf == nr ) then
!-----------------------------------------------------------------------
!        ... Fixed reactants only
!-----------------------------------------------------------------------
                        prdcnt = prdcnt+1
                        fixmap(ic,1,nf) = -rxno
                        prdmap(prdcnt,1) = rxno
                        if( nsp /= 0 ) then
                           prdmap(prdcnt,2:1+nsp) = rxttab(3,1:nsp)
                        end if
                        if( npp /= 0 ) then
                           ipl = ipl + 1
                           do k = 1,npp
                             ipcep(1) = ipcep(1) + 1
                             ic = ipcep(1)
                             pcep(ic,1,1) = rxttab(5,k)
                             pcep(ic,2,1) = rxno
                             pcep(ic,3,1) = ipl
                           end do
                        end if
                        cycle
                     end if
                  end if

                  if( nsr == nr .or. npr == 0 ) then
!-----------------------------------------------------------------------
!        ... Solution reactants only
!-----------------------------------------------------------------------
                     irc(nsr) = irc(nsr) + 1
                     ic = irc(nsr)
                     rxmap(ic,1,nsr) = rxno
                     rxmap(ic,2:1+nsr,nsr) = rxttab(2,1:nsr)
                     if( nsp /= 0 ) then
                        rxmap(ic,nsr+2:nsr+nsp+1,nsr) = rxttab(3,1:nsp)
                     end if
                     if( npp /= 0 ) then
                        ipl = ipl + 1
                        npl = nsr + 1
                        do k = 1,npp
                          ipcep(npl) = ipcep(npl) + 1
                          ic = ipcep(npl)
                          pcep(ic,1,npl) = rxttab(5,k)
                          pcep(ic,2,npl) = rxno
                          pcep(ic,nsr+3,npl) = ipl
                          pcep(ic,3:2+nsr,npl) = rxttab(2,1:nsr)
                        end do
                     end if
                  else
!-----------------------------------------------------------------------
!        Solution,fixed, and pce reactants possible.
!        There is a pce reactant, and either a solution/fixed
!        reactant or both.  If there is a pce product then
!        terminate with a reaction logic error.
!-----------------------------------------------------------------------
                     if( npp /= 0 ) then
                        call ERRMES( ' Reaction has both reactant and product pce species@', lout, char, 1, buff )
                     end if
                     npl = nsr + 1
                     ipcel(npl) = ipcel(npl)+1
                     ic = ipcel(npl)
                     pcel(ic,1,npl) = rxttab(4,1)
                     pcel(ic,2,npl) = rxno
                     if( npl == 2 ) then
			pcel(ic,3,2) = rxttab(2,1)
		     end if
                     if( nsp == 0 ) then
			cycle
		     end if
                     pcel(ic,npl+2:npl+nsp+1,npl) = rxttab(3,1:nsp)
                  end if
!-----------------------------------------------------------------------
!        ... Check for non-unity product coefficients
!-----------------------------------------------------------------------
                  if( coeff_flg ) then
                     pcoeff_cnt = pcoeff_cnt + 1
                     pcoeff_ind(rxno) = pcoeff_cnt
                     pcoeff(1:nsp,pcoeff_cnt) = pcoeffs(1:nsp)
                  end if
               end do

	    case( heterogeneous )
!=======================================================================
!        The heterogeneous loss chemistry list
!=======================================================================
               write(lout,7175)
	       ALLOCATE( toklen(64) )
	       ALLOCATE( tokens(64) )
               do
                  call CARDIN( lin, buff, nchar )
                  buffh = buff
                  call UPCASE( buffh )
                  if( buffh == 'ENDENT' ) then
		     DEALLOCATE( toklen )
		     DEALLOCATE( tokens )
                     cycle keyword_loop
                  end if

		  call GETTOKENS( buff, nchar, ',', 8, tokens, toklen, 64, tokcnt )
		  if( tokcnt <= 0 ) then
		     DEALLOCATE( toklen )
		     DEALLOCATE( tokens )
                     call ERRMES ( ' Error in het list@', lout, param, k, buff )
		  end if
het_tok_loop :    do j = 1,tokcnt
                     do m = 1,spccnt(1)
                        if( tokens(j) == spcsym(m,1) ) then
                           hetcnt = hetcnt + 1
                           if( hetcnt > hetlim ) then
                              call ERRMES( ' Hetero reaction count exceeds limit@', lout, buff, 1, buff )
                           end if
                           hetmap(hetcnt) = m
                           write(lout,7177) hetcnt, spcsym(m,1)
			   cycle het_tok_loop
			end if
                     end do
                     call ERRMES ( ' # is not in Solution list@', lout, tokens(j), toklen(j), buff )
                  end do het_tok_loop
               end do

	    case( extraneous )
!=======================================================================
!    	... The extraneous prod/loss chemistry list
!=======================================================================
               write(lout,8175)
	       ALLOCATE( toklen(64) )
	       ALLOCATE( tokens(64) )
               do
                  call CARDIN( lin, buff, nchar )
                  buffh = buff
                  call UPCASE( buffh )
                  if( buffh == 'ENDENT' ) then
		     DEALLOCATE( toklen )
		     DEALLOCATE( tokens )
                     cycle keyword_loop
                  end if

		  call GETTOKENS( buff, nchar, ',', 8, tokens, toklen, 64, tokcnt )
		  if( tokcnt <= 0 ) then
		     DEALLOCATE( toklen )
		     DEALLOCATE( tokens )
                     call ERRMES ( ' Error in ext prod list@', lout, param, k, buff )
		  end if
ext_tok_loop :    do j = 1,tokcnt
                     do m = 1,spccnt(1)
                        if( tokens(j) == spcsym(m,1) ) then
                           usrcnt = usrcnt + 1
                           if( usrcnt > hetlim ) then
                              call ERRMES( ' Extran reaction count exceeds limit@', lout, buff, 1, buff )
                           end if
                           usrmap(usrcnt) = m
                           write(lout,7177) usrcnt, spcsym(m,1)
			   cycle ext_tok_loop
			end if
                     end do
                     call ERRMES ( ' # is not in Solution list@', lout, tokens(j), toklen(j), buff )
                  end do ext_tok_loop
               end do
         end select
      end do keyword_loop

!-----------------------------------------------------------------------
!        ... Formats
!-----------------------------------------------------------------------
240   format('1',5x,'Photolysis')
260   format('0',5x,'Reactions')
7175  format('0 Heterogeneous loss species')
7177  format(1x,'(',i2,')',3x,a8)
8175  format('0 Extraneous prod/loss species')

      end subroutine CHEM

      subroutine RXTPRS( buff, &
                         nchar, &
                         rxtcnt, &
                         prdcnt, &
                         rxtsym, &
                         prdsym, &
                         rate, &
                         pcoeffs, &
                         coeff_flg, &
                         prdprms, &
                         sym_rate, &
                         loc_rxt_alias )
!-----------------------------------------------------------------------
!        Rxtprs parses the reaction and places the symbols
!        in the reactant and product character arrays rxtsym &
!        prdsym.  The reactants and products are checked for
!        symbol length violations.  The number of reactants
!        and products are limited to three each.  Reaction rate
!        information is checked for numeric validity.  There
!        must be at least one reactant.  No other error checking
!        is performed in this subroutine.
!-----------------------------------------------------------------------

      use IO, only : lout

      implicit none

      integer, parameter ::  rxtlim = 3, prdlim = 4, symlen = 8

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in)  ::     nchar
      integer, intent(out) ::     rxtcnt, prdcnt
      real, intent(out)    ::     rate(*), pcoeffs(4)
      character(len=120), intent(inout) ::  buff
      character(len=8), intent(out)  ::  loc_rxt_alias
      character(len=8), intent(out)  ::  rxtsym(*), prdsym(*)
      character(len=16), intent(out) :: prdprms(4)
      character(len=16), intent(out) :: sym_rate(2)
      logical, intent(out) ::     coeff_flg
      
      
!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer  ::   retcod
      integer  ::   ncharl
      integer  ::   k, j, length, slen(5), ratcnt, start, position
      character(len=16) :: rxparms(5)

      rxtcnt = 0
      prdcnt = 0
      coeff_flg = .false.
      
      rate(:2) = 0.
      sym_rate(:2) = ' '
      pcoeffs   = 1.
      ncharl = nchar
!-----------------------------------------------------------------------
!	... Check for reaction name alias
!-----------------------------------------------------------------------
      k = INDEX( buff(:ncharl), ']' ) - 1
      if( k > 0 ) then
         j = INDEX( buff(:ncharl), '[' ) + 1
         loc_rxt_alias = buff(j:k)
         buff = buff(k+2:ncharl)
	 ncharl = nchar - (k+1)
      else
         loc_rxt_alias = ' '
      end if

      length = INDEX( buff(:ncharl), '=' )
      if( length == 0 ) then
         length = INDEX( buff(:ncharl), '->' )
      end if
      if( length <= 1 ) then
         if( length == 0 ) then
            write(6,102)
         else if( length == 1 ) then
            write(6,100)
         end if
         rxtcnt = -1
         return
      end if
!-----------------------------------------------------------------------
!        ... Parse out the reactants
!-----------------------------------------------------------------------
      call GETTOKENS( buff, &
                      length-1, &
                      '+', &
                      symlen, &
                      rxtsym, &
                      slen, &
                      3, &
                      rxtcnt )
      if( rxtcnt < 0 ) then
         call ERRMES( 'Too many reactants in reaction@', lout, buff, 1, buff )
      else if ( rxtcnt == 0 ) then
         call ERRMES( 'Reactant symbol exceeds symbol length@', lout, buff, 1, buff )
      end if

      if( INDEX( buff(:ncharl),'->' ) /= 0 ) then
	 length = length + 1
      end if
      if( length == ncharl ) then      ! reaction has reactants only
         return
      end if
      start = length + 1               ! char after "=" sign
      position = INDEX( buff(start:ncharl), ';' )
      if( position == 1 ) then         ! no products; reaction rate
         start = start + 1
         go to 10                
      else if( position == 0 ) then    ! products, no rates
         length = ncharl - start + 1
      else
         length = position - 1
      end if
      
!-----------------------------------------------------------------------
!        ... Parse out the products and multipliers
!-----------------------------------------------------------------------
      call GETTOKENS( buff(start:), &
                      length, &
                      '+', &
                      16, &
                      prdprms, &
                      slen, &
                      4, &
                      prdcnt )

      if( prdcnt < 0 ) then
         call ERRMES( 'Too many products in reaction@', lout, buff, 1, buff )
      else if ( prdcnt == 0 ) then
         call ERRMES( 'Product symbol exceeds symbol length@', lout, buff, 1, buff )
      end if
!-----------------------------------------------------------------------
!        ... Check each "product" token for an explicit multiplier
!-----------------------------------------------------------------------
      do k = 1,prdcnt
         j = INDEX( prdprms(k)(:slen(k)), '*' )
         if( j == 0 ) then
            prdsym(k) = prdprms(k)(:symlen)
            cycle
         else if( j == 1 .or. j == slen(k) ) then
            call ERRMES( ' Product & multiplier syntax error@', lout, prdprms(k), 1, prdprms(k) )
         end if
         call RELCON( prdprms(k), j-1, pcoeffs(k), retcod )
         if( retcod /= 0 ) then
	    call ERRMES( 'number format error in product multiplier #@', lout, &
                          prdprms(k), slen(k), buff )
	 end if
         prdsym(k) = prdprms(k)(j+1:)
         if( pcoeffs(k) /= 1.e0 ) then
            coeff_flg = .true.
         end if
      end do
      if( position == 0 ) then
         return
      end if
      start = start + position
      
!-----------------------------------------------------------------------
!        ... Check for reaction rate terms
!-----------------------------------------------------------------------
10    call GETTOKENS( buff(start:), &
                      ncharl-start+1, &
                      ',', &
                      16, &
                      rxparms, &
                      slen, &
                      5, &
                      ratcnt )

      if( ratcnt <= 0 ) then
         call ERRMES( ' Syntax error in reaction rate parameters@', lout, buff, 1, buff )
      end if
      do k = 1,ratcnt
         call RELCON( rxparms(k), slen(k), rate(k), retcod )
         if( retcod /= 0 ) then
	    call ERRMES( 'number format error in reaction rate #@', lout, rxparms(k), slen(k), buff )
	 end if
	 if( ABS(rate(k)) <= 4.*TINY(rate(1)) ) then
	    rate(k) = 0.
	    if( k == 2 ) then
	       sym_rate(2) = ' '
	    end if
	 else 
	    sym_rate(k) = rxparms(k)
	 end if
      end do
     
!-----------------------------------------------------------------------
!        ... Formats
!-----------------------------------------------------------------------
100   format('0 **** Reaction string has no reactants ****')
102   format('0 **** Reaction string has no  =  sign separator ****')

      end subroutine RXTPRS

      subroutine MAPPER( nsol,     nfix,     npce,     rxtcnt, &
                         prdcnt,   rxtsym,   prdsym, &
                         nsr,      nsp, &
                         nf,       npr,      npp,      rxttab, &
                         rxtype,   lout,     buff,     coeff_flg, &
                         pcoeffs )
!-----------------------------------------------------------------------
!        ... Map reactants and products
!-----------------------------------------------------------------------

      use SYMBOLS, only : solsym, pcesym, fixsym

      implicit none

!-----------------------------------------------------------------------
!        ... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in)  :: lout
      integer, intent(in)  :: nsol, nfix, npce, &
                              rxtcnt, prdcnt, rxtype
     
      integer, intent(out) :: nsr, nsp, nf, npr, npp
      integer, intent(out) :: rxttab(5,*)
      real, intent(out)    :: pcoeffs(4)
      
      logical, intent(out) :: coeff_flg

      character(len=120), intent(in) ::  buff
      character(len=8), intent(in)  ::  rxtsym(*), prdsym(*)

!-----------------------------------------------------------------------
!        ... Local variables
!-----------------------------------------------------------------------
      integer  ::  k, l, photo = 0
      logical  ::  local_flag
      
      nf = 0
      nsp = 0
      nsr = 0
      npr = 0
      npp = 0
      
rxtnt_scan : &
      do k = 1,rxtcnt
         if( rxtype == photo ) then
            if( k == 2 ) then
               if( rxtsym(k) /= 'hv' ) then
                  call ERRMES( 'Photo reaction has misplaced or missing "hv" operator@', lout, buff, 1, buff )
               end if
               cycle rxtnt_scan
            else if( k > 2 ) then
               call ERRMES( 'Photo-reaction can have only one reactant@', lout, buff, 1, buff )
            else if( rxtsym(k) == 'hv' ) then
               call ERRMES( 'Photo-reaction has misplaced "hv" operator@', lout, buff, 1, buff )
            end if
         else if( rxtsym(k) == 'hv' ) then
            call ERRMES( ' Photolysis operator "hv" is illegal in a non-photolysis reaction@', lout, buff, 1, buff )
         end if
!-----------------------------------------------------------------------
!    	... Parse out fixed reactants
!-----------------------------------------------------------------------
         if( nfix /= 0 ) then
            do l = 1,nfix
               if( rxtsym(k) == fixsym(l) ) then
                  nf = nf + 1
                  rxttab(1,nf) = l
                  cycle rxtnt_scan
               end if
            end do
         end if
!-----------------------------------------------------------------------
!     	... Parse out solution reactants
!-----------------------------------------------------------------------
         do l = 1,nsol
            if( rxtsym(k) == solsym(l) ) then
               nsr = nsr + 1
               rxttab(2,nsr) = l
               cycle rxtnt_scan
            end if
         end do
!-----------------------------------------------------------------------
!     	... Parse out the pce reactants
!-----------------------------------------------------------------------
         if( npce /= 0 .and. (nsr+nf) /= rxtcnt ) then
            do l = 1,npce
               if( rxtsym(k) == pcesym(l) ) then
                  npr = npr + 1
                  rxttab(4,npr) = l
                  cycle rxtnt_scan
               end if
            end do
         end if
         call ERRMES( ' Reactant "#" is not in sol, pce, or fixed lists@', lout, rxtsym(k), LEN_TRIM(rxtsym(k)), buff )
      end do rxtnt_scan

!-----------------------------------------------------------------------
!     	... Parse out solution products
!-----------------------------------------------------------------------
      local_flag = .false.
      do k = 1,prdcnt
         do l = 1,nsol
            if( prdsym(k) == solsym(l) ) then
               nsp = nsp + 1
               rxttab(3,nsp) = l
               if( coeff_flg ) then
                  pcoeffs(nsp) = pcoeffs(k)
                  if( pcoeffs(k) /= 1.e0 ) then
                     local_flag = .true.
                  end if
               end if
               exit
            end if
         end do
      end do 
      coeff_flg = local_flag
!-----------------------------------------------------------------------
!     	... Parse out the pce products
!-----------------------------------------------------------------------
      do k = 1,prdcnt
         do l = 1,npce
            if( prdsym(k) == pcesym(l) ) then
               npp = npp + 1
               rxttab(5,npp) = l
               exit
            end if
         end do
      end do

      end subroutine MAPPER

      subroutine OUTP( rxparms, &
                       nr, &
                       np, &
                       rxtsym, &
                       prdsym, &
                       irxn, &
                       rate, &
                       use_t0, &
		       rate_t0, &
		       loc_rxt_alias )
!-----------------------------------------------------------------------
!        OUTP OUTPuts a single reaction and rate
!-----------------------------------------------------------------------

      use IO, only : lout

      implicit none

!-----------------------------------------------------------------------
!	... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in) ::  nr, np, irxn
      real, intent(in)    ::  rate(*)
      logical, intent(in) ::  use_t0
      character(len=16), intent(in) :: rate_t0
      character(len=16), intent(in) :: rxparms(*)
      character(len=8), intent(in)  :: loc_rxt_alias
      character(len=8), intent(in)  :: rxtsym(*), prdsym(*)

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer  ::    i, j, length, retcod
      real     ::    coeff
      character(len=120) :: buff

      buff = ' '
      j = 1

!-----------------------------------------------------------------------
!        ... Form the reactants
!-----------------------------------------------------------------------
      do i = 1,nr
         length = LEN_TRIM( rxtsym(i) )
         buff(j:length+j-1) = rxtsym(i)(:length)
         j = length + j + 1
         if( i == nr ) then
            buff(j:) = '->'
            j = j + 3
         else
            buff(j:) = '+'
            j = j + 2
         end if
      end do

!-----------------------------------------------------------------------
!        ... Form the products
!-----------------------------------------------------------------------
      do i = 1,np
         length = INDEX( rxparms(i), '*' )
         if( length /= 0 ) then
            call RELCON( rxparms(i), length-1, coeff, retcod )
	    if( retcod /= 0 ) then
	       call ERRMES( ' # is not a valid real number@', &
                            lout, &
                            rxparms(i), &
                            length-1, &
                            buff )
	    end if
            if( coeff /= 1.e0 ) then
               buff(j:length+j-1) = rxparms(i)(:length)
               j = length + j
            end if
         end if
         length = LEN_TRIM( prdsym(i) )
         buff(j:length+j-1) = prdsym(i)(:length)
         if( i == np ) then
            exit
         else
            j = length + j + 1
            buff(j:j) = '+'
         end if
         j = j + 2
      end do
      if( np == 0 ) then
         buff(j:) = '(No products)'
      end if

!-----------------------------------------------------------------------
!        ... The reaction rate
!-----------------------------------------------------------------------
      if( rate(1) /= 0. ) then
         write(buff(69:),'(1pe8.2)') rate(1)
      end if
      if( rate(2) /= 0. ) then
         buff(LEN_TRIM(buff)+1:) = '*EXP('
	 length = LEN_TRIM( buff ) + 2
         write(buff(length:),'(f8.0)') rate(2)
	 buff(length:) = ADJUSTL( buff(length:) )
	 if( use_t0 ) then
	    buff(LEN_TRIM(buff)+1:) = '*(1./' // rate_t0(:LEN_TRIM(rate_t0)) // ' - 1./t) )'
	 else
            buff(LEN_TRIM(buff)+1:) = '/t )'
	 end if
      end if
      
      write(lout,100) irxn, loc_rxt_alias, buff(:110), irxn

100   format(2x,'(',i4,')',2x,a8,3x,a110,3x,'(',i4,')')

      end subroutine OUTP
