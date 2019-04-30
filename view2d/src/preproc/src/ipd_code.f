! subversion Id for THIS file : $Id: ipd_code.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/ipd_code.f $
!-----------------------------------------------------------------------

      subroutine IPD_CODE( spccnt, &
			   clscnt, &
                           clsmap, &
                           cls_rxt_cnt, &
                           extcnt, &
                           cls_rxt_map, &
                           pcoeff_ind, &
                           pcoeff, &
			   permute, &
			   rxt_alias, &
			   solsym, &
			   f90 )
!-----------------------------------------------------------------------
!        ... Write the indepedent production code
!-----------------------------------------------------------------------
     
      use LIMITS

      implicit none

!-----------------------------------------------------------------------
!        ... Dummy args
!-----------------------------------------------------------------------
      integer, intent(in) ::      spccnt
      integer, intent(in) ::      clscnt(5), &
                                  extcnt(5), &
                                  clsmap(var_lim,5,2), &
                                  cls_rxt_map(rxt_lim,7,5), &
                                  cls_rxt_cnt(4,5)
      integer, intent(in) ::      permute(var_lim,5)
      integer, intent(in) ::      pcoeff_ind(*)
      real, intent(in) ::         pcoeff(4,*)
      logical, intent(in) ::      f90                     ! Fortran 90
      character(len=8), intent(in) :: solsym(var_lim)
      character(len=8), intent(in) :: rxt_alias(rxt_lim)
      
!-----------------------------------------------------------------------
!        ... Local variables
!-----------------------------------------------------------------------
      integer  ::   i, k, kl, ku, l, m, n, prdndx
      integer  ::   length, index
      integer  ::   line_pos, cnt
      integer  ::   class
      integer  ::   base
      integer  ::   species, rxno, spcno
      integer  ::   match_cnt
      integer  ::   max_loc(1)
      integer  ::   match_ind(4)
      integer  ::   freq(spccnt)
      integer  ::   permutation(var_lim)
      integer, allocatable :: indexer(:)
      real     ::   rate
      character(len=72) :: line
      character(len=72) :: buff
      character(len= 5) ::  num
      logical  ::  beg_line, flush
      logical  ::  lexist, first, indprds
      logical  ::  first_class = .true.
      logical, allocatable, dimension(:,:) :: match_mask, pmask
      
      INQUIRE( file = 'indprd.F', exist = lexist )
      if( lexist ) then
	 call SYSTEM( 'rm indprd.F' )
      end if
      OPEN( unit = 30, file = 'indprd.F' )

      line = ' '
      write(30,100) line(:LEN_TRIM(line))
      line = '      subroutine INDPRD( class'
      write(30,100) line(:LEN_TRIM(line))
      line = '     $,                  prod'
      write(30,100) line(:LEN_TRIM(line))
      line = '     $,                  y'
      write(30,100) line(:LEN_TRIM(line))
      if( SUM(extcnt) /= 0 ) then
         line = '     $,                  extfrc'
         write(30,100) line(:LEN_TRIM(line))
      end if
      line = '     $,                  rxt )'
      write(30,100) line(:LEN_TRIM(line))
      line = ' '
      write(30,100) line(:LEN_TRIM(line))
      line(7:) = 'use RXT_NAMES'
      write(30,100) line(:LEN_TRIM(line))
      line(7:) = 'use SPC_NAMES'
      write(30,100) line(:LEN_TRIM(line))
      line = ' '
      write(30,100) line(:LEN_TRIM(line))
      line = '      implicit none '
      write(30,100) line(:LEN_TRIM(line))
      line = ' '
      write(30,100) line(:LEN_TRIM(line))
      if( f90 ) then
         line = '!----------------------------------------------'
	 write(30,100) line(:LEN_TRIM(line))
         line = '!       ... Dummy args'
	 write(30,100) line(:LEN_TRIM(line))
         line = '!----------------------------------------------'
	 write(30,100) line(:LEN_TRIM(line))
         line = '      integer, intent(in) ::   class'
         write(30,100) line(:LEN_TRIM(line))
         line = '      real, intent(in) ::      y(PLEV,PCNST)'
         write(30,100) line(:LEN_TRIM(line))
         line = '      real, intent(in) ::      rxt(PLEV,RXNCNT)'
         write(30,100) line(:LEN_TRIM(line))
	 if( SUM(extcnt) /= 0 ) then
            line = '      real, intent(in) ::      extfrc(PLEV,EXTCNT)'
            write(30,100) line(:LEN_TRIM(line))
	 end if
         line = '      real, intent(out) ::     prod(PLEV,*)'
         write(30,100) line(:LEN_TRIM(line))
         line = ' '
         write(30,100) line(:LEN_TRIM(line))
      else
         line = '!----------------------------------------------'
	 write(30,100) line(:LEN_TRIM(line))
         line = '!       ... Input args'
	 write(30,100) line(:LEN_TRIM(line))
         line = '!----------------------------------------------'
	 write(30,100) line(:LEN_TRIM(line))
         line = '      integer   class'
         write(30,100) line(:LEN_TRIM(line))
         line = '      real      y(PLEV,PCNST)'
         write(30,100) line(:LEN_TRIM(line))
         line = '      real      rxt(PLEV,RXNCNT)'
         write(30,100) line(:LEN_TRIM(line))
	 if( SUM(extcnt) /= 0 ) then
            line = '      real      extfrc(PLEV,*)'
            write(30,100) line(:LEN_TRIM(line))
	 end if
         line = ' '
         write(30,100) line(:LEN_TRIM(line))
         line = '!----------------------------------------------'
	 write(30,100) line(:LEN_TRIM(line))
         line = '!       ... Output args'
	 write(30,100) line(:LEN_TRIM(line))
         line = '!----------------------------------------------'
	 write(30,100) line(:LEN_TRIM(line))
         line = '      real      prod(PLEV,*)'
         write(30,100) line(:LEN_TRIM(line))
         line = ' '
         write(30,100) line(:LEN_TRIM(line))
         line = '!----------------------------------------------'
	 write(30,100) line(:LEN_TRIM(line))
         line = '!       ... Local variables'
	 write(30,100) line(:LEN_TRIM(line))
         line = '!----------------------------------------------'
	 write(30,100) line(:LEN_TRIM(line))
         line = '      integer   j'
      end if
      write(30,100) line(:LEN_TRIM(line))
      line = ' '
      write(30,100) line(:LEN_TRIM(line))

Class_loop : &
      do class = 1,5
	 if( clscnt(class) /= 0 ) then
	    if( ALLOCATED( match_mask ) ) then
	       DEALLOCATE( match_mask )
	    end if
	    if( ALLOCATED( pmask ) ) then
	       DEALLOCATE( pmask )
	    end if
	    if( ALLOCATED( indexer ) ) then
	       DEALLOCATE( indexer )
	    end if
            line = '!------------------------------------------------------'
	    write(30,100) line(:LEN_TRIM(line))
            line = '!       ... "Independent" production for'
	    length = LEN_TRIM( line ) + 2
	    if( class == 1 ) then
	       line(length:) = 'Explicit species'
	    else if( class == 2 ) then
	       line(length:) = 'User defined species'
	    else if( class == 3 ) then
	       line(length:) = 'Hov species'
	    else if( class == 4 ) then
	       line(length:) = 'Implicit species'
	    end if
	    write(30,100) line(:LEN_TRIM(line))
            line = '!------------------------------------------------------'
	    write(30,100) line(:LEN_TRIM(line))
	    if( f90 ) then
	       if( first_class ) then
	          line = '      if( class =='
		  first_class = .false.
	       else
	          line = '      else if( class =='
	       end if
	    else
	       if( first_class ) then
	          line = '      if( class .eq.' 
	       else
	          line = '      else if( class .eq.' 
	       end if
	    end if
	    write(line(LEN_TRIM(line)+2:),'(i1)') class
	    line(LEN_TRIM(line)+2:) = ') then'
	    write(30,100) line(:LEN_TRIM(line))
	    if( .not. f90 ) then
	       line = ' ' ; line(10:) = 'do j = 1,PLEV'
	       write(30,100) line(:LEN_TRIM(line))
	    end if
100   format(a)
	    ku = MAX( cls_rxt_cnt(1,class),extcnt(class) )
	    if( ku == 0 ) then
               do species = 1,clscnt(class)
	          write(num,'(i5)') species
	          num =  ADJUSTL( num )
	          line = ' '
	          if( f90 ) then
	             line(10:) = 'prod(:,' // num(:LEN_TRIM(num)) // ') = 0.'
	          else
	             line(13:) = 'prod(j,' // num(:LEN_TRIM(num)) // ') = 0.'
	          end if
	          write(30,100) line(:LEN_TRIM(line))
	       end do
	       if( .not. f90 ) then
		  line = ' ' ; line(10:) = 'end do'
	          write(30,100) line(:LEN_TRIM(line))
	       end if
	       cycle Class_loop
	    end if
	    kl = 1
	    ALLOCATE( match_mask(ku,3) )
	    ALLOCATE( indexer(ku) )
	    ALLOCATE( pmask(ku,4) )
	    if( class <= 3 ) then
	       permutation(:clscnt(class)) = (/ (i,i=1,clscnt(class)) /)
	    else
	       permutation(:clscnt(class)) = permute(:clscnt(class),class)
	    end if

Species_loop : &
            do species = 1,clscnt(class)
	       flush = .false.
	       line = ' '
	       write(num,'(i5)') permutation(species)
	       num =  ADJUSTL( num )
	       if( f90 ) then
	          line(10:) = 'prod(:,' // num(:LEN_TRIM(num)) // ') = '
	       else
	          line(13:) = 'prod(j,' // num(:LEN_TRIM(num)) // ') = '
	       end if
	       ku = cls_rxt_cnt(1,class)
!-----------------------------------------------------------------------
!   	...Write code for "independent" production processes
!-----------------------------------------------------------------------
	       do k = kl,ku
		  pmask(k,:) = cls_rxt_map(k,4:7,class) == species
	          match_mask(k,1) = ANY( pmask(k,:) )
	       end do
!-----------------------------------------------------------------------
!	... No species products
!-----------------------------------------------------------------------
	       if( COUNT( match_mask(kl:ku,1) ) /= 0 ) then
		  indprds = .true.
	          first = .true.
	          do
	             do m = 1,spccnt
		        match_mask(kl:ku,3) = (ABS(cls_rxt_map(kl:ku,2,class)) == m .or. &
		                              ABS(cls_rxt_map(kl:ku,3,class)) == m) .and.&
					      match_mask(kl:ku,1)
		        freq(m) = COUNT( match_mask(kl:ku,3) )
	             end do
		     max_loc = MAXLOC( freq(:spccnt) )
		     cnt = MAXVAL( freq(:spccnt) )
		     if( cnt /= 0 ) then
		        match_mask(kl:ku,3) = (ABS(cls_rxt_map(kl:ku,2,class)) == max_loc(1) .or. &
		                           ABS(cls_rxt_map(kl:ku,3,class)) == max_loc(1)) .and. &
					   match_mask(kl:ku,1)
		        do k = kl,ku
		           if( match_mask(k,3) ) then
			      if( ABS( cls_rxt_map(k,2,class) ) == max_loc(1) ) then
			         indexer(k) = 3
			      else
			         indexer(k) = 2
			      end if
		           end if
		        end do
		     else
		        match_mask(kl:ku,3) = match_mask(kl:ku,1)
			indexer(kl:ku) = 0
			cnt = COUNT( match_mask(kl:ku,3) )
		     end if
		     if( cnt > 1 ) then
		        if( first ) then
		           buff = ' ('
		        else
		           buff = ' + ('
		        end if
		     else if( first ) then
		        buff = ' '
		     else
		        buff = ' +'
		     end if
		     if( first ) then
		        first = .false.
		     end if
		     m = cnt
		     do k = kl,ku
		        if( match_mask(k,3) ) then
		           index = pcoeff_ind(cls_rxt_map(k,1,class))
			   rate = 0.
			   do prdndx = 1,4
			      if( pmask(k,prdndx) ) then
			         rate = rate + pcoeff(prdndx,index)
			      end if
			   end do
!		   rate = SUM( pcoeff(:,index), mask = pmask(k,:) )
		           if( rate /= 0. .and. rate /= 1. ) then
			      call R2C( buff(LEN_TRIM(buff)+1:), rate, 'l' )
			      buff(LEN_TRIM( buff )+1:) = '*'
		           end if
	                   rxno = cls_rxt_map(k,1,class)
	                   write(num,'(i5)') rxno
	                   num =  ADJUSTL( num )
			   if( f90 ) then
		              buff(LEN_TRIM(buff)+1:) = 'rxt(:,rid_' // rxt_alias(rxno)(:LEN_TRIM(rxt_alias(rxno))) // ')'
			   else
		              buff(LEN_TRIM(buff)+1:) = 'rxt(j,' // num(:LEN_TRIM(num)) // ')'
			   end if
			   if( indexer(k) /= 0 .and. ABS( cls_rxt_map(k,indexer(k),class) ) /= 0 ) then
	                      spcno = ABS( cls_rxt_map(k,indexer(k),class) )
	                      write(num,'(i5)') spcno
	                      num =  ADJUSTL( num )
			      if( f90 ) then
			         if( m > 1 ) then
		                    buff(LEN_TRIM(buff)+1:) = '*y(:,vid_' // solsym(spcno)(:LEN_TRIM(solsym(spcno))) // ') +'
			         else if( cnt > 1 ) then
		                    buff(LEN_TRIM(buff)+1:) = '*y(:,vid_' // solsym(spcno)(:LEN_TRIM(solsym(spcno))) // '))'
			         else
		                    buff(LEN_TRIM(buff)+1:) = '*y(:,vid_' // solsym(spcno)(:LEN_TRIM(solsym(spcno))) // ')'
			         end if
			      else if( m > 1 ) then
		                 buff(LEN_TRIM(buff)+1:) = '*y(j,' // num(:LEN_TRIM(num)) // ') +'
			      else if( cnt > 1 ) then
		                 buff(LEN_TRIM(buff)+1:) = '*y(j,' // num(:LEN_TRIM(num)) // '))'
			      else
		                 buff(LEN_TRIM(buff)+1:) = '*y(j,' // num(:LEN_TRIM(num)) // ')'
			      end if
			   else
			      if( f90 ) then
			         if( m > 1 ) then
		                    buff(LEN_TRIM(buff)+1:) = ' +'
			         else if( cnt > 1 ) then
		                    buff(LEN_TRIM(buff)+1:) = ')'
			         end if
			      else if( m > 1 ) then
		                 buff(LEN_TRIM(buff)+1:) = ' +'
			      else if( cnt > 1 ) then
		                 buff(LEN_TRIM(buff)+1:) = ')'
			      end if
			   end if
			   call PUT_IN_LINE()
			   if( m == 1 ) then
			      if( indexer(k) /= 0 ) then
	                         spcno = max_loc(1)
	                         write(num,'(i5)') spcno
	                         num =  ADJUSTL( num )
			         if( f90 ) then
		                    buff = '*y(:,vid_' // solsym(spcno)(:LEN_TRIM(solsym(spcno))) // ')'
			         else
		                    buff = '*y(j,' // num(:LEN_TRIM(num)) // ')'
			         end if
			      end if
			      call PUT_IN_LINE()
			      exit
			   end if
			   m = m - 1
		        end if
		     end do
		     where( match_mask(kl:ku,3) )
		        match_mask(kl:ku,1) = .false.
		     endwhere
		     if( COUNT( match_mask(kl:ku,1) ) == 0 ) then
		        exit
		     end if
	          end do
	       else
	          indprds = .false.
	       end if
!-----------------------------------------------------------------------
!   	... Write code for "extraneous" production processes
!-----------------------------------------------------------------------
	       base = SUM( cls_rxt_cnt(1:4,class) )
	       match_mask(:,2) = .false.
	       match_mask(:extcnt(class),2) = cls_rxt_map(base+1:base+extcnt(class),2,class) == species
	       if( COUNT( match_mask(:extcnt(class),2) ) /= 0 ) then
	          do k = base+1,base+extcnt(class)
		     if( cls_rxt_map(k,2,class) == species ) then
                        write(num,'(i5)') cls_rxt_map(k,1,class)
		        num = ADJUSTL( num )
		        n = LEN_TRIM( num )
		        if( f90 ) then
		           buff = ' + extfrc(:,' // num(:n) // ')'
		        else
		           buff = ' + extfrc(j,' // num(:n) // ')'
		        end if
		        call PUT_IN_LINE()
		     end if
	          end do
	       else if( .not. indprds ) then
		  buff = ' 0.'
		  call PUT_IN_LINE()
	       end if
	       if( line /= ' ' ) then
		  write(30,100) line(:LEN_TRIM(line))
	       end if
	       line = ' '
	       write(30,100) line
	    end do Species_loop
	    if( .not. f90 ) then
	       line = '      end do'
	       write(30,100) line
	    end if
	 end if
      end do Class_loop
      line = '      end if'
      write(30,100) line

      line = ' '
      write(30,100) line
      if( f90 ) then
         line = '      end subroutine INDPRD'
      else
         line = '      end'
      end if
      write(30,100) line

      if( ALLOCATED( match_mask ) ) then
         DEALLOCATE( match_mask )
      end if
      if( ALLOCATED( pmask ) ) then
	 DEALLOCATE( pmask )
      end if
      if( ALLOCATED( indexer ) ) then
	 DEALLOCATE( indexer )
      end if
      CLOSE( 30 )

      CONTAINS

      subroutine PUT_IN_LINE( )
!-----------------------------------------------------------------------
!	... Put line piece in buff into the line
!-----------------------------------------------------------------------

      implicit none

!-----------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------
      integer :: blen, llen

      blen = LEN_TRIM( buff )
      llen = LEN_TRIM( line ) + 1
      if( blen + llen < 70 ) then
	 line(llen:) = buff(:blen)
	 flush = .false.
      else
	 write(30,'(a)') line(:LEN_TRIM(line))
	 flush = .true.
	 line = ' '
	 line(6:6) = '$'
	 line(18:) = buff(:length)
      end if
      buff = ' '

      end subroutine PUT_IN_LINE

      end subroutine IPD_CODE
