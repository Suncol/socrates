# include <version.h>
# include <chem.h>
prd_loss.F
# if !defined(CRAY)
/home/simonc/socrates/preproc/in/cache/imp_slv.F
# else
/home/simonc/socrates/preproc/in/vector/imp_slv.F
# endif
# if CLSINDPRD != 0 || EXTCNT != 0
indprd.F
# endif
# if CLSCNT4 != 0 || CLSCNT5 != 0
# if LINCNT != 0
linmat.F
# endif
# if NLNCNT != 0
nlnmat.F
# endif
lu_fac.F
lu_slv.F
# endif
# if SETRXNCNT != 0
# if defined(RXTNLOOKUP) && TDEPCNT != 0
rxttab.F
# endif
setrxt.F
# endif
# if RXNCNT != 0 && defined(RADJFLAG)
adjrxt.F
# endif
# if PHTCNT != 0
phtadj.F
# endif
# if GRPCNT != 0 || RELCNT != 0
rxtmod.F
# endif
# if defined(HISTTAPE) && defined(GRPHST)
mak_grp_vmr.F
# endif
# if defined(IBM)
/home/simonc/socrates/preproc/in/second.ibm.F
# endif
# if defined(DEC)
/home/simonc/socrates/preproc/in/second.dec.F
# endif
# if defined(HP)
/home/simonc/socrates/preproc/in/second.hp.F
# endif
/home/simonc/socrates/preproc/in/soc_inti.x.F
