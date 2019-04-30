# Makefile to compile the SOCRATES model (version v7s00) 
# on the Compaq GS160 Alpha server (aeolos.oma.be). 

SRCS = main.f      archive.f

MODS = main.mod.o  date_utils.mod.o    fit_utils.mod.o   \
       nf_new.mod.o    nf_utils.mod.o
       

FC = f90 -c -r8 -O5 -fast

src_obj = $(MODS) $(SRCS:.f=.o)

annavg : $(src_obj)
	f90 -o ../$@ $(src_obj) -L/home/simonc -lnetcdf_alphaES40

all :	$(src_obj)

.f.o :
	$(FC) $*.f

clean :
	rm -f $(src_obj)

clean_all :
	rm -f *.mod *.o
