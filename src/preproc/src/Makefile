#-----------------------------------------------------------------------
# subversion Id for THIS file : $Id: Makefile 8506 2016-01-28 10:09:58Z simonc $
# $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/preproc/src/Makefile $
#-----------------------------------------------------------------------
# This Makefile is for building socpp, the SOCRATES pre-processor
# This Makefile is intended for GNU make, probably called "gmake"
#------------------------------------------------------------------------

# Set up special characters
null  :=
space := $(null) $(null)

# Check for directory in which to put executable
# This will be one level above the present src directory
ifeq ($(MODEL_EXEDIR),$(null))
MODEL_EXEDIR := ../
endif

# Check for source list override
ifeq ($(SRCLIST),$(null))
SRCLIST := src_list
endif

# Check for name of executable
ifeq ($(EXENAME),$(null))
EXENAME := socpp
endif

# Determine platform 
UNAMES := $(shell uname -s)
UNAMEM := $(shell uname -m)

# Load dependency search path.
dirs := ./

# Set cpp search path, include netcdf
cpp_dirs := $(dirs)
cpp_path := $(foreach dir,$(cpp_dirs),-I$(dir)) # format for command line

# Expand any tildes in directory names. Change spaces to colons.
VPATH    := $(foreach dir,$(cpp_dirs),$(dir)) 
VPATH    := $(subst $(space),:,$(VPATH))               

# Get list of files and determine objects and dependency files
base_srclist_f := $(shell cat ./$(SRCLIST))
OBJS      := $(foreach file,$(base_srclist_f),OBJ/$(file:.f=.o))

#------------------------------------------------------------------------
# Default rules
#------------------------------------------------------------------------

$(MODEL_EXEDIR)/$(EXENAME):	$(OBJS)
	$(FC) -o $@ $(OBJS) $(LDFLAGS)

all: $(MODEL_EXEDIR)/$(EXENAME)

RM := rm

#------------------------------------------------------------------------
# Architecture-specific flags and rules
#------------------------------------------------------------------------
# Alpha
#------------------------------------------------------------------------

ifeq ($(UNAMEM),alpha)

objflag    = -o $@
FC          := f90
CPP         := /usr/bin/cpp
FFLAGS      := -O4 -c -tune ev56 -arch ev56 -free
CFLAGS      := $(cpp_path) -c
LDFLAGS     :=
FPPFLAGS    := -P -C -DDEC


.SUFFIXES:
.SUFFIXES: .f .F .c .o

OBJ/%.o : %.f
	$(FC) $(FFLAGS) $(objflag) $<

clean:
	$(RM) -f *.o $(MODEL_EXEDIR)/$(EXENAME)

endif

#------------------------------------------------------------------------
# Intel Pentium ?
#------------------------------------------------------------------------

ifeq ($(UNAMEM),i686)

objflag    = -o $@
FC          := pgf90
CPP         := /lib/cpp
FFLAGS      := -O2 -c -Mfree
CFLAGS      := $(cpp_path) -c
LDFLAGS     :=
FPPFLAGS    := -P -C -DDEC


.SUFFIXES:
.SUFFIXES: .f .F .c .o

OBJ/%.o : %.f
	$(FC) $(FFLAGS) $(objflag) $<

clean:
	$(RM) -f *.o $(MODEL_EXEDIR)/$(EXENAME)

endif

#------------------------------------------------------------------------
# hercules.oma.be is an HP-UX 9000/879
#------------------------------------------------------------------------

ifeq ($(UNAMEM),9000/879)

objflag    = -o $@
FC          := f90
CPP         := /usr/ccs/lbin/cpp
FFLAGS      := -c +source=free -g
CFLAGS      := $(cpp_path) -c
LDFLAGS     :=
FPPFLAGS    := -P -C


.SUFFIXES:
.SUFFIXES: .f .F .c .o

OBJ/%.o : %.f
	$(FC) $(FFLAGS) $(objflag) $<

clean:
	$(RM) -f *.mod OBJ/*.o $(MODEL_EXEDIR)/$(EXENAME)

endif
