
       A MINIMAL DOCUMENTATION TO INSTALL AND RUN SOCRATES 07.18
        ========================================================
                               
                                            Simon Chabrillat ( simonc@oma.be )
                                                              25 January 2016

                                  1. Warnings
                                  ___________

Copyleft 
--------
This software is licensed according to the terms in ../LICENSE.txt 

Intended users
--------------
Scientists and students with a decent knowledge of fortran90 and Unix.

Intended scientific usage
-------------------------
SOCRATES is a 2D (latitude-pressure) model of the Earth's atmosphere, 
extending from the surface to the lower tehrmosphere (~120 km altitude) 
and computing climatological values of the temperature, residual winds, 
and chemical composition.
This version is optimized for global change studies of the upper stratosphere
and the mesosphere, and does *not* include heterogeneous chemistry
(i.e. reactions on the surface of PSC/aerosol particles).

In short, do not expect useful results :
- for altitudes below ~30 km or above ~100km 
- in day-to-day variations 
- in longitude variations (there are none)
- if you study tidal processes
- if you study polar ozone depletion processes

Variations with solar local time are resolved only as far as they're driven by
photochemistry.

Intended non-scientific (e.g. commercial) usage
-----------------------------------------------
None.

Support provided in installing/using SOCRATES
---------------------------------------------
Very limited as this is not my current tool for research. 
No support can be provided to those who do not ask before installing the program.

                                  
                      2. Compiling SOCRATES 07.18 with gfortran 
                      _________________________________________

If it is not done already on your system, you must first install the 
netCDF package for fortran developers:
> sudo apt get install libnetcdff-dev

The source code (in ../src/) can be compiled using ../src/Makefile
which invokes gfortran as compiler and linker.
CAUTION: You could have to change the gfortran compiler rules ("FC=...") 
         to suit your server, and point correctly to the netCDF libraries if they are 
         not in the default library path.

Note that this version of SOCRATES can be run on several cores, in parallel, using OpenMP. 
If you intend to run on one CPU you can simply remove the OpenMP-activating option from the
compiler rules and set ncpus=1 in the input namelist file (../namelists/*.inp).

NOTES: 
- these insatllation instructions work only with socrates versions >= 07.18.01
      i.e. they will *not* work with socrates <= v7s18
- It is recommended to use gfortran version >= 5
  Previous versions may have a bug preventing proper reading of the input namelist
  io_parms (because it contains derived-type structures arch_set(:) )
- Usage of ifort instead of gfortran may actually be a good idea; 
  compare src/Makefile.ifort   with src/Makefile
- The Makefile is not aware of the module dependencies; whenever a module is modified, 
  it is recommended to re-compile the whole source code (i.e. "make clean ; make")

                                
                    3. First Run of SOCRATES and plotting the output
                    ________________________________________________

Your first run should attempt to duplicate the SAMPLE run with a test run. 
The recommended procedure is:
0. Have a look at the small launching script ../socrates.sh, in order to understand what you're doing
1. From the parent directory (where the executable "socrates" and the script "socrates.sh" are), type
   > ./socrates.sh test &    
   The output log-file (test.log) and several output netCDF files (test*.nc) will be written
   in the output directory data_out/test/ 
2. Compare this output with the sammple outout distributed in data_out/SAMPLE
   Check that they are (nearly) identical.

To easily make plots from the netCDF output files, you should purchase 
(or obtain somehow) the scientific visualization software IDL 
(the ownership of this software changes often; google "IDL visualization" to find out more).
IDL will allow you to use view2d, a nice application which interactively plots the SOCRATES 
netCDF output files. The IDL source code is in the subdirectory ../view2d

Alternatively, there are a large number of plotting packages for netCDF files which may work straight away.
Alternatively you could write your own plotting code, e.g. in python using modules scipy.io and matplotlib
      

                      4. Preparing your own experiments: prepare input namelist file
                      ______________________________________________________________
                                 
There are a number of exemple input namelist files in ../namelists/*.inp .
You may edit "test.inp" to design your own simulation experiments. Here are a few tips:

Initial conditions
------------------
These are usually read from a netCDF file containing the output of a previous run.
The path and name of this input file are set through key "rstrt_filespec" in namelist "io_parms".

Timestep duration: chemdtm
--------------------------

the timestep duration for chemistry is chemdtm (in minutes). 
It can be set in the *.inp file and is quite important, because its preferred value
depends on the way you want to use SOCRATES.
This timestep is absolutely constant, there is no special timestep at 
sunrise/sunset - contrarily to SOCRATES3 and to SOCRATES v6.33 (which is described 
in my thesis). The drawback is that chemistry results are not very precise at 
sunrise/sunset for chemdtm > 5. 
The advantage is that advection and horizontal diffusion are now applied
at every timestep, while in all previous versions this was done only at noon
(which resulted in huge discontinuities at noon for some mesospheric species). 
This change also allowed parallelization of chemistry.

SOCRATES v7 should be used in two different configurations: one for short 
simulations (a few days) with small chemdtm (=5) to get diurnally varying output
(arch_set(?)='dvout',...) with high precision. A typical exemple is "ubaim.inp".
The other typical configuration is for very long runs (several years or tens 
of years), with large chemdtm (60 or 720) to get diurnal averages or even 
monthly averages. A typical exemple is "gromos-long.inp" .
Now with the increase in CPU performance maybe it becomes possible to simulate
several years with chemdtm=5. You've got to try...

Model switches
--------------

There are a number of switches to test different parameterizations/configurations
in the model. These can be set in the *.inp and are described in "./input_switches/switches_v7s16.txt".
I would advise to first use the default values as these should give the most
realistic results.


Output settings
---------------
Finally the output settings are controlled thru the "arch_set" structures in the
*.inp . I will comment a few settings to give exemples. If label_short = 'sample' :

 arch_set(1)  =   'chem', 1993, 12, 17, 0,0.,  2006, 5, 1, 0,0., 30

  ... outputs all chem vars to sample.chem.nc from 1993/12/17 until 2006/5/1 
  (or end of simulation) every 30 days

 arch_set(2)  =   'save', 2002,  6, 17, 0,0.,  2008, 12, 22, 0,0.,  0

  ... creates a full restart file sample.save.nc once for date 2002,  6, 17

 arch_set(3)  =   'heat', 1993,  3, 17, 0,0.,  2008, 12, 22, 0,0., 10

  ... outputs a subset of vars (related to heating budget) to sample.heat.nc, 
   from 1993/3/17 to 2008/12/22 (or simulation stop) every 10 days

 arch_month_avg = .true.

  ... outputs the monthly averages of the same subset of vars to sample.ma.nc 
    for every month of the simulation (if I remember well)
    
 arch_set(4)  = 'dvout', 1996, 8, 12, 0,0.,  2012, 10, 8, 0,0.,  0
 
  ... diurnally varying outputs of many chem variables at every timestep 
     on date 1996/8/12 , to file sample.dvarch_1996_8_12.nc

If you want more details about output settings, look at src/set_arch.f 
(takes some courage though).



