			6. Technical description
			========================

6.1 Introduction
----------------

The SOCRATES model is written in the Fortran 90 language. This allows :

- use of array-syntax
- use of "module" instructions instead of old, FORTRAN77 "common" instructions
- use of structured data variables, especially for date representation
- use of namelist-directed input files

It is important to understand the two last features and their implementation in
the model before attempting execution. The structure of TIMING and IO_TIMING 
variables is explained in subsection 6.5. The format of the namelist-directed 
input file (e.g. "socrates.inp") where these variables are set is explained 
in subsection 6.6.

Another modern computing technique used by SOCRATES is netCDF, the Network
Common Data Format. These libraries MUST be installed on the computer where the
simulation is executed, because the initial conditions file is written in this
format. Utilities built in the Fortran 90 source code are also provided for 
convenient output of the SOCRATES model to netCDF format. 

The sequence of actions to install, compile, run and use the SOCRATES model is:
- Download and install the netCDF library (6.2)
- Download, install and compile the source code (6.4)
- Download and install the data files (6.4)
- Adapt the input file to the desired simulation (6.5 and 6.6)
- Eventually, download and install the IDL application "view2d" to visualize
  the output files (6.3)


6.2 What is netCDF ? How to get it ?
------------------------------------

NetCDF (Network Common Data Format) is a format standard for binary data files.
The binary format used is completely platform-independent. The data is 
self-describing and structured to allow easy reading and analyzis by programs 
written in C or Fortran, or by commercial packages (e.g. IDL, Matlab).
Unidata, a division of UCAR, has developed this format and the Fortran library 
to read and write data to this format.
This library is available for free on all platforms : PC, Mac, Un*x
workstations and CRAY computers. Your Unix admisnistrator should retrieve and
install this library before you compile the SOCRATES model.

To download the library, point a World Wide Web browser to
http://www.unidata.ucar.edu/packages/netcdf/index.html
and follow the instructions.

The exemples given here assume that the netCDF compiled library is available
with the following path and filename :
/usr/local/lib/netcdf

The recommended filename extension for netCDF files is *.nc .

CAUTION ! The current version of netCDF is 3.3 . Although it allows faster
read/write operations, a bug prevents it from being backward-compatible with
previous versions. If you are using version 3.3 of the netCDF library, please
compile the SOCRATES model using the module file src/modules/netcdf33.mod.f .
If you are using a version of netCDF with a smaller (or greater ?) version 
number, compile the SOCRATES model using the module file 
src/modules/netcdf.mod.f .		(CHECK WITH STACY)

If you have already installed the NetCDF library, but it is a version older
than 2.4.2, please update the library at the Unidata Web site given above.
The subroutines of SOCRATES using NetCDF will not work 

6.3 Post-processing of netCDF files through "view2d", an IDL application
------------------------------------------------------------------------

A direct advantage of using netCDF is the ability to write powerful and fast 
applications to perform graphical analysis on model data output. 
Such an application has been written for the SOCRATES model. It is a set of 
IDL routines (IDL is a commercial product for scientific and graphical data 
analysis). 
The name of the application is "view2d", packed to the file 'view2d.tar.gz' . 
"view2d" allows the user to draw contour plots or one-dimensional plots from a
netCDF output file from SOCRATES, and to compare two different netCDF files,
through a Graphical User Interface.
The documentation is contained in the package (file README.txt) .
If your institution does not own an IDL license but uses a similar product 
(e.g. Matlab, or NCAR graphics), "view2d" can be used as a template to write a
similar application.


6.4 Installation, compilation and execution of the SOCRATES model
-----------------------------------------------------------------

The file socrates.tar.gz contains all the source code of the SOCRATES model
version 1.0 . After uncompressing (e.g. "gunzip socrates.tar.gz") and unpacking 
(e.g. "tar -xlf socrates.tar) the contents are :

./README.txt    	 	--> this file
./socrates.short.inp		--> a minimal simulation input file
				    (see section 6.6)
./socrates.short.out   	 	--> corresponding output file 
				    (for checking purposes)
./socrates.inp    		--> a typical simulation input file 
				    (see section 6.6)
./cray.long.csh   		--> A C-shell script to compile and execute 
				    the model on CRAY computers
./cray.short.csh   		--> Another C-shell script for CRAY computers, 
				    using libraries from previous compilations
./src/*.f 			--> Fortran 90 source code for the main, 
				    subroutines and functions
./src/Makefile.ibm   		--> to compile the model on an IBM RS/6000 
				    workstation
./src/modules/*.mod.f 		--> Fortran 90 source code for the modules
./src/modules/Makefile.ibm	--> to compile the model on an IBM RS/6000 
				    workstation
./src/cray/*.f 		--> Alternative Fortran 90 source code to run on CRAY
			    computers. The workstation versions in ./src/*.f
			    will be overwritten by ./cray.long.csh
./src/cray/*.s		--> Compute-intensive subroutines optimized for CRAYs
			    using the Cray Assembly Language. Workstation 
			    versions in ./src/*.f will be overwritten 
			    by ./cray.long.csh

To compile and execute the model on a CRAY computer, adapt and execute 
cray.long.csh from a C-shell. If the next runs require only recompilations 
of a few subroutines and no recompilation of the modules files, you can adapt 
and execute cray.short.csh .

To compile and execute the model on a Unix workstation, adapt the Makefiles to
your environment (fortran 90 compiler command and options). The sequence of
commands on an IBM RS/6000 workstation is then :

$HOME/.../src/modules> make -f Makefile.ibm
$HOME/.../src> make -f Makefile.ibm
$HOME/...> xlf src/*.o src/modules/*.o -o socrates  -L/usr/local/lib -lnetcdf
$HOME/...> nohup socrates < socrates.inp > socrates.out & 

Makefiles for DEC alpha computers are available as well.

PLEASE NOTE ! If the source code of ANY module file is changed, the WHOLE
source code must be re-compiled again.

The SOCRATES model requires a set of data files to run. These are packed in the
file socrates_data.tar.gz . It should be downloaded, decompressed and unpacked
to a separate directory. The simulation input files provided here assume that
the data files are in the directory ./data , at the same directory level than 
/.src .


6.5 Structured variables of type TIMING and IO_TIMING
-----------------------------------------------------

In SOCRATES, structured variables have been used to specify the date of the
bginning of the simulation, the date of the end of the simulation, and the
dates of desired output.
A date in the model is described by a structured variable of type TIMING
(defined in the module TIME_CONTROLS, file src/modules/main.mod.f).
A structured variable is a group of subvariables, called "fields" of the
structured variable. Each variable of type TIMING contains eight fields:

type TIMING
  integer :: year		--> Full year, e.g. 1996
  integer :: month		--> From 1 to 12
  integer :: day		--> If mode="date", day of the month (1 to 31)
  				    If mode='days', number of days since the
  				    beginning of simulation
  integer :: days0		--> Absolute measure of time: number of days
  				    since date 0/0/0000. Managed within the
  				    model.
  integer :: cal_day		--> Julian day of the year (1 to 365)
  integer :: index		--> Number of 5-day timesteps since beginning
				    of simulation
  logical :: active		--> Managed by the model : will be set to .true.
				    if the variable has been specified by the
				    user and checked valid by the model.
  character(len=5) :: mode	--> set to "index", "days" or "date" depending
				    of the way the variable is specified

The three different modes allow the user to define a date in several different
ways. For example, assuming that the simulation begins January 1, 1995,
the three following settings define the same date for the end of the simulation:

    sim_stop_time = 1997, 1,   1, 0, 0,   0, .true., 'date'    ...or...
    sim_stop_time =    0, 0, 730, 0, 0,   0, .true., 'days'    ...or...
    sim_stop_time =    0, 0,   0, 0, 0, 146, .true., 'index'

The values 0 are just blank fields which will be calculated by the model,
depending of the mode ('date', 'days' or 'index') specified by the user.
Note that when using the 'date' mode, the first integer sets the year, the
second integer sets the month and the third integer sets the day of the month.

The model has two important variables of type TIMING : sim_start_time and
sim_stop_time. Both must be specified by the user as elements of the namelist
CONTROL_PARMS in the simulation input file (see next subsection). 
Contrarily to sim_stop_time (see exemple above), sim_start_time must be 
specified with the mode 'date'.
Furthermore, the "internal" initial conditions contained in the file 
data/initcond.nc correspond to the date January 1. If the user starts from
these conditions and not from a restart file calculated previously, the date
set by sim_start_time must be January 1. The year can be chosen freely
by the user, keeping in mind that the initial conditions correspond to the
atmospheric composition typical of the 1990's. The usual starting date is thus:

    sim_start_time = 1995, 1, 1, 0, 0, 0, .true., 'date'

The user can set precise simulation dates at which printed, archived 
(diurnal averages or diurnal variations) or saved output are required 
(see next subsection). These variables have the type TIMING, and are named
respectively ran_printout, ran_archive, ran_dvarch and ran_save.
It is also possible to output data at regular intervals between two specified 
dates, through the variables intv_printout, intv_archive and intv_save.
These variables have the type IO_TIMING, defined in the module TIME_CONTROLS
(file ./src/modules/main.mod.f) :

type IO_TIMING
  type( TIMING ) :: start_time  	--> set by any of the three modes
  type( TIMING ) :: stop_time  		--> set by any of the three modes
  integer :: increment			--> in 5-days timesteps
  logical :: active 			--> must be set to .true.
  character(len=5) :: mode 		--> must be set to 'index'
  
Although these variables are vectors dimensioned to 10, only the first element
of the vector is taken in account in the present version of the model.


6.6 Writing the namelist-directed simulation input files
--------------------------------------------------------

Runs of the SOCRATES model require an input file, named 'socrates.inp' in the 
present description. This file contains the settings of basic parameters of 
the simulation, and the settings of variables of type TIMING and IO_TIMING. 
These variables are set through the use of namelists, a feature of Fortran90 
which allows setting variables in a random order, with no formatting, by naming 
the variables to set.

'socrates.inp' contains two namelists: CONTROL_PARMS and IO_PARMS.
CONTROL_PARMS contains the settings of the simulation start date, stop date and
basic parameters, whose names and possible values are given in the headers of
the file src/main.f .

IO_PARMS contains the settings of the data directory name, and of the variables
to specify what, when and where to output data. There are four possible output
files, described by Table xx.

_______________________________________________________________________________________________
| Type of | Format | output to     | Data written     | TIMING           | IO_TIMING          |
| output  |        | file...       | in the file      | variables        | variables          |
|---------|--------|---------------|------------------|------------------|--------------------|
|---------|--------|---------------|------------------|------------------|--------------------|
|         |        |               | dynamical and/or |                  |                    |
| printed | ASCII  |  Standard     |chemical variables| ran_printout(50) | intv_printout(10)  |
|         |        |   out         |(diurnal averages)|                  |                    |
|---------|--------|---------------|------------------|------------------|--------------------|
|archived |        |  set by       | dynamical and    |                  |                    |
|(diurnal | NetCDF |arch_filespec  |chemical variables| ran_archive(50)  | intv_archive(10)   |
| average)|        |               |(diurnal averages)|                  |                    |
|---------|--------|---------------|------------------|------------------|--------------------|
|archived |        |  set by       | temperature and  |                  |                    |
|(diurnal | NetCDF |dvarch_filespec|chemical variables| ran_dvarch(10)   |       none         |
|   cycle)|        |               |(diurnal cycles)  |                  |                    |
|---------|--------|---------------|------------------|------------------|--------------------|
|         |        |  set by       |Everything needed |                  |                    |
|  saved  | NetCDF | save_filespec | to restart from  |   ran_save(50)   |   intv_save(10)    |
|         |        |               | last saved date  |                  |                    |
|---------|--------|---------------|------------------|------------------|--------------------|

		Table xx. Characteristics of SOCRATES-generated output files

To explain the structure of an input file, we will comment the exemple input 
file named "socrates.inp", a typical two-year simulation requiring all the
possible output files.

 &control_parms							--> settings for CONTROL_PARMS namelist
 sim_start_time%year = 1995,   					--> arbitrary, left to user's choice.
 sim_start_time%month = 1,					--> mandatory to use internal initial conditions
 sim_start_time%day = 1,					--> mandatory to use internal initial conditions
 sim_stop_time%year = 1997,					--> simulation will end for year 1997...
 sim_stop_time%month = 1,					--> ... month of january...
 sim_stop_time%day = 6,						--> ... first day of that month
 sim_stop_time%mode = 'date',					--> this was an absolute date
 particle = 1,							--> turn ON sulfate aerosol heterogeneous chemistry
 aero = .false.,						--> turn OFF aerosol effect on photodissociatiation 
 								    and heating rates calculation
 polar = 1,							--> turn ON heterogeneous chemistry on Polar
 								    Stratospheric Clouds
 liste = .true.,						--> output the initial values
 /								--> end of CONTROL_PARMS namelist
 &io_parms							--> settings for IO_PARMS namelist
 data_dir = 'data/',						--> name of the directory containing data files
 run_label = 'run30',						--> label to identify version number
 prntsw = 1,							--> generate ASCII output...
 dynprt = 1,							--> ... of dynamical variables ...
 chmprt = 0,							--> ... but not of chemical variables
 intv_printout(1) = 1995, 1,  1,  0, 0,  0, .true.,  'date' ,	--> ASCII output begins 1st day of simulation...
                       0, 0,  0,  0, 0, 12, .true., 'index' ,	--> ... and ends after 12 (5-days) timesteps
                    1,						--> between these two dates, write at each timestep
                    .true.,					--> mandatory for variables of type IO_TIMING
                    'index',					--> mandatory for variables of type IO_TIMING
 ran_printout(1:3) = 1996, 6, 20, 0, 0,  0, .true., 'date' ,	--> add ASCII printout for 20 June 1996
                        0, 0, 90, 0, 0,  0, .true., 'days' ,	--> add ASCII printout 90 days after simulation begins
                        0, 0,  0, 0, 0, 73, .true., 'index',	--> .. and 73 (5-days) timesteps after simulation begins
 filesw = 1,							--> do NetCDF output of all diurnally averaged variables
 arch_filespec = 'results/archive.nc',				--> directory and file name for NetCDF archive file
 intv_archive(1) =  1995, 1,  1,  0, 0, 0, .true., 'date' ,	--> archived output begins 1st day of simulation...
                    1997, 1,  6,  0, 0, 0, .true., 'date' ,	--> ... and ends last day of simulation
                    2,						--> between those two date, output every other timestep
                    .true.,					--> mandatory for variables of type IO_TIMING
                    'index',					--> mandatory for variables of type IO_TIMING
 dvarchsw = 1,							--> do NetCDF output of diurnal cycles of chemical vars
 diuvarch_filespec = 'results/diuvarch.nc',			--> directory and file name for NetCDF archive file
 ran_dvarch(1:2) =  1996,  6, 20, 0, 0,  0, .true., 'date' ,	--> output for 20 june 1996
                    1996, 12, 17, 0, 0,  0, .true., 'date' , 	--> output for 17 december 1996
 savesw = 1,							--> save all variables to restart from a "restart" file
 save_filespec = 'results/save30.nc',				--> directory and file name for NetCDF "restart" file
 intv_save(1) =  1995, 3,  1,  0, 0, 0, .true., 'date' ,	--> begin to save at 1 march 1995
                 1997, 1,  6,  0, 0, 0, .true., 'date' ,	--> stop savings at end of simulation
                 12,						--> save every 12 (5-days) timesteps
                 .true.,					--> mandatory for variables of type IO_TIMING
                 'index',					--> mandatory for variables of type IO_TIMING
 ran_save(1) = 1997, 1, 1, 0, 0, 0, .true., 'date',		--> make sure to save simulation at 1 January 1997
/								--> end of IO_PARMS namelist



The exemple provided above makes use of the "initernal" initial conditions file
'data/initcond.nc' . The user can start the simulation with any other "restart" 
file saved from a previous run, provided that he knows the date of the last 
conditions saved in the "restart" file generated by the previous run (this date
is written in the ASCII standard output file, and can also be retrieved
directly from the "restart" file using 'savedater.pro', an IDL subroutine
contained in the IDL application package 'view2d.tar.gz').

Let us suppose that the user wants to use a restart file containing the 
conditions saved by a previous run, for the date 20 june 1995.
First, this must be the date of the start of his run:
 &control_parms
 sim_start_time%year = 1995, 
 sim_start_time%month = 1,
 sim_start_time%day = 1,

The following line must be inserted in the CONTROL_PARMS namelist settings:
 restrt = .true.,
 
The path and name of the "restart" file must be set in the IO_PARMS namelist .
If the "restart" file is named 'save29.nc' and is in the directory 'results', 
the next line should be inserted in the IO_PARMS namelist settings:
 rstrt_filespec = 'results/save29.nc'
 
