! subversion Id for THIS file : $Id: nf_new.mod.f 8506 2016-01-28 10:09:58Z simonc $
! $URL: http://svn-ae/ae/COMMON/stand-alone/atmos_models/socrates/tags/07.18.01/src/nf_new.mod.f $
!-----------------------------------------------------------------------

      module NETCDF
!--------------------------------------------------------------------------
!	... This module provides a "nice" interface to the netCDF library
!           version 3 and above. The following subroutines are available
!           for external access:
!  NETCDF_CREATE, NETCDF_DIM_DEF, NETCDF_VAR_DEF, SET_ATTRIBUTE,
!  NETCDF_ARCH, NETCDF_READ, SET_TIME, SET_FILE, OPEN_FILE, CLOSE_FILE
!  CLOSE_ALL_FILES
!--------------------------------------------------------------------------

      implicit none

!--------------------------------------------------------------------------
!	... Parameters interfacing the library (from "netcdf.inc")
!--------------------------------------------------------------------------
! External netCDF data types:
      integer, parameter :: nf_byte = 1
      integer, parameter :: nf_char = 2
      integer, parameter :: nf_short = 3
      integer, parameter :: nf_long = 4
      integer, parameter :: nf_float = 5
      integer, parameter :: nf_double = 6
! Mode flags for opening and creating a netCDF dataset:
      integer, parameter :: nf_nowrite = 0
      integer, parameter :: nf_write = 1
      integer, parameter :: nf_clobber = 0
      integer, parameter :: nf_noclobber = 4
      integer, parameter :: nf_fill = 0
      integer, parameter :: nf_nofill = 256
! 'size' argument for the unlimited dimension
      integer, parameter :: nf_unlimited = 0
! Attribute id to put/get a global attribute
      integer, parameter :: nf_global  = 0
! Implementation limits:
      integer, parameter :: nf_max_dims = 100
      integer, parameter :: nf_max_attrs = 2000
      integer, parameter :: nf_max_vars = 2000
      integer, parameter :: nf_max_name = 128
      integer, parameter :: nf_max_var_dims = nf_max_dims
! No Error 
      integer, parameter :: nf_noerr = 0
! Error handling modes:
      integer, parameter :: nf_fatal = 1
      integer, parameter :: nf_verbos = 2
      
!-----------------------------------------------------------------------
!	... Declaration of the functions of the netCDF lib (usage - see doc)
!-----------------------------------------------------------------------
      integer, external :: NF_CREATE, NF_OPEN, NF_SET_FILL, NF_REDEF
      integer, external :: NF_ENDDEF, NF_CLOSE, NF_INQ_NDIMS, NF_DEF_DIM
      integer, external :: NF_INQ_DIM, NF_INQ_DIMID, NF_INQ_DIMLEN
      integer, external :: NF_INQ_ATT, NF_GET_ATT_TEXT
      integer, external :: NF_PUT_ATT_TEXT, NF_PUT_ATT_DOUBLE
      integer, external :: NF_DEF_VAR, NF_INQ_VARID, NF_INQ_VAR
      integer, external :: NF_PUT_VAR_INT, NF_GET_VAR_INT
      integer, external :: NF_PUT_VAR_TEXT, NF_GET_VAR_TEXT
      integer, external :: NF_PUT_VAR_REAL, NF_GET_VAR_REAL
      integer, external :: NF_PUT_VAR_DOUBLE, NF_GET_VAR_DOUBLE
      integer, external :: NF_PUT_VAR1_INT, NF_GET_VAR1_INT
      integer, external :: NF_PUT_VAR1_REAL, NF_GET_VAR1_REAL
      integer, external :: NF_PUT_VAR1_DOUBLE, NF_GET_VAR1_DOUBLE
      integer, external :: NF_PUT_VARA_INT, NF_GET_VARA_INT
      integer, external :: NF_PUT_VARA_REAL, NF_GET_VARA_REAL
      integer, external :: NF_PUT_VARA_DOUBLE, NF_GET_VARA_DOUBLE
      character(len=80), external :: NF_STRERROR, NF_INQ_LIBVERS
      
      end module NETCDF
