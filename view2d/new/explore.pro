; procedure to explore the NetCDF file from Simon
; [06Mar03 JBishop]
;
; refer to IDL NCDF procedure/function definitions

; ---------------
; NCDF_OPEN      function for opening nc-file of interest & assigning it a name

filename = '/bira-iasb/datam/socrates/for/kazil/v7s11w.dvout_10_23_1984.nc'

door = NCDF_OPEN( filename )                  ;; i.e., the "door" into the file

; ---------------
; NCDF_INQUIRE   now inquire about the contents

info = NCDF_INQUIRE( door )

print, info.ndims, info.nvars, info.ngatts, info.recdim

; results:  info.ndims   5     dimensions
;           info.nvars  31     variables
;           info.ngatts  7     global attributes
;           info.recdim  2     unlimited dimension ID

print, ' '

; ---------------
; NCDF_DIMINQ    identify each dimension

NCDF_DIMINQ, door, 0, name_0, size_0
NCDF_DIMINQ, door, 1, name_1, size_1
NCDF_DIMINQ, door, 2, name_2, size_2
NCDF_DIMINQ, door, 3, name_3, size_3
NCDF_DIMINQ, door, 4, name_4, size_4

;print, name_0, size_0
;print, name_1, size_1
;print, name_2, size_2
;print, name_3, size_3
;print, name_4, size_4

; results:  0:     'lat'      35
;           1:     'lev'     121
;           2:    'time'     288
;           3:  'nltext'      43
;           4:  'nrtext'      80

;print, ' '

; ---------------
; NCDF_ATTNAME   get names of each attribute

att_0 = NCDF_ATTNAME( door, /GLOBAL, 0)
att_1 = NCDF_ATTNAME( door, /GLOBAL, 1)
att_2 = NCDF_ATTNAME( door, /GLOBAL, 2)
att_3 = NCDF_ATTNAME( door, /GLOBAL, 3)
att_4 = NCDF_ATTNAME( door, /GLOBAL, 4)
att_5 = NCDF_ATTNAME( door, /GLOBAL, 5)
att_6 = NCDF_ATTNAME( door, /GLOBAL, 6)

;print, att_0
;print, att_1
;print, att_2
;print, att_3
;print, att_4
;print, att_5
;print, att_6

; results:  0:   'lat'
;           1:   'lev'
;           2:   'model_name'
;           3:   'run_label'
;           4:   'label_long'
;           5:   'missing_value'
;           6:   'sim_days0'

;print, ' '

; ---------------
; NCDF_ATTINQ    get info on global attributes

att_0_info = NCDF_ATTINQ( door, /GLOBAL, 'lat')
att_1_info = NCDF_ATTINQ( door, /GLOBAL, 'lev')
att_2_info = NCDF_ATTINQ( door, /GLOBAL, 'model_name')
att_3_info = NCDF_ATTINQ( door, /GLOBAL, 'run_label')
att_4_info = NCDF_ATTINQ( door, /GLOBAL, 'label_long')
att_5_info = NCDF_ATTINQ( door, /GLOBAL, 'missing_value')
att_6_info = NCDF_ATTINQ( door, /GLOBAL, 'sim_days0')

;print, att_0_info
;print, att_1_info
;print, att_2_info
;print, att_3_info
;print, att_4_info
;print, att_5_info
;print, att_6_info

; results:  0:  { CHAR          9 }
;           1:  { CHAR          6 }
;           2:  { CHAR          8 }
;           3:  { CHAR          6 }
;           4:  { CHAR         25 }
;           5:  { DOUBLE        1 }
;           6:  { DOUBLE        1 }

;print, ' '

; ---------------
; NCDF_VARINQ    identify each variable

var_00 = NCDF_VARINQ( door,  0 )
var_01 = NCDF_VARINQ( door,  1 )
var_02 = NCDF_VARINQ( door,  2 )
var_03 = NCDF_VARINQ( door,  3 )
var_04 = NCDF_VARINQ( door,  4 )
var_05 = NCDF_VARINQ( door,  5 )
var_06 = NCDF_VARINQ( door,  6 )
var_07 = NCDF_VARINQ( door,  7 )
var_08 = NCDF_VARINQ( door,  8 )
var_09 = NCDF_VARINQ( door,  9 )
var_10 = NCDF_VARINQ( door, 10 )
var_11 = NCDF_VARINQ( door, 11 )
var_12 = NCDF_VARINQ( door, 12 )
var_13 = NCDF_VARINQ( door, 13 )
var_14 = NCDF_VARINQ( door, 14 )
var_15 = NCDF_VARINQ( door, 15 )
var_16 = NCDF_VARINQ( door, 16 )
var_17 = NCDF_VARINQ( door, 17 )
var_18 = NCDF_VARINQ( door, 18 )
var_19 = NCDF_VARINQ( door, 19 )
var_20 = NCDF_VARINQ( door, 20 )
var_21 = NCDF_VARINQ( door, 21 )
var_22 = NCDF_VARINQ( door, 22 )
var_23 = NCDF_VARINQ( door, 23 )
var_24 = NCDF_VARINQ( door, 24 )
var_25 = NCDF_VARINQ( door, 25 )
var_26 = NCDF_VARINQ( door, 26 )
var_27 = NCDF_VARINQ( door, 27 )
var_28 = NCDF_VARINQ( door, 28 )
var_29 = NCDF_VARINQ( door, 29 )
var_30 = NCDF_VARINQ( door, 30 )

print, var_00
print, var_01
print, var_02
print, var_03
print, var_04
print, var_05
print, var_06
print, var_07
print, var_08
print, var_09
print, var_10
print, var_11
print, var_12
print, var_13
print, var_14
print, var_15
print, var_16
print, var_17
print, var_18
print, var_19
print, var_20
print, var_21
print, var_22
print, var_23
print, var_24
print, var_25
print, var_26
print, var_27
print, var_28
print, var_29
print, var_30

; results:
;      { name           datatype      ndims      natts      dim(ndims) }
;  0:   'latitudes'     FLOAT         1          1          0
;  1:   'levels'        FLOAT         1          1          1
;  2:   'time'          FLOAT         1          1          2
;  3:   'description'   CHAR          2          0          4    3
;  4:   'temperature'   FLOAT         2          1          0    1
;  5:   'totdens'       FLOAT         2          1          0    1
;  6:   'n2o'           FLOAT         3          1          0    1    2
;  7:   'ch4'           FLOAT         3          1          0    1    2
;  8:   'h2o'           FLOAT         3          1          0    1    2
;  9:   'co'            FLOAT         3          1          0    1    2
; 10:   'hcl'           FLOAT         3          1          0    1    2
; 11:   'hbr'           FLOAT         3          1          0    1    2
; 12:   'co2'           FLOAT         3          1          0    1    2
; 13:   'h2'            FLOAT         3          1          0    1    2
; 14:   'ch2o'          FLOAT         3          1          0    1    2
; 15:   'o3'            FLOAT         3          1          0    1    2
; 16:   'o3p'           FLOAT         3          1          0    1    2
; 17:   'h'             FLOAT         3          1          0    1    2
; 18:   'oh'            FLOAT         3          1          0    1    2
; 19:   'h2o2'          FLOAT         3          1          0    1    2
; 20:   'cl2'           FLOAT         3          1          0    1    2
; 21:   'clono2'        FLOAT         3          1          0    1    2
; 22:   'n'             FLOAT         3          1          0    1    2
; 23:   'no'            FLOAT         3          1          0    1    2
; 24:   'no2'           FLOAT         3          1          0    1    2
; 25:   'hno3'          FLOAT         3          1          0    1    2
; 26:   'n2o5'          FLOAT         3          1          0    1    2
; 27:   'o2'            FLOAT         3          1          0    1    2
; 28:   'o2dg'          FLOAT         3          1          0    1    2
; 29:   'o2s'           FLOAT         3          1          0    1    2
; 30:   'J_O3-OP'       FLOAT         3          1          0    1    2

; ---------------
; NCDF_CLOSE     close the file
NCDF_CLOSE, door

end
