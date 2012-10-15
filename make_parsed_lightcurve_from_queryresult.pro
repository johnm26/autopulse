;+
;; @brief Function to parse the result of the SQL query for a Kepler
;; light curve.
;; @author Brian L. Lee
;; @date 2012 Sep 13
;;
;; Summary of actions
;; 3.  Read in query result and parse columns
;-
function make_parsed_lightcurve_from_queryresult, $
                                                  in_queryresultfile_name=in_queryresultfile_name, $
                                                  min_lines_required_in_queryresultfile=min_lines_required_in_queryresultfile, $
                                                  out_time=out_time, $
                                                  out_flux=out_flux, $
                                                  out_err_flux=out_err_flux, $
                                                  out_quarter=out_quarter, $
                                                  out_channel=out_channel, $
                                                  log_lun=log_lun

spawn,'hostname',hostname,/noshell
PRINT, SYSTIME(/UTC) + " "+hostname+"|Running make_parsed_lightcurve_from_queryresult"

;;=============================================================================
;;0.0. Check log file
;;=============================================================================
IF SIZE(log_lun, /TYPE) EQ 0 THEN BEGIN
    log_lun = -1l
ENDIF
;;=============================================================================
;;1.  Integrity checks on inputs
;;=============================================================================
IF SIZE(min_lines_required_in_queryresultfile, /TYPE) NE 2 AND SIZE(min_lines_required_in_queryresultfile, /TYPE) NE 3 THEN BEGIN
    err_msg = SYSTIME(/UTC) + " "+hostname+"|ERROR|make_parsed_lightcurve_from_queryresult|The specified min_lines_required_in_queryresultfile was not an int or a long int."
    PRINT, err_msg
    PRINTF, log_lun, err_msg
    SAVE, FILENAME='error_make_parsed_lightcurve_from_queryresult.sav', /ALL
    RETURN, err_msg
ENDIF
IF SIZE(in_queryresultfile_name, /TYPE) NE 7 THEN BEGIN
    err_msg = SYSTIME(/UTC) + " "+hostname+"|ERROR|make_parsed_lightcurve_from_queryresult|The specified input file name is not a string."
    PRINT, err_msg
    PRINTF, log_lun, err_msg
    SAVE, FILENAME='error_make_parsed_lightcurve_from_queryresult.sav', /ALL
    RETURN, err_msg
ENDIF ELSE BEGIN
    IF FILE_TEST(in_queryresultfile_name) NE 1 THEN BEGIN
        err_msg = SYSTIME(/UTC) + " "+hostname+"|ERROR|make_parsed_lightcurve_from_queryresult|The specified input file was not found: " + in_queryresultfile_name
        PRINT, err_msg
        PRINTF, log_lun, err_msg
        SAVE, FILENAME='error_make_parsed_lightcurve_from_queryresult.sav', /ALL
        RETURN, err_msg
    ENDIF
    n_lines_in_queryresultfile_name=file_lines(in_queryresultfile_name)
    IF n_lines_in_queryresultfile_name LE min_lines_required_in_queryresultfile THEN BEGIN
        err_msg = SYSTIME(/UTC) + " "+hostname+"|ERROR|make_parsed_lightcurve_from_queryresult|The number of lines in the input file was suspiciously small (less than "+string(min_lines_required_in_queryresultfile)+").  Number of lines: " +string(n_lines_in_queryresultfile_name)
        PRINT, err_msg
        PRINTF, log_lun, err_msg
        SAVE, FILENAME='error_make_parsed_lightcurve_from_queryresult.sav', /ALL
        RETURN, err_msg
    ENDIF
ENDELSE
;;=============================================================================
;;2.  Set up internal variables
;;=============================================================================
time_column_name='time'
;flux_column_name='cbvsap_flux'
flux_column_name='sap_flux'
err_flux_column_name='sap_flux_err'
quarter_column_name='quarter'
channel_column_name='channel'
;;=============================================================================
;;3.  Read in query result
;;=============================================================================
;;3.1  Get number of columns and their names
;;3.1.1  Initialize string variable to hold the column names
column_names=''
;;3.1.2  Read in the first line of the file and store the text as the
;;column names
openr,lun,in_queryresultfile_name,/get_lun
readf,lun,column_names,format='(a)'
;;3.1.3  Close the file
free_lun,lun
;;3.1.4  Split the column names line into an array of individual name strings
column_names=strsplit(column_names,/extract)
;;3.2  Figure out which columns we want to extract
index_column_time=where(strmatch(column_names,time_column_name,/fold_case),count_column_time)
IF count_column_time NE 1 THEN BEGIN
        err_msg = SYSTIME(/UTC) + " "+hostname+"|ERROR|make_parsed_lightcurve_from_queryresult|The specified column name was not found or ambiguous: " + time_column_name
        PRINT, err_msg
        PRINTF, log_lun, err_msg
        SAVE, FILENAME='error_make_parsed_lightcurve_from_queryresult.sav', /ALL
        RETURN, err_msg
ENDIF
index_column_flux=where(strmatch(column_names,flux_column_name,/fold_case),count_column_flux)
IF count_column_flux NE 1 THEN BEGIN
        err_msg = SYSTIME(/UTC) + " "+hostname+"|ERROR|make_parsed_lightcurve_from_queryresult|The specified column name was not found or ambiguous: " + flux_column_name
        PRINT, err_msg
        PRINTF, log_lun, err_msg
        SAVE, FILENAME='error_make_parsed_lightcurve_from_queryresult.sav', /ALL
        RETURN, err_msg
ENDIF
index_column_err_flux=where(strmatch(column_names,err_flux_column_name,/fold_case),count_column_err_flux)
IF count_column_err_flux NE 1 THEN BEGIN
        err_msg = SYSTIME(/UTC) + " "+hostname+"|ERROR|make_parsed_lightcurve_from_queryresult|The specified column name was not found or ambiguous: " + err_flux_column_name
        PRINT, err_msg
        PRINTF, log_lun, err_msg
        SAVE, FILENAME='error_make_parsed_lightcurve_from_queryresult.sav', /ALL
        RETURN, err_msg
ENDIF
index_column_quarter=where(strmatch(column_names,quarter_column_name,/fold_case),count_column_quarter)
IF count_column_quarter LT 1 THEN BEGIN
        err_msg = SYSTIME(/UTC) + " "+hostname+"|ERROR|make_parsed_lightcurve_from_queryresult|The specified column name was not found: " + quarter_column_name
        PRINT, err_msg
        PRINTF, log_lun, err_msg
        SAVE, FILENAME='error_make_parsed_lightcurve_from_queryresult.sav', /ALL
        RETURN, err_msg
ENDIF
index_column_channel=where(strmatch(column_names,channel_column_name,/fold_case),count_column_channel)
IF count_column_channel NE 1 THEN BEGIN
        err_msg = SYSTIME(/UTC) + " "+hostname+"|ERROR|make_parsed_lightcurve_from_queryresult|The specified column name was not found or ambiguous: " + channel_column_name
        PRINT, err_msg
        PRINTF, log_lun, err_msg
        SAVE, FILENAME='error_make_parsed_lightcurve_from_queryresult.sav', /ALL
        RETURN, err_msg
ENDIF
;;3.3  Assume the format of the file is one header line followed by
;;float data
rdfloat, $
  in_queryresultfile_name, $
  out_time, $
  out_flux, $
  out_err_flux, $
  out_quarter, $
  out_channel, $
  columns=[ $
            index_column_time+1, $
            index_column_flux+1, $
            index_column_err_flux+1, $
            index_column_quarter[0]+1, $
            index_column_channel+1 $
          ], $
  skipline=1, $
  /double, $
  /silent

;;3.4  Integrity check on number of lines read compared to number of
;;lines in the file
if n_elements(out_time) ne n_lines_in_queryresultfile_name-1 then begin
        err_msg = SYSTIME(/UTC) + " "+hostname+"|ERROR|make_parsed_lightcurve_from_queryresult|The number of data lines ("+string(n_elements(out_time))+")read in from file "+in_queryresultfile_name+" did not match the expected number "+string(n_lines_in_queryresultfile_name-1)
        PRINT, err_msg
        PRINTF, log_lun, err_msg
        SAVE, FILENAME='error_make_parsed_lightcurve_from_queryresult.sav', /ALL
        RETURN, err_msg
endif
;;=============================================================================
;;4.  Return success.
;;=============================================================================
return,1
end
