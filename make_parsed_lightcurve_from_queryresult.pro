;+
;; @brief Function to parse the result of the SQL query for a Kepler
;; light curve.
;; @author Brian L. Lee
;; @date 2012 Sep 13
;;
;; Summary of actions
;; 2.  Read in query result
;; 3.  Parse columns
;-
function make_parsed_lightcurve_from_queryresult, $
                                                  in_queryresultfile_name=in_queryresultfile_name, $
                                                  min_lines_required_in_queryresultfile=min_lines_required_in_queryresultfile, $
                                                  out_time=out_time, $
                                                  out_flux=out_flux, $
                                                  out_flux_err=out_flux_err, $
                                                  log_lun=log_lun

PRINT, SYSTIME(/UTC), "|Running make_parsed_lightcurve_from_queryresult"

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
    err_msg = SYSTIME(/UTC) + "|ERROR|make_parsed_lightcurve_from_queryresult|The specified min_lines_required_in_queryresultfile was not an int or a long int."
    PRINT, err_msg
    PRINTF, log_lun, err_msg
    SAVE, FILENAME='error_make_parsed_lightcurve_from_queryresult.sav', /ALL
    RETURN, err_msg
ENDIF
IF SIZE(in_queryresultfile_name, /TYPE) NE 7 THEN BEGIN
    err_msg = SYSTIME(/UTC) + "|ERROR|make_parsed_lightcurve_from_queryresult|The specified input file name is not a string."
    PRINT, err_msg
    PRINTF, log_lun, err_msg
    SAVE, FILENAME='error_make_parsed_lightcurve_from_queryresult.sav', /ALL
    RETURN, err_msg
ENDIF ELSE BEGIN
    IF FILE_TEST(in_queryresultfile_name) NE 1 THEN BEGIN
        err_msg = SYSTIME(/UTC) + "|ERROR|make_parsed_lightcurve_from_queryresult|The specified input file was not found: " + in_queryresultfile_name
        PRINT, err_msg
        PRINTF, log_lun, err_msg
        SAVE, FILENAME='error_make_parsed_lightcurve_from_queryresult.sav', /ALL
        RETURN, err_msg
    ENDIF
    n_lines_in_queryresultfile_name=file_lines(in_queryresultfile_name)
    IF n_lines_in_queryresultfile_name LE min_lines_required_in_queryresultfile THEN BEGIN
        err_msg = SYSTIME(/UTC) + "|ERROR|make_parsed_lightcurve_from_queryresult|The number of lines in the input file was suspiciously small (less than "+string(min_lines_required_in_queryresultfile)+").  Number of lines: " +string(n_lines_in_queryresultfile_name)
        PRINT, err_msg
        PRINTF, log_lun, err_msg
        SAVE, FILENAME='error_make_parsed_lightcurve_from_queryresult.sav', /ALL
        RETURN, err_msg
    ENDIF
ENDELSE
;;=============================================================================
;;2.  Read in query result
;;=============================================================================
;;=============================================================================
;;3.  Parse columns
;;=============================================================================
;;=============================================================================
;;4.  Return success.
;;=============================================================================
stop
return,1
end
