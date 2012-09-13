;+
;; @brief Function to write an SQL query for a Kepler ID into an SQL script file.
;; @author Brian L. Lee
;; @date 2012 Sep 13
;;
;; Summary of actions
;; 2.  Form text of query
;; 3.  Write query to file
;-
function make_sql_query_macro, $
                               kepler_id=kepler_id, $
                               out_scriptfile_name=out_scriptfile_name, $
                               log_lun=log_lun

PRINT, SYSTIME(/UTC), "|Running make_sql_query_macro"

;;=============================================================================
;;0.0. Check log file
;;=============================================================================
IF SIZE(log_lun, /TYPE) EQ 0 THEN BEGIN
    log_lun = -1l
ENDIF
;;=============================================================================
;;1.  Integrity checks on inputs
;;=============================================================================
IF SIZE(out_scriptfile_name, /TYPE) NE 7 THEN BEGIN
    err_msg = SYSTIME(/UTC) + "|ERROR|make_sql_query_macro|The specified output file name is not a string."
    PRINT, err_msg
    PRINTF, log_lun, err_msg
    SAVE, FILENAME='error_make_sql_query_macro.sav', /ALL
    RETURN, err_msg
ENDIF
IF SIZE(kepler_id, /TYPE) NE 2 AND SIZE(kepler_id, /TYPE) NE 3 THEN BEGIN
    err_msg = SYSTIME(/UTC) + "|ERROR|make_sql_query_macro|The specified kepler_id was not an int or a long int."
    PRINT, err_msg
    PRINTF, log_lun, err_msg
    SAVE, FILENAME='error_make_sql_query_macro.sav', /ALL
    RETURN, err_msg
ENDIF
;;=============================================================================
;;2.  Form text of SQL query script
;;=============================================================================
text_line_1 = 'SELECT * FROM source where KEPLERID='+string(kepler_id)
;;=============================================================================
;;3.  Write text to file
;;=============================================================================
openw,lun,out_scriptfile_name,/get_lun
printf,lun,text_line_1
free_lun,lun
;;=============================================================================
;;4.  Return success
;;=============================================================================
return,1
end
