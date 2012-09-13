;+
;; @brief Function to execute a macro containing an SQL query
;; @author Brian L. Lee
;; @date 2012 Sep 13
;;
;; Summary of actions
;; 2.  Form a command string to execute the SQL macro file
;; 3.  Use spawn to execute the command string
;-
function make_query_to_sql_database_by_macro, $
                                 in_scriptfile_name=in_scriptfile_name, $
                                 out_queryresultfile_name=out_queryresultfile_name, $
                                 log_lun=log_lun

PRINT, SYSTIME(/UTC), "|Running make_query_to_sql_database_by_macro"

;;=============================================================================
;;0.0. Check log file
;;=============================================================================
IF SIZE(log_lun, /TYPE) EQ 0 THEN BEGIN
    log_lun = -1l
ENDIF
;;=============================================================================
;;1.  Integrity checks on inputs
;;=============================================================================
IF SIZE(in_scriptfile_name, /TYPE) NE 7 THEN BEGIN
    err_msg = SYSTIME(/UTC) + "|ERROR|make_query_to_sql_database_by_macro|The specified input file name is not a string."
    PRINT, err_msg
    PRINTF, log_lun, err_msg
    SAVE, FILENAME='error_make_query_to_sql_database_by_macro.sav', /ALL
    RETURN, err_msg
ENDIF ELSE BEGIN
    IF FILE_TEST(in_scriptfile_name) NE 1 THEN BEGIN
        err_msg = SYSTIME(/UTC) + "|ERROR|make_query_to_sql_database_by_macro|The specified input file was not found: " + in_scriptfile_name
        PRINT, err_msg
        PRINTF, log_lun, err_msg
        SAVE, FILENAME='error_make_query_to_sql_database_by_macro.sav', /ALL
        RETURN, err_msg
    ENDIF
ENDELSE
IF SIZE(out_queryresultfile_name, /TYPE) NE 7 THEN BEGIN
    err_msg = SYSTIME(/UTC) + "|ERROR|make_query_to_sql_database_by_macro|The specified output file name is not a string."
    PRINT, err_msg
    PRINTF, log_lun, err_msg
    SAVE, FILENAME='error_make_query_to_sql_database_by_macro.sav', /ALL
    RETURN, err_msg
ENDIF
;;=============================================================================
;;2.  Form a command string to execute the SQL macro file
;;=============================================================================
command_line_1 = 'mysql -h tddb -u bvegaff --password=tddbKepler Kepler < '+in_scriptfile_name+' > '+out_queryresultfile_name
;;=============================================================================
;;3.  Use spawn to execute the command string
;;=============================================================================
spawn,command_line_1,linux_output_messages
;;=============================================================================
;;4.  Return success
;;=============================================================================
return,1
end
