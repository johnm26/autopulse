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
                               in_kepler_id=in_kepler_id, $
                               out_scriptfile_name=out_scriptfile_name, $
                               log_lun=log_lun

spawn,'hostname',hostname,/noshell
PRINT, SYSTIME(/UTC) + " "+hostname+"|Running make_sql_query_macro"

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
    err_msg = SYSTIME(/UTC) + " "+hostname+"|ERROR|make_sql_query_macro|The specified output file name is not a string."
    PRINT, err_msg
    PRINTF, log_lun, err_msg
    SAVE, FILENAME='error_make_sql_query_macro.sav', /ALL
    RETURN, err_msg
ENDIF
IF SIZE(in_kepler_id, /TYPE) NE 7 THEN BEGIN
    err_msg = SYSTIME(/UTC) + " "+hostname+"|ERROR|make_sql_query_macro|The specified in_kepler_id was not a string."
    PRINT, err_msg
    PRINTF, log_lun, err_msg
    SAVE, FILENAME='error_make_sql_query_macro.sav', /ALL
    RETURN, err_msg
ENDIF
;;=============================================================================
;;2.  Form text of SQL query script
;;=============================================================================
;;text_line_1 = 'select distinct SOURCEID,CADENCENO,TIME,SAP_FLUX,SAP_FLUX_ERR,PDCSAP_FLUX,PDCSAP_FLUX_ERR,CBVSAP_MODL,CBVSAP_FLUX,SAP_QUALITY,source.KEPLERID,source.QUARTER,LCFLAG,CHANNEL,SKYGROUP,MODULE,SEASON,OUTPUT,DATA_REL,RA_OBJ,DEC_OBJ,EQUINOX,PMRA,PMDEC,PMTOTAL,PARALLAX,GLON,GLAT,GMAG,RMAG,IMAG,ZMAG,D51MAG,JMAG,HMAG,KMAG,KEPMAG,GRCOLOR,JKCOLOR,GKCOLOR,TEFF,LOGG,FEH,EBMINUSV,AV,RADIUS,TMINDEX,BJDREFI from source inner join object on (object.keplerid =source.keplerid and object.quarter = source.quarter) where (source.KEPLERID='+in_kepler_id+' and lcflag=1)'
text_line_1 = 'select distinct SOURCEID,CADENCENO,TIME,SAP_FLUX,SAP_FLUX_ERR,PDCSAP_FLUX,PDCSAP_FLUX_ERR,SAP_QUALITY,source.KEPLERID,source.QUARTER,LCFLAG,CHANNEL,SKYGROUP,MODULE,SEASON,OUTPUT,DATA_REL,RA_OBJ,DEC_OBJ,EQUINOX,PMRA,PMDEC,PMTOTAL,PARALLAX,GLON,GLAT,GMAG,RMAG,IMAG,ZMAG,D51MAG,JMAG,HMAG,KMAG,KEPMAG,GRCOLOR,JKCOLOR,GKCOLOR,TEFF,LOGG,FEH,EBMINUSV,AV,RADIUS,TMINDEX,BJDREFI from source inner join object on (object.keplerid =source.keplerid and object.quarter = source.quarter) where (source.KEPLERID='+in_kepler_id+' and lcflag=1)'
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
