;+
;; @brief idlvm_driver_for_compute_qats.pro 
;; @author Brian L. Lee 
;; @date 05Oct2012
;;
;; @detail This procedure is an IDL virtual machine wrapper for
;; compute_qats.pro
;;
;; @purpose: To pass arguments for compute_qats.pro
;;
;; @precompile:  Get the virtual machine ready to go before using:
;; IDL> .comp compute_qats
;; IDL> resolve_all
;; IDL> .comp compute_qats
;; IDL> .comp idlvm_driver_for_compute_qats
;; IDL> resolve_all
;; IDL> save,/routines,filename='idlvm_driver_for_compute_qats.sav'
;;
;; @example: idl -vm=./idlvm_driver_for_compute_qats.sav -args 1432214
;; 0.005 '[1.0,300.0]' 1
;; '/astro/net/astro-agol/blevlee/CODE/condor/test3/test_working_dir'
;; '/astro/net/astro-agol/blevlee/CODE/IDL/KEPLER_REDUX/autopulse' 'path_to_lightcurve_fits_file1,path_to_lightcurve_fits_file2,path_to_lightcurve_fits_file3,...'
;;
;; @inputs: None
;; 
;; @outputs: None
;;
;; @optional: Give an optional TRUE to activate 
;-

PRO idlvm_driver_for_compute_qats

;;0.  Define an error handler
on_ioerror, error_clause

;;1.  Dump command line arguments into string array
result = COMMAND_LINE_ARGS(COUNT=acount)
;***ADDED FOR TESTING:
print,'results0 = '+result[0]
IF(N_ELEMENTS(result) GE 7) THEN print,'results6 = '+result[6]
IF((acount NE 7) AND (acount NE 6)) THEN BEGIN
   print,'Usage: idl -vm=./idlvm_driver_for_compute_qats.sav -args \"kid f prange mask_planet working_dir common_data_root_dir\"'
   return
ENDIF 

;;2.  Extract command line arguments to separate string variables  
kid_str = result[0]
f_str = result[1]
prange_str = result[2]
mask_planet_str=result[3]
working_dir_str=result[4]
common_data_root_dir_str=result[5]
IF(N_ELEMENTS(result) GE 7) THEN kid_fits_filenames_str=result[6]
print,'Raw argument list:'
print,kid_str
print,f_str
print,prange_str
print,mask_planet_str
print,working_dir_str
print,common_data_root_dir_str
IF(N_ELEMENTS(result) GE 7) THEN print,kid_fits_filenames_str

;;3.  Convert string variables to correct types for ingestion into compute_qats
;;3.1  Convert Kepler ID
kid=long(kid_str)
;;3.2  Convert fractional period variation
f=double(f_str)
;;3.3  Convert array of min, max periods to search
prange_str_split=strsplit(prange_str,"'[,]",/extract,count=count_prange_str_split)
print,count_prange_str_split
print,prange_str_split
if count_prange_str_split eq 2 then begin
    period_lo=double(prange_str_split[0])
    period_hi=double(prange_str_split[1])
    prange=[period_lo,period_hi]
endif else begin
    print,systime()+"|ERROR|idlvm_driver_for_compute_qats|The prange entered was not a valid array of two numbers enclosed by single quotes.  Example of expected format: '[1.0,300.0]'.  Exiting program now."
    return
endelse
;;3.4  Convert planet masking flag
mask_planet=long(mask_planet_str)
;;3.5  Convert working_dir string
working_dir=working_dir_str
;;3.6  Convert common_data_root_dir string
common_data_root_dir=common_data_root_dir_str
;;3.7  Convert kid_fits_filenames string array
IF(N_ELEMENTS(result) GE 7) THEN BEGIN
    kid_fits_filenames=strsplit(kid_fits_filenames_str,",",/extract,count=count_kid_fits_filenames_str)
    print,count_kid_fits_filenames_str
    if count_kid_fits_filenames_str le 0 then begin
        print,systime()+"|ERROR|idlvm_driver_for_compute_qats|You did not specify any valid fits lightcurve files for this KID.  Exiting program now."
        return
    endif
endif

help,kid
help,f
help,prange
print,prange
help,mask_planet
help,working_dir
help,common_data_root_dir
IF(N_ELEMENTS(result) GE 7) THEN help,kid_fits_filenames

compute_qats, $
  kid,$
  f,$
  prange,$
  mask_planet=mask_planet,$
  working_dir=working_dir,$
  common_data_root_dir=common_data_root_dir,$
  kid_fits_filenames=kid_fits_filenames

return

error_clause: begin
    print,systime()+'|ERROR|idlvm_driver_for_compute_qats|I/O error detected.  Check values of arguments.'
    print,"Example run: idl -vm=./idlvm_driver_for_compute_qats.sav -args 1432214 0.005 '[1.0,300.0]' 1 '/astro/net/astro-agol/blevlee/CODE/condor/test3/test_working_dir' '/astro/net/astro-agol/blevlee/CODE/IDL/KEPLER_REDUX/autopulse'  'path_to_lightcurve_fits_file1,path_to_lightcurve_fits_file2,path_to_lightcurve_fits_file3,...'"
end

END
