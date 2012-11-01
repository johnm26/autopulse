;+
;; @brief Function to read in the cotrending basis vectors and apply
;; them to sap_flux.
;; @author Eric Agol, Brian L. Lee
;; @date 14Sep2012
;;
;; Summary of actions
;;0.  Check log file
;;1.  Integrity checks
;;2.  Set up internal variables
;;3.  Loop over quarters
;;3.1  Build CBV's, matching quarters and channels to input light
;;curve
;;3.2  Clean CBV's
;;3.3  Apply CBV's
;;4.  Return
;-
function make_cbv_corrected_lightcurve, $
                                        time=time, $
                                        flux=flux, $
                                        err_flux=err_flux, $
                                        channel=channel, $
                                        quarter=quarter, $
                                        cbv_data_dir=cbv_data_dir, $
                                        log_lun=log_lun

PRINT, SYSTIME(/UTC), "|Running make_cbv_corrected_lightcurve"

;;=============================================================================
;;0.0. Check log file
;;=============================================================================
IF SIZE(log_lun, /TYPE) EQ 0 THEN BEGIN
    log_lun = -1l
ENDIF
;;=============================================================================
;;1.  Integrity checks on inputs
;;=============================================================================
index_uniq_quarters=uniq(quarter,sort(quarter))
uniq_quarters=quarter[index_uniq_quarters]
n_quarters=n_elements(uniq_quarters)
;;=============================================================================
;;2.  Set up internal variables
;;=============================================================================
;;=============================================================================
;;3.  Loop over quarters to apply median normalization.  The loop only
;;goes over the quantity of quarters available (it does not start at
;;quarter 0)
;;=============================================================================
is_first_valid_quarter=1
for i=0L,n_quarters-1 do begin
;;3.1  Set up indices into arrays for this quarter
                                ;print,uniq_quarters[i]
;;3.1.1  Get indices that match this quarter (=uniq_quarters[i])
    index_this_quarter=where(quarter eq uniq_quarters[i])
;;3.1.2  Figure out which channel was used this quarter
    channels_this_quarter=channel[index_this_quarter]
    index_uniq_channels_this_quarter=uniq(channels_this_quarter,sort(channels_this_quarter))
    uniq_channels=channels_this_quarter[index_uniq_channels_this_quarter]
    n_channels=n_elements(uniq_channels)
    if n_channels ne 1 then begin
        print,'ERROR|make_cbv_corrected_lightcurve.pro|There was not exactly one channel used for this KID in this quarter.'
        stop
    endif
;;3.1.3  Read in the CBV for this quarter and channel
    cbv_read,uniq_quarters[i],uniq_channels[0],cbv
    if size(cbv,/type) ne 8 then begin
        err_msg = SYSTIME(/UTC) + "|WARN|make_cbv_corrected_lightcurve|No CBV data was available for quarter="+string(uniq_quarters[i])+", channel="+string(uniq_channels[0])+", so we are excluding this segment of light curve."
        PRINT, err_msg
        PRINTF, log_lun, err_msg
        ;;SAVE, FILENAME='error_make_sql_query_macro.sav', /ALL
        ;;RETURN, err_msg
    endif else begin
;;3.1.4  Extract the light curve values for this quarter
        time_quarter_i=time[index_this_quarter]
        flux_quarter_i=flux[index_this_quarter]
        err_flux_quarter_i=err_flux[index_this_quarter]
;;3.2  Normalize this quarter's flux to 1.0 using the median flux
        normalized_flux_quarter_i=flux_quarter_i/median(flux_quarter_i)
        index_nan_err_flux=where(finite(err_flux_quarter_i) eq 0, count_nan_err_flux, complement=index_finite_err_flux)
        if count_nan_err_flux gt 0 then err_flux_quarter_i[index_nan_err_flux] = median(err_flux_quarter_i[index_finite_err_flux])
        normalized_err_flux_quarter_i=err_flux_quarter_i/median(flux_quarter_i)
;;3.3  Concatenate each quarter's values together
;;3.3.1  Initialize the concatenation with the first available quarter
        if is_first_valid_quarter then begin
            time_concat=time_quarter_i
            normalized_flux_concat=normalized_flux_quarter_i
            normalized_err_flux_concat=normalized_err_flux_quarter_i
            vec1=cbv.vector_1
            vec2=cbv.vector_2
            vec3=cbv.vector_3
            vec4=cbv.vector_4
            vec5=cbv.vector_5
            vec6=cbv.vector_6
            vec7=cbv.vector_7
            vec8=cbv.vector_8
            qtr=quarter[index_this_quarter]
            is_first_valid_quarter=0
;;3.3.2  Continue the concatenation by joining on to the previous quarters
        endif else begin
            time_concat=[time_concat,time_quarter_i]
            normalized_flux_concat=[normalized_flux_concat,normalized_flux_quarter_i]
            normalized_err_flux_concat=[normalized_err_flux_concat,normalized_err_flux_quarter_i]
            vec1=[vec1,cbv.vector_1]
            vec2=[vec2,cbv.vector_2]
            vec3=[vec3,cbv.vector_3]
            vec4=[vec4,cbv.vector_4]
            vec5=[vec5,cbv.vector_5]
            vec6=[vec6,cbv.vector_6]
            vec7=[vec7,cbv.vector_7]
            vec8=[vec8,cbv.vector_8]
            qtr=[qtr,quarter[index_this_quarter]]
        endelse
    endelse
endfor
;;=============================================================================
;;4.  Clean non-finite fluxes out of all arrays
;;=============================================================================
index_finite_normalized_flux = where(finite(normalized_flux_concat) eq 1 and normalized_flux_concat ge 0)
time_concat=time_concat[index_finite_normalized_flux]+54833d0
normalized_flux_concat=normalized_flux_concat[index_finite_normalized_flux]
normalized_err_flux_concat=normalized_err_flux_concat[index_finite_normalized_flux]
vec1=vec1[index_finite_normalized_flux]
vec2=vec2[index_finite_normalized_flux]
vec3=vec3[index_finite_normalized_flux]
vec4=vec4[index_finite_normalized_flux]
vec5=vec5[index_finite_normalized_flux]
vec6=vec6[index_finite_normalized_flux]
vec7=vec7[index_finite_normalized_flux]
vec8=vec8[index_finite_normalized_flux]
qtr=qtr[index_finite_normalized_flux]
weight=1d0/normalized_err_flux_concat^2
ntime=n_elements(time_concat)
fflat=dblarr(n_elements(normalized_flux_concat))
correction=dblarr(n_elements(normalized_flux_concat))
;;=============================================================================
;;5.  Loop over quarters to apply CBV per quarter
;;=============================================================================
for i=0L,n_quarters-1 do begin
    iqtr=uniq_quarters[i]
    index_qtr=where(qtr eq iqtr, count_qtr)
    if count_qtr gt 0 then begin
        flux_quarter=normalized_flux_concat[index_qtr]
        median_flux_quarter=median(flux_quarter)
        flux_quarter=flux_quarter/median_flux_quarter - 1d0
        coeff=regress(transpose([[vec1[index_qtr]],[vec2[index_qtr]],[vec3[index_qtr]],[vec4[index_qtr]],[vec5[index_qtr]]]),flux_quarter)
        correction1=median_flux_quarter*(coeff[0]*vec1[index_qtr]+coeff[1]*vec2[index_qtr]+coeff[2]*vec3[index_qtr]+coeff[3]*vec4[index_qtr]+coeff[4]*vec5[index_qtr])
        flux_quarter=normalized_flux_concat[index_qtr]-correction1+median(correction1)
        fflat[index_qtr]=flux_quarter/median(flux_quarter)
        correction[index_qtr]=correction1-median(correction1)
    endif
endfor
;;=============================================================================
;;6.  Plot the CBV-corrected light curve
;;=============================================================================
;plot,time_concat,fflat,ys=1,psym=3
;oploterror,time_concat,fflat,normalized_err_flux_concat,psym=3,/nohat
;oplot,time_concat,fflat,psym=3,color=255
;;=============================================================================
;;7.  Overwrite the trimmed, CBV-corrected vectors into the original
;;input variable names.
;;=============================================================================
time=time_concat
flux=fflat
err_flux=normalized_err_flux_concat
;;=============================================================================
;;8.  Return success.
;;=============================================================================
return,1
end
