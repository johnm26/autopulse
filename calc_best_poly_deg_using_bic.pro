;+
;; @brief This function applies the BIC to input data, to
;; determine the best polynomial order to use to model the input data.
;; @author Brian L. Lee
;; @date 17Oct2012
;-
function calc_best_poly_deg_using_bic, $
                                       data_x=data_x, $
                                       data_y=data_y, $
                                       data_err_y=data_err_y, $
                                       max_trial_poly_deg=max_trial_poly_deg, $
                                       plot_to_screen=plot_to_screen
;For this subsection of data, determine the best order to use for modelling
k_start=2L
if ~keyword_set(max_trial_poly_deg) then max_trial_poly_deg=12L
if ~finite(max_trial_poly_deg) then max_trial_poly_deg=12L
if max_trial_poly_deg lt k_start then max_trial_poly_deg=12L
bic_array=dblarr(max_trial_poly_deg)
aic_array=dblarr(max_trial_poly_deg)
chisq_array=dblarr(max_trial_poly_deg)
chisq_prev_array=dblarr(max_trial_poly_deg)
n=n_elements(data_y)
!p.multi=[0,long(ceil(sqrt(max_trial_poly_deg))),long(ceil(sqrt(max_trial_poly_deg)))]
for k=k_start,k_start+max_trial_poly_deg-1 do begin
;;    coeffs=svdfit(data_x,data_y,k-1,measure_errors=data_err_y,chisq=chisq,/double,yfit=yfit,status=status,/legendre)
;;    coeffs=poly_fit(data_x,data_y,k-1,measure_errors=data_err_y,chisq=chisq,/double,yfit=yfit,status=status)
    coeffs=calc_polyfit_regress( $
                                 data_x=data_x, $
                                 data_y=data_y, $
                                 measure_errors=data_err_y, $
                                 poly_order=k-1, $
                                 chisq=chisq, $
                                 yfit=yfit, $
                                 plot_to_screen=0 $
                               )
                                ;print,'Fit status:'+string(status)
    if keyword_set(plot_to_screen) then plot,data_x,data_y,psym=7,symsize=0.4,title='N_coeffs in poly fit:'+strtrim(string(k),2),charsize=2,xtitle='x',ytitle='y',/ynozero
    if keyword_set(plot_to_screen) then oplot,data_x,yfit,color=255
    chisq_array[k-k_start]=chisq
    bic=calc_bic(k=k,n=n,chisq=chisq)
    bic_array[k-k_start]=bic
    aic=calc_aic_corrected(k=k,n=n,chisq=chisq)
    aic_array[k-k_start]=aic
                                ;print,'k:'+strtrim(string(k),2)+' n:'+strtrim(string(n),2)+' chisq:'+string(chisq)+' BIC:'+string(bic)+' AIC:'+string(aic)
    if keyword_set(plot_to_screen) then legend,['chisq:'+strtrim(string(chisq),2),'BIC:'+strtrim(string(bic),2),'AIC:'+strtrim(string(aic),2)],charsize=1.5
endfor
;if keyword_set(plot_to_screen) then dummy=get_kbrd()
!p.multi=[0,1,1]
dummy=min(bic_array,index_min_bic)
bic_chosen_poly_degree=index_min_bic+k_start-1
npoints_in_fitted_window=n
print,'BIC-chosen poly degree: ',index_min_bic+k_start-1,' total chisq: ',chisq_array[index_min_bic]
;;Review the best BIC-selected fit
;coeffs=svdfit(data_x,data_y,bic_chosen_poly_degree,measure_errors=data_err_y,chisq=chisq,/double,yfit=yfit,status=status,/legendre)
;coeffs=poly_fit(data_x,data_y,bic_chosen_poly_degree,measure_errors=data_err_y,chisq=chisq,/double,yfit=yfit,status=status)
coeffs=calc_polyfit_regress( $
                             data_x=data_x, $
                             data_y=data_y, $
                             measure_errors=data_err_y, $
                             poly_order=bic_chosen_poly_degree, $
                             chisq=chisq, $
                             yfit=yfit, $
                             plot_to_screen=0 $
                           )
bic=calc_bic(k=bic_chosen_poly_degree,n=n,chisq=chisq)
aic=calc_aic_corrected(k=k,n=n,chisq=chisq)
;;Replot the best BIC-selected fit
if keyword_set(plot_to_screen) then plot,data_x,data_y,psym=7,symsize=0.4,title='Selected polynomial order:'+strtrim(string(bic_chosen_poly_degree),2),charsize=2,xtitle='x',ytitle='y',/ynozero
if keyword_set(plot_to_screen) then oplot,data_x,yfit,color=255
if keyword_set(plot_to_screen) then legend,['chisq:'+strtrim(string(chisq),2),'BIC:'+strtrim(string(bic),2),'AIC:'+strtrim(string(aic),2)],charsize=1.5
return,bic_chosen_poly_degree
end
