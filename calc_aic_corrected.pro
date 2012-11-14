;+
;; @brief Calculate the value of the Akaike Information Criterion,
;; corrected for small sample sizes (Sugiura 1978, Liddle 2008)
;; @author Brian L. Lee
;; @date 10Oct2012
;;
;; @param_in k: The number of free parameters
;; @param_in chisq:  The total chi-squared
;-

function calc_aic_corrected, $
                             k=k, $
                             n=n, $
                             chisq=chisq
return, chisq + 2*k + 2*k*(k+1)/(n-k-1)
end
