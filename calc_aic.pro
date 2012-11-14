;+
;; @brief Calculate the value of the Akaike Information Criterion
;; @author Brian L. Lee
;; @date 27Sep2012
;;
;; @param_in k: The number of free parameters
;; @param_in chisq:  The total chi-squared
;-

function calc_aic, $
                   k=k, $
                   chisq=chisq
return,chisq+2*k
end
