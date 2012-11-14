;+
;; @brief Calculate the value of the Bayesian Information Criterion
;; @author Brian L. Lee
;; @date 27Sep2012
;;
;; @param_in k: The number of free parameters
;; @param_in n: The number of data points
;; @param_in chisq:  The total chi-squared
;-

function calc_bic, $
                   k=k, $
                   n=n, $
                   chisq=chisq
return,chisq+k*alog(n)
end
