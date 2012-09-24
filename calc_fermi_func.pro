;+
;; @brief This program calculates a Fermi function.
;; @author Brian L. Lee
;; @date 21Sep2012
;;
;; @param_in x: The independent variable (scalar or vector)
;; @param_in p: The array of parameters for the Fermi function:
;; p[0]=mu
;; p[1]=kT
;; p[2]=amplitude scaling
;; p[3]=baseline offset
;; @param_out ymod:  The returned dependent variable values
;;
;; @example calc_fermi_func,indep_x_values_vec,[1.5,0.15,-0.25,4.0],return_values_vec
;-
pro calc_fermi_func, x, p, ymod
ymod=p[3] + p[2]/( exp((x-p[0])/p[1]) + 1 )
end
