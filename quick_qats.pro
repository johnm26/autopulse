;+
; NAME: QUICK_QATS
;
;
;
; PURPOSE: Detect box-like events of fixed width q (in index units)
; and depth in a sequence, x, of zero-median values. The interval between
; successive boxes is optimally determined constrained between
; DeltaMin and DeltaMax (in index units).  The total box signal is
; returned. The indices of the instants of the optimal transits are
; also returned.  
;
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
; SBest = QUICK_QATS(d,q,DeltaMin,DeltaMax,indices)
;
;
;
; INPUTS: d - convolved array to be searched, q - box duration, DeltaMin -
;             minimum interval, DeltaMax - maximum interval
;
;
;
; OPTIONAL INPUTS: 
;                     
;                         
;
;
;
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:SBest - Maximum signal, indices  -
;                    indices of instants of boxes (corresponding to indices of d)
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY: First written 10/18/2012 - JAC
;
;-

FUNCTION shConvol,x,q

  PARITY = -1
  N = n_elements(x)

  cv = PARITY*convol(x,replicate(1d0,q),center=0)
  return,cv((q-1):*)

END

FUNCTION maxgammaN,G,i,DeltaMin,DeltaMax,q,N,index=index
  
  a = 0
  ap = DeltaMax-q
  b = N-q
  bm = N-DeltaMax
  lb = (i-DeltaMax gt a ? i-DeltaMax : a) 
  if (ap ge a+DeltaMin-1) then begin

     
     if i ge a and i le a+DeltaMin-1 then begin
        index = a-1
        return,0
     endif
     if i ge a+DeltaMin and i le ap then begin
        mm = max([0,G(lb:(i-DeltaMin))],A)
        index = (A eq 0 ? -1 : lb+A-1)
        return, mm
     endif
     if i ge ap+1 and i le b then begin
        mm = max(G(lb:(i-DeltaMin)),A)
        index = A+lb
        return,mm
     endif

  endif else begin
     
     if i ge a and i le ap then begin
        index = a-1
        return,0
     endif
     if i ge ap+1 and i le a+DeltaMin-1 then begin
        index = -2
        return,0
     endif
     if i ge a+DeltaMin and i le b then begin
        mm = max(G(lb:(i-DeltaMin)),A)
        index = A+lb
        return, mm
     endif
  endelse

end
     

;;;;;;
;;start debug
;;;;;;
FUNCTION maxgammaN_debug,G,i,DeltaMin,DeltaMax,q,N,index=index
  
  a = 0
  ap = DeltaMax-q
  b = N-q
  bm = N-DeltaMax
;  print,'In maxgammaN_debug 1',index
  lb = (i-DeltaMax gt a ? i-DeltaMax : a) 
;  print,'In maxgammaN_debug 2',index,lb,ap,a+DeltaMin-1,a
  if (ap ge a+DeltaMin-1) then begin
;      print,'In maxgammaN_debug 3',index,lb,ap,a+DeltaMin-1,a
     
     if i ge a and i le a+DeltaMin-1 then begin
        index = a-1
;        print,'In maxgammaN_debug 4',index,lb,ap,a+DeltaMin-1,0
        return,0
     endif
     if i ge a+DeltaMin and i le ap then begin
        mm = max([0,G(lb:(i-DeltaMin))],A)
        index = (A eq 0 ? -1 : lb+A-1)
;        print,'In maxgammaN_debug 5',index,lb,ap,a+DeltaMin-1,mm
        return, mm
     endif
;     print,'In maxgammaN_debug 6',i,ap+1,i,b,a
     if i ge ap+1 and i le b then begin
;        print,'In maxgammaN_debug 7',a,lb,i-DeltaMin
        mm = max(G(lb:(i-DeltaMin)),A)
;        print,'In maxgammaN_debug 8',index,lb,ap,a,DeltaMin,a+DeltaMin-1,mm
        index = A+lb
;        print,'In maxgammaN_debug 9',index,lb,ap,a+DeltaMin-1,mm
        return,mm
     endif

  endif else begin
     
     if i ge a and i le ap then begin
        index = a-1
        return,0
     endif
     if i ge ap+1 and i le a+DeltaMin-1 then begin
        index = -2
        return,0
     endif
     if i ge a+DeltaMin and i le b then begin
        mm = max(G(lb:(i-DeltaMin)),A)
        index = A+lb
        return, mm
     endif
  endelse

end
;;;;;;
;;end debug
;;;;;;
     
FUNCTION gn,d,DeltaMin,DeltaMax,q
  
  N = n_elements(d)
  BIG = 2000d0
  g = replicate(-BIG,N)

  i = dindgen(N)
  omega = where(floor(i/DeltaMin)-floor((i+q-1)/DeltaMax) ge 0 and $
                floor((N-q-i)/DeltaMin)- floor((N-i-1)/DeltaMax) ge 0)
  g(omega) = d(omega)
  
  return,g
end

FUNCTION quick_qats,d,DeltaMin,DeltaMax,q,indices

  gI = gn(d,DeltaMin,DeltaMax,q)
  N = n_elements(gI)
  BIG = 2000d0
  a = 0
  ap = DeltaMax-q
  b = N-q
  bm = N-DeltaMax
  
  G = gI

  for i=0L,N-1,1 do begin
     if (ap ge a+DeltaMin-1) then G(i) += maxgammaN(G,i,DeltaMin,DeltaMax,q,N)
     if (ap lt a+DeltaMin-1) then begin
        if (i ge a and i le ap) or (i ge a+DeltaMin and i le b) then G(i) += maxgammaN(G,i,DeltaMin,DeltaMax,q,N)
        if (i ge ap+1 and i le a+DeltaMin-1) then G(i) = -BIG
     endif
  endfor

  Gmax = max(G(bm:b),mu)

  mu += bm

  mus = [mu]
  icountdebug=0
  while mu ne a-1 do begin
      icountdebug+=1
;     if icountdebug eq 25 then begin
;         mm = maxgammaN_debug(G,mu,DeltaMin,DeltaMax,q,N,index=mu)
;         print,icountdebug,mm,mu,a-1
;     endif else begin
         mm = maxgammaN(G,mu,DeltaMin,DeltaMax,q,N,index=mu)
         print,icountdebug,mm,mu,a-1
;     endelse
     mus = [mus,mu]
  endwhile
stop
  M = n_elements(mus)-1

  indices = lindgen(M)

  for mm = 1, M,1 do indices(mm-1) = mus[M-mm]
  
  return, GMax/sqrt(1.0*M*q)
end
