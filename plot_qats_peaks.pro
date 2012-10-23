;+
;; @brief This is a program to plot the QATS peaks from
;; qats_depth_dur.sav as a colourmap.
;; @author Brian L. Lee
;; @date 16Oct2012
;-

pro plot_qats_peaks, $
                     saved_file=saved_file
;;1.  Restore saved file
if file_test(saved_file) then begin
    restore,saved_file
endif else begin
    print,systime(/UTC)+'|ERROR|plot_qats_peaks|Could not find the file '+saved_file
endelse
;;2.  Plot data
;;2.1  Load colour scheme
LoadCT, 39                      ; Rainbow+white
c = { black : 	0,  $
      blue : 	.25*!d.n_colors, $
      ltblue : 	.40*!d.n_colors, $
      green : 	.65*!d.n_colors, $
      yellow : 	.75*!d.n_colors, $
      orange : 	.80*!d.n_colors, $
      red : 	.90*!d.n_colors, $
      white : 	!d.n_colors-1   }
ps=1
if keyword_set(ps) then begin                                     
    mydevice=!d.name
    set_plot, 'ps'
    device,filename='plot_qats_peaks.ps',/color,xsize=21,ysize=27,yoffset=0,xoffset=0
endif
;;2.2  Set up custom symbol
X = [-1,  1,  1, -1, -1]
Y = [-1, -1,  1,  1, -1]
USERSYM, X, Y, /FILL
symscale=1.5
;;2.3  Set up the plot window axes
plot, $
  tdur, $
  depth, $
  xtitle='Trial duration (d)', $
  ytitle='Trial fractional depth', $
  /ylog, $
  yrange=[min(depth[where(depth gt 0)])/1.5,1.5*max(depth)], $
  ystyle=1, $
  title=file_basename(file_dirname(file_dirname(saved_file)))+PATH_SEP()+file_basename(file_dirname(saved_file))+PATH_SEP()+file_basename(saved_file),$
  /nodata
;;2.4  Loop through the sntot array and plot the peaks
all_period_grids=sntot[0,*,*,*]
for idur=0L,ndur-1 do begin
    for idepth=0L,ndepth-1 do begin
        tdur_curr=tdur[idur]
        depth_curr=depth[idepth]
        period_grid=sntot[0,idepth,idur,*]
        period_power_envelopenormalized=sntot[2,idepth,idur,*]
        best_power=max(period_power_envelopenormalized,index_best_power)
        best_period=period_grid[index_best_power]

        zero_periods=where(period_grid eq 0.0,count_zero_periods)
        if count_zero_periods lt n_elements(period_grid) then begin
;        print,tdur_curr,depth_curr,alog10(best_power),alog10(best_period)/(alog10(max(period_grid))-alog10(min(period_grid[where(period_grid gt 0.0)])))*!d.n_colors
            oplot,$
              replicate(tdur_curr,2),$
              replicate(depth_curr,2),$
              color=alog10(best_period)/(alog10(max(all_period_grids))-alog10(min(all_period_grids[where(all_period_grids gt 0.0)])))*!d.n_colors,$
              symsize=symscale*alog10(best_power),$
              psym=8
            
        endif
    endfor
endfor
;;2.4  Add a legend
legend_ticks=2^(lindgen(9))
legend_labels=string(legend_ticks)
legend_labels='P='+strtrim(legend_labels,2)+'d'
legend_colors=alog10(legend_ticks)/(alog10(max(all_period_grids))-alog10(min(all_period_grids[where(all_period_grids gt 0.0)])))*!d.n_colors
legend_symbols=replicate(8,n_elements(legend_labels))
idlastro_legend,$
  legend_labels,$
  color=legend_colors,$
  psym=legend_symbols,$
  /right
sample_powers=2*(findgen(9)+1)
legend_sizes=symscale*alog10(sample_powers)
legend_labels=string(sample_powers)
legend_labels='QATS peak pow='+strtrim(legend_labels,2)
idlastro_legend,$
  legend_labels,$
  psym=legend_symbols,$
  symsize=legend_sizes,$
  /left,$
  /top

;;2.5  Close plot device
device,/close
set_plot,'x'
stop
end
