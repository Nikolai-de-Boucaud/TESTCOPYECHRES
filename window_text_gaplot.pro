;restore, '$IDLSOURCE/ga_plot_new/ga_plot.compile'

pro text_event_gaplot, ev
common wintxtech, prtr
Widget_Control, ev.top, get_uvalue=file
Widget_Control, ev.id, get_uvalue=uval

case uval of
	'close': widget_control, ev.top, /destroy

	'setptr': begin
		widget_control, ev.id, get_value=prtr
		print, '  *** Changed default text file printer to ' + prtr
	end
	
	'print': begin
		if strtrim(PDatNBI.text_printer_name) eq '' then s=dialog_message('Set printer name in window first.') $
		else begin
			spawn, 'lp -d' + PDatNBI.text_printer_name + ' ' + file
			print, "  *** Printed file '" + file + "' to printer " + PDatNBI.text_printer_name
		endelse
	end

endcase

end

pro window_text_gaplot, file
common wintxtech, prtr
b=strarr(3000)
fh=file_search(file)
if fh[0] ne '' then begin
	openr, lun, fh[0], /get_lun
	width=0
	line=''
	i=0
	while not EOF(lun) do begin
		readf, lun, line
		b[i++]=line
		l=strlen(line)
		if l gt width then width=l
	endwhile
	close, lun
	free_lun, lun
	width=min([width, 104])
	width=max([width, 64])
	j=where(b ne '')
	if j[0] ge 0 then b=b[0:max(j)+1]
endif else print, '  !!! window_text_ech: File '+file+' not found.'

if not xregistered('text_ech') then begin
	wWin=widget_base(/column, title=title)
	wBase=widget_base(title=file, /column)
	wBaseActions=widget_base(wBase, /row)
	;wSave=widget_button(wBaseActions, value='Save as', uvalue='save')
	prtr=getenv('PRINTER')
	wPrinterName=cw_field(wBaseActions, title='  Printer:', xsize=12, value=prtr, $
		uvalue='setptr', /return_events, /string)
	wPrint=widget_button(wBaseActions, value=' Print ', uvalue='print')
	wClose=widget_button(wBaseActions, value=' Close ', uvalue='close')
	wBaseText=widget_base(wBase)
	wText=widget_text(wBaseText, xsize=80, ysize=30, value=b, /scroll)
	widget_control, wBase, /realize, set_uvalue=file
	xmanager, 'text_ech', wBase, event_handler='text_event_gaplot', /no_block	
endif

end
