;+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;		Begin get_path_name
;
;  Return the path and the name of a given absolute name
;  The specifier between directories is "/"
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro get_path_name,full_name,path,name
;
path=''
name='False'
start_pos=0
while name eq 'False' do begin
	slash_pos=strpos(full_name,'/',start_pos)
	next_slash_pos=strpos(full_name,'/',slash_pos+1)
	if next_slash_pos eq -1 then begin
		name_len=strlen(full_name)-slash_pos-1	
		name=strmid(full_name,slash_pos+1,name_len)
	endif else begin
		dir_name_len=next_slash_pos-slash_pos
		path=path+strmid(full_name,slash_pos,dir_name_len)
		start_pos=next_slash_pos
	endelse
endwhile
;
end; end get_path_name()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; End get_path_name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-
