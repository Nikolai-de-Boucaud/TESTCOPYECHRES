pro update_anglesetter
; updates the data in the anglesetter window

Common ECHCOM	; defined in reg.pro
Common WIDGETIDS, WidIDs

if not xregistered('angles') then return	; anglesetter window not open

Common ANGCOM, dP, dG, blankP, blankG, AngIDs

for dd=0, n_elements(dP)-1 do dP[dd]=blankP
for dd=0, n_elements(dG)-1 do dG[dd]=blankG
set_angles, AngIDs

return

end
