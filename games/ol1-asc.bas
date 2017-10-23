1 ' Copyright 2017, Ted Felix
2 ' File date: 3/15/1983 11∶00∶00
10 HE$=STRING$(3,3):H1$=CHR$(3)
200 CLS:PRINT HE$;:COLOR 0,7:PRINT OL$;:COLOR 7,0:PRINT HE$
210 LOCATE 12,1:PRINT"Press ENTER to play."
220 A$=INPUT$(1)
230 CLS:FOR I=1 TO 1000:PRINT H1$;:NEXT
240 IF T$="M"THEN P$="woman":PR$="She":LPR$="she"
250 IF T$="F"THEN P$="man":PR$="He":LPR$="he"
260 PRINT"You are not alone, a "P$" is near by."
270 INPUT"What now";A$:PRINT PR$;" is wierd."
280 GOTO 270

