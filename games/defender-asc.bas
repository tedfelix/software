1 ' Copyright 2017, Ted Felix
2 ' File date: 4/17/1983 11:00:00 PM
10 DEF SEG=&H1900:DEF USR=0
20 BLOAD"sidegrph.bld",0
30 STRIG ON
100 CLS:DIM SH(12)
110 SCREEN 1:COLOR 0,0
120 LINE(0,0)-(0,8)
130 LINE(0,8)-(16,8)
140 LINE(2,0)-(6,4)
150 LINE(7,4)-(16,4)
160 LINE(16,4)-(18,6)
170 PSET(17,7):PSET(1,0)
180 GET(0,0)-(18,8),SH
1000 Y=180:Y1=180
1010 FOR X=315 TO 319
1020 Y=Y+INT(RND*7)-3:IF Y>199 THEN Y=199
1025 IF Y<100 THEN Y=100
1030 IF X=315 THEN LINE(X-1,Y1)-(X,Y),1 ELSE LINE-(X,Y),1
1040 NEXT:Y1=Y
1060 IF STRIG(5)THEN LINE(20,SY+7)-(319,SY+7),2:LINE(20,SY+7)-(319,SY+7),0
1090 PY=SY:SY=STICK(0):SY=STICK(1)/1.35
1100 PUT(0,PY),SH:X=USR(0)
1110 PUT(0,SY),SH:GOTO 1010
