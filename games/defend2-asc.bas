1 ' Copyright 2017, Ted Felix
2 ' File date: 4/16/1983 08:43:27 AM
10 DEF SEG=&H1900:DEF USR=0
20 BLOAD"sidegrph.bld",0
1000 Y=180:Y1=180
1010 FOR X=315 TO 319
1020 Y=Y+INT(RND*5)-2:IF Y>199 THEN Y=199
1030 IF X=315 THEN LINE(X-1,Y1)-(X,Y) ELSE LINE-(X,Y)
1040 NEXT:Y1=Y
1050 X=USR(0):GOTO 1010
