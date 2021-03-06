1 ' Life v2
2 DEF SEG=&H1900:BLOAD"life.bld",0:DEF USR0=&HA4
3 ' Copyright 2017, Ted Felix
4 ' File date: 4/6/1983 11:00:00 PM
5 DEFINT A-Z:DIM L$(22):DX=243:DY=243:STRIG ON
10 SCREEN 0:CLS:WIDTH 40:PRINT">> Life v2 <<"
20 PRINT"By: Ted Felix
25 INPUT"Instructions (Y/N)";A$
26 A$=LEFT$(A$,1):IF A$="y"OR A$="Y"THEN GOSUB 7000
35 INPUT"Joystick (Y/N)";A$:A$=LEFT$(A$,1):IF A$="Y"OR A$="y"THEN JOYST=1
40 INPUT"Printer (Y/N)";A$:A$=LEFT$(A$,1):IF A$="Y"OR A$="y"THEN PR=1 ELSE PR=0
50 CLS:X=1:Y=1:LX=MX:LY=MY:GX=0:GY=0
60 PS=32:GOSUB 1000:FOR I=1 TO 1000:NEXT
70 GEN=1
75 IF PR THEN LOCATE 23,1:PRINT"Printout (Y/N)?";:A$=INPUT$(1):IF A$="Y"OR A$="y"THEN GOSUB 5000
80 LOCATE 23,1:PRINT">> Life <<     ";
90 LOCATE 24,1:PRINT"Generation:";GEN;
100 TEP=STRIG(0):TEP=STRIG(4):GOTO 3000
999 ' Set-up screen
1000 IF JOYST=1 THEN 1200
1002 A$=INKEY$:IF A$="" THEN 1002
1005 LOCATE Y,X:PRINT CHR$(PS);
1010 IF A$="8" THEN Y=Y-1:IF Y<1 THEN Y=1
1020 IF A$="2" THEN Y=Y+1:IF Y>21 THEN Y=21
1030 IF A$="4" THEN X=X-1:IF X<1 THEN X=1
1040 IF A$="6" THEN X=X+1:IF X>40 THEN X=40
1050 PS=SCREEN(Y,X):LOCATE Y,X:PRINT"�";
1070 IF A$=CHR$(13) THEN LOCATE Y,X:PRINT CHR$(2);:PS=2
1080 IF A$=" " THEN LOCATE Y,X:PRINT" ";:PS=32
1090 IF A$=CHR$(27) THEN IF PS<>2 THEN LOCATE Y,X:PRINT" ";:RETURN ELSE LOCATE Y,X:PRINT CHR$(2);:RETURN
1100 GOTO 1002
1199 ' Joystick
1200 LOCATE Y,X:PRINT CHR$(PS);
1210 X=STICK(0)*(40/DX)+.5:IF X<1 THEN X=1 ELSE IF X>40 THEN X=40
1220 Y=STICK(1)*(21/DY)+.5:IF Y<1 THEN Y=1 ELSE IF Y>21 THEN Y=21
1230 PS=SCREEN(Y,X):LOCATE Y,X:PRINT"�";
1240 IF STRIG(5) THEN FOR I=1 TO 500:NEXT:IF PS=2 THEN LOCATE Y,X:PRINT CHR$(32);:PS=32 ELSE LOCATE Y,X:PRINT CHR$(2);:PS=2
1250 IF STRIG(1) THEN IF PS<>2 THEN LOCATE Y,X:PRINT" ";:RETURN ELSE LOCATE Y,X:PRINT CHR$(2);:RETURN
1260 GOTO 1200
1500 RETURN
2999 ' Life
3000 X=USR(0)
3700 GEN=GEN+1:LOCATE 24,1:PRINT"Generation:"GEN;
3799 ' Read keyboard
3800 A$=INKEY$
3810 IF A$="P"OR A$="p"THEN GOSUB 5000
3990 IF A$=CHR$(27) OR STRIG(0)THEN RUN 5
4000 IF A$=CHR$(13) OR STRIG(4)THEN Y=1:X=1:GOSUB 1000:FOR I=1 TO 1000:NEXT:GOTO 70
4010 IF LY-1<1 THEN LY=1 ELSE LY=LY-1
4020 IF LX-1<1 THEN LX=1 ELSE LX=LX-1
4030 IF GX+1>MX THEN GX=MX ELSE GX=GX+1
4040 IF GY+1>MY THEN GY=MY ELSE GY=GY+1
4100 GOTO 3000
4999 ' Print
5000 LPRINT">> Life <<"
5005 LPRINT CHR$(27)"W"CHR$(1);:LOCATE 23,15:PRINT"*** Working ***";
5010 FOR Y=1 TO 22:L$(Y)=""
5020 FOR X=1 TO 40
5030 IF SCREEN(Y,X)=2 THEN L$(Y)=L$(Y)+"*"ELSE L$(Y)=L$(Y)+" "
5040 NEXT:NEXT:LOCATE 23,15:PRINT"                  ";
5050 FOR L=1 TO 22:LPRINT L$(L);:NEXT
5060 LPRINT CHR$(27)"W"CHR$(0)
5070 LPRINT"Generation:";GEN
5080 LPRINT STRING$(80,"-")
5090 RETURN
6999 ' Instructions
7000 CLS:PRINT"Life v2  (Machine Language)"
7010 PRINT:PRINT"The life board is set-up by moving the
7020 PRINT"cursor with the cursor keys in the
7030 PRINT"Num-Lock state.  To put down a person,
7040 PRINT"press ENTER.  To kill a person, press
7050 PRINT"the space bar.  Press Esc to begin
7060 PRINT"Life.  While Life is working, you can
7070 PRINT"press `P' for a printout of the next
7080 PRINT"generation.  You can press ENTER to
7090 PRINT"change the screen after the next
7100 PRINT"generation or press Esc to restart the
7110 PRINT"program.
7140 PRINT:PRINT"Have fun."
7150 PRINT:PRINT
7160 RETURN
8999 ' Save routine
9000 SAVE"b:life"
9010 DEF SEG=&H1900
9020 BLOAD"life.bld",0
9030 BSAVE"b:life.bld",0,&H116

