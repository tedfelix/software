1 ' Copyright 2017, Ted Felix
2 ' File date: 12/23/1982 11:00:00 PM
5 DEF FNRAND(X)=INT(RND*X)+1
10 CLS :SCREEN 0 :WIDTH 40 :DIM BOARD$(30,20): GOSUB 4000
20 X=0 :INPUT"One or two players";PLAYERS
25 RANDOMIZE VAL(RIGHT$(TIME$,2)+MID$(TIME$,3,2))
30 MATCH=1
40 PRINT"The computer is setting up the board" :FOR Y=1 TO 20: FOR X=1 TO 30: BOARD$(X,Y)=" ": NEXT :NEXT
100 IF PLAYERS=1 THEN GOSUB 3000 :CLS :GOTO 2000
110 IF PLAYERS=2 THEN CPL=1 :SETUP=2 :GOTO 1000
999 REM  Player set up
1000 PRINT"Player";SETUP;"come here" :PRINT"Player";CPL;"turn your head"
1010 FOR I=1 TO 10
1020 PRINT"Mine #";I
1030 INPUT"Position x,y (1-30,1-20)";X,Y
1035 IF X>30 OR X<1 OR Y>20 OR Y<1 THEN 1020 ELSE IF BOARD$(X,Y)<>" " THEN 1020
1040 BOARD$(X,Y)="*"
1050 IF FNRAND(5)=3 THEN PRINT"I think player";CPL;"is peeking!"
1060 NEXT I
1070 PRINT"Thank you player";SETUP
1080 FOR I=0 TO 2000 :NEXT
1090 CLS :PRINT"Now tell player"CPL"that he can look"
1100 CLS :GOTO 2000
1999 REM  Play
2000 IF X=.5 THEN 5000 ELSE GOTO 2000
2100 PX=PX+XD :PY=PY+YD
2110 MD=0:FOR I=1 TO 10:MD=SQR((X(I)-PX)^2+(Y(I)-PY)^2):NEXT
2120 LOCATE 22,1:PRINT MD;
2205 IF PX>30 THEN PX=30 ELSE IF PY>20 THEN PY=20 ELSE IF PY<1 THEN PY=1 ELSE IF PX<1 THEN PX=1
2210 IF PY=LY AND PX=LX THEN 2000 ELSE IF BOARD$(PX,PY)<>" " THEN GOTO 5000
2220 BOARD$(PX,PY)="�" :LX=PX :LY=PY
2230 LOCATE PY,PX :PRINT BOARD$(PX,PY); :P=P+1 :GOTO 2000
2999 REM  Computer set up
3000 MINES=0
3010 X=FNRAND(30) :Y=FNRAND(20)
3020 IF BOARD$(X,Y)="*" THEN 3010
3030 BOARD$(X,Y)="*" :MINES=MINES+1:X(MINES)=X:Y(MINES)=Y:IF MINES>=10 THEN RETURN
3040 GOTO 3010
3999 REM  Keyboard set-up
4000 FOR I=11 TO 14: KEY(I) ON: NEXT: ON KEY(11) GOSUB 4100
4010 ON KEY(12) GOSUB 4110
4020 ON KEY(13) GOSUB 4120
4030 ON KEY(14) GOSUB 4130
4040 RETURN
4100 YD=-1: XD=0: RETURN 2100
4110 YD=0: XD=-1: RETURN 2100
4120 YD=0: XD=1: RETURN 2100
4130 YD=1: XD=0: RETURN 2100
4999 REM  BOOM!!
5000 X=.5 :FOR I=11 TO 12 :KEY(I) OFF :NEXT :LOCATE PY,PX :PRINT"*"
5010 LOCATE 21 :PRINT"BOOM!!!"
5020 PRINT"Nice job!!" :FOR I=0 TO 2000 :NEXT :CLS
5030 MATCH=MATCH+1 :IF MATCH>=5 THEN PRINT"Game over!" :GOTO 6000
6000 IF PLAYERS=1 THEN PRINT"You got";P :END
6010 PRINT"Player 1 has";P1
6020 PRINT"Player 2 has";P2

