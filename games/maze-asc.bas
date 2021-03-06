10 REM Maze game by Michael Gordon
20 REM For PC Magazine readers
40 DEFINT A-Y
50 DIFF=5
70 CLS:SCREEN 0:WIDTH 40
80 KEY OFF
90 PRINT"You are about to play a game of maze.
100 PRINT"To win a round, you must have the
110 PRINT"best (least) time.
130 PRINT"Difficulty is 1-10 with 5 being assumed.
150 PRINT"Difficulty? (1-10) [";DIFF;"]
160 INPUT IN:IF IN=0 THEN 200
170 DIFF=IN
180 IF DIFF=0 THEN DIFF=5:GOTO 200
190 IF DIFF<1 OR DIFF>10 THEN 150
200 Z1=.9000001-DIFF/20
210 Z2=.9000001-DIFF/20
310 RANDOMIZE (PEEK(&H46C)+PEEK(&H46D)*256)-32768!
320 SCREEN 1:COLOR 0,3,1
330 CLS
340 FOR X=10 TO 300 STEP 10
350 FOR Y=0 TO 190 STEP 10
360 PSET(X,Y),3
370 IF RND>Z1 THEN DRAW"d10"
380 IF RND>Z2 THEN DRAW"r10"
390 NEXT:NEXT
400 REM Make a box for the timer
410 LINE(0,0)-(320,200),3,B
420 LINE(1,0)-(80,10),0,BF
430 LINE(1,10)-(80,10),3
440 LINE(80,1)-(80,10),3
450 IF RESTART=0 THEN TIME$="0"
460 RESTART=0
470 REM ----- Part Two -----
480 PX=5:PY=95:REM Initial Position
490 CIRCLE(PX,PY),3,3
500 D$=INKEY$:IF D$<>""THEN 530
510 LOCATE 1,2:PRINT TIME$
520 REM Check SPACE bar
530 IF D$=" "THEN RESTART=1:GOTO 320
540 D=LEN(D$)
550 IF D<>2 THEN 500
560 D=ASC(MID$(D$,2,1))
570 IF D=72 THEN 620
580 IF D=75 THEN 670
590 IF D=77 THEN 720
600 IF D=80 THEN 770
610 GOTO 500
620 Y=POINT(PX,PY-5)
630 IF Y=3 GOTO 500
640 GOSUB 830
650 PY=PY-10:IF PY<0 THEN PY=5
660 GOTO 490
670 Y=POINT(PX-5,PY)
680 IF Y=3 THEN 500
690 GOSUB 830
700 PX=PX-10:IF PX<0 THEN PX=5
710 GOTO 490
720 Y=POINT(PX+5,PY)
730 IF Y=3 THEN 500
740 GOSUB 830
750 PX=PX+10:IF PX>300 THEN 860
760 GOTO 490
770 Y=POINT(PX,PY+5)
780 IF Y=3 THEN 500
790 GOSUB 830
800 PY=PY+10:IF PY>200 THEN PY=195
810 GOTO 490
830 CIRCLE(PX,PY),3,0
840 CIRCLE(PX,PY),2,1:RETURN
850 REM Winner
860 LINE(100,0)-(300,10),0,BF
870 LINE(100,0)-(100,10),3
880 LINE(100,10)-(300,10),3
890 LOCATE 1,15:FOR I=1 TO 1000:A$=INKEY$:NEXT
900 PRINT"Got it! Another game?";
910 A$=INPUT$(1):IF A$="y"OR A$="Y"THEN 70
920 SCREEN 0:WIDTH 40:KEY ON

