1 ' Copyright 2017, Ted Felix
2 RANDOMIZE VAL(TIME$)+VAL(MID$(TIME$,4,2))+VAL(RIGHT$(TIME$,2))
3 ' File date: 4/12/1987 01:16:18 AM
5 STRIG ON:DEF FNRAND(X)=INT(RND*X)+1
10 GOTO 650
20 SCREEN 1:COLOR 0,0
40 DIM A(48)
50 CLS
60 GOSUB 7000
70 FOR I=4 TO 28:PSET(I,8):PSET(I,7):PSET(I,14):PSET(I,13):NEXT
80 FOR I=5 TO 12:PSET(17,I):NEXT
90 FOR I=14 TO 18:PSET(I,10):PSET(I,12):NEXT
100 FOR I=9 TO 12:PSET(9,I):PSET(25,I):NEXT
110 FOR I=13 TO 21:PSET(I,11):NEXT
120 PSET(17,9)
130 PSET(13,16):PSET(15,15):PSET(20,16):PSET(19,15)
140 PRESET(4,8):PRESET(28,8):PRESET(15,8):PRESET(19,8)
150 PRESET(4,14):PRESET(28,14)
160 GET(0,0)-(34,20),A
170 CLS
180 R=1:T=0:TT=0:S=0:MV=1
190 X=0:Y=0
200 X=FNRAND(200)-100:Y=FNRAND(180)-86
210 PX=X:PY=Y:PUT(113+PX,86+PY),A:A=0:B=0
220 PSET(128,86):PSET(128,106):PSET(108,96):PSET(148,96):PSET(128,85):PSET(128,107):PSET(106,96):PSET(150,96)
230 X=X-(INT(A/XS3)-2):Y=Y+INT(B/YS3)-2
240 T=T+1:IF T>84 GOTO 440
250 IF Y>75 THEN Y=75
260 IF Y<-75 THEN Y=-75
270 IF X<-112 THEN X=-112
280 IF X>108 THEN X=108
290 PUT(113+PX,86+PY),A,XOR:PUT(113+X,86+Y),A,XOR
300 IF STRIG(1) THEN 350
310 PX=X:PY=Y:X=X+FNRAND(5)-3:Y=Y+FNRAND(3)-2
320 MV=-MV:IF MV=1 THEN 240
330 A=STICK(0):B=STICK(1)
340 IF STRIG(1)=0 THEN 220
350 S=S+1:SOUND 50,2:PUT(113+X,86+Y),A,XOR
360 LINE(64,192)-(128,96):LINE(192,192)-(128,96)
370 LINE(64,192)-(128,96),0:LINE(192,192)-(128,96),0
380 IF X>-10 AND X<10 AND Y>-5 AND Y<5 THEN 400
385 PUT(113+X,86+Y),A,XOR:PX=X:PY=Y
390 GOTO 220
400 FOR I%=15 TO 30 STEP 2:CIRCLE(128,96),I%,,,,1:NEXT
410 FOR I%=15 TO 30 STEP 2:SOUND(200-I%*2),1:CIRCLE(128,96),I%,0,,,1:NEXT
420 H=H+1
430 FOR TM=1 TO 500:NEXT
440 CLS
460 PRINT TAB(6)"After "R"airplanes":PRINT
470 PRINT TAB(5)"Shots    ="S
480 PRINT TAB(5)"Hits     ="H
490 IF T<=84 THEN 510
500 PRINT"Too much time, he got away!":G=G+1
510 PRINT TAB(5)"Got away ="G
520 TT=TT+T:PRINT TAB(5)"Score    ="INT((250*R-TT)*SQR(TT/250*R)*SQR(H/(S+1)))
530 IF TT>1008 THEN 640
540 PRINT:PRINT TAB(5)"Time remaining"
550 PRINT"Minutes"TAB(16)"Seconds":M=INT((1008-TT)/168):SC=INT(((1008-TT)-M*168)/2.8)
560 PRINT TAB(2)M TAB(18)SC:PRINT
570 PRINT:PRINT"Press the firing button to cont."
580 IF STRIG(1)=0 THEN 580
590 CLS
620 T=0:R=R+1
630 GOTO 190
640 PRINT"Time has eluded your craft.":END
650 CLS:GOTO 20
5000 PRINT STICK(0);:GOTO 5000
6999 ' Read Joystick model
7000 OPEN"joystick.dat"FOR INPUT AS #1
7010 INPUT #1,U,D,R,L
7020 CLOSE #1
7030 XS3=(L+R)/5
7040 YS3=(U+D)/5
7090 RETURN
