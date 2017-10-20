5       KEY 4,"goto 10000"+CHR$(13)
10 ' Army 1
11 ' Copyright 2017, Ted Felix
12 ' File date: 4/12/1987 12:09:32 AM
20 DEF SEG=0:RANDOMIZE (PEEK(&H46C)+PEEK(&H46D)*256)-32768!
30 DIM GM(11),BM(11),COPTER(14),ANT(12)
40 KEY OFF:CLS:STRIG ON
50 SP$=SPACE$(39)
90 GOSUB 30000
100 DEF FNRNDX=INT(RND*240)+30
110 DEF FNRNDY=INT(RND*120)+30
120 DEF FNDIS(DX,DY)=ABS(DX)+ABS(DY)
200 GOSUB 9000
210 LOCATE 25,1:PRINT">>>>Army 1<<<<";
220 GOSUB 8000
230 LOCATE 25,1:PRINT"An original game written by a demented";
240 FOR I=1 TO 3000:NEXT
250 LOCATE 25,1:PRINT"mind:   Ted Felix                     ";
260 GOSUB 8000
270 LOCATE 25,1:PRINT SP$;
280 FOR I=1 TO 1000:NEXT
290 LOCATE 1,1:PRINT">>>>Army 1<<<<":PRINT"Take over an Army base!";
299 ' Wave 1 set-up and instructions
300 GOSUB 9200
310 BGX(1)=BX-5:BGY(1)=BY-5
320 BGX(2)=BX-5:BGY(2)=BY+10
330 BGX(3)=BX+5:BGY(3)=BY-8
400 FOR I=1 TO 3:PUT(BGX(I),BGY(I)),BM:NEXT
410 GX=21:GY=170:PUT(GX,GY),GM
415 LOCATE 24,1:PRINT"INSTRUCTIONS:";:FOR I=1 TO 3000:NEXT
420 FOR IN=1 TO 10:LOCATE 25,1:PRINT I1$(IN);
430 GOSUB 8000:NEXT
999 ' Wave 1
1000 F1=0:LOCATE 24,1:PRINT">>>>>GO!!<<<<<";
1100 FOR I=1 TO 3
1105 X=INT(STICK(0)/X3):Y=INT(STICK(1)/X3)
1110 PX=GX:PY=GY
1120 IF X=0 THEN GX=GX-1:IF GX<21 THEN GX=21
1130 IF X=3 THEN GX=GX+1:IF GX>295 THEN GX=295
1140 IF Y=0 THEN GY=GY-1:IF GY<21 THEN GY=21
1150 IF Y=3 THEN GY=GY+1:IF GY>172 THEN GY=172:IF GX<27 AND F1=1 THEN 2000
1160 PUT(PX,PY),GM:PUT(GX,GY),GM
1170 DX=BGX(I)-GX:DY=BGY(I)-GY
1180 IF FNDIS(DX,DY)<10 THEN 1900
1190 IF F1=0 THEN DX=TX-GX:DY=TY-GY:IF FNDIS(DX,DY)<10 THEN F1=1:PLAY"mbc8d8e8f8g8a8b8o5c4o4":LINE(21,180)-(31,180),0
1199 ' **** Bad Guys
1200 BPX=BGX(I):BPY=BGY(I)
1210 MX=SGN(GX-BPX)*2
1220 MY=SGN(GY-BPY)*2
1230 BGX(I)=BPX+MX:BGY(I)=BPY+MY
1240 PUT(BPX,BPY),BM:PUT(BGX(I),BGY(I)),BM
1490 NEXT:GOTO 1100
1899 ' Boom!
1900 PRINT"You're gone!";
1910 GOSUB 8000
1920 CLS:SCREEN 0:PRINT"Luckily you took your cyanide pill!
1930 END
1999 'Wave 2
2000 FOR I=1 TO 20:COLOR 2:FOR T=1 TO 100:NEXT:COLOR 0:FOR T=1 TO 100:NEXT:NEXT
2010 LOCATE 24,1:PRINT"Congratulations, you succesfully";
2020 LOCATE 25,1:PRINT"finished your first mission!";
2030 FOR I=1 TO 5000:NEXT
2040 FOR I=1 TO 3:PUT(BGX(I),BGY(I)),BM:NEXT
2045 PAINT(TX,TY),0
2050 FOR I=1 TO 3:ANX(I)=FNRNDX:ANY(I)=FNRNDY
2060 PUT(ANX(I),ANY(I)),ANT:NEXT
2070 PUT(PX,PY),GM:GX=21:GY=168
2080 PUT(GX,GY),COPTER
2100 LOCATE 24,1:PRINT SP$;
2110 LOCATE 25,1:PRINT SP$;
2120 FOR IN=1 TO 7:LOCATE 25,1
2130 PRINT I2$(IN);:GOSUB 8000:NEXT
2140 LOCATE 25,1:PRINT SP$;
2150 GOSUB 8000
2160 LOCATE 24,1:PRINT">>>>>GO!!<<<<<";
2170 F1=0
2199 ' Wave 2
2200 X=INT(STICK(0)/X3):Y=INT(STICK(1)/Y3)
2210 PX=GX:PY=GY
2220 IF X=0 THEN GX=GX-2:IF GX<21 THEN GX=21
2230 IF X=3 THEN GX=GX+2:IF GX>280 THEN GX=280
2240 IF Y=0 THEN GY=GY-2:IF GY<21 THEN GY=21
2250 IF Y=3 THEN GY=GY+2:IF GY>169 THEN GY=169:IF GX<27 AND F1=1 THEN 3000
2260 PUT(PX,PY),COPTER:PUT(GX,GY),COPTER
2270 FOR I=1 TO 3:DX=ANX(I)-GX:DY=ANY(I)-GY
2275 LINE(0,I*5+20)-(T(I),I*5+20):LINE(T(I),I*5+20)-(11,I*5+20),0
2280 IF FNDIS(DX,DY)<10 THEN T(I)=T(I)+1:IF T(I)<11 THEN IF STRIG(5) THEN KI=I:I=500 ELSE ELSE 2900 ELSE T(I)=0
2290 NEXT:IF I>100 THEN 2500
2390 GOTO 2200
2499 ' Fire!
2500 FOR I=1 TO 10:CIRCLE(GX+12,GY+6),I,2:NEXT
2510 FOR I=1 TO 10:CIRCLE(GX+12,GY+6),I,0:NEXT
2515 PUT(GX,GY),COPTER
2520 KN=KN+1:IF KN>=3 THEN F1=1
2530 ANX(KI)=0:ANY(KI)=0
2540 GOTO 2200
2899 ' Boom!
2900 FOR I=10 TO 1 STEP-1:CIRCLE(GX+12,GY+6),I,1:NEXT
2910 FOR I=10 TO 1 STEP-1:CIRCLE(GX+12,GY+6),I,2:NEXT
2920 FOR I=10 TO 1 STEP-1:CIRCLE(GX+12,GY+6),I,3:NEXT
2930 PRINT"  You were nailed!!";:GOSUB 8000
2940 CLS:SCREEN 0:PRINT"Bye!
2999 ' Wave 3
3000 LOCATE 24,1:PRINT"Good Job! You got rid of the ants!";
3010 LOCATE 25,1:PRINT"Now that you have gained entrance...";
3020 FOR I=1 TO 5000:NEXT:LOCATE 24,1:PRINT SP$;
3030 LOCATE 25,1:PRINT SP$;
3040 FOR IN=1 TO 5:LOCATE 25,1:PRINT I3$(IN);:GOSUB 8000:NEXT
7998 ' Routines
7999 ' Delay
8000 FOR I=0 TO 2500:NEXT:RETURN
8999 ' Screen Set-up
9000 SCREEN 1:CLS:LINE(20,20)-(300,180),,B
9010 BX=FNRNDX:DX=BX-20
9020 BY=FNRNDY:DY=BY-180:IF FNDIS(DX,DY)<100 THEN 9010
9030 LINE(BX,BY)-(BX+20,BY+20),,BF
9040 TX=FNRNDX:DX=BX-TX
9050 TY=FNRNDY:DY=BY-TY:IF FNDIS(DX,DY)<50 THEN 9040
9060 CIRCLE(TX,TY),5,2
9070 PAINT(TX,TY),2
9080 LINE(21,180)-(31,180),2
9100 FOR I=1 TO 10:READ I1$(I):NEXT
9110 FOR I=1 TO 7:READ I2$(I):NEXT
9120 FOR I=1 TO 5:READ I3$(I):NEXT
9190 RETURN
9199 ' Set-up Graphics "Players"
9200 FOR I=3 TO 2 STEP-1
9210 PSET(200,0),I:PSET(199,1),I:PSET(201,1),I:PSET(200,2),I
9220 LINE(200,2)-(200,5),I
9230 LINE(200,5)-(198,7),I
9240 LINE(200,5)-(202,7),I
9250 IF I=3 THEN GET(198,0)-(202,7),GM
9260 IF I=2 THEN GET(198,0)-(202,7),BM
9280 NEXT
9290 LINE(198,0)-(202,7),0,BF
9299 ' Heliocopter
9300 LINE(205,0)-(219,0)  '-Prop
9302 LINE(212,1)-(212,6)  '-Shaft
9304 LINE(201,6)-(211,4)  '-R. prop
9306 LINE(201,7)-(212,7)  ' holder
9308 LINE(200,8)-(203,5)  '-R. prop
9310 LINE(210,8)-(210,9)  '-Ski hold
9312 LINE(205,10)-(215,10)'-Skid
9314 PSET(216,9)          '
9316 LINE(213,3)-(213,7)  '-Cockpit
9318 LINE(214,2)-(217,2)  ' Top
9320 LINE(218,3)-(218,6)  ' Front
9322 LINE(214,7)-(217,7)  ' Bottom
9324 GET(200,0)-(219,10),COPTER
9326 LINE(200,0)-(219,10),0,BF
9329 ' Giant Mutated Ant
9330 LINE(203, 4)-(203,12)  '-Body
9332 LINE(207, 4)-(207,12)  '
9334 LINE(204, 3)-(206, 3)  '
9336 LINE(204,13)-(205,14)  '
9338 PSET(206,13)
9340 LINE(204, 6)-(204, 8)  '
9342 LINE(206, 6)-(206, 8)  '
9344 LINE(200, 3)-(202, 5)  '-Legs
9346 LINE(200, 7)-(202, 7)  ' Right
9348 LINE(200,11)-(202, 9)  '
9350 LINE(208, 5)-(210, 3)  ' Right
9352 LINE(208, 7)-(210, 7)  ' r
9354 LINE(208, 9)-(210,11)  ' r
9356 LINE(203, 2)-(203, 1)  '-Pinchers
9358 LINE(207, 2)-(207, 1)  '
9360 PSET(204, 0)
9362 PSET(206, 0)
9364 GET(200,0)-(210,14),ANT
9366 LINE(200,0)-(210,14),0,BF
9490 LINE(198,0)-(202,7),0,BF:RETURN
9499 ' Instructions Mission 1
9500 DATA "Move your white man with the joystick..
9510 DATA "Avoid the red guards...                "
9520 DATA "Your job...             "
9530 DATA "Plant mutated giant ant eggs...
9540 DATA "At the red circle...           "
9550 DATA "Do this by simply going in the circle!
9560 DATA "Then you have to get out...           "
9570 DATA "Before the guards nail you...
9580 DATA "And the ants hatch...        "
9590 DATA "Good Luck!           "
9599 ' Instructions Mission 2
9600 DATA "Fly the heliocopter using the joystick
9610 DATA "Kill giant ants with the firing button
9620 DATA "Don't stay on top of an ant too long...
9630 DATA "Or he'll grab the heliocopter, and...  "
9640 DATA "Destroy it!                          "
9650 DATA "Fly back to start for next mission...
9660 DATA "Good Luck!                           "
9700 DATA "You must kill the guards within...
9710 DATA "Once you make it to the center... "
9720 DATA "You have completed your mission...
9730 DATA "And can go home or play again...  "
9740 DATA "Good Luck!                      "
10000 SAVE"army1":SAVE"b:army1":END
30000 OPEN "joystick.dat"FOR INPUT AS #1
30010 INPUT #1,U,D,L,R
30020 X3=(L+R)/4
30030 Y3=(U+D)/4
30080 CLOSE
30090 RETURN

