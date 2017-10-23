10 ' TFS Menu system
11 ' Copyright 2017, Ted Felix
12 ' File date: 10/15/1991 08:45:04 PM
90 DIM TT$(50),TF$(50)
100 GOSUB 1000
110 SCREEN 0:WIDTH 80:CLS
150 TN=0
160 READ TT$(TN),TF$(TN)
170 IF TT$(TN)="END" THEN TN=TN-1:GOTO 190
180 TN=TN+1
185 GOTO 160
190 GOSUB 3000
200 PRINT"--- Games ---"
210 Y=2:X=1
220 FOR I=0 TO TN
230 LOCATE Y,X:PRINT USING "##> \                     \";I+1;TT$(I);
240 Y=Y+1:IF Y>20 THEN Y=2:X=X+40
250 NEXT
300 LOCATE 22,1
310 LINE INPUT"Selection > ",A$:A=VAL(A$)
320 IF A>TN+1 OR A<1 THEN PRINT"Please enter a number between 1 and";TN:GOTO 300
350 CLS
360 RUN TF$(A-1)
900 END
999 ' TFS Logo
1000 SCREEN 1:CLS:X=110:Y=20:CT=3:CS=3
1010 LINE(X,Y)-(X+50,Y+10),CT,BF
1020 LINE(X+20,Y+10)-(X+30,Y+50),CT,BF
1030 LINE(X+30,Y+20)-(X+40,Y+30),CT,BF
1040 CIRCLE(X+70,Y+18.75),18.75,CS,1.5708,4.7124,1
1050 CIRCLE(X+70,Y+31.25),18.75,CS,4.7124,1.5708,1
1060 LINE(X+70,Y)-(X+70,Y+50),CS
1070 PAINT(X+69,Y+1),CS:PAINT(X+71,Y+49),CS
1080 LOCATE 11,11:PRINT"Ted Felix Software"
1100 LOCATE 13,15:PRINT"Presents..."
1110 FOR I=1 TO 1000
1120 IF INKEY$<>"" THEN I=1000
1130 NEXT
1199 RETURN
1999 ' Game names and filenames
2000 DATA Alien Invasion,ALIENINV
2005 DATA Blockade (2 Players),BLOCKADE
2010 DATA Cannon,CAN
2020 DATA Capture!,CAPTURE
2030 DATA Eliza,ELIZA
2040 DATA Football,FOOTBALL
2050 DATA Highway Driving,DRIVER
2060 DATA Hot Lust,HL
2070 DATA Lunar Lander,LANDER
2080 DATA Maze,TMAZE
2090 DATA Miner,MINER2
2100 DATA Mix-up,MIXUP
2120 DATA Simon,SIMON
2130 DATA Space Craft Sim.,SPACCRFT
2140 DATA Speedway,SPEEDWAY
2150 DATA Star Trek,STARTREK
2160 DATA Submarine,SUB
2170 DATA Ted Team Nife,NIFE2
2180 DATA Wheel of Fortune,WOF
2190 DATA William Tell,WILLTELL
2200 DATA Zap,ZAP
2900 DATA END,END
2999 ' Sort
3000 FL=0
3010 FOR I=0 TO TN-1
3020 IF TT$(I)>TT$(I+1) THEN SWAP TT$(I+1),TT$(I):SWAP TF$(I+1),TF$(I):FL=-1
3030 NEXT
3040 IF FL THEN 3000
3050 RETURN
