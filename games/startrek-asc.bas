100 '  STAR TREK v1.00 For the IBM Personal Computer By: Ted Felix
200 '  For the Greater WMA IBM PC Club
250 '  I am open for suggestion at (301) 869-4414
260 '  I would also appreciate a picture of the U.S.S. Enterprise
270 '  for the program to draw on the opening frame.
280 '  CALL ME IF YOU HAVE ONE THAT I CAN BORROW!
290 CLEAR,29000!,3000
300 DIM D(6):SCREEN 0:WIDTH 80:COLOR 7,0,0:CLS:LOCATE,,0
305 '    Functions
310 DEF FN RAND(X)=INT(RND*X)+1
320 DEF FN ADJUST$(X)=RIGHT$("00"+MID$(STR$(X),2),3)
330 DEF FN PYTHAGORAS(A,B,C,D)=SQR((A-C)^2+(B-D)^2)
395 '  Initialize constants
400 Q$=".EKB*":CL(1)=7:CL(2)=7:CL(3)=4:CL(4)=2:CL(5)=1
450 LIN$=STRING$(18,196):V$=CHR$(179)
500 FOR I=0 TO 9:READ D$(I):NEXT
510 DATA Warp engines,Short range scan,Long range scan,Phasers,Photon torpedoes,Galactic record,Torpedo Computer,Review instructions,Damage control report,Commands,Warp  1,SRS   2,LRS   3,Phaser4,Torpdo5,Record6,Comptr7,Instr 8,Damage9,Comand0
550 '  Set up "Softkeys"
600 KEY OFF:FOR I=1 TO 10:READ A$:KEY I,A$:NEXT:KEY ON
650 '  Make RND really random:
700 RANDOMIZE VAL(RIGHT$(TIME$,2))+VAL(MID$(TIME$,4,2))+VAL(LEFT$(TIME$,2))
750 '  Play Star Trek theme
800 PLAY "mnmbo2c4f8a#2a4f8d4f8o3c2p16c4e2"
900 GOSUB 23000 DRAW ENTERPRISE
1000 GOSUB 23300
1100 PRINT"Who do you compare yourself to?"
1200 LINE INPUT"1 - Cabin boy, 2 - Scotty, 3 - Sulu, 4 - Spock, 5 - Admiral Kirk >";P8$
1210 P8$=LEFT$(P8$,1)
1220 ZO=ASC(P8$):IF ZO>57 OR ZO<49 THEN 1200
1230 ZO=ZO-47:X1=.05+ZO/10
1300 A=.96:PRINT"The galaxy is being set up now.":COLOR 23,0,0:PRINT"--- Stand By ---":COLOR 7,0,0:GOSUB 23300
1400 GOSUB 6700:GOSUB 5100:Q1=X:Q2=Y:X=8:Y=1:Y1=6.28:X2=3.28
1500 Y2=1.8:C=100:W=10:K9=0:B9=0:S9=400:T9=5495:GOTO 1700
1600 K=K+(N<X2)+(N<Y2)+(N<.28)+(N<.08)+(N<.03)+(N<.01):K9=K9-K:GOTO 1900
1700 T0=5475:T=T0:E0=5000:E=E0:P0=10:P=P0:FOR I=0 TO 7
1800 FOR J=0 TO 7:K=0:N=RND:IF N<X1 THEN N=N*64:K=(N<Y1)-Y:GOTO 1600
1900 B=(RND>A):B9=B9-B:Q(I,J)=K*C+B*W-INT(RND*X+Y):NEXT:NEXT
2000 T9=5505
2100 IF B9>0 THEN GOTO 2300
2200 GOSUB 5100:Q(X,Y)=Q(X,Y)-10:B9=1
2300 GOSUB 23800:IF L1>(T9-T0)THEN T9=T9+(L1-30)
2390 K0=K9:LOCATE 7:PRINT"Galaxy completed."
2400 INPUT"Do you want instructions for the game";F$
2410 F$=LEFT$(F$,1)
2420 IF F$="Y"OR F$="y"THEN F$="Y":GOTO 2450
2430 IF F$="N"OR F$="n"THEN F$="N":GOTO 2450
2440 GOTO 2400
2450 REM
2490 IF F$="Y" THEN GOSUB 23300:GOSUB 18100
2500 IF F$="Y" THEN 2800
2600 PRINT STRING$(79,196):PRINT"Your objective is to destroy";K9;"Klingon Battle Cruisers"
2700 PRINT"in";T9-T0;"Stardates.  There are";B9;"Starbases in the galaxy.":FOR I=0 TO 2500:NEXT
2800 A=0:IF Q1<0 OR Q1>7 OR Q2<0 OR Q2>7 THEN N=0:S=0:K=0:GOTO 3000
2900 N=ABS(Q(Q1,Q2)):Q(Q1,Q2)=N:S=N-INT(N/10)*10:K=INT(N/100)
3000 B=INT(N/10-K*10):GOSUB 5100:S1=X:S2=Y
3100 FOR I=0 TO 7:FOR J=0 TO 7:S(I,J)=1:NEXT:NEXT:S(S1,S2)=2
3200 FOR I=0 TO 7:K3(I)=0:X=8:IF I<K THEN GOSUB 5200:S(X,Y)=3:K3(I)=S9
3300 K1(I)=X:K2(I)=Y:NEXT:I=S
3400 IF B>0 THEN GOSUB 5200:S(X,Y)=4
3500 IF I>0 THEN GOSUB 5200:S(X,Y)=5:I=I-1:GOTO 3500
3600 GOSUB 6100:IF TORP<>0 OR WF<1 OR K<1 THEN 3700
3610 WF=0:FOR SO=1 TO 200:IF RND<.5 THEN SOUND 10000,1
3620 SOUND 32767,0:NEXT:COLOR 23,0:PRINT"Shields up":COLOR 7,0
3630 FOR I=1 TO 5:SOUND 1000,10:SOUND 20000,10:NEXT
3690 GOSUB 5400
3700 IF E<=0 THEN GOTO 13800
3800 I=1:IF D(I)>0 THEN WARP=0:GOTO 6800
3900 PRINT:PRINT TAB(30)"Captain's Log:":PRINT TAB(29)CHR$(218)+LIN$+CHR$(191)
4000 FOR I=0 TO 7:PRINT I+1;:FOR J=0 TO 7:COLOR CL(S(I,J)),0,0:PRINT MID$(Q$,S(I,J),1);"  ";:COLOR 7,0,0:NEXT J
4100 PRINT" ";:ON I GOTO 4400,4500,4600,4700,4800,4900,5000
4200 PRINT V$;"Stardate: ";T TAB(48)V$;" Direction key:" TAB(67)"Symbols:"
4300 NEXT:PRINT"   1  2  3  4  5  6  7  8" TAB(29)CHR$(192)+LIN$+CHR$(217):GOTO 7000
4400 PRINT V$;"Years Left: ";T9-T TAB(48)V$ TAB(54)"4  3  2":GOTO 4300
4500 PRINT V$;"Condition - ";:COLOR CO,0,0:PRINT C$;:COLOR 7,0,0:PRINT TAB(48)V$ TAB(55)"\ | /" TAB(65)"E - Enterprise":GOTO 4300
4600 PRINT V$;"Quadrant";Q2+1;"-";Q1+1 TAB(48)V$ TAB(56)"\|/";:COLOR 1,0,0:PRINT TAB(65)"*";:COLOR 7,0,0:PRINT" - Star":GOTO 4300
4700 PRINT V$;"Position";S2+1;"-";S1+1 TAB(48)V$ TAB(54)"5--*--1" TAB(65)". - Empty":GOTO 4300
4800 PRINT V$;"Energy:";:IF CO=6 THEN COLOR 20,0,0
4850 PRINT INT(E);:COLOR 7,0,0:PRINT TAB(48)V$ TAB(56)"/|\";:COLOR 2,0,0:PRINT TAB(65)"B";:COLOR 7,0,0:PRINT" - Starbase":GOTO 4300
4900 PRINT V$;"Torpedoes:";:IF D(4)>0 OR P<4 THEN COLOR 20,0,0
4950 PRINT P;:COLOR 7,0,0:PRINT TAB(48)V$ TAB(55)"/ | \";:COLOR 4,0,0:PRINT TAB(65)"K";:COLOR 7,0,0:PRINT" - Klingon":GOTO 4300
5000 PRINT V$;"Klingons:";K9 TAB(48)V$ TAB(54)"6  7  8":GOTO 4300
5100 X=FNRAND(8)-1:Y=FNRAND(8)-1:RETURN
5200 GOSUB 5100:IF S(X,Y)>1 THEN 5200
5300 RETURN
5400 IF K<1 THEN RETURN
5500 IF C$="Docked" THEN PRINT"Starbase protects Enterprise":RETURN
5600 FOR I=0 TO 7:IF K3(I)<1 THEN 5800 ELSE FOR SO=1000 TO 100 STEP-20:SOUND SO,.1:NEXT
5700 H=K3(I)*.4*RND:K3(I)=K3(I)-H:M1=FNPYTHAGORAS(K1(I),K2(I),S1,S2):H=H/(M1^.4):E=E-H:N=E:PRINT INT(H);"unit hit on Enterprise from sector";K2(I)+1;"-";K1(I)+1;"  (";INT(N);"left )"
5800 NEXT:RETURN
5900 PRINT INT(H);"unit hit on ";E$;" sector";K2(I)+1;"-";K1(I)+1;
6000 PRINT" (";INT(N);"left )":RETURN
6100 FOR I=S1-1 TO S1+1:FOR J=S2-1 TO S2+1
6200 IF I<0 OR I>7 OR J<0 OR J>7 THEN GOTO 6400
6300 IF S(I,J)=4 THEN C$="Docked":CO=23:E=E0:P=P0:FOR N=0 TO 6:D(N)=0:NEXT:RETURN
6400 NEXT J:NEXT I:IF K>0 THEN C$="-RED-":CO=20:RETURN
6500 IF E<E0*.1 THEN C$="Yellow":CO=6:RETURN
6600 C$="Green":CO=2:RETURN
6700 FOR I=0 TO 6:D(N)=0:NEXT:RETURN
6800 PRINT D$(I);" damaged";
6900 PRINT" ";D(I);" years estimated for repair.":PRINT:IF WARP=1 THEN WARP=0:RETURN
7000 PRINT:PRINT"Command ---> ";
7100 A$=INKEY$:IF A$="" THEN 7100 ELSE IF A$="Q" OR A$="q" THEN 14400
7110 IF A$<"0" OR A$>"9" THEN 7100 ELSE IN=VAL(A$):GOSUB 25300:GOTO 7100
7200 PRINT:CLS:FOR I=0 TO 8:PRINT"Command `";I+1;"' = ";D$(I):NEXT:PRINT:PRINT"Klingons remaining =";K9:PRINT"Starbases remaining =";B9:PRINT"Stardates remaning =";T9-T:GOTO 7000
7300 IF D(4)>0 THEN PRINT"Space crud is blocking the torpedo tubes";:I=4:GOTO 6900
7400 N=15:IF P<1 THEN PRINT"No torpedoes left":GOTO 7000
7500 IF TORP=1 THEN PRINT"Torpedo ";
7600 INPUT"Course (1 - 8.999...)";C:IF C<1 THEN 7000
7700 IF C>=9 THEN GOTO 7500
7800 IF TORP=1 THEN P=P-1:FOR I=6000 TO 37 STEP-20:SOUND I,.001:NEXT:SOUND 32767,0:PRINT"Track :":GOTO 9400
7900 WF=1:INPUT"Warp (0 - 12)";W:IF W<=0 OR W>12 THEN 7500
8000 IF W<=.2 OR D(0)<=0 THEN 8200
8100 I=0:PRINT D$(I);" damaged, maximum is a warp of .2 ";:GOSUB 6900:GOTO 7900
8200 GOSUB 5400:IF E<=0 THEN 13800
8250 FOR SO=37 TO 400 STEP 10:SOUND SO,1:NEXT
8300 IF RND>.25 THEN 9100
8400 X=FNRAND(6)-1:IF RND>.5 THEN 9100
8500 D(X)=D(X)+(5-FNRAND(4)):PRINT"** Space storm, ";
8600 PRINT D$(X);" damaged **":I=X:GOSUB 6900:D(X)=D(X)+1:GOTO 9100
8700 FOR I=X TO 5:IF D(I)>0 THEN 9000
8800 NEXT
8900 FOR I=0 TO X:IF D(I)<=0 THEN NEXT:GOTO 9100
9000 D(I)=.5:PRINT"** Spock used a new repair technique **"
9100 FOR I=0 TO 5:IF D(I)=0 THEN GOTO 9300
9200 D(I)=D(I)-1:IF D(I)<=0 THEN D(I)=0:PRINT D$(I);" repaired!"
9300 NEXT:N=INT(W*8):E=E-N-N+.5:T=T+1:S(S1,S2)=1
9400 Y1=S1+.5:X1=S2+.5:IF T>T9 THEN 13800
9500 Y=(C-1)*.785398:X=COS(Y):Y=-SIN(Y)
9600 FOR I=1 TO N:Y1=Y1+Y:X1=X1+X:Y2=INT(Y1):X2=INT(X1)
9700 IF X2<0 OR X2>7 OR Y2<0 OR Y2>7 THEN GOTO 11500
9800 IF TORP=1 THEN PRINT X2+1;"-";Y2+1;" ";
9900 IF S(Y2,X2)=1 THEN NEXT:GOTO 11000
10000 PRINT:IF TORP=0 THEN PRINT"Blocked by ";
10100 ON S(Y2,X2)-3 GOTO 10800,10600
10150 IF TORP=1 THEN COLOR 23,0,0
10200 PRINT"Klingon";:COLOR 7,0,0:IF TORP=0 THEN 10900
10300 FOR I=0 TO 7:IF Y2<>K1(I)THEN 10500
10400 IF X2=K2(I)THEN K3(I)=0
10500 NEXT:K=K-1:K9=K9-1:GOTO 11100
10600 PRINT"star";:IF TORP=1 THEN S=S-1:GOTO 11100
10700 GOTO 10900
10800 PRINT"Starbase";:IF TORP=1 THEN B=2:GOTO 11100
10900 PRINT" at sector";X2+1;"-";Y2+1:Y2=INT(Y1-Y):X2=INT(X1-X)
11000 S1=Y2:S2=X2:S(S1,S2)=2:GOTO 3600
11100 PRINT" destroyed!";:IF B=2 THEN B=0:PRINT" . . . YOU IDIOT!!!!!";:B9=B9-1
11200 PRINT:S(Y2,X2)=1:Q(Q1,Q2)=K*100+B*10+S:IF K9<1 THEN 14100
11300 GOSUB 5400:IF E<=0 AND C$<>"Docked" THEN GOTO 13800
11400 GOSUB 6100:GOTO 7000
11500 IF TORP=1 THEN PRINT"Missed!":GOTO 11300
11600 Q1=INT(Q1+W*Y+(S1+.5)/8):Q2=INT(Q2+W*X+(S2+.5)/8)
11700 Q1=Q1-(Q1<0)+(Q1>7):Q2=Q2-(Q2<0)+(Q2>7):GOTO 2800
11800 GOSUB 25000:E$=FNADJUST$(Q(Q1,Q2)):IF VAL(E$)<99 THEN PRINT"There are no Klingons in this quadrant":GOTO 7000 ELSE I=3:IF D(I)>0 THEN GOTO 6800
11900 PRINT"Phasers ready: available energy =";INT(E):INPUT"Energy units to fire ";X:IF X<=0 THEN GOTO 7000
12000 IF X>E AND C$<>"Docked" THEN PRINT"You only have";INT(E):GOTO 11900
12100 E=E-X:Y=K:FOR I=0 TO 7:IF K3(I)<=0 THEN GOTO 12700
12200 M1=FNPYTHAGORAS(K1(I),K2(I),S1,S2):H=X/(Y*(M1^.4)):K3(I)=K3(I)-H
12300 E$="Klingon at":N=K3(I):GOSUB 5900
12400 IF K3(I)>=1 THEN GOTO 12700
12500 K3(I)=0:PRINT"** Klingon destroyed **"
12600 K=K-1:K9=K9-1:S(K1(I),K2(I))=1:Q(Q1,Q2)=Q(Q1,Q2)-100
12700 NEXT:IF K9<1 THEN GOTO 14100
12800 GOTO 11300
12900 I=2:IF D(I)>0 THEN GOTO 6800
13000 PRINT D$(I);" quadrant";Q2+1;"-";Q1+1
13100 FOR I=Q1-1 TO Q1+1:FOR J=Q2-1 TO Q2+1:PRINT"   ";:IF I<0 OR I>7 OR J<0 OR J>7 THEN PRINT"***";ELSE Q(I,J)=ABS(Q(I,J)):A$=FNADJUST$(Q(I,J)):COLOR 4:PRINT LEFT$(A$,1);:COLOR 2:PRINT MID$(A$,2,1);:COLOR 7:PRINT RIGHT$(A$,1);
13200 NEXT J:PRINT:NEXT I:GOTO 7000
13300 I=5:IF D(I)>0 THEN GOSUB 6800:GOTO 7000
13400 PRINT"** Galactic record - your present quadrant is,";Q2+1;"-";Q1+1;" **":PRINT:PRINT"     1   2   3   4   5   6   7   8":FOR I=0 TO 7:PRINT I+1;:FOR J=0 TO 7:PRINT" ";
13450 IF Q1=I AND Q2=J THEN ADD=16 ELSE ADD=0
13500 IF Q(I,J)<0 THEN COLOR 7:PRINT"***";ELSE A$=FNADJUST$(Q(I,J)):COLOR 4+ADD:PRINT LEFT$(A$,1);:COLOR 2+ADD:PRINT MID$(A$,2,1);:COLOR 7+ADD:PRINT RIGHT$(A$,1);
13600 NEXT J:PRINT:NEXT I:GOTO 7000
13700 PRINT:PRINT"It is stardate";T:RETURN
13800 GOSUB 13700:PRINT"Thanks to your bungling, the Federation will be"
13900 PRINT"conquered by the remaining";K9;"Klingon cruisers!"
14000 PRINT"You are demoted to Cabin boy!":GOTO 14400
14100 GOSUB 13700:PLAY"MBMNT124O2C4F8A#2A4G8F4E8D#1D4C2O3C2O2A#4A8G4F8D#2P4D4C2L4DEFG8A4G8A#2A4L2GCD4C1"
14200 PRINT"The Federation has been saved!":PRINT"You are promoted to Admiral":PRINT K0;"Klingons in";T-T0;"years.  "
14300 PRINT"Rating =";INT(K0/(T-T0)*1000)
14400 PRINT"Would you like to play again?";:GOSUB 24800:IF F$="Y" THEN RUN
14500 WIDTH 40:PRINT"Hope you enjoyed the game":GOTO 27400
14600 PRINT TAB(10)"Damage control report":PRINT:PRINT TAB(4)"SYSTEM" TAB(20)"Years left to repair":FOR I=0 TO 5:IF D(I)>0 THEN COLOR 0,6
14650 PRINT D$(I)TAB(28)D(I):COLOR 7,0:NEXT:PRINT:PRINT
14700 RETURN
14800 PRINT"Made it to Pythagoras 1":PRINT"Inform the librarian of what you just did to get here!":STOP
14900 PRINT"What are the coordinates of the Klingon you would like to destroy?"
15000 C1=S1+1:A=S2+1
15100 GOSUB 24500:PRINT X;:GOSUB 24200:PRINT W1
15200 X=X-A
15300 A=C1-W1
15400 IF X<0 THEN GOTO 16600
15500 IF A<0 THEN GOTO 17200
15600 IF X>0 THEN GOTO 15800
15700 IF A=0 THEN GOTO 16800
15800 C1=1
15900 IF ABS(A)<=ABS(X)THEN GOTO 16300
16000 W=C1+(((ABS(A)-ABS(X))+ABS(A))/ABS(A))
16100 GOSUB 18000
16200 GOTO 17900
16300 W=C1+(ABS(A)/ABS(X))
16400 GOSUB 18000
16500 GOTO 17900
16600 IF A>0 THEN GOTO 17000
16700 IF X=0 THEN GOTO 17200
16800 C1=5
16900 GOTO 15900
17000 C1=3
17100 GOTO 17300
17200 C1=7
17300 IF ABS(A)=>ABS(X)THEN GOTO 17700
17400 W=C1+(((ABS(X)-ABS(A))+ABS(X))/ABS(X))
17500 GOSUB 18000
17600 GOTO 17900
17700 W=C1+(ABS(X)/ABS(A))
17800 GOSUB 18000
17900 RETURN
18000 PRINT"To destroy this Klingon, fire a torpedo at direction";W:RETURN
18100 CLS:PRINT"You are the commander of the U.S.S. Enterprise."
18200 PRINT"Your mission is to destroy";K9;"Klingon Battle Cruisers"
18300 PRINT"in";T9-T0;"stardates.  There are";B9;"Starbases in the galaxy."
18400 GOSUB 22900
18500 PRINT"Command `1' - Warp engine control"
18600 PRINT:PRINT"   Course is in a circular, numerical" TAB(44)"4  3  2"
18700 PRINT"   vector, arranged as shown ->" TAB(45)"\ | /"
18800 PRINT"   Integer and real values may be" TAB(46)"\|/"
18900 PRINT"   used.  Therfore course 1.5 is" TAB(44)"5--*--1"
19000 PRINT"   half way between 1 and 2." TAB(46)"/|\"
19100 PRINT TAB(45)"/ | \"
19200 PRINT"   A vector of 9 is undefined.  But" TAB(44)"6  7  8"
19300 PRINT"   values may approach 9." TAB(44)"COURSE"
19400 PRINT:PRINT"   One warp factor is the size of one quadrandt on the"
19500 PRINT"   horizontal and vertical only.  To get from one corner of"
19600 PRINT"   the galaxy to the opposite corner takes 12 warp factors."
19700 PRINT:PRINT"Command `2' - Short Range Scan"
19800 PRINT:PRINT"   Prints the quadrant you are currently in.  Including"
19900 PRINT"   stars (*), Klingons (K), Starbases (B), the Enterprise (E)"
20000 PRINT"   and other pertinent information."
20100 GOSUB 22900
20200 PRINT:PRINT"Command `3' - Long Range Scan"
20300 PRINT:PRINT"   Shows condition in space for one quadrant on each side"
20400 PRINT"   of the Enterprise, which is in the middle of the scan. The scan"
20500 PRINT"   is coded in the following form: XXX where the first digit is"
20600 PRINT"   the number of Klingons, the second digit is the number of"
20700 PRINT"   Starbases and the third digit is the number of stars."
20800 PRINT:PRINT"Command `4' - Phaser control"
20900 PRINT:PRINT"   Allows you to destroy the Klingons by hitting them with"
21000 PRINT"   a suitably large number of energy units to depleate their"
21100 PRINT"   shield power.  Keep in mind that when you shoot at them,"
21200 PRINT"   they will shoot back at you, which will depleat some of"
21300 PRINT"   your energy.  Phasers will destroy a Klingon on the other"
21400 PRINT"   side of a star."
21500 GOSUB 22900:PRINT:PRINT"Command `5' - Photon Torpedo control"
21600 PRINT:PRINT"   The torpedo course is the same as the Warp engine control course."
21700 PRINT"   If you hit a Klingon, he is destroyed and cannot fire back."
21800 PRINT"   If you miss, he will fire back. Torpedoes cannot go through"
21900 PRINT"   stars, but they will destroy Starbases so be very careful!!"
22000 PRINT:PRINT"Command `6' - Galactic Record"
22100 PRINT:PRINT"   Gives display of computer memory of the results of all"
22150 PRINT"   previous Long Range Scans.":PRINT
22200 PRINT"Command `7' - Torpedo Computer":PRINT
22210 PRINT"   The Torpedo Computer will compute the direction you must"
22220 PRINT"   fire a torpedo to hit a Klingon.  When you execute"
22230 PRINT"   this command, you will be asked to enter the";
22300 PRINT"   coordinates of the Klingon as two numbers (x,y or column,row)."
22310 PRINT"   The computer will then give you the direction you must"
22320 PRINT"   fire a torpedo to destroy that Klingon Battle Cruiser."
22400 GOSUB 22900:PRINT:PRINT"Command `8' - Review Instructions"
22500 PRINT:PRINT"   This command displays this list of instructions anytime"
22510 PRINT"   During the game.  It will not change the game at all."
22600 PRINT:PRINT"Command `9' - Damage"
22610 PRINT
22620 PRINT"   This command will give a list of all damaged systems and show"
22630 PRINT"   how many years remain before they are repaired."
22700 PRINT:PRINT
22710 PRINT"Remember, it is possible to exceed the limits of the galaxy."
22720 PRINT"This is dark space and you must return to the"
22730 PRINT"galaxy to continue the game."
22800 GOSUB 22900
22810 PRINT"The soft key display at the bottom of the screen contains"
22820 PRINT"abbreviations of the commands and their key numbers."
22830 PRINT"`0' will display a short list of the commands."
22840 PRINT
22850 PRINT"  Remember, you can use the number or the function (F) keys"
22860 PRINT:PRINT:PRINT"  <Q>  Is for Quitters!  "
22900 COLOR 7,0:LOCATE 23,1:PRINT"Press ";:COLOR 23,0,0:PRINT"ENTER ";:COLOR 7,0,0:INPUT"to continue";A$:CLS:RETURN
23000 ' Draw Enterprise
23100 LOCATE 1,30:PRINT"STAR TREK"
23105 PRINT STRING$(79,205)
23110 PRINT"v1.00   By: Ted Felix"
23200 RETURN
23300 RETURN ' Enterprise phaser effect
23400 P8$=INKEY$:IF P8$="" THEN 23400
23500 ZO=ASC(P8$):IF ZO>57 OR ZO<49 THEN GOTO 23400
23600 ZO=ZO-47:X1=.05+ZO/10
23700 RETURN
23800 L1=0:FOR A1=0 TO 7:FOR A2=0 TO 7:U$=STR$(Q(A1,A2)):U$="00"+MID$(U$,2):U$=RIGHT$(U$,3):IF VAL(U$)>99 THEN L1=L1+1
23900 NEXT:NEXT
24000 IF L1>30 THEN L1=L1+3
24100 RETURN
24200 W1$=INKEY$:IF W1$="" THEN GOTO 24200
24300 ZO=ASC(W1$):IF ZO<49 OR ZO>57 THEN GOTO 24200
24400 W1=VAL(W1$):RETURN
24500 X1$=INKEY$:IF X1$="" THEN GOTO 24500
24600 ZO=ASC(X1$):IF ZO<49 OR ZO>57 THEN GOTO 24500
24700 X=VAL(X1$):RETURN
24800 F$=INKEY$:IF F$="" THEN GOTO 24800 ELSE IF F$="n" OR F$="N" THEN F$="N":RETURN
24900 IF F$="y" OR F$="Y" THEN F$="Y":RETURN ELSE GOTO 24800
25000 IF D(3)>=1 THEN PRINT"Your phasers are damaged and cannot be used."
25100 IF D(3)>=1 THEN GOTO 7000
25200 RETURN
25300 IF IN=0 THEN IN=10 '  Use Input
25350 PRINT D$(IN-1):PRINT
25400 ON IN GOSUB 26400,26500,26600,26700,26800,26900,27000,27100,27200,27300
26300 RETURN
26400 TORP=0:WARP=1:RETURN 7500
26500 RETURN 3600
26600 RETURN 12900
26700 RETURN 11800
26800 TORP=1:WARP=0:RETURN 7300
26900 RETURN 13300
27000 GOSUB 14900:RETURN 3600
27100 CLS:GOSUB 18500:RETURN 7000
27200 CLS:GOSUB 14600:RETURN 7000
27300 CLS:RETURN 7200
27400 KEY 1,"List ":KEY 2,"Run"+CHR$(13):KEY 3,"Load"+CHR$(34)
27500 KEY 4,"Save"+CHR$(34):KEY 5,"Cont"+CHR$(13):KEY 6,"Cls"+CHR$(13)
27600 KEY 7,"Tron"+CHR$(13):KEY 8,"Troff"+CHR$(13):KEY 9,"Key "
27700 KEY 10,"Screen 0,0,0"+CHR$(13)
27800 FOR I=0 TO 3000:NEXT:WIDTH 80:END

