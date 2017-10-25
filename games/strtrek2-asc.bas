10 REM
20 REM ****       **** STAR TREK ****       ****
30 REM ****  Simulation of a mission of the starship ENTERPRISE
40 REM ****  as seen on the Star Trek tv show.
50 REM ****  Original program in Creative Computing
60 REM ****  Basic Computer Games by Dave Ahl.
70 REM ****  Modifications by Bob Fritz and Sharon Fritz
80 REM **** for the IBM Personal Computer, October-November 1981
90 REM **** Bob Fritz, 9915 Caninito Cuadro, San Diego, Ca., 92129
100 REM ****  (714) 484-2955
110 REM ****
120 '****
130 '**** Modifications to modifications by Mike Stafford
140 '****
150 DIM G(8,8),C%(9,2),K%(3,3),N(3),Z(8,8),D(8),Q%(8,8), MOVES%(10,2)
160 STAR$=" "+CHR$(15)+" "
170 STARBASE$=CHR$(174)+CHR$(127)+CHR$(175)
180 ENTERPRISE$=CHR$(204)+CHR$(144)+CHR$(185)
190 KLINGON$="+"+CHR$(2)+"+"
200 TORPEDO$=CHR$(232)
210 UPPERLINE$=CHR$(205)+CHR$(209)+CHR$(205)
220 UPPERLINE$=UPPERLINE$+UPPERLINE$
230 UPPERLINE$=CHR$(201)+UPPERLINE$+UPPERLINE$+UPPERLINE$+UPPERLINE$+CHR$(187)
240 LOWERLINE$=CHR$(205)+CHR$(207)+CHR$(205)
250 LOWERLINE$=LOWERLINE$+LOWERLINE$
260 LOWERLINE$=CHR$(200)+LOWERLINE$+LOWERLINE$+LOWERLINE$+LOWERLINE$+CHR$(188)
270 A1$="NAVSRSLRSPHATORSHIDAMCOMRESCLE"
280 '**********************************************************
290 '*  Theme to Startrek
300 '*  Music in variables Z1$, Z2$, Z3$, Z4$, and Z5$
310 '**********************************************************
320 Z1$="T200MLO2L2GO3FL4FEL6DCO2BL4B-L8B-L32B-B-B-P32B-L1B-" 'Beyond the rim of the starlight
330 Z2$="L2GO3GL4GFL6EDCO2L4BL8BL32BBBP32L1BL4B" ' My love is wandring in star flight
340 Z3$="L4B-AAABO3C#DL6EF#GL2AB-B-B-O2B-L4B-O3CDE-L6FGA-L2B-BBB"' I know he'll find in star clustered reaches love, strange love a star woman teaches.
350 Z4$="O2GO3FL4FEL6DCO2BL4B-L8B-L32B-B-B-P32L1B-L4B-A-L2GO3GL4GFL6EDCO2L4BL8BL32BBBP32L1BL4B"
360 Z5$="O2B-AAABO3CDL6EFEL2GL4GL32GGGP32L4GB-B-B-AL2GL1CL2DL1CC"
370 E6LINE$=STRING$(6,CHR$(196))
380 FOR I=1 TO 10
390   MOVES%(I,1)=0
400 NEXT I
410 '****************************************************************
420 '*  CREATE MATRIX C(9,2) FOR CALCULATION OF MOVEMENTS
430 '****************************************************************
440 FOR I=1 TO 9
450   C%(I,1)=0
460   C%(I,2)=0
470 NEXT I
480 C%(3,1)=-1
490 C%(2,1)=-1
500 C%(4,1)=-1
510 C%(4,2)=-1
520 C%(5,2)=-1
530 C%(6,2)=-1
540 C%(1,2)=1
550 C%(2,2)=1
560 C%(6,1)=1
570 C%(7,1)=1
580 C%(8,1)=1
590 C%(8,2)=1
600 C%(9,2)=1
610 CLS
620 KEY OFF
630 LOCATE 13,1
640 PRINT "                                    ,"+E6LINE$;
650 COLOR 31,0
660 PRINT CHR$(15);
670 COLOR 7,0
680 PRINT E6LINE$+","
690 PRINT "                    ,"+STRING$(13,CHR$(196))+",  '"+STRING$(3,CHR$(196))+"  "+E6LINE$+"'"
700 PRINT "                     '"+STRING$(7,CHR$(196))+"  "+STRING$(2,CHR$(196))+"'      / /"
710 PRINT "                        ,"+STRING$(3,CHR$(196))+"' '"+STRING$(7,CHR$(196))+"/ /"+STRING$(2,CHR$(196))+","
720 PRINT "                          '"+STRING$(16,CHR$(196))+"'"
730 PRINT
740 PRINT "                   THE USS ENTERPRISE --- NCC-1701"
750 RANDOMIZE(VAL(MID$(TIME$,4,2)+RIGHT$(TIME$,2)))
760 Z$="                         "
770 PLAY "MB"+Z1$+Z2$+Z3$
780 PLAY Z4$+Z5$
790 '**********************************************************
800 '* Initialize variables
810 '* T - CURRENT DATE
820 '* T0 - INITIAL DATE
830 '* T9 - COMPLETION DATE
840 '* B9 - TOTAL NUMBER OF BASES
850 '* K9 - TOTAL NUMBER OF KLINGONS
860 '* K3 - NUMBER OF KLINGONS IN CURRENT QUADRANT
870 '* P - NUMBER OF PHOTON TORPEDOES
880 '**********************************************************
890 START=1
900 SRS.CLEARED=1
910 GALRECSET=0
920 CLEARFLAG=0
930 MOVERANDOM%=0
940 T=INT(RND(1)*20+20)*100
950 T0=T
960 T9=25+INT(RND(1)*10)
970 D0=0
980 E=3000
990 E0=E
1000 P=10
1010 P0=P
1020 S9=200
1030 S=0
1040 B9=0
1050 K9=0
1060 X$=""
1070 X0$=" is "
1080 DEF FND(D)=SQR((K%(I,1)-S1)^2+(K%(I,2)-S2)^2)
1090 DEF FNR(R)=INT(RND(R)*7.98+1.01)
1100 REM initialize enterprise's position
1110 Q1=FNR(1)
1120 Q2=FNR(1)
1130 S1=FNR(1)
1140 S2=FNR(1)
1150 FOR I=1 TO 8
1160   D(I)=0
1170 NEXT I
1180 '****************************************************************
1190 '*  set up what exists in galaxy
1200 '****************************************************************
1210 REM k3=#klingons  b3=#starbases  s3=#stars
1220 FOR I=1 TO 8
1230   FOR J=1 TO 8
1240     Z(I,J)=0
1250     R1=RND(1)
1260     IF R1<.8 THEN K3=0: GOTO 1290
1270     IF R1<.95 THEN K3=1: GOTO 1290
1280     IF R1<.9799999 THEN K3=2: ELSE K3=3
1290     K9=K9+K3
1300     IF RND(1)>.96 THEN B3=1: B9=B9+1 ELSE B3=0
1310     G(I,J)=K3*100+B3*10+FNR(1)
1320   NEXT J
1330 NEXT I
1340 IF K9>T9 THEN T9=K9+1
1350 IF B9<>0 THEN 1410
1360 IF G(Q1,Q2)<200 THEN G(Q1,Q2)=G(Q1,Q2)+100: K9=K9+1
1370 B9=1
1380 G(Q1,Q2)=G(Q1,Q2)+10
1390 Q1=FNR(1)
1400 Q2=FNR(1)
1410 K7=K9
1420 IF B9<>1 THEN X$="s":X0$=" are "
1430 LOCATE 1,1:PRINT"      Your orders are as follows: ":PRINT
1440 PRINT"      Destroy the";K9;"Klingon warships which have invaded"
1450 PRINT"    the galaxy before they can attack Federation headquarters"
1460 PRINT"    on stardate";T0+T9;". This gives you";T9;"days.  There";X0$
1470 PRINT"   ";B9;"starbase";X$;" in the galaxy for resupplying your ship."
1480 FOR ITEMP=1 TO 2500: NEXT ITEMP
1490 REM here any time new quadrant entered
1500 Z4=Q1
1510 Z5=Q2
1520 K3=0
1530 B3=0
1540 S3=0
1550 G5=0
1560 D4=.5*RND(1)
1570 Z(Q1,Q2)=G(Q1,Q2)
1580 IF Q1<1 OR Q1>8 OR Q2<1 OR Q2>8 THEN 1840
1590 GOSUB 7770
1600 PRINT
1610 IF T0 <>T THEN 1710
1620 PRINT
1630 PRINT "Hit enter key when ready to accept command."
1640 ATEMP$=INKEY$+"0"
1650 IF ASC(LEFT$(ATEMP$,1))<>13 THEN 1640
1660 PRINT"Your mission begins with your starship located"
1670 PRINT"in the galactic quadrant, '";G2$;"'."
1680 FOR I=1 TO 800
1690 NEXT I
1700 GOTO 1720
1710 PRINT"Now entering ";G2$;" quadrant."
1720 PRINT
1730 K3=INT(G(Q1,Q2)*.01)
1740 B3=INT(G(Q1,Q2)*.1)-10*K3
1750 S3=G(Q1,Q2)-100*K3-10*B3
1760 FOR I=1 TO 8
1770   FOR J=1 TO 8
1780     Q%(I,J)=0
1790   NEXT J
1800 NEXT I
1810 FOR I=1 TO 3
1820   K%(I,3)=0
1830 NEXT I
1840 SRS.CLEARED=1
1850 '***************************************************************
1860 '*  position enterprise in quadrant, then place "k3" klingons,&
1870 '***************************************************************
1880 REM "b3" starbases &"s3" stars elsewhere.
1890 Q%(S1,S2)=30000
1900 IF K3<1 THEN 1980
1910 FOR I=1 TO K3
1920   GOSUB 7570
1930   K%(I,1)=R1
1940   K%(I,2)=R2
1950   K%(I,3)=INT(S9*(.5+RND(1)))
1960   Q%(R1,R2)=-1
1970 NEXT I
1980 IF B3<1 THEN 2010
1990 GOSUB 7570
2000 Q%(R1,R2)=1 'STARBASE
2010 FOR I=1 TO S3
2020   GOSUB 7570
2030   Q%(R1,R2)=2 'STARS
2040 NEXT I
2050 GOSUB 6030
2060 IF S+E>10 THEN IF E>10 OR D(7)=0 THEN 2120
2070 COLOR 16,7 : PRINT"*** FATAL ERROR ***";:COLOR 7,0:GOSUB 9790
2080 PRINT"You've just stranded your ship in "
2090 PRINT"space":PRINT"You have insufficient maneuvering energy,";
2100 PRINT" and shield control":PRINT"is presently incapable of cross";
2110 PRINT"-circuiting to engine room!!":GOTO 5750
2120 GOSUB 6360
2130 LOCATE 13,1
2140 PRINT STRING$(26," ");
2150 LOCATE 13,1
2160 INPUT "COMMAND";A$
2170 GOSUB 8230
2180 IF GALRECSET=1 THEN GOSUB 9680: GOTO 2250
2190 FOR ITEMP=14 TO 24
2200   LOCATE ITEMP,1
2210   PRINT STRING$(54," ");
2220 NEXT ITEMP
2230 LOCATE 14,1
2240 IF GALRECSET=1 THEN GOSUB 9680: GOTO 2250
2250 FOR I=1 TO 10: IF LEFT$(A$,3)<> MID$(A1$,3*I-2,3) THEN 2270
2260 ON I GOTO 2430,2050,3630,3910,4540,5080,5250,6520,5810,9510
2270 NEXT I:GOSUB 9680: LOCATE 1,34: PRINT"Enter one of the following:";
2280 LOCATE 3,36: PRINT "NAV   (to set course)";
2290 LOCATE 4,36: PRINT "SRS   (for short range sensor scan)";
2300 LOCATE 5,36: PRINT "LRS   (for long range sensor scan)";
2310 LOCATE 6,36: PRINT "PHA   (to fire phasers)";
2320 LOCATE 7,36: PRINT "TOR   (to fire photon torpedoes)";
2330 LOCATE 8,36: PRINT "SHE   (to raise or lower shields)";
2340 LOCATE 9,36: PRINT "DAM   (for damage control reports)";
2350 LOCATE 10,36: PRINT "COM   (to call on library-computer)";
2360 LOCATE 11,36: PRINT "RES   (to resign your command)";0
2370 LOCATE 12,36: PRINT "CLE   (to clear and redraw screen)";
2380 GALRECSET=1
2390 GOTO 2060
2400 '*****************************************************************
2410 '*  course control begins here
2420 '*****************************************************************
2430 INPUT"Course (0-9)";C1
2440 IF C1=9 THEN C1=1
2450 IF C1>=1 AND C1<9 THEN 2490
2460 PRINT "Lt. Sulu reports"
2470 PRINT "  'Incorrect course data, sir!'"
2480 GOTO 2060
2490 X$="8"
2500 IF D(1)<0 THEN X$="0.2"
2510 PRINT"Warp factor(0-";X$;")";:INPUT W1:IF D(1)<0 AND W1>.2 THEN 2570
2520 IF W1>0 AND W1<8 THEN 2590
2530 IF W1=0 THEN 2060
2540 PRINT "Chief Engineer Scott reports"
2550 PRINT "  'The engines won't take";
2560 PRINT " warp ";W1;"!'.":GOTO 2060
2570 PRINT "Warp engines are damaged."
2580 PRINT "Maximum speed = warp 0.2":GOTO 2060
2590 N=INT(W1*8+.5) ' N=ENERGY USED BY NAVIGATION
2600 IF E-N>=0 THEN 2710
2610 PRINT "Engineering reports"
2620 PRINT "   'Insufficient energy available"
2630 PRINT "    for maneuvering at warp";W1;"!'"
2640 IF S<N-E OR D(7)<0 THEN 2060
2650 PRINT "Deflector control room acknowledges"
2660 PRINT S;"UNITS OF ENERGY DEPLOYED TO SHIELDS."
2670 GOTO 2060
2680 '*****************************************************************
2690 '*  klingons move/fire on moving starship . . .
2700 '*****************************************************************
2710 D1=0
2720 D6=W1
2730 IF W1>=1 THEN D6=1
2740 FOR I=1 TO 8
2750   IF D(I)>=0 THEN D(I)=0: GOTO 2840
2760   D(I)=D(I)+D6
2770   IF D(I)>-.1  AND D(I)<0 THEN D(I)=-.1:GOTO  2840
2780   IF D(I)<0 THEN 2840
2790   IF D1<>1 THEN D1=1:PRINT"DAMAGE CONTROL REPORT:"
2800   PRINT TAB(8);
2810   R1=I
2820   GOSUB 7620
2830   PRINT G2$;" Repair completed."
2840 NEXT I
2850 IF RND(1)>.2 THEN 2980
2860 R1=FNR(1)
2870 IF RND(1)>=.6 THEN 2910
2880 D(R1)=D(R1)-(RND(1)*5+1)
2890 PRINT"DAMAGE CONTROL REPORT:"
2900 GOSUB 7620:PRINT G2$;" damaged":PRINT:GOTO 2980
2910 D(R1)=D(R1)+RND(1)*3+1
2920 PRINT"DAMAGE CONTROL REPORT:"
2930 GOSUB 7620
2940 PRINT G2$;" State of repair improved"
2950 '*****************************************************************
2960 '*  begin moving starship
2970 '*****************************************************************
2980 MOVES%(1,1)=INT(S1)
2990 MOVES%(1,2)=INT(S2)
3000 CR1=INT(C1)
3010 X1=C%(CR1,1)+(C%(CR1+1,1)-C%(CR1,1))*(C1-CR1)
3020 X=S1
3030 Y=S2
3040 X2=C%(CR1,2)+(C%(CR1+1,2)-C%(CR1,2))*(C1-CR1)
3050 Q4=Q1
3060 Q5=Q2
3070 FOR I=1 TO N
3080   S1=S1+X1
3090   S2=S2+X2
3100   SR1=INT(.5+S1)
3110   SR2=INT(.5+S2)
3120   IF SR1<1 OR SR1>=9 OR SR2<1 OR SR2>=9 THEN 3350
3130   IF Q%(SR1,SR2)=0 THEN 3190
3140   S1=INT(S1+.5-X1)
3150   S2=INT(.5+S2-X2)
3160   PRINT"Warp engines shut down at "
3170   PRINT "sector";S1;",";S2;"due to bad navigation."
3180   GOTO 3250
3190   MOVES%(I+1,1)=SR1
3200   MOVES%(I+1,2)=SR2
3210 NEXT I
3220 S1=SR1
3230 S2=SR2
3240 GOSUB 5510
3250 GOSUB 8050
3260 MOVERANDOM%=4
3270 GOSUB 8230
3280 GOSUB 3570
3290 T8=1
3300 IF W1<1 THEN T8=.1*INT(10*W1)
3310 T=T+T8:IF T>T0+T9 THEN 5750
3320 REM see if docked then get command
3330 GOTO 2050
3340 REM exceeded quadrant limits
3350 GOSUB 8050
3360 LOCATE 3+INT(.5+S1-X1),48+3*INT(.5+S2-X2)
3370 PRINT "   ";
3380 LOCATE 15,1
3390 X=8*Q1+X+N*X1:Y=8*Q2+Y+N*X2:Q1=INT(X/8):Q2=INT(Y/8):S1=INT(X-Q1*8)
3400 S2=INT(Y-Q2*8):IF S1=0 THEN Q1=Q1-1:S1=8
3410 IF S2=0 THEN Q2=Q2-1:S2=8
3420 X5=0:IF Q1<1 THEN X5=1:Q1=1:S1=1
3430 IF Q1>8 THEN X5=1:Q1=8:S1=8
3440 IF Q2<1 THEN X5=1:Q2=1:S2=1
3450 IF Q2>8 THEN X5=1:Q2=8:S2=8
3460 IF X5=0 THEN 3540
3470 PRINT"Lt. Uhura reports message from Starfleet Command:"
3480 PRINT"  'Permission to attempt crossing of galactic perimeter"
3490 PRINT"  is hereby *DENIED*.  Shut down your engines.'"
3500 PRINT "Chief Engineer Scott reports"
3510 PRINT "  'Warp engines shut down"
3520 PRINT "  at sector";S1;",";S2;"of quadrant";Q1;",";Q2".'"
3530 IF T>T0 THEN 5750
3540 IF 8*Q1+Q2=8*Q4+Q5 THEN 3250
3550 T=T+1:GOSUB 3570:GOTO 1500
3560 REM maneuver energy s/r **
3570 E=E-N-10:IF E>0 THEN RETURN
3580 PRINT "Shield control supplies energy"
3590 PRINT "to complete the maneuver."
3600 S=S+E:E=0:IF S<=0 THEN S=0
3610 RETURN
3620 '** Long range sensor scan code
3630 IF D(3)<0 THEN PRINT"Long Range Sensors are inoperable":GOTO 2060
3640 IF GALRECSET=1 THEN GOSUB 9680 ELSE GOSUB 9600
3650 LOCATE 14,58
3660 PRINT"LONG RANGE SCAN";
3670 LOCATE 15,58
3680 PRINT"QUADRANT";Q1;",";Q2;
3690 O1$="-------------------":FOR I=16 TO 22 STEP 2: LOCATE I,55: PRINT O1$;: NEXT I
3700 ITEMP=0
3710 FOR I=Q1-1 TO Q1+1
3720   N(1)=-1
3730   N(2)=-2
3740   N(3)=-3
3750   FOR J=Q2-1 TO Q2+1
3760     IF I>0 AND I<9 AND J>0 AND J<9 THEN N(J-Q2+2)=G(I,J):Z(I,J)=G(I,J)
3770   NEXT J
3780   ITEMP=ITEMP+1
3790   LOCATE 15+ITEMP*2,55
3800   FOR L=1 TO 3
3810     PRINT": ";
3820     IF N(L)<0 THEN PRINT"*** ";: GOTO 3840
3830     PRINT RIGHT$(STR$(N(L)+1000),3);" ";
3840   NEXT L
3850   PRINT":";
3860 NEXT I
3870 GOTO 2060
3880 '*****************************************************************
3890 '*  phaser control code begins here
3900 '*****************************************************************
3910 IF D(4)<0 THEN PRINT"Phasers Inoperative":GOTO 2060
3920 IF K3>0 THEN 3970
3930 PRINT "Science Officer Spock reports"
3940 PRINT "  'Sensors show no enemy ships"
3950 PRINT "   in this quadrant'"
3960 GOTO 2060
3970 IF D(8)<0 THEN PRINT"Computer failure hampers accuracy"
3980 IF K3>1 THEN A$="s" ELSE A$=" "
3990 PRINT "Phasers locked on target"+A$
4000 PRINT"Energy available = ";E;"units"
4010 INPUT "Numbers of units to fire";X
4020 IF X<=0 THEN 2060
4030 IF E-X<0 THEN 4000
4040 E=E-X
4050 GOSUB 5500
4060 GOSUB 9920
4070 IF D(7)<0 THEN X=X*RND(1)
4080 H1=INT(X/K3)
4090 FOR I=1 TO 3
4100   IF K%(I,3)<=0 THEN 4370
4110   H=INT((H1/FND(0))*(RND(1)+2))
4120   IF H>.15*K%(I,3) THEN 4150
4130   PRINT"Sensors show no damage to enemy at ";K%(I,1);",";K%(I,2)
4140   GOTO 4370
4150   K%(I,3)=K%(I,3)-H
4160   PRINT H;"Unit hit on Klingon at sector";K%(I,1);",";K%(I,2)
4170   GOSUB 9290
4180   LOCATE 3+K%(I,1),48+3*K%(I,2)
4190   IF K%(I,3)<=0 THEN GOTO 4260
4200   COLOR 10,0
4210   IF SRS.CLEARED=0 THEN PRINT KLINGON$;
4220   COLOR 7,0
4230   LOCATE CURSY%,CURSX%
4240   PRINT "  (Sensors show";K%(I,3);"units remaining)"
4250   GOTO 4370
4260   COLOR 31,0
4270   IF SRS.CLEARED=0 THEN PRINT KLINGON$;
4280   LOCATE CURSY%,CURSX%
4290   COLOR 16,7
4300   PRINT "**** KLINGON DESTROYED ****"
4310   COLOR 7,0
4320   K3=K3-1
4330   K9=K9-1
4340   K%(I,3)=0
4350   G(Q1,Q2)=G(Q1,Q2)-100
4360   Z(Q1,Q2)=G(Q1,Q2)
4370 NEXT I
4380 GOSUB 9290
4390 FOR I=1 TO 600
4400 NEXT I
4410 FOR I=1 TO 3
4420   LOCATE 3+K%(I,1),48+3*K%(I,2)
4430   IF Q%(K%(I,1),K%(I,2))>=0 THEN GOTO 4470
4440   IF K%(I,3)>0 THEN PRINT KLINGON$;: GOTO 4470
4450   PRINT "   ";
4460   Q%(K%(I,1),K%(I,2))=0
4470 NEXT I
4480 LOCATE CURSY%,CURSX%
4490 IF K9<=0 THEN 5970
4500 GOTO 2060
4510 '*****************************************************************
4520 '*  photon torpedo code begins here
4530 '*****************************************************************
4540 IF P<=0 THEN PRINT "All photon torpedoes expended": GOTO 2060
4550 IF D(5)<0 THEN PRINT "Photon tubes are not operational": GOTO 2060
4560 INPUT"Photon torpedo course (1-9)";C1:IF C1=9 THEN C1=1
4570 IF C1>=1 AND C1<9 THEN 4610
4580 PRINT "Ensign Chekov reports:"
4590 PRINT "  'Incorrect course data, sir!'"
4600 GOTO 2060
4610 CR1=INT(C1)
4620 MOVERANDOM%=1
4630 GOSUB 8230
4640 GOSUB 5500
4650 ITEMP=1
4660 X1=C%(CR1,1)+(C%(CR1+1,1)-C%(CR1,1))*(C1-CR1)
4670 E=E-2
4680 P=P-1
4690 X2=C%(CR1,2)+(C%(CR1+1,2)-C%(CR1,2))*(C1-CR1):X=S1:Y=S2
4700 X=X+X1:Y=Y+X2:X3=INT(X+.5):Y3=INT(Y+.5)
4710 MOVES%(ITEMP,1)=X3
4720 MOVES%(ITEMP,2)=Y3
4730 IF X3<1 OR X3>8 OR Y3<1 OR Y3>8 THEN GOSUB 8450: GOTO 5030
4740 ITEMP=ITEMP+1
4750 IF Q%(X3,Y3)=0 THEN 4700 ELSE GOSUB 8450
4760 IF Q%(X3,Y3)>0 THEN 4870
4770 COLOR 16,7 :PRINT"**** KLINGON DESTROYED ****";:COLOR 7,0
4780 Q%(X3,Y3)=0
4790 K3=K3-1
4800 K9=K9-1
4810 I=1
4820 WHILE K%(I,1)<>X3 OR K%(I,2)<>Y3
4830   I=I+1
4840 WEND
4850 K%(I,3)=0
4860 IF K9<=0 THEN 5970 ELSE GOTO 5020
4870 IF Q%(X3,Y3)<>2 THEN 4900
4880 PRINT"Star at";X3;",";Y3;"absorbed torpedo energy."
4890 GOTO 2060
4900 IF Q%(X3,Y3)<>1 THEN 4560
4910 COLOR 16,7:PRINT"*** STARBASE DESTROYED ***";:COLOR 7,0:PRINT SPC(54)
4920 Q%(X3,Y3)=0
4930 B3=B3-1
4940 B9=B9-1
4950 IF B9>0 OR K9>T-T0-T9 THEN 4990
4960 PRINT"THAT DOES IT, CAPTAIN!!  You are hereby relieved of command"SPC(21)
4970 PRINT"and sentenced to 99 stardates of hard labor on CYGNUS 12!!"
4980 GOTO 5810
4990 PRINT"Starfleet reviewing your record to consider"
5000 PRINT"court martial!":D0=0
5010 Q%(X3,Y3)=0
5020 G(Q1,Q2)=K3*100+B3*10+S3:Z(Q1,Q2)=G(Q1,Q2):GOSUB 5500:GOTO 2060
5030 PRINT "Torpedo missed"
5040 GOTO 2060
5050 '*****************************************************************
5060 '*  shield control
5070 '*****************************************************************
5080 IF D(7)<0 THEN PRINT"Shield control inoperable":GOTO 2060
5090 PRINT "Energy available = ";E+S
5100 INPUT "Number of units to shields";X
5110 IF X<0 OR S=X THEN PRINT"<shields unchanged>":GOTO 2060
5120 IF X<E+S THEN 5160
5130 PRINT "Shield Control reports"
5140 PRINT "  'This is not the federation treasury.'"
5150 PRINT"<shields unchanged>:goto 1990
5160 E=E+S-X
5170 S=X
5180 PRINT "Deflector Control Room report:"
5190 PRINT "  'Shields now at";INT(S);"units"
5200 PRINT "  per your command.'"
5210 GOTO 2060
5220 '*****************************************************************
5230 '*  damage control
5240 '*****************************************************************
5250 IF D(6)>=0 THEN 5400
5260 PRINT "Damage control report not available"
5270 IF D0=0 THEN 2060
5280 D3=0:FOR I=1 TO 8:IF D(I)<0 THEN D3=D3+1
5290 NEXT I:IF D3=0 THEN 2060
5300 PRINT:D3=D3+D4:IF D3>=1 THEN D3=.9000001
5310 PRINT "Technicians standing by to effect"
5320 PRINT "repairs to your ship estimated time"
5330 PRINT "to repair:";.01*INT(100*D3);"stardates"
5340 INPUT "Will you authorize the repair order (Y/N)";A$
5350 A$=LEFT$(A$,1):IF A$<>"y" AND A$<>"Y" THEN 2060
5360 FOR I=1 TO 8
5370   IF D(I)<0 THEN D(I)=0
5380 NEXT I
5390 T=T+D3+.1
5400 IF CLEARFLAG<2 THEN GOSUB 9680
5410 GALRECSET=1
5420 LOCATE 8,30:PRINT"Device            state of repair":FOR R1=1 TO 8
5430 GOSUB 7620:LOCATE 9+R1,30: PRINT G2$;:GG2$=LEFT$(Z$,25-LEN(G2$)): PRINT GG2$;
5440 GG2=INT(D(R1)*100)*.01:LOCATE 9+R1,55: PRINT GG2
5450 NEXT R1: PRINT:IF D0<>0 THEN LOCATE 16,1: GOTO 5280
5460 GOTO 2060
5470 '*****************************************************************
5480 '*  klingons shooting
5490 '*****************************************************************
5500 IF K3<=0 THEN RETURN
5510 IF D0<>0 THEN PRINT"Starbase shields protect the ENTERPRISE":RETURN
5520 TOTHIT=0
5530 FOR I=1 TO 3:IF K%(I,3)<=0 THEN 5570
5540   TOTHIT=TOTHIT+INT((K%(I,3)/FND(1))*(1+RND(1)))
5550   S=S-TOTHIT
5560   E=E-TOTHIT
5570 NEXT I
5580 IF TOTHIT=0 THEN 5710
5590 COLOR 16,7
5600 PRINT "ENTERPRISE HIT!"
5610 COLOR 7,0
5620 GOSUB 9980
5630 PRINT TOTHIT;"Unit hit on ENTERPRISE"
5640 IF S<=0 THEN 5790
5650 PRINT "      <shields down to";S;"units>"
5660 IF TOTHIT<20 THEN 5710
5670 IF RND(1)>.6 OR TOTHIT/S<=.02 THEN 5710
5680 R1=FNR(1):D(R1)=D(R1)-TOTHIT/S-.5*RND(1):GOSUB 7620
5690 PRINT "Damage control reports"
5700 PRINT "'  '";G2$;" damaged by the hit'"
5710 RETURN
5720 '*****************************************************************
5730 '*  end of game
5740 '*****************************************************************
5750 CLS
5760 LOCATE 5,1
5770 PRINT "It is stardate";T
5780 GOTO 5810
5790 PRINT:PRINT "The ENTERPRISE has been destroyed.  The Federation";
5800 PRINT" will be conquered.":GOTO 5750
5810 PRINT"There were";K9;"Klingon battle cruisers left at end of you mission."
5820 PRINT:PRINT:IF B9=0 THEN 5960
5830 PRINT "The Federation is in need of a new starship commander for a similar"
5840 PRINT "mission -- if there is a volunteer, let him or her step forward and"
5850 INPUT"enter 'aye'";A$:IF A$="aye" OR A$="AYE" THEN 610
5860 KEY 1,"LIST "
5870 KEY 2,"RUN"+CHR$(13)
5880 KEY 3,"LOAD"+CHR$(34)
5890 KEY 4, "SAVE"+CHR$(34)
5900 KEY 5, "CONT"+CHR$(13)
5910 KEY 6,","+CHR$(34)+"LPT1:"+CHR$(34)+CHR$(13)
5920 KEY 7, "TRON"+CHR$(13)
5930 KEY 8, "TROFF"+CHR$(13)
5940 KEY 9, "KEY "
5950 KEY 10,"SCREEN 0,0,0"+CHR$(13)
5960 END
5970 PRINT"Congratulations, Captain! The last Klingon battle cruiser"
5980 PRINT"menacing the Federation has been destroyed.":PRINT
5990 PRINT"Your efficiency rating is";1000*(K7/(T-T0))^2:GOTO 5820
6000 '*****************************************************************
6010 '*  short range sensor scan & startup subroutine
6020 '*****************************************************************
6030 IF START=1 THEN CLS: GOSUB 9330: GOSUB 6240: GOSUB 6360: START=0
6040 LOCATE 1,1: START=0
6050 FOR I=S1-1 TO S1+1
6060   FOR J=S2-1 TO S2+1
6070     IF INT(I+.5)<1 OR INT(I+.5)>8 OR INT(J+.5)<1 OR INT(J+.5)>8 THEN 6090
6080     IF Q%(I,J)=1 THEN 6130
6090   NEXT J
6100 NEXT I
6110 D0=0
6120 GOTO 6200
6130 D0=1
6140 CC$="docked"
6150 E=E0
6160 P=P0
6170 PRINT"Shields dropped for docking purposes"
6180 S=0
6190 GOTO 6210
6200 IF E<E0*.1 THEN C$="YELLOW" ELSE C$="GREEN "
6210 GOSUB 8890
6220 RETURN
6230 '*****************************************************************
6240 '*  LIST INFORMATION HEADINGS
6250 '*****************************************************************
6260 LOCATE 4,1: PRINT "Stardate";
6270 LOCATE 5,1: PRINT "Condition";
6280 LOCATE 6,1: PRINT "Quadrant";
6290 LOCATE 7,1: PRINT "Sector";
6300 LOCATE 8,1: PRINT "Photon torpedoes";
6310 LOCATE 9,1: PRINT "Total energy";
6320 LOCATE 10,1: PRINT "Shields";
6330 LOCATE 11,1: PRINT "Klingons remaining";
6340 RETURN
6350 '*****************************************************************
6360 '*  LIST INFORMATION
6370 '*****************************************************************
6380 LOCATE 4,19: TT=INT(10*T)*.1: PRINT TT;
6390 LOCATE 5,20: PRINT "      ";
6400 LOCATE 5,20
6410 IF K3>0  THEN COLOR 23,7: PRINT "*";: COLOR 31,7: PRINT "RED";: COLOR 23,7: PRINT "*";: COLOR 7,0 ELSE PRINT C$;
6420 LOCATE 6,19: PRINT Q1;",";Q2;
6430 LOCATE 7,19: PRINT S1;",";S2;
6440 LOCATE 8,19: PRINT INT(P);
6450 LOCATE 9,19: PRINT INT(E+S);
6460 LOCATE 10,1
6470 IF S<200 AND K3>0 THEN COLOR 31,0: PRINT "SHIELDS LOW";: COLOR 7,0: ELSE PRINT "Shields    ";
6480 LOCATE 10,19: PRINT INT(S);
6490 LOCATE 11,19: PRINT INT(K9);
6500 IF K3>0 THEN GOSUB 9790
6510 RETURN
6520 REM library computer code
6530 CM1$="GALSTATORBASDIRREG"
6540 IF D(8)<0 THEN PRINT"Computer Disabled":GOTO 2060
6550 KEY 1, "GAL RCD"+CHR$(13)
6560 KEY 2, "STATUS"+CHR$(13)
6570 KEY 3, "DIR/DIST"+CHR$(13)
6580 KEY 4, "BASE NAV"+CHR$(13)
6590 KEY 5, "TOR DATA"+CHR$(13)
6600 KEY 6, "REG MAP"+CHR$(13)
6610 KEY 7,CHR$(13):KEY 8,CHR$(13):KEY 9,CHR$(13):KEY 10,CHR$(13)
6620 INPUT"Computer awaiting your command";CM$
6630 H8=1
6640 FOR K= 1 TO 6
6650   IF LEFT$(CM$,3)<>MID$(CM1$,3*K-2,3) THEN 6670
6660   ON K GOTO 6890,7100,7220,7480,7280,6810
6670 NEXT K
6680 GOSUB 9680
6690 LOCATE 3,36: PRINT "Functions available from library-computer:";
6700 LOCATE 4,36: PRINT "   KEY 1= Cumulative galactic record";
6710 LOCATE 5,36: PRINT "   KEY 2 = Status report";
6720 LOCATE 6,36: PRINT "   KEY 3 = Direction/distance calculator";
6730 LOCATE 7,36: PRINT "   KEY 4 = Starbase nav data";
6740 LOCATE 8,36: PRINT "   KEY 5 = Photon torpedo data";
6750 LOCATE 9,36: PRINT "   KEY 6 = Galaxy 'region name' map";
6760 GALRECSET=1
6770 GOTO 6620
6780 '*****************************************************************
6790 '*  setup to change cum gal record to galaxy map
6800 '*****************************************************************
6810 GOSUB 9330
6820 H8=0
6830 G5=1
6840 GOSUB 9680
6850 LOCATE 2,55
6860 PRINT"THE GALAXY";
6870 GOTO 6940
6880 '*****************************************************************
6890 '*  cum galactic record
6900 '*****************************************************************
6910 GOSUB 9330
6920 GOSUB 9680
6930 LOCATE 2,33: PRINT "Computer record of galaxy for quadrant";Q1;",";Q2;
6940 LOCATE 4,28
6950 PRINT"     1     2     3     4     5     6     7     8"
6960 O1$= "----- ----- ----- ----- ----- ----- ----- -----"
6970 GALRECSET=1
6980 FOR I=5 TO 21 STEP 2
6990 LOCATE I,31: PRINT O1$;
7000 NEXT I
7010 FOR I=1 TO 8:LOCATE 4+I*2,28: PRINT I;" ";: IF H8=0 THEN 7050
7020 FOR J=1 TO 8: IF Z(I,J)=0 THEN PRINT"***   ";:GOTO 7040
7030 PRINT RIGHT$(STR$(Z(I,J)+1000),3);"   ";
7040 NEXT J:GOTO 7070
7050 Z4=I:Z5=1:GOSUB 7770:J0=INT(43-.5*LEN(G2$)): PRINT TAB(J0);G2$;
7060 Z5=5:GOSUB 7770:J0=INT(67-.5*LEN(G2$)):PRINT TAB(J0);G2$;
7070 NEXT I
7080 GOTO 2060
7090 REM status report
7100 GOSUB 9330:GOSUB 9660: LOCATE 2,33: PRINT"Status Report":X$="":IF K9>1 THEN X$="s"
7110 LOCATE 3,30: PRINT"Klingon";X$;" left: ";K9
7120 LOCATE 4,30: PRINT"Mission must be completed in";.1*INT((T0+T9-T)*10);"stardates."
7130 X$="s":IF B9<2 THEN X$="":IF B9<1 THEN 7190
7140 LOCATE 5,30: PRINT "The federation is maintaining";B9;"starbase";X$;
7150 LOCATE 6,30: PRINT "in the galaxy.";
7160 CLEARFLAG=5
7170 GOTO 5250
7180 LOCATE 18,1
7190 LOCATE 5,30: PRINT "Your stupidity has left you on your own in"
7200 LOCATE 6,30: PRINT "the galaxy -- you have no starbases left!":GOTO 5250
7210 REM torpedo, base nav, d/d calculator
7220 GOSUB 9330:IF K3<=0 THEN 3930
7230 X$="":IF K3>1 THEN X$="s"
7240 PRINT"From ENTERPRISE to Klingon battle cruiser";X$
7250 H8=0:FOR I=1 TO 3:IF K%(I,3)<=0 THEN 7470
7260 W1=K%(I,1):X=K%(I,2)
7270 C1=S1:A=S2:GOTO 7320
7280 GOSUB 9330:PRINT"Direction/Distance Calculator:"
7290 PRINT"You are at quadrant ";Q1;",";Q2;" sector ";S1;",";S2
7300 INPUT " Initial coordinates (x,y)";C1,A
7310 INPUT " Final corrdinates (x,y)";W1,X
7320 X=X-A: A=C1-W1:IF X<0 THEN 7400
7330 IF A<0 THEN 7420
7340 IF X>0 THEN 7360
7350 IF A=0 THEN C1=5:GOTO 7370
7360 C1=1
7370 IF ABS(A)<=ABS(X) THEN 7390
7380 PRINT"Direction =";C1+(((ABS(A)-ABS(X))+ABS(A))/ABS(A)):GOTO 7460
7390 PRINT"Direction =";C1+(ABS(A)/ABS(X)):GOTO 7460
7400 IF A>0 THEN C1=3:GOTO 7430
7410 IF X<>0 THEN C1=5:GOTO 7370
7420 C1=7
7430 IF ABS(A)>=ABS(X) THEN 7450
7440 PRINT"Direction =";C1+(((ABS(X)-ABS(A))+ABS(X))/ABS(X)):GOTO 7460
7450 PRINT"Direction =";:CC1=C1+(ABS(X)/ABS(A)):PRINT CC1
7460 PRINT"Distance =";SQR(X^2+A^2):IF H8=1 THEN 2060
7470 NEXT I:GOTO 2060
7480 GOSUB 9330:IF B3<>0 THEN PRINT"From ENTERPRISE to Starbase:":W1=B4:X=B5
7490 GOTO 7270
7500 PRINT "Mr. Spock reports:"
7510 PRINT "  'Sensors show no starbases"
7520 PRINT "   in this quadrant.'"
7530 GOTO 2060
7540 '***************************************************************
7550 '* FINDS RANDOM EMPTY SPOT IN QUADRANT
7560 '***************************************************************
7570 R1=FNR(1)
7580 R2=FNR(1)
7590 IF Q%(R1,R2)=0 THEN RETURN ELSE 7570
7600 RETURN
7610 REM prints device name
7620 ON R1 GOTO 7630,7640,7650,7660,7670,7680,7690,7700
7630 G2$="Warp Engines":RETURN
7640 G2$="Short Range Sensors":RETURN
7650 G2$="Long Range Sensors":RETURN
7660 G2$="Phaser Control":RETURN
7670 G2$="Photon Tubes":RETURN
7680 G2$="Damage Control":RETURN
7690 G2$="Shield Control":RETURN
7700 G2$="Library-Computer":RETURN
7710 REM string comparison in quadrant array
7720 Z1=INT(Z1+.5):Z2=INT(Z2+.5):S8=(Z2-1)*3+(Z1-1)*24+1:Z3=0
7730 IF MID$(Q$,S8,3)<>A$ THEN RETURN
7740 Z3=1:RETURN
7750 REM quadrant name in g2$ from z4,z5 (=q1,q2)
7760 REM call with g5=1 to get region name only
7770 IF Z5<+4 THEN ON Z4 GOTO 7790,7800,7810,7820,7830,7840,7850,7860
7780 GOTO 7870
7790 G2$="Antares":GOTO 7960
7800 G2$="Rigel":GOTO 7960
7810 G2$="Procyon":GOTO 7960
7820 G2$="Vega":GOTO 7960
7830 G2$="Canopus":GOTO 7960
7840 G2$="Altair":GOTO 7960
7850 G2$="Sagittarius":GOTO 7960
7860 G2$="Pollux":GOTO 7960
7870 ON Z4 GOTO 7880,7890,7900,7910,7920,7930,7940,7950
7880 G2$="Sirius":GOTO 7960
7890 G2$="Deneb":GOTO 7960
7900 G2$="Capella":GOTO 7960
7910 G2$="Betelgeuse":GOTO 7960
7920 G2$="Aldebaran":GOTO 7960
7930 G2$="Regulus":GOTO 7960
7940 G2$="Arcturus":GOTO 7960
7950 G2$="Spica"
7960 IF G5<>1 THEN ON Z5 GOTO 7980,7990,8000,8010,7980,7990,8000,8010
7970 RETURN
7980 G2$=G2$+" i":RETURN
7990 G2$=G2$+" ii":RETURN
8000 G2$=G2$+" iii":RETURN
8010 G2$=G2$+" iv":RETURN
8020 '*********************************************************
8030 '* MOVE ENTERPRISE ALONG COURSE IN MOVE%
8040 '*********************************************************
8050 Q%(MOVES%(1,1),MOVES%(1,2))=0
8060 GOSUB 9290
8070 I=2
8080 WHILE MOVES%(I,1)<>0
8090   LOCATE 3+MOVES%(I-1,1),48+3*MOVES%(I-1,2)
8100   IF SRS.CLEARED=0 THEN PRINT "   ";
8110   LOCATE 3+MOVES%(I,1),48+3*MOVES%(I,2)
8120   IF SRS.CLEARED=0 THEN PRINT ENTERPRISE$;
8130   MOVES%(I-1,1)=0
8140   I=I+1
8150 WEND
8160 Q%(MOVES%(I-1,1),MOVES%(I-1,2))=30000
8170 MOVES%(I-1,1)=0
8180 LOCATE CURSY%,CURSX%
8190 RETURN
8200 '*****************************************************************
8210 '*  MOVE KLINGONS
8220 '*****************************************************************
8230 FOR I=1 TO K3
8240   IF K%(I,3)=0 THEN 8380
8250   FOR ITEMP=1 TO MOVERANDOM%+1
8260     GOSUB 7570
8270     IF ABS(R1-K%(I,1))>1 OR ABS(R2-K%(I,2))>1 THEN 8370
8280     IF SRS.CLEARED=1 THEN 8330
8290     LOCATE 3+K%(I,1),48+3*K%(I,2)
8300     PRINT "   ";
8310     LOCATE 3+R1,48+3*R2
8320     PRINT KLINGON$;
8330     Q%(K%(I,1),K%(I,2))=0
8340     Q%(R1,R2)=-1
8350     K%(I,1)=R1
8360     K%(I,2)=R2
8370   NEXT ITEMP
8380 NEXT I
8390 LOCATE 15,1
8400 MOVERANDOM%=0
8410 RETURN
8420 '*****************************************************************
8430 '*  MOVE TORPEDO ALONG COURSE MOVES%
8440 '*****************************************************************
8450 GOSUB 8890
8460 GOSUB 9290   'SAVE CURSOR POSITION
8470 GOSUB 9860
8480 IF MOVES%(2,1)<>0 THEN LOCATE 3+MOVES%(1,1),49+3*MOVES%(1,2) ELSE ITEMP=1: GOTO 8630
8490 ITEMP=3
8500 IF SRS.CLEARED=0 THEN PRINT TORPEDO$;
8510 WHILE MOVES%(ITEMP,1)<>0
8520   LOCATE 3+MOVES%(ITEMP-2,1),49+3*MOVES%(ITEMP-2,2)
8530   IF SRS.CLEARED=0 THEN PRINT " ";
8540   LOCATE 3+MOVES%(ITEMP-1,1),49+3*MOVES%(ITEMP-1,2)
8550   IF SRS.CLEARED=0 THEN PRINT TORPEDO$;
8560   MOVES%(ITEMP-2,1)=0
8570   MOVES%(ITEMP-2,2)=0
8580   ITEMP=ITEMP+1
8590 WEND
8600 LOCATE 3+MOVES%(ITEMP-2,1),49+3*MOVES%(ITEMP-2,2)
8610 IF SRS.CLEARED=0 THEN PRINT " "; ELSE GOTO 8750
8620 ITEMP=ITEMP-1
8630 IF MOVES%(ITEMP,1)<1 OR MOVES%(ITEMP,2)<1 OR MOVES%(ITEMP,1)>8 OR MOVES%(ITEMP,2)>8 THEN 8750
8640 IF Q%(MOVES%(ITEMP,1),MOVES%(ITEMP,2))=0 THEN 8750
8650 LOCATE 3+MOVES%(ITEMP,1),48+3*MOVES%(ITEMP,2)
8660 IF Q%(MOVES%(ITEMP,1),MOVES%(ITEMP,2))<0 THEN A$=KLINGON$: GOTO 8680
8670 IF Q%(MOVES%(ITEMP,1),MOVES%(ITEMP,2))=1 THEN A$=STARBASE$ ELSE A$=STAR$
8680 COLOR 31,0
8690 PRINT A$;
8700 COLOR 7,0
8710 FOR JTEMP=1 TO 500
8720 NEXT JTEMP
8730 LOCATE 3+MOVES%(ITEMP,1),48+3*MOVES%(ITEMP,2)
8740 IF A$=STAR$ THEN PRINT A$ ELSE PRINT "   ";
8750 MOVES%(ITEMP,1)=0
8760 LOCATE CURSY%,CURSX%
8770 RETURN
8780 '*****************************************************************
8790 '*  CLEAR SHORT RANGE SCAN AREA
8800 '*****************************************************************
8810 FOR I=1 TO 13
8820   LOCATE I,30
8830   PRINT STRING$(49," ");
8840 NEXT I
8850 RETURN
8860 '*********************************************************
8870 '* DRAW QUADRANT FOR SRS
8880 '*********************************************************
8890 GOSUB 9290
8900 IF SRS.CLEARED=0 THEN SRS.CLEARED=2: GOTO 8920
8910 IF GALRECSET THEN GOSUB 9680 ELSE  GOSUB 8810
8920 IF D(2)>=0 THEN 9000
8930 GOSUB 8800
8940 LOCATE 6,50
8950 PRINT "SHORT RANGE SENSORS";
8960 LOCATE 7,53
8970 PRINT "ARE DISABLED";
8980 SRS.CLEARED=1
8990 GOTO 9240
9000 IF SRS.CLEARED=2 THEN 9230
9010 LOCATE 1,55
9020 PRINT "SHORT RANGE SCAN";
9030 LOCATE 2,52
9040 PRINT "1  2  3  4  5  6  7  8";
9050 LOCATE 3,50
9060 PRINT UPPERLINE$;
9070 FOR I=1 TO 8
9080   LOCATE I+3,49
9090   PRINT CHR$(48+I)+CHR$(199);
9100   FOR J=1 TO 8
9110     IF Q%(I,J)=0 THEN 9170
9120     IF Q%(I,J)=2 THEN LOCATE I+3,49+3*J: PRINT CHR$(15);: GOTO 9170
9130     LOCATE I+3,48+3*J
9140     IF Q%(I,J)=30000 THEN PRINT ENTERPRISE$;: GOTO 9170
9150     IF Q%(I,J)<0 THEN PRINT KLINGON$;: GOTO 9170
9160     PRINT STARBASE$;
9170   NEXT J
9180   LOCATE I+3,75
9190   PRINT CHR$(182);
9200 NEXT I
9210 LOCATE 12,50
9220 PRINT LOWERLINE$;
9230 SRS.CLEARED=0
9240 LOCATE CURSY%,CURSX%
9250 RETURN
9260 '***************************************************************
9270 '*  SAVE CURSOR POSITION IN CURSX% AND CURSY%
9280 '***************************************************************
9290 CURSX%=POS(N)
9300 CURSY%=CSRLIN
9310 RETURN
9320 '***************************************************************
9330 '*   ------- set function keys  for game -------
9340 '***************************************************************
9350 KEY OFF
9360 KEY 1,"NAV"+CHR$(13)
9370 KEY 2,"SRS"+CHR$(13)
9380 KEY 3,"LRS"+CHR$(13)
9390 KEY 4,"PHASERS"+CHR$(13)
9400 KEY 5,"TORPEDO"+CHR$(13)
9410 KEY 6,"SHIELDS"+CHR$(13)
9420 KEY 7,"DAMAGE REPORT"+CHR$(13)
9430 KEY 8,"COMPUTER"+CHR$(13)
9440 KEY 9,"RESIGN"+CHR$(13)
9450 KEY 10,"CLEAR"+CHR$(13)
9460 KEY ON
9470 RETURN
9480 '*****************************************************************
9490 '*  CLEAR AND REDRAW SCREEN
9500 '*****************************************************************
9510 CLS
9520 GALRECSET=0
9530 SRS.CLEARED=1
9540 GOSUB 6260
9550 GOSUB 8890
9560 GOTO 2060
9570 '***************************************************************
9580 '*  CLEAR LOWER SCREEN FOR LRS SCAN
9590 '***************************************************************
9600 FOR ITEMP=13 TO 24
9610   LOCATE ITEMP,1: PRINT STRING$(79," ");
9620 NEXT ITEMP
9630 LOCATE 14,1
9640 RETURN
9650 '***************************************************************
9660 '*  CLEAR MOST OF SCREEN
9670 '***************************************************************
9680 FOR ITEMP=1 TO 11
9690   LOCATE ITEMP,26: PRINT STRING$(53," ");
9700 NEXT ITEMP
9710 FOR ITEMP=12 TO 24
9720   LOCATE ITEMP,1: PRINT STRING$(79," ");
9730 NEXT ITEMP
9740 SRS.CLEARED=1
9750 GALRECSET=0
9760 CLEARFLAG=0
9770 LOCATE 14,1
9780 RETURN
9790 '    red alert sound
9800 FOR J= 1 TO 5
9810   FOR K=1000 TO 2000 STEP 20
9820     SOUND K,.01*18.2
9830   NEXT K
9840 NEXT J
9850 RETURN
9860 '   torpedo sound
9870 FOR J = 1500 TO 100 STEP -20
9880   SOUND J,.01*18.2
9890   SOUND 3600-J,.01*18.2
9900 NEXT J
9910 RETURN
9920 '    phaser sound
9930 FOR J= 1 TO 60
9940   SOUND 800,.01*18.2
9950   SOUND 2500,8.000001E-03*18.2
9960 NEXT J
9970 RETURN
9980 '           alarm sound
9990 FOR SI = 1 TO 4
10000  FOR J=  800 TO 1500 STEP 20
10010    SOUND J,.01 *18.2
10020  NEXT J
10030  FOR K = 1500 TO 800 STEP -20
10040    SOUND K, .01 *18.2
10050  NEXT K
10060 NEXT SI
10070 RETURN

