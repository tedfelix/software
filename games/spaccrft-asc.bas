1 CLS:FOR XX=1 TO 6:PRINT:NEXT:DIM A(100),A0(100),A1(7),A2(7),A3(6),A4(6),AY(7)
2 WIDTH 40:LOCATE 1,1,0:PRINT"A Spacecraft Simulator":FOR XX=1 TO 1400:NEXT
3 PRINT"By: Gary Sivak":FOR XX=1 TO 500:NEXT
4 PRINT:PRINT"IBM Adapted By:":FOR XX=1 TO 500:NEXT
5 PRINT"Ted Felix"
6 FOR XX=1 TO 3000:NEXT:CLS:WIDTH 80
7 PRINT"This program enables the player to design and put into orbit a multistage
8 PRINT"spacecraft launched from earth-based conditions.  By asking you for engine
9 PRINT"throttle settings, thrust angles, and firing times, your IBM puts you at
10 PRINT"the controls of a multistage aircraft of your own design as you pilot it
11 PRINT"from the earth's surface into orbit.":PRINT
12 PRINT"Continuous data displays of the user's status are presented, as well as
13 PRINT"arrays of altitude and range information for possible plotting at mission's
14 PRINT"end.":PRINT
15 PRINT"The program first asks for and verifies all ship design parameters; overall
16 PRINT"(as in number of stages, iteration time, orbital height, and ship's payload
17 PRINT"weight) and individual stage parameters (as in stage X's fuel and hull
18 PRINT"weights, thrust, and specific impulse).":PRINT
19 PRINT"A complete program description is provided in the article `A Spacecraft
20 PRINT"Simulator' -- Nov. 79 issue of Byte magazine.":PRINT:PRINT
21 PRINT TAB(25)"Press ENTER to take control"
29 AB$=INPUT$(1):IF AB$<>CHR$(13)THEN 29
30 CLS:FOR XX=1 TO 700:NEXT:AA$="Section 1: Spacecraft Design"
31 PRINT AA$:FOR XX=1 TO 100:NEXT:PRINT
35 PRINT:PRINT"Enter:   * 1). Number of rocket stages (Max. 6)
40 PRINT"           2). Iteration time (.1 or .01)
45 PRINT"           3). Desired orbital height (Miles)
50 PRINT"           4). Ship's payload weight (Pounds)
55 INPUT A5:LOCATE 4,10:PRINT" ";:LOCATE 5,10:PRINT"*";:A6=A5+1
60 LOCATE 9,1:INPUT A7:LOCATE 5,10:PRINT" ";:LOCATE 6,10:PRINT"*";
65 LOCATE 10,1:INPUT A8:LOCATE 6,10:PRINT" ";:LOCATE 7,10:PRINT"*";
67 LOCATE 11,1:INPUT A2(A6)
70 FOR XX=1 TO 25:PRINT:NEXT:AB$=STRING$(22,196)
71 PRINT"Verification:":PRINT AB$:PRINT:PRINT
72 PRINT"Number of stages: ";A5
73 PRINT"Iterations:       ";A7
74 PRINT"Desired orbit:    ";A8
75 PRINT"Ship's payload:   ";A2(A6):PRINT:PRINT:PRINT
76 PRINT"Press ENTER to continue"
77 A$=INPUT$(1):IF A$<>CHR$(13)THEN 77
78 CLS:FOR XX=1 TO 500:NEXT
100 FOR A9=1 TO A5:B=A6-A9:B0=B+1:CLS:PRINT AA$
110 PRINT TAB(12);"Stage number #";B:PRINT
115 PRINT"Enter:   * 1). Fuel weight (Pounds)
120 PRINT"           2). Hull (or tank) weight (Pounds)
125 PRINT"           3). Thrust
130 PRINT"           4). Specific impulse fuel (or) oxidizer:
135 PRINT TAB(15)"Gasoline=250, Peroxide=300, Hydrogen=500"
140 INPUT A1(B):LOCATE 4,10:PRINT" ";:LOCATE 5,10:PRINT"*";
145 LOCATE 10,1:INPUT A2(B):LOCATE 5,10:PRINT" ";:LOCATE 6,10:PRINT"*";:AY(B)=A2(B)
150 A2(B)=A2(B)+A2(B0)+A1(B0):B1=A2(B)+A1(B)
155 LOCATE 6,23:PRINT"---------> At least"B1"pounds."
160 LOCATE 11,1
165 INPUT A3(B):LOCATE 6,10:PRINT" ";:LOCATE 7,10:PRINT"*";:LOCATE 12,1:INPUT A4(B)
170 FOR XX=1 TO 25:PRINT:NEXT:PRINT"Verification:":PRINT
175 XY=A6-1:FOR XX=1 TO A9
180 PRINT"Stage"XY": Fuel="STR$(A1(XY));
185 PRINT", hull="STR$(AY(XY))", thrust="STR$(A3(XY));
190 PRINT", impulse="STR$(A4(XY)):XY=XY-1:NEXT:PRINT
195 PRINT"Press ENTER to continue..."
200 AB$=INPUT$(1):IF AB$<>CHR$(13)THEN 200
210 NEXT A9
305 PRINT"Initializing variables, please wait..."
310 B2=10
320 B3=B2*A7
330 B4=360
340 B5=B3/100
350 B6=5280*.3048
360 B7=6.6E-11*5.983001E+24
370 B8=ATN(1)/45
380 B9=90
390 C=1
400 C0=SQR(B7/9.80665)
410 C1=C0
420 C2=SQR(B7/(C0+B6*A8))/.3048
430 C3=0:C4=0:C5=0:C6=0:C7=0:C8=0:C9=0
500 D=0:D0=0:D1=0:D2=0:D3=0
543 FOR XX=1 TO 25:PRINT:NEXT
550 PRINT"The ship can swivel"B2"degrees/second."
560 PRINT"Earth's gravity is 32.174 feet/second/second."
570 PRINT"Forward velocity needed for orbit"C2"feet/second."
571 FOR XX=1 TO 1600:NEXT:FOR XX=1 TO 25:PRINT:NEXT
572 LOCATE 23,20:PRINT"All systems go":FOR XX=1 TO 500:NEXT
580 D=D+1
590 D4=A2(D)/2.2046
600 D5=A3(D)/A4(D)/2.2046
610 D6=A1(D)/2.2046
620 D7=D6
630 D8=A3(D)/2.2046*9.80665:PRINT
640 PRINT"Ready to ignite stage"D"... Press ENTER for ignition.";
646 AB$=INPUT$(1):IF AB$<>CHR$(13)THEN 646
648 CLS:WIDTH 40:LOCATE 12,15:PRINT"Ignition":GOTO 1090
660 LOCATE 15,1:PRINT"Enter:   * 1). Throttle setting in percent (0-100).
661 PRINT"           2). Thrust angle in degrees ((-"B4") to"B4")
662 PRINT"           3). Burn time in seconds.":ZZ$=""
665 INPUT ZZ$:LOCATE 15,10:PRINT" ";:LOCATE 16,10:PRINT"*";
666 D9=VAL(ZZ$):LOCATE 19,1:INPUT E:LOCATE 16,10:PRINT" ";:LOCATE 17,10:PRINT"*";
667 LOCATE 20,1:INPUT E0:CLS
700 D9=ABS(D9/100)
710 E1=D9*D8
720 E2=D9*D5*A7
730 E3=E2/100
740 E4=E0-(A7/100)
750 E5=C5*C1
760 E6=0
770 IF E0=0 THEN 1080
780 IF C1<C0 THEN 1080
790 E6=E6+A7
791 IF X1=1 THEN 800 ELSE CLS:LOCATE 2,13:PRINT"*** Flight Log ***"
792 PRINT:X1=1:PRINT"Elapsed flight time:"
793 PRINT"Fuel left:"TAB(36)"%"
794 PRINT"Ship's angle:"TAB(36)"ø"
795 PRINT"Altitude:"TAB(37)"Miles"
796 PRINT"Ascent rate:"TAB(37)"Miles/Hour"
797 PRINT"Orbital velocity:"TAB(36)"%
798 PRINT"Orbital height:"TAB(36)"%
799 LOCATE 24,32:PRINT"Stage Number"D;
800 E7=D7-E2
810 E8=E1/(D4+(D7+E7)/2)
820 IF E7>=E3 THEN 850
830 E7=0:E8=0
850 IF ABS(E-B9)<B5 THEN 930
860 IF E<B9 THEN 890
870 B9=B9+B3
880 GOTO 900
890 B9=B9-B3
900 E9=B9*B8
910 C4=COS(E9)
920 C=SIN(E9)
930 F=E8*C4
940 F0=E8*C
950 F1=C5+F*A7
960 C6=(C5+F1)/2
970 C7=C7+C6*A7
980 F2=F0+C6^2/C1-B7/C1^2
990 F3=C8+F2*A7
1000 F4=C1+(C8+F3)/2*A7
1010 IF D9<>0 THEN 1030
1020 F1=E5/F4
1030 D7=E7
1039 XX=((F4-C0)/.3048)/5280:XY=XX*5280
1040 C5=F1:TIM=C3+E6:H=TIM\3600:TIM=TIM-H*3600:M=(TIM+.0005)\60:TIM=TIM-M*60:LOCATE 4,23:PRINT" "USING"##:##:##.###";H;M;TIM
1041 LOCATE 5,32:PRINT" "USING"###";INT(100*D7/D6);
1042 LOCATE 6,31:PRINT" "USING"####";B9;
1043 LOCATE 7,26:PRINT" "USING"#####.###";XX;
1044 LOCATE 8,26:PRINT" "USING"#####.###";((F3/.3048)*15)/22;
1045 LOCATE 9,27:PRINT" "USING"####.###";((C5/.3048)*100)/C2;
1046 LOCATE 10,27:PRINT" "USING"####.###";100*(XY/(A8*5280));
1050 C8=F3
1060 C1=F4
1070 IF E6<E4 THEN 770
1080 C3=C3+E6
1090 D2=D2+1
1100 A(D2)=(C1-C0)/.3048
1110 IF C9>=A(D2) THEN 1130
1120 C9=A(D2)
1130 IF A(D2)>=0 THEN 1150
1140 A(D2)=0
1150 IF A(D2)<400000! THEN 1170
1160 D3=D3+1
1170 F5=A(D2)/5280
1180 F6=C8/.3048
1190 F7=F6*15/22
1200 F8=C5/.3048
1210 F9=F8*15/22
1220 A0(D2)=C7/B6
1230 G=100*D7/D6
1240 G0=D7/D5
1250 G1=B7/C1^2-C6^2/C1
1260 G2=D8/(D4+D7)/.3048
1270 G3=G2*15/22
1280 G4=G2-(G1/.3048)
1290 G5=G4*15/22
1300 G6=G1/.3048/G2
1310 G7=100*G6
1320 G8=90
1330 IF G6>1 THEN 1350
1340 G8=ATN(G6/SQR(1-G6^2))/B8
1350 G9=SQR(B7/C1)/.3048
1360 H=100*F8/C2
1370 H0=100*A(D2)/(A8*5280)
1380 H1=100*F8/G9
1390 H2=(C2-F8)/G2
1400 H3=(G9-F8)/G2
1410 IF F6>=0 THEN 1440    ' was if f6>0 then 1440 !!!!
1420 H4=(A8*5280-A(D2))/F6
1430 IF H4<>9999.99 THEN 1460
1440 H4=9999.99
1460 CLS:WIDTH 80:IF X2 THEN 1464 ELSE X2=1
1462 PRINT"Section 2: Launch and orbit spacecraft":PRINT
1464 IF D3<>1 THEN 1480
1470 PRINT"400K Feet acheived, you are in vacuum."
1480 PRINT"Flight time","Fuel left","@ Full throt.","Ship Angle"
1490 PRINT C3;"Sec,",G"%",G0"Sec,",B9"ø"
1500 PRINT:X1=0
1510 PRINT"Altitude","Ascent rate","Forward V.","Range"
1520 PRINT A(D2)"Ft.",F6"Ft/Sec",F8"Ft/Sec",A0(D2)"Miles"
1530 PRINT F5"Mi",F7"Mi/Hr",F9"Mi/Hr
1540 FOR XX=1 TO 5:PRINT:NEXT
1541 PRINT TAB(35)"Press ENTER to continue"
1542 AB$=INPUT$(1):IF AB$<>CHR$(13)THEN 1541
1543 CLS:FOR XX=1 TO 200:NEXT
1550 PRINT"Max Accel","Max Vert Accel","Angle (C.A.)","Throt (C.A.)"
1580 PRINT G2"FSS",G4"FSS",,"Full Throt.","Vert. Pos."
1590 PRINT G3"Mi/H/S",G5"Mi/H/S",,G8"ø",G7"%"
1600 PRINT
1610 PRINT H"% Orbital velocity",H0"% Orbital height."
1620 PRINT H1"% Velocity needed for orbit at current altitude."
1630 PRINT
1640 PRINT"Time to achieve:"
1650 PRINT"Orb. Alt.","Orb. Vel.","Cur. Alt. Orb. Vel.
1660 PRINT"@ Cur. rate","@ full Throt.","@ full Throt."
1670 PRINT H4"Sec.",H2"Sec.",H3"Sec."
1680 PRINT
1690 IF H<100 THEN 1760
1700 IF H0<100 THEN 1760
1710 D0=D0+1
1720 IF D0>1 THEN 1760
1730 PRINT"In desired orbit.  Enter (1) Continue, (2) Plot."
1740 INPUT H5
1750 IF H5=2 THEN 1920
1760 IF C3=0 THEN 660
1770 IF D7<=E3 THEN 1800
1780 IF A(D2)<=0 THEN 1800
1790 GOTO 660
1800 IF A(D2)=0 THEN 1890
1810 IF D<A5 THEN 580
1820 D1=D1+1
1830 IF D1<>1 THEN 1850
1840 PRINT"Last stage shut-down."
1850 IF D0<>0 THEN 1880
1860 IF A(D2)<=0 THEN 1880
1870 GOTO 660
1880 IF A(D2)>0 THEN 1920
1890 H6=INT(SQR(F6^2+F8^2)+.5)
1900 H7=INT(SQR(F7^2+F9^2)+.5)
1910 PRINT"You crashed at"H6"Ft/Sec,"H7"Mi/Hr.
1920 PRINT"After"D2"plot points:"
1930 FOR H8=1 TO D2
1940 ' Graphics?
1950 NEXT
1960 H9=25
1970 '
1980 I=C9*H9/100*1.0001
1990 I0=D2+1
2000 I0=I0-1
2010 IF A(I0)>I THEN 2000
2020 I1=100*A0(I0)/A0(D2)
2030 PRINT"Lower"H9"% or"I"miles of maximum altitude attained."
2040 PRINT"First"I1"% or"A0(I0)"miles of total range."
2050 PRINT"With"I0"steps:
2060 FOR I2=1 TO I0
2070 ' Graphics?
2080 NEXT

