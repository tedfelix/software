10 CLS:SCREEN 0:WIDTH 80
1000 X=14
1090 FOR I=1 TO 4
1100 B$(0)=B$(0)+" ллллллл "+SPACE$(X)
1110 B$(1)=B$(1)+"ллллллллл"+SPACE$(X)
1120 B$(2)=B$(2)+"ллллллллл"+SPACE$(X)
1130 B$(3)=B$(3)+"лл     лл"+SPACE$(X)
1140 NEXT:FOR I=0 TO 3:B$(I)=LEFT$(B$(I),79):NEXT
1200 LOCATE 18,1
1210 FOR I=0 TO 3:PRINT B$(I):NEXT
1299 ' Invaders
1300 L$(0)="-   -   -   -   -   -   -   -   -   -   - "
1310 L$(1)="=   =   =   =   =   =   =   =   =   =   = "
1320 L$(2)="|   |   |   |   |   |   |   |   |   |   | "
1330 L$(3)="*   *   *   *   *   *   *   *   *   *   * "
1340 L$(4)="#   #   #   #   #   #   #   #   #   #   # "
1350 L$(5)="V   V   V   V   V   V   V   V   V   V   V "
1360 L$(6)="o   o   o   o   o   o   o   o   o   o   o "
1370 L$(7)="O   O   O   O   O   O   O   O   O   O   O "
1380 L$(8)="@   @   @   @   @   @   @   @   @   @   @ "
1390 L$(9)="л   л   л   л   л   л   л   л   л   л   л "
1400 LOCATE 3,1:FOR I=0 TO 9:PRINT L$(I):NEXT
1410 L(9)=219:L(8)=64:L(7)=79:L(6)=111:L(5)=86:L(4)=35:L(3)=42:L(2)=124:L(1)=61:L(0)=45
1420 LOCATE 23,5:PRINT CHR$(24);
2000 R=3:I=0
2005 I=I+1:IF I>30 THEN 2030 ELSE LOCATE R,1
2010 PRINT TAB(I)L$(0):PRINT TAB(I)L$(1):PRINT TAB(I)L$(2):PRINT TAB(I)L$(3):PRINT TAB(I)L$(4):PRINT TAB(I)L$(5):PRINT TAB(I)L$(6):PRINT TAB(I)L$(7):PRINT TAB(I)L$(8):PRINT TAB(I)L$(9)
2020 GOTO 2005
2030 FOR I=30 TO 1 STEP-1:LOCATE R,1
2040 PRINT TAB(I)L$(0):PRINT TAB(I)L$(1):PRINT TAB(I)L$(2):PRINT TAB(I)L$(3):PRINT TAB(I)L$(4):PRINT TAB(I)L$(5):PRINT TAB(I)L$(6):PRINT TAB(I)L$(7):PRINT TAB(I)L$(8):PRINT TAB(I)L$(9)
2050 NEXT:LOCATE R,1:PRINT SPACE$(50):R=R+1:GOTO 2005
RINT TAB(I)L$(6):PRINT TAB(I)L$(7):PRINT TAB