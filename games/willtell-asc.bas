1 ' Copyright 2017, Ted Felix
2 ' File date: 7/16/1982
50 SP=1.3:KEY OFF
100 RANDOMIZE VAL(TIME$)+VAL(MID$(TIME$,4,2))+VAL(RIGHT$(TIME$,2))
200 DEF FNRAND(X)=INT(RND*X)+1
250 ARROW$=">>---}"
300 COLOR 7,0,0:SCREEN 0,0,0:WIDTH 40:CLS
320 PRINT"            William Tell"
400 LOCATE 9,37:COLOR 4,0,0:PRINT"�";
500 COLOR 7,0,0:LOCATE 10,37:PRINT CHR$(2)
600 LOCATE 11,36:PRINT"_|_"
700 LOCATE 12,37:PRINT"|"
800 LOCATE 13,36:PRINT"/ \
900 Y=FNRAND(4)+9
1000 LOCATE 20,1:PRINT"score:";PO:GOSUB 6000:FOR I=200 TO 1 STEP -1:IF I<10 THEN LOCATE Y,1,0:PRINT ARROW$;
1100 LOCATE 1,1,0:PRINT I;:NEXT
1200 FOR I=2 TO 32 STEP SP:GOSUB 7000
1300 LOCATE Y,I-1:PRINT SPACE$(SP+1);ARROW$
1400 NEXT
1500 LOCATE 18,1:GOSUB 5000:GOTO 300
5000 IF Y=9 THEN PRINT"Well Done!!":PO=PO+30:SP=SP+.1
5010 IF Y=10 THEN PRINT"You killed him!!":PO=PO-30:SP=SP-.1:IF SP<=0 THEN SP=.1
5020 IF Y=11 THEN PRINT"You got him through the heart!!":PO=PO-20:SP=SP-.05:IF SP<=0 THEN SP=.1503 IF Y=12 THEN PRINT"Right through the stomach!!":PO=PO-15:SP=SP-.3
5030 IF Y=12 THEN PRINT"Right through the stomach!!":PO=PO-15:SP=SP-.3
5040 IF Y=13 THEN PRINT"You've amputated his legs!!":PO=PO-10:SP=SP-.02:IF SP<=0 THEN SP=.1
5050 IF Y>13 OR Y<9 THEN PRINT"You missed!!"
5060 FOR I=1 TO 3000:NEXT
5070 RETURN
6000 PRINT"Press enter to fire arrow, use the      <-> and </> keys to control the arrow";
6005 LOCATE 24,12:PRINT"By: Ted Felix";
6010 A$=INKEY$
6020 IF A$=CHR$(13) THEN RETURN
6040 GOTO 6010
7000 A$=INKEY$
7010 IF A$="-" THEN LOCATE Y,I-1:PRINT SPACE$(7+INT(SP+.5));:Y=Y-1:IF Y<2 THEN Y=2
7020 IF A$="/" THEN LOCATE Y,I-1:PRINT SPACE$(7+INT(SP+.5));:Y=Y+1: IF Y>18 THEN Y=18
7030 RETURN
