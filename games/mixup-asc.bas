1 ' Copyright 2017, Ted Felix
2 ' File date: 5/7/1989 06∶59∶26 PM
5 CLS:DEF FNRAND(X)=INT(RND*(X+1))
7 RANDOMIZE VAL(RIGHT$(TIME$,2))+VAL(MID$(TIME$,3,2))+VAL(TIME$)
10 PRINT"Mix-up"
20 PRINT"Based on `Anagram' By: Ted Felix"
30 INPUT"What is your name";NAM$
35 IF NAM$="library" THEN 7000
40 PRINT:PRINT"Well "NAM$", the rules of"
50 PRINT"this game are simple.  I will think"
60 PRINT"of a word and mix it up.  Then I will"
70 PRINT"ask you what you think the word is."
80 INPUT"If you're ready, press ENTER";A$
90 CLS
100 RESTORE:READ WN
110 FOR I=1 TO FNRAND(WN)
120 READ A$:NEXT
130 FOR I=0 TO 10:IF A$=A$(I)THEN 100
140 NEXT
150 FOR I=10 TO 1 STEP-1::A$(I)=A$(I-1):NEXT:A$(0)=A$
155 FOR I=0 TO 10:A[I]=0:NEXT
157 L=LEN(A$)
160 FOR I=1 TO L
170 A=FNRAND(L):IF A[A]<>0 THEN 170
180 A[A]=1:B$=B$+MID$(A$,A,1)
190 NEXT
200 IF A$=B$ THEN 155 ELSE PRINT B$
210 INPUT"What word is that";C$
215 GOSUB 6000
220 IF C$=B$ THEN PRINT"I didn't spell it right!!!":GOTO 210
230 IF C$=A$ THEN 1000
240 GOTO 2000
300 FOR I=1 TO 2000:NEXT:CLS:PRINT"You have";C;"right and";W;"wrong."
310 PRINT"That's"INT(C/(C+W)*100)"%"
330 FOR I=0 TO 10:A(I)=0:NEXT:B$=""
400 GOTO 100
999 ' Right
1000 C=C+1:Z=FNRAND(5)
1010 ON Z GOTO 1020,1030,1040,1050,1060
1020 PRINT"Good job!":GOTO 300
1030 PRINT"Not bad!":GOTO 300
1040 PRINT"Great job!":GOTO 300
1050 PRINT"That's right!!!":GOTO 300
1060 PRINT"Correct!":GOTO 300
1999 ' Wrong
2000 W=W+1:Z=FNRAND(5)
2010 ON Z GOTO 2020,2030,2040,2050,2060
2020 PRINT"The answer was ";A$;"!":GOTO 300
2030 PRINT"What about ";A$;"?!!":GOTO 300
2040 PRINT"I think you meant ";A$;"!!":GOTO 300
2050 PRINT"Incorrect!":PRINT"It's Really, "A$"!!":GOTO 300
2060 PRINT"Not ";C$;", ";A$;"!!!":GOTO 300
4999 '  Library
5000 DATA 78
5010 DATA GOAT,MAN,NAME,RED,PLOT,TABLE,POTATO,TOMATO,ONION,COMPUTER,TELEVISION,RAYON,POLYESTER,HOME,PASS,LIP,PILL,SHOT,RECORD
5020 DATA PLAY,REWIND,FORWARD,FAST,OBSCURE,ALLOCATION,DISKETTE,DRIVE,TAPE,VERBATIM,YELLOW,MEMOREX,LIST,RUN,SAVE,LOAD,ANAGRAM,MIX
5030 DATA WRITE,READ,RIGHT,ERASE,RENAME,ELEVEN,TWELVE,THIRD,THIRTEEN,WAX,FROM,WET,DRY,FUZZY,BEAR,AUTHOR,WOMAN,GIRL,BOY,BOUY,SHIP
5040 DATA FILES,ZAP,PRINT,LOOK,COOL,WARM,HOT,CRACKERS,BOX,ROPE,JUGGLE,BASIC,GOAT,SICK,SALUTE,SOLUTION,SOUL,ROCK,COUNTRY
5999 ' Capitalize
6000 FOR I=1 TO LEN(C$)
6010 L$=MID$(C$,I,1)
6020 IF L$<="z" AND L$>="a" THEN MID$(C$,I,1)=CHR$(ASC(L$)-32)
6030 NEXT:RETURN
6999 '  Library Diagnostics
7000 RESTORE:READ WN
7010 CLS:PRINT WN;"is the word number"
7020 PRINT"Now checking if it's true"
7030 ON ERROR GOTO 8000
7040 ZZ=0:FOR I=1 TO WN:READ A$:NEXT
7050 ZZ=1:READ A$
7060 PRINT"Word number too small":END
7070 PRINT"Word number is correct"
7080 PRINT"Now checking for repeated words"
7090 RESTORE:READ WN:DIM A$(WN)
7095 FOR I=1 TO WN:READ A$(I):NEXT:RESTORE:READ WN
7100 FOR I=1 TO WN:READ B$
7110 FOR J=1 TO WN
7120 IF I<>J THEN IF B$=A$(J)THEN 7200
7130 NEXT:NEXT
7140 PRINT"Everything is OK."
7150 END
7200 PRINT B$;" has a double"
7210 END
8000 IF ZZ=0 AND ERR=4 THEN PRINT"Word number is too big":CLEAR:END
8010 IF ZZ=1 AND ERR=4 THEN RESUME 7070
8020 PRINT ERR;ERL

