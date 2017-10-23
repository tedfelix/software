1 DEF SEG=0:RANDOMIZE (PEEK(&H46C)+PEEK(&H46D)*256)-32768!
2 ' Copyright 2017, Ted Felix
3 ' File date: 3/15/1983 11∶00∶00 PM
5 DEF FNRAND(X)=INT(RND*X)+1
10 CLS:PRINT"Outerlude"
20 PRINT"By: Ted Felix
30 PRINT:PRINT"*** Prelude ***"
40 PRINT:PRINT:PRINT:PRINT"Press ENTER for Prelude"
50 A$=INPUT$(1)
60 CLS:PRINT"*** Prelude ***"
70 PRINT:INPUT"Enter your name";NME$
80 PRINT NME$", the following will be
90 PRINT"multiple choice evaluations.
100 PRINT:PRINT"Press ENTER":A$=INPUT$(1)
110 CLS:PRINT"1> Are you Male or Female?
120 PRINT"   a) Male
130 PRINT"   b) Female
140 T$=INPUT$(1):IF T$="a"OR T$="A"THEN T$="M":GOTO 160
150 T$="F"
160 CLS:PRINT"2> Do you like sex?
170 PRINT"   a) Yes
180 PRINT"   b) I like makeing love cleanly
190 PRINT"   c) No
200 A$=INPUT$(1):IF A$="A"OR A$="a"THEN 230
210 IF A$="b"OR A$="B"THEN PRINT"Well, then this game is not for you!":END
220 IF A$="C"OR A$="c"THEN PRINT"Why are you playing this?":END
230 CLS:PRINT"3> What do you think of yourself?"
240 PRINT"   a) Great
250 PRINT"   b) Feelable
260 PRINT"   c) Not too hot
270 PRINT"   d) Yucky
280 A$=INPUT$(1):IF INSTR("abAB",A$)THEN 300
290 PRINT"You should stay away from women.":END
300 CLS:PRINT"Evaluation is completed"
310 RESTORE 9000:READ N:DIM TY$(N)
320 FOR I=0 TO N:READ TY$(I):NEXT
330 PRINT:PRINT"You are a:
340 PRINT TY$(FNRAND(N+1)-1)
350 DIM OL(15),OL$(15)
360 RESTORE 8000:FOR I=0 TO 15:READ OL(I),OL$(I):NEXT
370 A=FNRAND(16)-1:OL=OL(A):OL$=OL$(A)
375 IF T$="F"AND A=15 THEN 370
380 PRINT"Outerlude #"OL
390 PRINT OL$:FOR I=1 TO 6000:NEXT
400 IF A=15 THEN RUN"hl
410 COMMON OL,OL$,T$
420 CHAIN"ol1
8000 DATA 1,Feelathon
8010 DATA 10,Squeez play
8020 DATA 15,Head Games
8030 DATA 21,Wet T-Shirt contest
8040 DATA 26,Go for the Gusto
8050 DATA 29,The ultimate partner
8060 DATA 34,Round things
8070 DATA 46,Inside the opening
8080 DATA 58,Outer complex
8090 DATA 63,How to see a body
8100 DATA 68,Screaming won't help
8110 DATA 75,Dark dreams
8120 DATA 82,Kiss of desire
8130 DATA 93,Is dinner on?
8140 DATA 98,The grab
8150 DATA 99,The ultimate outerlude
9000 DATA 5,Hot lover
9010 DATA Feeler
9020 DATA Sex Maniac
9030 DATA Crazy grabber
9040 DATA Squeezer
9050 DATA Chopper

