10 CLS:SCREEN 0:WIDTH 80:PRINT TAB(36);"ELIZA"
20 PRINT TAB(30);"Creative Computing"
30 PRINT TAB(28);"Morristown, New Jersey"
40 PRINT:PRINT:PRINT
80 '-------------INITIALIZATION-------------------
100 DIM S(36),R(36),N(36)
110 N1=36:N2=26:N3=112:N2.2=N2/2
120 RESTORE 2530
130 FOR X=1 TO N1
140 READ S(X),L:R(X)=S(X):N(X)=S(X)+L-1
150 NEXT X
160 PRINT"Hi!  I'm Eliza. What's your problem?"
170 '------------USER INPUT SECTION----------------
200 LINE INPUT">";I$
201 I$=" "+I$+"  "
210 '*****GET RID OF APOSTROPHES*****
220 COC=0:FOR L=1 TO LEN(I$)
225 CH$=MID$(I$,L,1)
230 IF INSTR("',.!?",CH$)>1 THEN COC=COC+1:R$=RIGHT$(I$,LEN(I$)-L):I$=LEFT$(I$,L-1)+R$:GOTO 225
240 IF L+4<LEN(I$)THEN IF MID$(I$,L,4)="SHUT"THEN PRINT"SHUT UP...":END
245 IF CH$>="a"AND CH$<="z"THEN MID$(I$,L,1)=CHR$(ASC(CH$)-32)
250 NEXT
260 IF I$=P$THEN PRINT"PLEASE DON'T REPEAT YOURSELF!":GOTO 170
270 '------------FIND KEYWORD IN I$------------------
290 RESTORE
295 S=0
300 FOR K=1 TO N1
310 READ K$
315 IF S>0 THEN 360
320 L=INSTR(I$,K$)
340 IF L THEN S=K:T=L:F$=K$
360 NEXT K
365 IF S>0 THEN K=S:L=T:GOTO 390
370 K=36:GOTO 560 'WE DIDN'T FIND ANY KEYWORDS
390 '      TAKE RIGHT PART OF STRING AND CONJUGATE IT
400 '      USING THE LIST OF STRINGS TO BE SWAPPED
420 RESTORE 1230 'SKIP OVER KEYWORDS
430 C$=" "+RIGHT$(I$,LEN(I$)-LEN(F$)-(L)+1)+" "
440 FOR X=1 TO N2.2
450 READ S$,R$
460 FOR L=1 TO LEN(C$)
470 IF L+LEN(S$)>LEN(C$)THEN 510
480 IF MID$(C$,L,LEN(S$))<>S$THEN 510
490 C$=LEFT$(C$,L-1)+R$+RIGHT$(C$,LEN(C$)-L-LEN(S$)+1)
495 L=L+LEN(R$)
500 GOTO 450
510 IF L+LEN(R$)>LEN(C$)THEN 540
520 IF MID$(C$,L,LEN(R$))<>R$THEN 540
530 C$=LEFT$(C$,L-1)+S$+RIGHT$(C$,LEN(C$)-L-LEN(R$)+1)
535 L=L+LEN(S$)
540 NEXT L
550 NEXT X
555 IF MID$(C$,2,1)=" "THEN C$=RIGHT$(C$,LEN(C$)-1)'ONLY ONE SPACE
556 FOR L=1 TO LEN(C$)
557 IF MID$(C$,L,1)="!"THEN C$=LEFT$(C$,L-1)+RIGHT$(C$,LEN(C$)-L):GOTO 557
558 NEXT L
560 '----------------NOW USING THE KEYWORD (K) GET REPLY-------------------
590 RESTORE 1330
600 FOR X=1 TO R(K):READ F$:NEXT X ' READ RIGHT REPLY
610 R(K)=R(K)+1:IF R(K)>N(K)THEN R(K)=S(K)
620 IF RIGHT$(F$,1)<>"*"THEN PRINT F$:P$=I$:GOTO 170
630 PRINT LEFT$(F$,LEN(F$)-1);C$
640 P$=I$:GOTO 170
650 '---------------------Convert a string to all CAPS---------------------
1000 '*********************************************************************
1010 '*           PROGRAM DATA FOLLOWS                                    *
1020 '*********************************************************************
1030 '---------------KEYWORDS-------------------
1050 DATA "CAN YOU","CAN I","YOU ARE","YOURE","I DONT","I FEEL"
1060 DATA "WHY DONT YOU","WHY CANT I","WHY ARE YOU","I CANT","I AM","IM "
1070 DATA "YOU ","I WANT","WHAT","HOW","WHO","WHERE","WHEN","WHY"
1080 DATA "NAME","CAUSE","SORRY","DREAM","HELLO","HI ","MAYBE"
1090 DATA " NO","YOUR","ALWAYS","THINK","ALIKE","YES","FRIEND"
1100 DATA "COMPUTER","NOKEYFOUND"
1200 '---------------STRING DATA FOR CONJUGATIONS---------------------------
1230 DATA " I "," !YOU "," YOURS "," !MINE "," MINE "," YOURS "," MY "," !YOUR "," ARE "," AM ","WERE ","WAS "," YOU "," I "," YOUR "," MY "," IVE "," YOU'VE "," IM "," YOU'RE "," ME "," !YOU "," OUR "," YOUR "," OURS "," YOURS "
1300 '---------------------REPLIES----------------
1330 DATA "DONT YOU BELIVE THAT I CAN*"
1340 DATA "PERHAPS YOU WOULD LIKE TO BE ABLE TO*"
1350 DATA "YOU WANT ME TO BE ABLE TO*"
1360 DATA "PERHAPS YOU DON'T WANT TO*"
1365 DATA "DO YOU WANT TO BE ABLE TO*"
1370 DATA "WHAT MAKES YOU THINK I AM*"
1380 DATA "DOES IT PLEASE YOU TO BELIEVE I AM*"
1390 DATA "PERHAPS YOU WOULD LIKE TO BE*"
1400 DATA "DO YOU SOMETIMES WISH YOU WERE*"
1410 DATA "DON'T YOU REALLY*"
1420 DATA "WHY DON'T YOU*"
1430 DATA "DO YOU WISH TO BE ABLE TO*"
1440 DATA "Does that trouble you?"
1450 DATA "Tell me more about such feelings."
1460 DATA "DO YOU OFTEN FEEL*"
1470 DATA "DO YOU ENJOY FEELING*"
1480 DATA "DO YOU REALLY BELIEVE I DON'T*"
1490 DATA "PERHAPS IN GOOD TIME I WILL*"
1500 DATA "DO YOU WANT ME TO*"
1510 DATA "DO YOU THINK YOU SHOULD BE ABLE TO*"
1520 DATA "WHY CAN'T YOU*"
1530 DATA "WHY ARE YOU INTERESTED IN WHETHER OR NOT I AM*"
1540 DATA "WOULD YOU PREFER IF I WERE NOT*"
1550 DATA "PERHAPS IN YOUR FANTASIES I AM*"
1560 DATA "HOW DO YOU KNOW YOU CAN'T*"
1570 DATA "Have you tried?"
1580 DATA "PERHAPS YOU CAN NOW*"
1590 DATA "DID YOU COME TO ME BECAUSE YOU ARE*"
1600 DATA "HOW LONG HAVE YOU BEEN*"
1610 DATA "DO YOU BELIEVE IT IS NORMAL TO BE*"
1620 DATA "DO YOU ENJOY BEING*"
1630 DATA "We're discussing you-- not me."
1640 DATA "OH, I*"
1650 DATA "You're not really talking about me, are you?"
1660 DATA "WHAT WOULD IT MEAN TO YOU IF YOU GOT*"
1670 DATA "WHY DO YOU WANT*"
1680 DATA "SUPPOSE YOU SOON GOT*"
1690 DATA "WHAT IF YOU NEVER GOT*"
1700 DATA "I SOMETIMES ALSO WANT*"
1710 DATA "Why do you ask?"
1720 DATA "Does that question interest you?"
1730 DATA "What answer would please you the most?"
1740 DATA "What do you think?"
1750 DATA "Are such questions on your mind often?"
1760 DATA "What is it that you really want to know?"
1770 DATA "Have you asked anyone else?"
1780 DATA "Have you asked such questions before?"
1790 DATA "What else comes to mind when you ask that?"
1800 DATA "Names don't interest me."
1810 DATA "I don't care about names-- please go on."
1820 DATA "Is that the real reason?"
1830 DATA "Don't any other reasons come to mind?"
1840 DATA "Does that reason explain anything else?"
1850 DATA "What other reasons might there be?"
1860 DATA "Please don't apologize!"
1870 DATA "Apologies are not necessary."
1880 DATA "What feelings do you have when you apologize?"
1890 DATA "Don't be so defensive!"
1900 DATA "What does that dream suggest to you?"
1910 DATA "Do you dream often?"
1920 DATA "What persons appear in your dreams?"
1930 DATA "Are you disturbed by your dreams?"
1940 DATA "How do you do ... please state your problem."
1950 DATA "You don't seem quite certain."
1960 DATA "Why the uncertain tone?"
1970 DATA "Can't you be more positive?"
1980 DATA "You aren't sure?"
1990 DATA "Don't you know?"
2000 DATA "Are you saying no just to be negative?"
2010 DATA "You are being a bit negative."
2020 DATA "Why not?"
2030 DATA "Are you sure?"
2040 DATA "Why no?"
2050 DATA "ARE YOU CONCERNED ABOUT MY*"
2060 DATA "WHAT ABOUT YOUR OWN*"
2070 DATA "Can you think of a specific example?"
2080 DATA "When?"
2090 DATA "What are you thinking of?"
2100 DATA "Really, always?"
2110 DATA "Do you really think so?"
2120 DATA "BUT YOU ARE NOT SURE THAT*"
2130 DATA "DO YOU DOUBT YOU*"
2140 DATA "In what way?"
2150 DATA "What resemblance do you see?"
2160 DATA "What does the similarity suggest to you?"
2170 DATA "What other connections do you see?"
2180 DATA "Could there really be some connection?"
2190 DATA "How?"
2200 DATA "You seem quite positive."
2210 DATA "Are you sure?"
2220 DATA "I see."
2230 DATA "I understand."
2240 DATA "Why do you bring up the topic of friends?"
2250 DATA "Do your friends worry you?"
2260 DATA "Do your friends pick on you?"
2270 DATA "Are you sure you have any friends?"
2280 DATA "Do you impose on your friends?"
2290 DATA "Perhaps your love for friends worries you?"
2300 DATA "Do computers worry you?"
2310 DATA "Are you talking about me in particular?"
2320 DATA "Are you frightened by machines?"
2330 DATA "Why do you mention computers?"
2340 DATA "What do you think machines have to do with your problem?"
2350 DATA "Don't you think computers can help people?"
2360 DATA "What is it about machines that worries you?"
2370 DATA "Say, do you have any psychological problems?"
2380 DATA "What does that suggest to you?"
2390 DATA "I see."
2400 DATA "Do you speaka my language?"
2410 DATA "Come come, elucidate your thoughts."
2420 DATA "Can you elaborate on that?"
2430 DATA "That is quite interesting."
2440 DATA "Are any other problems connected to that?"
2450 DATA "Tell me about your dreams, and what they tell you."
2460 DATA "How do you react in that situation?"
2470 DATA "I can see a touch of extreme paranoia in your thoughts, please GO ON!!!!"
2480 DATA "Can you tell me of any problems at home?"
2490 DATA "Don't strain yourself, just think about your landscapes."
2491 DATA "Have you ever felt like a Slime Bucket?"
2492 DATA "Has music ever had any effect on your sleeping patterns?"
2493 DATA "Hypnotism might help.  What do you think of such methods?"
2494 DATA "There are many alternatives, such as suicide, do you think you might want to    consider suicide?"
2495 DATA "I'm sorry, I don't think that suicide suits you.  I could get out my rifle, and blow you away!"
2496 DATA "Have you ever considered a career in psychiatry, or artificial intelligence?"
2497 DATA "Is this important to your possible diagnosis?"
2498 DATA "How many times have you seen yourself falling?"
2499 DATA "That's too many.  What are you planning to solve this problem?"
2500 DATA "I'm not an expert on those procedures, please consider more alternatives"
2501 DATA "Anyone could realise what you have, tell me more on these procedures."
2502 DATA "I think you have found a better answer, go on."
2503 DATA "A specialist in this area might be convinced that your beliefs are correct, whatdo you think?
2504 DATA "Only Dr. Jekyl has seen connections between what you are saying, and            horseradish."
2505 DATA "Don't be puzzled at reasons that are beyond your mental capacity, just tell me  more about your current lifestyle."
2506 DATA "You may need more vitamins to keep that up, please tell me about your diet."
2507 DATA "Are you ever affected by what a famous person says?"
2508 DATA "Do you ever see stars?"
2509 DATA "When?"
2510 '-----------DATA FOR FINDING RIGHT REPLIES-------------------
2530 DATA 1,3,4,2,6,4,6,4,10,4,14,3,17,3,20,2,22,3,25,3
2540 DATA 28,4,28,4,32,3,35,5,40,9,40,9,40,9,40,9,40,9,40,9
2550 DATA 49,2,51,4,55,4,59,4,63,1,63,1,64,5,69,5,74,2,76,4
2560 DATA 80,3,83,7,90,3,93,6,99,7,106,32

