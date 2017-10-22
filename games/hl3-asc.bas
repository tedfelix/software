1 ' Copyright 2017, Ted Felix
2 ' File date: 5/7/1989 09:06:14 PM
15 ' By: Ted Felix
21 DIM L(20)
22 DEF SEG=0
25 RANDOMIZE (PEEK(&H46C)+PEEK(&H46D)*256)-32768!
30 DEF FNRAND(X)=INT(RND*X)+1
40 H=3:CLS:SCREEN 0:WIDTH 40
50 READ N:DIM A$(N):FOR I=0 TO N:READ A$(I):NEXT
65 IF A$(N)<>"END"THEN CLS:PRINT"Data error":END
67 A$(N)="":N=N-1
70 RESTORE 9800:READ NQ:DIM Q$(NQ):FOR I=0 TO NQ:READ Q$(I):NEXT
80 RESTORE 9900:READ NE:DIM E$(NE):FOR I=0 TO NE:READ E$(I):NEXT
90 H$=STRING$(3,3)
100 CLS:COLOR 0,7:PRINT H$"Hot Lust"H$:COLOR 7,0:PRINT
105 PRINT"Rated   ";:COLOR 0,7:PRINT"xXx";:COLOR 7,0:PRINT
110 PRINT"Not for children under 50."
115 PRINT:PRINT:PRINT:PRINT"Press ENTER to play."
120 A$=INPUT$(1):CLS
130 PRINT H$:PRINT A$(0):PRINT
140 PRINT Q$(FNRAND(NQ+1)-1):LINE INPUT A$:GOSUB 5000:PRINT:H=H+.2:PRINT STRING$(H,3)
145 T=T+2:PRINT"Time is now 12";T:IF T>58 THEN CLS:GOTO 200
150 X=FNRAND(N)
152 FOR LC=0 TO 20:IF X=L(LC) THEN LC=100
153 NEXT:IF LC>30 THEN 150
155 L(L)=X:L=L+1:IF L>20 THEN L=0
160 PRINT A$(X):GOTO 140
200 PRINT E$(FNRAND(NE+1)-1)
210 END:GOTO 210
4999 ' Command Interpret
5000 FOR I=1 TO LEN(A$):CH$=MID$(A$,I,1):CH$=CHR$(ASC(CH$)AND 223)
5010 MID$(A$,I)=CH$:NEXT
5020 IF INSTR(A$,"GUN")THEN PRINT">>>>BANG<<<<":PRINT"You almost shot yourself!!!!!"
5030 IF INSTR(A$,"GRAB")THEN PRINT"Grabbing not allowed"
5040 IF INSTR(A$,"FEEL")THEN PRINT"OOOOHHHH AAAAHHHHH!!!!!"
5050 IF INSTR(A$,"SEX")THEN PRINT"That's an xXx Rated thought, make sure  there are no children in the room!!!!"
5060 IF INSTR(A$,"BRA")OR INSTR(A$,"PANT")OR INSTR(A$,"GARTER")THEN PRINT"GIVE ME MORE DESCRIPTIONS!!!!"
5070 IF INSTR(A$,"RUN")THEN PRINT"She uses her tractor beam and pulls you back!!!"
5080 IF INSTR(A$,"GUARD")THEN PRINT"The guard stares at you":PRINT"HINT: He's Gay"
5090 IF INSTR(A$,"STARE")THEN PRINT"She slaps you and you flip out the      window.  Then the BODY guard throws you back inside. (Neat plot Huh?)
5100 IF INSTR(A$,"RUBBER")THEN PRINT"She didn't hear right, so she strips andnow wants you to RUB HER (Rubber)
5110 IF INSTR(A$,"SNAKE")THEN PRINT"You're the only one in the bed with a   Snake!!!"
5120 IF INSTR(A$,"GET") THEN PRINT"Got it!"
5900 RETURN
8999 ' Responses
9000 DATA 44
9010 DATA"You are in a dark room with a strange   woman.  The room has no ceiling and     there is a glowing cane near the bed.   It is 12 am, you have until 1 am.       Your goal: To Please!"
9020 DATA"The woman grabs your foot and starts    pulling you into her bed."
9030 DATA"Your strength is no match for the       woman's lust and desire!  She pulls you into bed."
9040 DATA"The woman grabs a vase and breaks it    over your head.
9050 DATA"She begins to tug at your clothes."
9060 DATA"She pulls your clothes off and feels    you."
9070 DATA"She throws a lamp at you.  You ducked   and it missed you."
9080 DATA"Three gorrilas appear and run towards   you."
9090 DATA"She gags you with her bra."
9100 DATA"Her clothes slide off gently."
9110 DATA"She reaches for something near the bed."
9120 DATA"She starts having spasms."
9130 DATA"The woman stares at your body."
9140 DATA"Suddenly, you appear in another room.   A different woman resides here."
9150 DATA"The woman becomes invisible."
9160 DATA"You fire three shots as she hurls the   fire cane at you."
9170 DATA"A fire cane is near the bed."
9180 DATA"The woman touches a button."
9190 DATA"Suddenly the room explodes."
9200 DATA"Fire begins to engulf the room."
9210 DATA"A light switches on."
9220 DATA"Someone fires a gun in the next room."
9230 DATA"You suddenly appear in a large hall withmany doors.  You enter the handiest one."
9240 DATA"Someone says, `You are good'."
9250 DATA"A large bulldozer appears."
9260 DATA"The woman becomes visible."
9270 DATA"Her provocative pose excites you."
9280 DATA"Her hair ignites."
9290 DATA"She lights a match."
9300 DATA"The bed goes up in smoke."
9310 DATA"She likes it."
9320 DATA"Her BODY gaurd appears."
9330 DATA"Her BODY gaurd dies."
9340 DATA"You are thrown into a time warp, and shecomes with you.
9350 DATA"She dissapears and then grabs the cane."
9360 DATA"The BODY guard fires 5 shots into the   bed."
9370 DATA"500 snakes fill the bed."
9380 DATA"She grabs a gun and shoots up your legs."
9390 DATA"You suddenly appear in the fourth       dimension and you can see through her.
9400 DATA"She slowly reveals her body."
9410 DATA"Your eyes pop out as her clothes slide."
9420 DATA"Five jets fly over your head."
9430 DATA"She touches various parts of your body."
9440 DATA"She puts her arms around you."
9790 DATA"END"
9799 ' Questions
9800 DATA 6
9810 DATA"What will you do?"
9820 DATA"Now what?"
9830 DATA"What would you like, rubber or run?"
9840 DATA"Your move:"
9850 DATA"The balls are in your court:"
9860 DATA"She awaits your decision:"
9870 DATA"There are only two exits:"
9899 ' Endings
9900 DATA 4
9910 DATA"She said she had a great time!!  You got5000 points!"
9920 DATA"The rubber broke, and you had 5 babies. You LOSE!!!"
9930 DATA"She said you were a waste of time.      You LOSE!!!"
9940 DATA"She said that your personality was bad, but your body was great, 1000 points."
9950 DATA"She said, she likes you and wants to seemore.  2000 points for not showing her  everything."

