1 ' Copyright 2017, Ted Felix
2 ' File date: 8/20/1982
100 RANDOMIZE VAL(TIME$)+VAL(RIGHT$(TIME$,2))+VAL(MID$(TIME$,4,2))
110 DEF FN RAND(X)=INT(RND*X)+1
120 DEF FN PYTH(A,B,C,D)=SQR((A-C)^2+(B-D)^2)
125 DIM EN(1)
130 KEY OFF:SCREEN 0:WIDTH 40:COLOR 6,0,0:CLS
140 YOU$=CHR$(2):BAD$=CHR$(1):TR$=CHR$(24)
150 BL$=SPACE$(39)
160 PRINT"   ��Ŀ  ��Ŀ  ��Ŀ  ��Ŀ  ��Ŀ  ��Ŀ
170 PRINT"   �  �  �  �  �  �  �  �  �  �  �  �
180 PRINT"   �     �     �     �  �  �  �  �
190 PRINT"   �     �     �     �  �  �  �  �
200 PRINT"   ���   ��Ŀ  �     ��Ĵ  ����  ���
210 PRINT"   �        �  �     �  �  �     �
220 PRINT"   �        �  �     �  �  �     �
230 PRINT"   �  �  �  �  �  �  �  �  �     �  �
240 PRINT"   ����  ����  ����  �  �  �     ����
250 B=0:Y=15:PB=1.5:PY=Y+1:LOCATE 1,1,0
260 B=B+1.5:Y=Y+1
270 LOCATE 12,PB:PRINT" ";:LOCATE 12,PY:PRINT" ";
280 IF B>38 THEN 330
290 LOCATE 12,B:PRINT BAD$;
300 IF Y<38 THEN LOCATE 12,Y:PRINT YOU$
310 PB=B:IF Y<38 THEN PY=Y
320 FOR I=0 TO 50:NEXT:GOTO 260
330 LOCATE 12,13:PRINT"By: Ted Felix
340 LOCATE 14,1:INPUT"Difficulty Level (1-10)";DIFF
350 IF DIFF>10 OR DIFF<1 THEN LOCATE 14,1:PRINT BL$;:GOTO 340
360 GOSUB 9000
1000 GOSUB 7000:GOSUB 8000:PX=X:PY=Y
1010 X=X+XM:Y=Y+YM:IF X<2 THEN X=2
1015 XM=0:YM=0
1020 IF X>38 THEN X=38
1030 IF Y<2 THEN Y=2
1040 IF Y>23 THEN Y=23
1050 LOCATE PY,PX:PRINT" "
1060 A=SCREEN(Y,X):IF A<>32 THEN 1090
1070 LOCATE Y,X:PRINT YOU$:GOTO 1000
1090 IF A=24 THEN X=PX:Y=PY:LOCATE Y,X:PRINT YOU$;:GOTO 1000
1100 IF A=1 THEN 9800
1110 IF A=79 THEN 9800
1120 IF A=64 THEN GOSUB 9650:GOTO 1000
1130 IF A=61 THEN GOTO 9400
1210 A=24:GOTO 1090
7000 IF DIE=EN+1 THEN RETURN
7005 ENW=ENW+1:IF ENW>EN THEN ENW=0
7010 IF EN(ENW,0)=0 THEN 7005
7020 ON EN(ENW,0) GOTO 7030,7250
7030 EX=EN(ENW,1):EY=EN(ENW,2)
7035 EXM=SGN(X-EX):EYM=SGN(Y-EY)
7040 NX=EX+EXM:NY=EY+EYM:A=SCREEN(NY,NX)
7050 IF A=24 THEN RETURN
7060 IF A=2 THEN 9800
7070 IF A=1 THEN EN(ENW,0)=0:DIE=DIE+1:GOTO 7200
7080 IF A=79 THEN EN(ENW,0)=0:DIE=DIE+1:BLA=1:GOTO 7200
7090 IF A=64 THEN ZOO=1:GOSUB 9600:GOTO 7200
7190 IF A<>32 THEN RETURN
7200 LOCATE EY,EX:PRINT" ";
7210 LOCATE NY,NX:PRINT BAD$;
7215 IF BLA=1 THEN BLA=0:LOCATE NY,NX:PRINT"@"
7220 EN(ENW,1)=NX:EN(ENW,2)=NY
7230 RETURN
7320 LOCATE CY+2,CX:PRINT"OOO"
7999 END
8000 SX=STICK(0):SY=STICK(1)
8010 SX=SX-50:IF ABS(SX)>40 THEN XM=SGN(SX)
8020 SY=SY-50:IF ABS(SY)>40 THEN YM=SGN(SY)
8030 RETURN
8999 ' Screen set-up
9000 COLOR 4,0,0:LOCATE 1,1:PRINT"�"+STRING$(37,"�")+"�"
9010 FOR I=1 TO 22:PRINT"�";SPC(37);"�":NEXT
9020 PRINT"�"+STRING$(37,"�")+"�";
9025 COLOR 2,0,0
9030 G=FNRAND(DIFF*20):FOR I=0 TO G
9040 LOCATE FNRAND(22)+1,FNRAND(36)+1:PRINT TR$;:NEXT
9045 DIE=0:COLOR 6,0,0
9050 LOCATE 1,15
9052 PRINT"Ƕ       Ƕ
9053 LOCATE 2,15
9054 PRINT"���������ж
9055 LOCATE 3,15
9056 PRINT"�         �
9057 LOCATE 4,15
9058 PRINT"�   �Ŀ   �
9059 LOCATE 5,15
9060 PRINT"�   �=�   �
9061 LOCATE 6,15
9062 PRINT"���; �����
9090 Y=FNRAND(21)+2
9100 EN=FNRAND(DIFF*4)
9110 ERASE EN
9120 DIM EN(EN,2)
9130 FOR I=0 TO EN
9140 Y=FNRAND(13)+7
9150 X=FNRAND(36)+2
9160 IF SCREEN(Y,X)<>32 THEN 9140
9170 LOCATE Y,X:PRINT BAD$
9180 EN(I,1)=X:EN(I,2)=Y:EN(I,0)=1:NEXT
9190 X=20:Y=23
9390 RETURN
9400 GOSUB 9700:LOCATE 12,7:PRINT"You have captured the castle!"
9410 LOCATE 13,13:PRINT"Congratulations"
9420 FOR I=0 TO 3000:NEXT:GOTO 130
9600 LOCATE NY,NX:PRINT" ";
9605 NY=FNRAND(21)+2:NX=FNRAND(36)+2
9610 IF SCREEN(NY,NX)<>32 THEN 9600
9620 RETURN
9650 LOCATE Y,X:PRINT" ";
9655 Y=FNRAND(21)+2:X=FNRAND(36)+2
9660 IF SCREEN(Y,X)<>32 THEN 9650
9670 RETURN
9700 RETURN
9800 REM BOOM!
9810 FOR I=0 TO 50
9820 LOCATE Y,X:PRINT CHR$(FNRAND(75)+179);
9830 NEXT:CLS:GOTO 130
9900 REM Made It!
9910 GOSUB 9700
9920 LOCATE 12,16:PRINT"Nice job!"
9930 FOR I=1 TO 2000:NEXT:GOTO 130