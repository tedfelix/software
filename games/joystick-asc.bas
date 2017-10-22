1 ' Copyright 2017, Ted Felix
2 ' File date: 11/12/1984 06:28:04 PM
1000 ' Joystick Modeler
2000 CLS:SCREEN 1:WIDTH 40
2010 PRINT"Joystick Modeling"
2020 PRINT"Move your joystick to whatever position the computer asks you to and then press ENTER."
2030 PRINT:INPUT"Move the Joystick all of the way up";A$
2040 T=STICK(0):U=STICK(1)
2050 PRINT:INPUT"Move the Joystick all of the way down";A$
2060 T=STICK(0):D=STICK(1)
2070 PRINT:INPUT"Move the Joystick to the far right";A$
2080 R=STICK(0)
2090 PRINT:INPUT"Move the Joystick to the far left";A$
2100 L=STICK(0)
2110 CLS:PRINT"Thank you"
2170 OPEN"JOYSTICK.DAT"FOR OUTPUT AS #1
2180 PRINT#1,U;D;R;L
2190 CLOSE #1

