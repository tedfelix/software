SCREEN 13
DEFINT A-Z  ' For performance
CLS

' Set aside enough space for our sprite
DIM Ball%(37)

' Draw the sprite on the screen
CIRCLE (4, 3), 4, 4
PAINT (4, 3), 12, 4

' Get the sprite from the screen into Ball%
GET (0, 0)-(8, 7), Ball%

' Color in half of the screen to show limitations of XOR
LINE (0, 0)-(160, 199), 13, BF

PRINT "          *** Sprite Demo ***"

' Dump the data
'FOR I = 0 TO 62
'  H$ = HEX$(Ball%(I))
'  PRINT STRING$(4 - LEN(H$), "0"); H$; " ";
'NEXT I

' Get/Put data format for SCREEN 13
' 0050 - Height left shifted 3 bits, 8, 10, 18, 20, 28,
' 000B - Width
' Data follows immediately


' Move the sprite around

' Screen size
CONST XMax = 320
CONST YMax = 200

' Initial position
X = 160
Y = 100

' Motion
DX = 1
DY = 1

DO
  PUT (X, Y), Ball%

  ' Save the old values so that we can remove the sprite before
  ' we redraw it in its new location.
  PX = X
  PY = Y

  ' Do some motion
  X = X + DX
  Y = Y + DY

  ' Check if we have hit a side
  IF X > XMax - 10 THEN
    DX = -1
  END IF
  IF X < 1 THEN
    DX = 1
  END IF

  ' Check if we have hit the top or bottom
  IF Y > YMax - 9 THEN
    DY = -1
  END IF
  IF Y < 1 THEN
    DY = 1
  END IF

  ' Lousy timing loop.  Need a better solution.
  FOR I = 1 TO 30000
  NEXT I

  ' Use XOR to remove the sprite
  PUT (PX, PY), Ball%
LOOP WHILE INKEY$ = ""

