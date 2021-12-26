DECLARE SUB TrimEnds (msg$)
DECLARE SUB CheckScriptFile ()
DECLARE SUB PrintCentered (msg$, col%)
DECLARE SUB DrawPaletteBar (ColorID%, ColorName$)
DECLARE FUNCTION FileExist% (FileName$)
DECLARE SUB InitPalette ()
DECLARE SUB ResetGame ()
DECLARE SUB HandleHit ()
DECLARE SUB HandleState ()
DECLARE SUB DrawPalette ()
DECLARE SUB DrawFakeRoom (c!, T!, position!)
DECLARE SUB fxASCIINoise ()
DECLARE SUB HandleUserAnswer ()
DECLARE SUB fxNoise (dT!)
DECLARE SUB DrawP1XLogo (T!)
DECLARE SUB Init2DSprite ()
DECLARE SUB Init3DModel ()
DECLARE SUB TrimMsg (m$)
DECLARE SUB PlayMenuSound ()
DECLARE SUB HandleMenu ()
DECLARE SUB DrawIntro ()
DECLARE SUB DrawMenu (position!)
DECLARE SUB SaveGame ()
DECLARE SUB CheckSaveGame ()
DECLARE SUB HandleTurn ()
DECLARE SUB ReadScriptScene ()
DECLARE SUB HandleSound ()
DECLARE SUB PlaySound ()
DECLARE SUB RandomizeSound (bank!)
DECLARE SUB InitSound ()
DECLARE SUB PlayTune ()
DECLARE SUB DrawScene ()
DECLARE SUB LoadTunes ()
DECLARE SUB DrawBottomBar ()
DECLARE SUB DrawBackground (character!)
DECLARE SUB GetUserAnswer ()
DECLARE SUB DrawWindow (title$, msg$)
DECLARE SUB ReadScriptCMD
DECLARE SUB ReadMsgs ()
DECLARE SUB DrawBanner (x!, y!)
DECLARE SUB FxNnoise ()
DECLARE SUB fxFall ()
DECLARE SUB DrawLine ()
DECLARE SUB DrawTopBar ()
DECLARE SUB DrawCustomLine (x!, y!, size!, char!)

' IFBE / Interactive Fiction BASIC Engine
' Developed by Krzysztof Krystian Janowski / P1X
' https://p1x.in
'
' Unnamed Game Project
' Version 10 - 26/12/2021 THE LOST CODE
' -------------------------------------------------------------------- '


' Declaring variables
' -------------------------------------------------------------------- '
CONST STATEINTRO = 0
CONST STATEMENU = 1
CONST STATEGAME = 2
CONST STATEOUTRO = 3
CONST STATEQUIT = 4
CONST STATEPALETTE = 5

CONST CMDMSG = "MSG"
CONST CMDQST = "QST"
CONST CMDJMP = "JMP"
CONST CMDEND = "END"
CONST CMDHIT = "HIT"

CONST VER = 10
CONST APPNAME = "SAMPLE STORY"
CONST APPLONGNAME = "KKJ'S SAMPLE STORY"

CONST FILESAVE = "SAVE.DAT"
CONST FILESCRIPT = "SCRIPT.DAT"

CONST COLORMAIN = 15
CONST COLOR2 = 14
CONST COLORBG = 9
CONST ColorName = 2
CONST COLORTURN = 11
CONST COLORHP = 3

CONST ROWS = 25
CONST COLS = 40
CONST MAXWIDTH = 320
CONST MAXHEIGHT = 200
CONST TOPBARHEIGHT = 10

CONST TURNTIME = 30
CONST STARTHP = 12


' Application settings
TYPE settings
  scene AS INTEGER
  centerX AS INTEGER
  centerY AS INTEGER
  soundEnabled AS INTEGER
  gfxNoise AS INTEGER
  gfxFall AS INTEGER
  state AS INTEGER
  qstPosX AS INTEGER
  qstPosY AS INTEGER
END TYPE
DIM SHARED app AS settings
app.scene = 1
app.centerX = INT(COLS / 2)
app.centerY = INT(ROWS / 2)
app.soundEnabled = -1
app.gfxNoise = -1
app.gfxFall = -1
app.state = STATINTRO

' Command & Control variables
TYPE command
 cmd AS STRING * 3
 msg AS STRING * 180
 jmp AS INTEGER
 q1 AS STRING * 26
 q2 AS STRING * 26
 q3 AS STRING * 26
 jmp1 AS INTEGER
 jmp2 AS INTEGER
 jmp3 AS INTEGER
 turn AS INTEGER
 hp AS INTEGER
 cubes AS INTEGER
 walls AS INTEGER
END TYPE
DIM SHARED cnc AS command
cnc.jmp = app.scene
cnc.hp = 10

' Graphics
SCREEN 13
InitPalette
COLOR 0
CLS

' Sound settings
TYPE soundsystem
 length AS INTEGER
 speed AS SINGLE
 bank AS INTEGER
 note AS INTEGER
END TYPE
DIM SHARED sfx AS soundsystem
DIM SHARED sounds(16)
InitSound

' 3D System

DIM SHARED x(20), y(20), z(20)
DIM SHARED col(20), row(20)
DIM SHARED surf(12, 6)
Init3DModel

' 2D System
DIM SHARED sprite(16, 22)
Init2DSprite

DIM SHARED chars(78) AS INTEGER
chars(0) = 179
chars(1) = 180
chars(2) = 191
chars(3) = 192
chars(4) = 193
chars(5) = 194
chars(6) = 195
chars(7) = 196
chars(8) = 197

turnTimer = 1

' Start of the program
' -------------------------------------------------------------------- '

CheckSaveGame
CheckScriptFile

' Main loop
DO
  ' Read user keyboard input
  g$ = INKEY$

  ' New tune requested, randomize and init play
  HandleSound

  ' Timer for turn delay
  IF turnTimer > 0 THEN
    IF app.state = STATEGAME THEN
      IF app.gfxNoise > 0 THEN fxNoise 1
      IF app.gfxFall > 0 THEN fxFall
    END IF
    turnTimer = turnTimer - 1
    IF turnTimer = 0 THEN
      HandleState
      PlaySound
    END IF
  END IF
         
  IF turnTimer < 1 AND (g$ <> "" OR cnc.cmd = CMDJMP) THEN
    turnTimer = TURNTIME
    PlaySound
  END IF

  WAIT &H3DA, 8
LOOP UNTIL g$ = CHR$(27) OR app.state = STATEQUIT


' DATA

' ARNE 16 PALETTE
DATA 00,00,00
DATA 157,157,157
DATA 255, 255, 255
DATA 190, 38, 51
DATA 224, 111, 139
DATA 73, 60, 43
DATA 164, 100, 34
DATA 235, 137, 49
DATA 247, 226, 107
DATA 47, 72, 78
DATA 68, 137, 26
DATA 163, 206, 39
DATA 27, 38, 50
DATA 0, 87, 132
DATA 49, 162, 242
DATA 178, 220, 239

' 3D OBJRCT VERTS
DATA  0.0000,-0.3568, 0.9342
DATA  0.5774,-0.5774, 0.5774
DATA  0.9342, 0.0000, 0.3568
DATA  0.5774, 0.5774, 0.5774
DATA  0.0000, 0.3568, 0.9342
DATA -0.5774, 0.5774, 0.5774
DATA -0.9342, 0.0000, 0.3568
DATA -0.5774,-0.5774, 0.5774
DATA -0.3568,-0.9342, 0.0000
DATA  0.3568,-0.9342, 0.0000
DATA  0.5774,-0.5774,-0.5774
DATA  0.9342, 0.0000,-0.3568
DATA  0.5774, 0.5774,-0.5774
DATA  0.3568, 0.9342, 0.0000
DATA -0.3586, 0.9342, 0.0000
DATA -0.5774, 0.5774,-0.5774
DATA -0.9342, 0.0000,-0.3568
DATA -0.5774,-0.5774,-0.5774
DATA  0.0000,-0.3568,-0.9342
DATA  0.0000, 0.3568,-0.9342

'SURFACES
DATA 1,2,3,4,5,1
DATA 1,5,6,7,8,1
DATA 1,8,9,10,2,1
DATA 2,10,11,12,3,2
DATA 3,12,13,14,4,3
DATA 4,14,15,6,5,4
DATA 6,15,16,17,7,6
DATA 7,17,18,9,8,7
DATA 9,18,19,11,10,9
DATA 11,19,20,13,12,11
DATA 13,20,16,15,14,13
DATA 16,20,19,18,17,16

'SPRITE
DATA 1,1,1,1,1,1,0,0,1,0,0,1,0,0,0,1
DATA 1,0,0,0,0,1,0,1,1,0,0,1,0,0,0,1
DATA 1,0,0,0,0,1,0,0,1,0,0,1,0,0,0,1
DATA 1,0,0,0,0,1,0,0,1,0,0,1,0,0,0,1
DATA 1,0,0,0,0,1,0,0,1,0,0,1,0,0,0,1
DATA 1,0,0,0,0,1,0,0,1,0,0,1,0,0,0,1
DATA 1,0,0,0,0,1,0,0,1,0,0,1,0,0,0,1
DATA 1,0,0,0,0,1,0,0,1,0,0,1,0,0,0,1
DATA 1,0,0,0,0,1,0,0,1,0,0,0,1,0,1,0
DATA 1,0,0,0,0,1,0,0,1,0,0,0,0,1,0,0
DATA 1,0,0,0,0,1,0,0,1,0,0,0,1,0,1,0
DATA 1,1,1,1,1,1,0,0,1,0,0,1,0,0,0,1
DATA 1,0,0,0,0,0,0,0,1,0,0,1,0,0,0,1
DATA 1,0,0,0,0,0,0,0,1,0,0,1,0,0,0,1
DATA 1,0,0,0,0,0,0,0,1,0,0,1,0,0,0,1
DATA 1,0,0,0,0,0,0,0,1,0,0,1,0,0,0,1
DATA 1,0,0,0,0,0,0,0,1,0,0,1,0,0,0,1
DATA 1,0,0,0,0,0,0,0,1,0,0,1,0,0,0,1
DATA 1,0,0,0,0,0,0,0,1,0,0,1,0,0,0,1
DATA 1,0,0,0,0,0,0,0,1,0,0,1,0,0,0,1
DATA 1,0,0,0,0,0,0,0,1,0,0,1,0,0,0,1
DATA 1,0,0,0,0,0,0,0,1,0,0,1,0,0,0,1

SUB CheckSaveGame
  IF FileExist%(FILESAVE) THEN
    OPEN FILESAVE FOR INPUT AS #2
    INPUT #2, lastScene%, hp%, turn%
  ELSE
    OPEN FILESAVE FOR OUTPUT AS #2
    lastScene% = 1
    hp% = GAMESTARTHP
    turn% = 0
    WRITE #2, lastScene%, hp%, turn%
  END IF
 
  app.scene = lastScene%
  cnc.hp = hp%
  cnc.turn = turn%
  CLOSE #2
END SUB

SUB CheckScriptFile

  IF FileExist%(FILESCRIPT) THEN
  ELSE
    OPEN FILESCRIPT FOR OUTPUT AS #1
   
    msg$ = "Welcome to the Interactive Fiction Basic Engine Version" + STR$(VER) + "."
    WRITE #1, CMDMSG
    WRITE #1, msg$
 
    msg$ = "Missing <" + FILESCRIPT + "> for game story... End."
    WRITE #1, CMDEND
    WRITE #1, msg$
   
    CLOSE #1
  END IF

END SUB

SUB DrawBackground (character)
 
  COLOR COLORBG

  ' Fill the background with custom char
  FOR x = 1 TO COLS
  FOR y = 1 TO ROWS
          LOCATE y, x
          PRINT CHR$(character);
  NEXT y
  NEXT x

END SUB

SUB DrawBottomBar
  
  LINE (48, MAXHEIGHT - 26)-(MAXWIDTH - 48, MAXHEIGHT - 6), 12, BF
  LINE (0, MAXHEIGHT - 6)-(MAXWIDTH, MAXHEIGHT), 12, BF

  COLOR 2
  SELECT CASE cnc.cmd
    CASE "QST"
      LOCATE ROWS - 2, 9
      PRINT "CHOOSE PATH USING ARROWS"
      LOCATE ROWS - 1, 13
      PRINT "AND PRESS ENTER";
    CASE "JMP"
    CASE ELSE
      LOCATE ROWS - 1, 14
      PRINT "PRESS ANY KEY";
  END SELECT

END SUB

SUB DrawCustomLine (x, y, size, char)
 
  FOR i = 1 TO size
    LOCATE x, y + i
    PRINT CHR$(char);
  NEXT i

END SUB

SUB DrawFakeRoom (c, T, position)

    bgX = 100 + SIN(T * .5) * 10
    bgY = 66 + COS(T * .25) * 4 + position * 4
    bgW = 120
    bgH = 68
    bgT = 14
    bgB = 180
   
    shade = 0
    IF c > 0 THEN shade = -6

    'cross lines
    LINE (bgX, bgY)-(0, bgT), c
    LINE (bgX + bgW, bgY)-(320, bgT), c
    LINE (bgX, bgY + bgH)-(0, bgB), c
    LINE (bgX + bgW, bgY + bgH)-(320, bgB), c + shade
    
    'fill lines
    FOR i = 0 TO 1 STEP 1 / 16
      LINE (bgX, bgY + bgH * i)-(0, bgT + ((bgB - bgT) * i)), c
      LINE (bgX + bgW, bgY + bgH * i)-(320, bgT + ((bgB - bgT) * i)), c + shade
      LINE (bgX + bgW * i, bgY)-(320 * i, bgT), c
      LINE (bgX + bgW * i, bgY + bgH)-(320 * i, bgB), c + shade
    NEXT i

    'box
    ix = 0
    iy = 0
    ar = (bgW / bgH) * .75
    FOR i = 0 TO 6
      ix = i + ix * ar
      iy = i + iy * ar
      LINE (bgX - ix, bgY - iy)-(bgX - ix, bgY + bgH + iy), c
      LINE (bgX + bgW + ix, bgY - iy)-(bgX + bgW + ix, bgY + bgH + iy), c + shade
      LINE (bgX - ix, bgY - iy)-(bgX + bgW + ix, bgY - iy), c
      LINE (bgX - ix, bgY + bgH + iy)-(bgX + bgW + ix, bgY + bgH + iy), c + shade
    NEXT i
END SUB

SUB DrawIntro

COLOR COLORMAIN
CONST PI = 3.141592
LET xa = .01
LET ya = 0
LET za = 24
LET XB = 0
LET yb = 0
LET ZB = 0

fi = 180
K = 8
d = 90
xd = XB - xa
yd = yb - ya
zd = ZB - za

d1 = SQR(xd * xd + yd * yd)
IF d1 = 0 THEN d1 = .001
d2 = SQR(xd * xd + yd * yd + zd * zd)

OMEGA = fi * PI / 180
co = COS(OMEGA)
so = SIN(OMEGA)

fog = 32
pitch = 0
rol = 0
yaw = 0
c = 0
cc = 4
T = 0
middle = COLS / 2 - 6
lineColorStart = 80
lineColor = lineColorStart
sfx.bank = 5
sfx.length = 10
sfx.speed = 1 / 2
ENDINTRO = -1

DO
    g$ = INKEY$
   
    T = T + .0333
   
    fxASCIINoise
    HandleSound
  
    COLOR COLORMAIN
    SELECT CASE INT(T)
      CASE 2
        PrintCentered "P1X PRESENTS", 3
        PlaySound
      CASE 6
        PrintCentered "INTERACTIVE FICTIONL BASIC ENGINE", 5
        PlaySound
      CASE 10
        PrintCentered "POWERING THE GAME...", 7
        PlaySound
      CASE 14
        PrintCentered "---======---", 13
        PrintCentered APPLONGNAME, 14
        PrintCentered "---======---", 15
        PlaySound
      CASE 18
        PrintCentered "MADE ON & FOR MS DOS", 21
        PlaySound
      CASE 22
        PrintCentered "RUN FROM A:... 1.44MB FLOPPY!", 23
        PlaySound
   
       CASE 27
        LOCATE 3, COLS - 6
        PRINT "GREETZ";
        LOCATE 4, COLS - 2
        PRINT "TO";
        PlaySound
  
      CASE 30
        LOCATE 7, COLS - 7
        PRINT CHR$(3) + " MONIS";
        PlaySound
     
      CASE 31
        LOCATE 9, COLS - 8
        PRINT "P1X TEAM";
        PlaySound

      CASE 32
        LOCATE 11, COLS - 11
        PRINT "BEFFIO TEAM";
        PlaySound

       CASE 44
        ENDINTRO = 1
   
    END SELECT

    IF T > 26 AND T < 36 THEN
      DrawP1XLogo T
    END IF

  WAIT &H3DA, 8
    FOR i = 1 TO 12
    FOR j = 1 TO 5
      LINE (col(surf(i, j)), row(surf(i, j)))-(col(surf(i, j + 1)), row(surf(i, j + 1))), 0
    NEXT j
    NEXT i
    
    pitch = pitch + COS(T * .1) * 6
    IF pitch > 360 THEN pitch = 0

    yaw = yaw + SIN(5 + T * .1) * 6
    IF yaw > 360 THEN yaw = 0

    rol = rol + SIN(T * .1) * 6
    IF rol > 360 THEN rol = 0

    FOR i = 1 TO 20
    
      xr1 = x(i)
      yr1 = COS(pitch * PI / 180) * y(i) - SIN(pitch * PI / 180) * z(i)
      zr1 = SIN(pitch * PI / 180) * y(i) + COS(pitch * PI / 180) * z(i)

      xr2 = COS(yaw * PI / 180) * xr1 - SIN(yaw * PI / 180) * zr1
      yr2 = yr1
      zr2 = SIN(yaw * PI / 180) * xr1 + COS(yaw * PI / 180) * zr1

      xr3 = COS(rol * PI / 180) * xr2 - SIN(rol * PI / 180) * yr2
      yr3 = SIN(rol * PI / 180) * xr2 + COS(rol * PI / 180) * yr2
      zr3 = zr2

      x1 = xr3 - SIN(T) * 1.5 + xa
      y1 = yr3 - COS(T * 1.2) * 2 + ya
      z1 = zr3 - SIN(T * .75) * 12 + za

      x2 = x1
      y2 = z1
      z2 = -y1
                
      x3 = (yd * x2 + xd * z2) / d1
      y3 = y2
      z3 = (-xd * x2 + yd * z2) / d1
     
      x4 = x3
      y4 = (d1 * y3 + zd * z3) / d2
      z4 = (-zd * y3 + d1 * z3) / d2

      x5 = co * x4 + so * y4
      y5 = -so * x4 + co * y4
      z5 = z4

      x6 = x5
      y6 = y5
      z6 = -z5

      'IF z6 > -fog THEN lineColor(i) = app.bgColor
      'IF z6 > -fog + 1 THEN lineColor(i) = 7
      'IF z6 > -fog + 15 THEN lineColor(i) = 15
    
      u = x6 / z6 * d
      V = y6 / z6 * d

      col(i) = 160 + K * u
      IF col(i) < 0 THEN col(i) = 0
      IF col(i) > 319 THEN col(i) = 320
      row(i) = 100 - K * V
      IF row(i) < 0 THEN row(i) = 0
      IF row(i) > 199 THEN row(i) = 200
    NEXT i

                 
    FOR i = 1 TO 12
    FOR j = 1 TO 5
      LINE (col(surf(i, j)), row(surf(i, j)))-(col(surf(i, j + 1)), row(surf(i, j + 1))), COLOR2
    NEXT j
    NEXT i

    WAIT &H3DA, 8
    
  LOOP UNTIL g$ <> "" OR ENDINTRO = 1

  app.state = STATEMENU
  cnc.cmd = "JMP"

END SUB

SUB DrawMenu (position)

  COLOR COLOR2
  menuX = 15
  menuY = app.centerY
  menuInit = 0
  IF app.scene = 1 THEN menuInit = 1

  FOR i = menuInit TO 4
    IF i < 2 THEN
      COLOR COLORMAIN
    ELSE
      COLOR COLOR2
    END IF
    LOCATE menuY + i, menuX
    IF i = position THEN
      PRINT "-" + CHR$(16);
    ELSE
      PRINT "  ";
    END IF

    SELECT CASE i
    CASE 0
      PRINT "CONTINUE";
    CASE 1
      PRINT "NEW GAME";
    CASE 2
      COLOR COLOR2
      PRINT "SOUND:";
      IF app.soundEnabled > 0 THEN
        PRINT CHR$(14);
      ELSE
        PRINT CHR$(22);
      END IF
    CASE 3
      PRINT "PALETTE";
    CASE 4
      PRINT "QUIT";
    END SELECT


    IF i = position THEN
      PRINT CHR$(17) + "-";
    ELSE
      PRINT "  ";
    END IF
 
  NEXT i

  COLOR COLORMAIN
  LOCATE 24, 3
  PRINT "2021 by Krzysztof Krystian Jankowski";
  LOCATE 25, app.centerX
  PRINT "P1X";

END SUB

SUB DrawP1XLogo (T)

  COLOR 0
 
  ' Get center position of the window
  posx = app.centerX - 8
  posy = app.centerY - 11

  FOR y = 1 TO 22
  FOR x = 1 TO 16
    LOCATE posy + y, posx + x
    IF sprite(x, y) = 1 THEN
      charColor = 80 + (7 + COS(3 * T + x * y) * 7)
      COLOR charColor
      PRINT CHR$(219);
    END IF
  NEXT x
  NEXT y



END SUB

SUB DrawPalette
 
  COLOR COLOR2
  PRINT
  FOR i = 1 TO 40
    PRINT CHR$(220);
  NEXT i
  PRINT
  PRINT
  COLOR COLORMAIN
  PRINT " ARNE 16 COLOR PALETTE"
  PRINT
 
  DrawPaletteBar 0, "00 Black"
  DrawPaletteBar 1, "01 Gray"
  DrawPaletteBar 2, "02 White"
  DrawPaletteBar 3, "03 Red"
  DrawPaletteBar 4, "04 Meat"
  DrawPaletteBar 5, "05 Dark Brown"
  DrawPaletteBar 6, "06 Brown"
  DrawPaletteBar 7, "07 Orange"
  DrawPaletteBar 8, "08 Yellow"
  DrawPaletteBar 9, "09 Dark Green"
  DrawPaletteBar 10, "10 Green"
  DrawPaletteBar 11, "11 Slime Green"
  DrawPaletteBar 12, "12 Night Blue"
  DrawPaletteBar 13, "13 Sea Blue"
  DrawPaletteBar 14, "14 Sky Blue"
  DrawPaletteBar 15, "15 Cloud Blue"

 
 
  COLOR COLOR2
  PRINT
  FOR i = 1 TO 40
    PRINT CHR$(220);
  NEXT i
  PRINT
  PRINT

  cnc.cmd = "JMP"
  SLEEP

END SUB

SUB DrawPaletteBar (ColorID%, ColorName$)
  COLOR ColorID%
  PRINT " ";
  FOR i = 0 TO 16
    PRINT CHR$(219);
  NEXT i
  PRINT " ";
  COLOR COLORMAIN
  PRINT ColorName$
END SUB

SUB DrawScene
  CLS
  LINE (0, 0)-(MAXWIDTH, MAXHEIGHT), 77, BF
 
  ' Set main message
  msg$ = cnc.msg

  ' For jump command there is no message
  IF cnc.cmd = CMDJMP THEN msg$ = "Loading..."
                       
  ' Draws window with title and message
  DrawWindow cnc.cmd, msg$

  IF cnc.cmd = CMDHIT THEN
    PlaySound
    HandleHit
  END IF

  ' If question command then ask for input
  IF cnc.cmd = CMDQST THEN
    PlaySound
    HandleUserAnswer
  ELSE
    ' Set next scene number
    cnc.jmp = cnc.jmp1
  END IF
        
END SUB

SUB DrawTopBar

  endLineName = MAXWIDTH
  endLineTurn = MAXWIDTH - 74

  IF app.state = STATEGAME THEN
    endLineName = MAXWIDTH - 148
  END IF
 
  LINE (0, 0)-(endLineName, TOPBARHEIGHT), ColorName, BF
  COLOR ColorName
  IF app.state = STATEMENU THEN
    PrintCentered APPLONGNAME + " / V" + STR$(VER), 1
  END IF

  IF app.state = STATEGAME THEN
    LOCATE 1, 2
    PRINT APPNAME;
    LINE (endLineName, 0)-(endLineTurn, TOPBARHEIGHT), COLORTURN, BF
    COLOR COLORTURN
    LOCATE 1, COLS - 16
    PRINT "TURN:" + STR$(cnc.turn);
   
    LINE (endLineTurn, 0)-(MAXWIDTH, TOPBARHEIGHT), COLORHP, BF
    COLOR COLORHP
    LOCATE 1, COLS - 6
    PRINT "HP:" + STR$(cnc.hp);
  END IF
END SUB

SUB DrawWindow (title$, msg$)
       
  COLOR COLORMAIN
  ptr = 1
  maxWindowWidth = COLS - 8

  ' Trim the message and questions of empty space at the end
  TrimMsg msg$
  TrimEnds msg$

  DIM qsts(2) AS STRING
  IF cnc.cmd = "QST" THEN
    qsts(0) = cnc.q1
    qsts(1) = cnc.q2
    qsts(2) = cnc.q3
    FOR q = 0 TO 2
      TrimMsg qsts(q)
    NEXT q
  END IF


  ' Calculate size of the box
 
  ' Set window width
  w = maxWindowWidth
  maxColumns = w
  ' Brake message in lines with word-wrap
  row = 0
  ptr = 1
  lastLine = -1
  DIM textLines(20) AS STRING
  filledLines = 0
  curLine = 0
 
  DO
    w$ = ""
    endCol = -1
    FOR j = 0 TO maxColumns
      IF endCol < 0 THEN
        n$ = MID$(msg$, ptr + j, 1)
       
        IF n$ <> " " AND n$ <> "" AND j < maxColumns THEN
          w$ = w$ + n$
        ELSE
          IF j + LEN(w$) <= maxColumns AND LEN(w$) > 0 THEN
            IF textLines(curLine) = "" THEN
              textLines(curLine) = w$
            ELSE
              textLines(curLine) = textLines(curLine) + " " + w$
            END IF
          ELSE
            overflow = overflow + LEN(w$)
            endCol = 1
            row = row + 1
            ptr = ptr + j - LEN(w$)
            curLine = curLine + 1
          END IF
          w$ = ""
        END IF
      END IF
    NEXT j
  LOOP UNTIL ptr > LEN(msg$)

  filledLines = curLine

  ' Set minimum window height
  h = filledLines + 2

  ' Add space for questions
  IF cnc.cmd = CMDQST THEN
    h = h + 3
    IF cnc.jmp3 > 0 THEN h = h + 1
  END IF
 
  ' Add space for hit messages
  IF cnc.cmd = CMDHIT THEN h = h + 6

  ' Get center position of the window
  posx = INT((COLS - w) / 2)
  posy = INT((ROWS - h) / 2)
  cx = MAXWIDTH / 2
  cy = MAXHEIGHT / 2
  pw = w * (320 / 39)
  ph = h * (200 / 23)
  hpw = pw * .5
  hph = ph * .5

  ' Draw background box
  shadowX = 1
  shadowY = 2
  LINE (cx - hpw + shadowX, cy - hph + shadowY)-(cx + hpw + shadowX, cy + hph + shadowY), 12, BF
 
  LINE (cx - hpw, cy - hph)-(cx + hpw, cy + hph), 14, BF
  LINE (cx - hpw, cy - hph)-(cx + hpw, cy + hph), 15, B
 
  FOR j = 1 TO ph STEP 2
    LINE (cx - hpw + 1, cy - hph + j)-(cx + hpw - 1, cy - hph + j), 12
  NEXT j


  ' Draw message in lines
  FOR l = 0 TO filledLines
    LOCATE posy + 2 + l, posx + 2
    PRINT textLines(l);
  NEXT l
  

  ' Draw questions
  IF cnc.cmd = CMDQST THEN
    FOR q = 0 TO 2
      LOCATE posy + 4 + q + filledLines, posx + 3
      IF LEN(qsts(q)) > 0 THEN
        PRINT CHR$(49 + q) + CHR$(16) + " " + qsts(q);
      END IF
    NEXT q
    app.qstPosX = posx + 1
    app.qstPosY = posy + 3 + filledLines
  END IF

  IF cnc.cmd = CMDHIT THEN
    app.qstPosX = posx + 1
    app.qstPosY = posy + 3 + filledLines
  END IF

  ' Draw bottom message if needed
  ' Moved to the DrawBottomBar SUB

END SUB

FUNCTION FileExist% (FileName$)
F% = FREEFILE
OPEN FileName$ FOR APPEND AS #F%
IF LOF(F%) THEN
  FileExist% = -1
  CLOSE #F%
ELSE
  FileExist% = 0
  CLOSE #F%
  KILL FileName$
  CLOSE #F%
END IF
END FUNCTION

SUB fxASCIINoise


  FOR i = 0 TO 8
    x = 1 + INT(RND * COLS)
    y = 1 + INT(RND * ROWS)
   
    LOCATE y, x
    COLOR 192 + RND * 7
    PRINT CHR$(chars(INT(RND * 8)));
  NEXT i


END SUB

SUB fxFall

  w = MAXWIDTH
  h = MAXHEIGHT
  l = 12
  top = 12
          
  s = RND(1)
 
  FOR iter = 0 TO 128
    x = RND(1) * w
    y = RND(1) * h
          
    IF RND(1) < .5 THEN
      DEF SEG = &HA000
      LET c = PEEK((y * 320&) + x)
      IF c > 2 THEN
        ' Put pixel on screen
        POKE ((y * 320&) + x), 0
        FOR i = 1 TO 1 + RND(1) * l
          IF y + i < h THEN POKE (((y + i) * 320&) + x), c
        NEXT i
      END IF
      DEF SEG
    END IF
  NEXT iter
  'NEXT x
  'NEXT y

END SUB

SUB fxNoise (T)

  w = 320
  h = 200

  FOR i = 0 TO 64
    x = RND(1) * w
    y = RND(1) * h
    
    DEF SEG = &HA000
    POKE ((y * 320&) + x), 96 + SIN(T * .1) * 7
    DEF SEG
  NEXT i
END SUB

SUB HandleHit

  c = 1 + INT(RND * cnc.cubes)
  w = 1 + INT(RND * cnc.walls)
  roll = c * w
 
  LOCATE app.qstPosY + 0, app.qstPosX + 1
  PRINT "ENEMY ROLLS " + STR$(cnc.cubes) + "K" + STR$(cnc.walls) + " CUBE...";
  LOCATE app.qstPosY + 1, app.qstPosX + 1
  PRINT "ROLL --> " + STR$(roll) + "pts";
 
  cnc.hp = cnc.hp - roll
  LOCATE app.qstPosY + 3, app.qstPosX + 1
  PRINT "You lost " + STR$(roll) + " Hit Points.";
  LOCATE app.qstPosY + 4, app.qstPosX + 1
  IF cnc.hp > 0 THEN
    PRINT "You still have " + STR$(cnc.hp) + "HP";
    SaveGame
  ELSE
    PRINT "You have died.";
    cnc.cmd = CMDJMP
    cnc.jmp1 = 1
    ResetGame
  END IF

END SUB

SUB HandleMenu

initPos = 0
maxPos = 4
IF app.scene = 1 THEN initPos = 1
position = initPos

DrawMenu initPos
DrawFakeRoom 0, 0, initPos
T = 0
DO
  ' Read user keyboard input
  g$ = INKEY$
  DrawFakeRoom 0, T, position
  WAIT &H3DA, 8
 
  SELECT CASE RIGHT$(g$, 1)
  CASE "H"
    IF position > initPos THEN
      position = position - 1
    END IF
    PlayMenuSound
  CASE "P"
    IF position < maxPos THEN
      position = position + 1
    END IF
    PlayMenuSound
  END SELECT
  T = T + .1
  DrawFakeRoom 170, T, position
  DrawMenu position
  WAIT &H3DA, 8
LOOP UNTIL g$ = CHR$(13)

SELECT CASE position
  CASE 0
  app.state = STATEGAME
  sfx.bank = 0
  sfx.length = 5
 
  DrawBackground 176
 
  CASE 1
  sfx.bank = 0
  sfx.length = 5

  app.scene = 1
  cnc.hp = STARTHP
  app.state = STATEGAME
  DrawBackground 176
 
  CASE 2
  app.soundEnabled = app.soundEnabled * -1
  
  CASE 3
  app.state = STATEMENU
  DrawPalette

  CASE 4
  app.state = STATEQUIT
END SELECT
cnc.cmd = "JMP"

END SUB

SUB HandleSound
  IF app.soundEnabled > 0 THEN
    IF sfx.note > sfx.bank THEN
      SOUND sounds(sfx.bank + sfx.note), sfx.speed
      sfx.note = sfx.note - 1
    END IF
  END IF
END SUB

SUB HandleState

  SELECT CASE app.state
  CASE STATEINTRO
    CLS
    DrawIntro

  CASE STATEMENU
    CLS
    DrawTopBar
    HandleMenu

  CASE STATEGAME
    CLS
    ReadScriptScene
    DrawScene
    DrawTopBar
    DrawBottomBar
    SaveGame

    ' Increate turn counter but not for jump comands
    IF cnc.cmd <> "JMP" AND cnc.cmd <> "END" THEN
      cnc.turn = cnc.turn + 1
    END IF

    app.scene = cnc.jmp

  CASE STATEPALETTE
    CLS
    DrawPalette

  END SELECT
END SUB

SUB HandleUserAnswer
LET cursor = 1
LET maxPos = 2
IF cnc.jmp3 > 0 THEN maxPos = 3
DrawTopBar
DrawBottomBar

DO
  ans$ = INKEY$

  FOR i = 1 TO maxPos
    LOCATE app.qstPosY + i, app.qstPosX + 1
    IF i = cursor THEN
      PRINT CHR$(16);
    ELSE
      PRINT CHR$(186);
    END IF
  NEXT i

  ' Play sound on not supported answer
  IF app.soundEnabled > 0 THEN
    IF ans$ <> "" THEN SOUND 500, sfx.speed
    HandleSound
  END IF

  SELECT CASE RIGHT$(ans$, 1)
  CASE "H"
    IF cursor > 1 THEN cursor = cursor - 1
 
  CASE "P"
    IF cursor < maxPos THEN cursor = cursor + 1

  CASE CHR$(13)
    SELECT CASE cursor
      CASE 1
        cnc.jmp = cnc.jmp1
      CASE 2
        cnc.jmp = cnc.jmp2
      CASE 3
        cnc.jmp = cnc.jmp3
    END SELECT
    cnc.cmd = "JMP"
 
  END SELECT

' End when supported answer given
LOOP UNTIL cnc.cmd = "JMP"

END SUB

SUB Init2DSprite

  FOR y = 1 TO 22
  FOR x = 1 TO 16
    READ sprite(x, y)
  NEXT x
  NEXT y

END SUB

SUB Init3DModel

  FOR i = 1 TO 20
    READ x(i), y(i), z(i)
  NEXT i

  FOR i = 1 TO 12
  FOR j = 1 TO 6
    READ surf(i, j)
  NEXT j
  NEXT i

END SUB

SUB InitPalette
  FOR i = 0 TO 15
    OUT &H3C8, i
    READ r, g, B
    r% = INT(r / 4)
    g% = INT(g / 4)
    B% = INT(B / 4)
   
    OUT &H3C9, r%
    OUT &H3C9, g%
    OUT &H3C9, B%
  NEXT i
END SUB

SUB InitSound
       
  sfx.bank = 0
  sfx.speed = 1 / 4
  sfx.length = 5
  sfx.note = 0
         
  sounds(0) = 44
  sounds(1) = 100
  sounds(2) = 44
  sounds(3) = 150
  sounds(4) = 100

  sounds(5) = 100
  sounds(6) = 200
  sounds(7) = 300
  sounds(8) = 400
  sounds(9) = 250
  sounds(10) = 300
  sounds(12) = 360
  sounds(13) = 340
  sounds(14) = 330
  sounds(15) = 300
END SUB

SUB PlayMenuSound

  IF app.soundEnabled > 0 THEN
    SOUND 250, sfx.speed
  END IF

END SUB

SUB PlaySound
 
  bank = sfx.bank
  IF app.state = STATEGAME THEN
    RandomizeSound bank
  END IF
  sfx.note = sfx.length

END SUB

SUB PrintCentered (msg$, col%)
  center% = INT(COLS * .5 - LEN(msg$) * .5) + 1
  LOCATE col%, center%
  PRINT msg$;
END SUB

SUB RandomizeSound (bank)

  ta = RND(1) * 250
  tb = RND(1) * 250
  sounds(bank) = 44 + ta
  sounds(bank + 1) = 100 + tb
  sounds(bank + 2) = 44 + ta
  sounds(bank + 3) = 150 + tb
  sounds(bank + 4) = 100 + ta + tb

END SUB

SUB ReadScriptScene

  OPEN FILESCRIPT FOR INPUT AS #1
  i = 1
  DO
    ' Read line of a script
    LINE INPUT #1, cmd$
                   
    ' If the line is wanted scene
    IF i = app.scene THEN
                   
      ' Check command at that line
      TrimEnds cmd$
      cnc.cmd = cmd$
      SELECT CASE cmd$
                         
      ' Message user
      CASE CMDMSG
        LINE INPUT #1, cnc.msg$
        cnc.jmp1 = app.scene + 2
                         
      ' End game screen
      CASE CMDEND
        LINE INPUT #1, cnc.msg$
        ResetGame

      ' Ask user for decision
      CASE CMDQST
        INPUT #1, cnc.msg, cnc.q1, cnc.q2, cnc.q3, cnc.jmp1, cnc.jmp2, cnc.jmp3
                         
      ' Jump to other scene
      CASE CMDJMP
        INPUT #1, cnc.jmp1
     
      CASE CMDHIT
        INPUT #1, cnc.msg, cnc.cubes, cnc.walls
        cnc.jmp1 = app.scene + 2

      CASE ELSE
        cnc.cmd = CMDJMP
        cnc.jmp1 = 1
        ResetGame

      END SELECT
 
    END IF
    i = i + 1
  LOOP UNTIL EOF(1) OR i > app.scene
  CLOSE #1

END SUB

SUB ResetGame

    app.scene = 1
    cnc.hp = STARTHP
    cnc.turn = 0
    SaveGame
    app.state = STATEMENU

END SUB

SUB SaveGame

  OPEN "save.dat" FOR OUTPUT AS #2
    PRINT #2, STR$(app.scene), STR$(cnc.hp), STR$(cnc.turn)
  CLOSE #2

END SUB

SUB TrimEnds (msg$)
  msg$ = LEFT$(msg$, LEN(msg$) - 1)
  msg$ = RIGHT$(msg$, LEN(msg$) - 1)
END SUB

SUB TrimMsg (m$)

  DO
    m$ = LEFT$(m$, LEN(m$) - 1)
  LOOP UNTIL RIGHT$(m$, 1) <> " "

END SUB

