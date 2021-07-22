DECLARE SUB DrawP1XLogo ()
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
DECLARE SUB fxNoise ()
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

' SDGI / Story Driven Game Interpreter
' Developed by Krzysztof Krystian Janowski
' https://bits.p1x.in
'
' Version 3 - 28/06/2021
' -------------------------------------------------------------------- '


' Declaring variables
' -------------------------------------------------------------------- '
CONST STATEINTRO = 0
CONST STATEMENU = 1
CONST STATEGAME = 2
CONST STATEOUTRO = 3
CONST STATEQUIT = 4

CONST CMDQST = 1
CONST CMDMSG = 0
CONST CMDJMP = 2

' Application settings
TYPE settings
  version AS INTEGER
  bgColor AS INTEGER
  mainColor AS INTEGER
  secondaryColor AS INTEGER
  scene AS INTEGER
  rows AS INTEGER
  cols AS INTEGER
  maxWidth AS INTEGER
  maxHeight AS INTEGER
  centerX AS INTEGER
  centerY AS INTEGER
  turnTime AS INTEGER
  soundEnabled AS INTEGER
  state AS INTEGER
  qstPosX AS INTEGER
  qstPosY AS INTEGER
END TYPE
DIM SHARED app AS settings
app.version = 5
app.bgColor = 3
app.mainColor = 10
app.secondaryColor = 11
app.scene = 1
app.rows = 25
app.cols = 40
app.maxWidth = 320
app.maxHeight = 200
app.centerX = INT(app.cols / 2)
app.centerY = INT(app.rows / 2)
app.turnTime = 10
app.soundEnabled = 1
app.state = STATINTRO

' Command & Control variables
TYPE command
 cmd AS STRING * 3
 msg AS STRING * 180
 JMP AS INTEGER
 q1 AS STRING * 26
 q2 AS STRING * 26
 q3 AS STRING * 26
 jmp1 AS INTEGER
 jmp2 AS INTEGER
 jmp3 AS INTEGER
 turn AS INTEGER
 hp AS INTEGER
END TYPE
DIM SHARED cnc AS command
cnc.JMP = app.scene
cnc.hp = 10

' Sound settings
TYPE soundsystem
 length AS INTEGER
 speed AS SINGLE
 bank AS INTEGER
 note AS INTEGER
END TYPE
DIM SHARED sfx AS soundsystem
DIM SHARED sounds(5)
InitSound

DIM SHARED x(20), y(20), z(20)
DIM SHARED col(20), row(20)
DIM SHARED surf(12, 6)

Init3DModel

DIM SHARED sprite(16, 22)
Init2DSprite

turnTimer = 1

' Start of the program
' -------------------------------------------------------------------- '

SCREEN 13
COLOR 3
CLS
DrawBackground 219

CheckSaveGame

' Main loop
DO
        ' Read user keyboard input
        g$ = INKEY$

        ' New tune requested, randomize and init play
        HandleSound

        
        ' Timer for turn delay
        IF turnTimer > 0 THEN
          IF app.state = STATEGAME THEN
            fxNoise
            fxFall
          END IF
          turnTimer = turnTimer - 1
          IF turnTimer = 0 THEN
            HandleTurn
            PlaySound
          END IF
        END IF
                
       
        IF turnTimer < 1 AND (g$ <> "" OR cnc.cmd = "JMP") THEN
                turnTimer = app.turnTime
                PlaySound
        END IF

        WAIT &H3DA, 8
LOOP UNTIL g$ = CHR$(27) OR cnc.cmd = "END" OR app.state = STATEQUIT

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

  OPEN "save.dat" FOR INPUT AS #2
  INPUT #2, lastScene$
  app.scene = VAL(lastScene$)
  CLOSE #2

END SUB

SUB DrawBackground (character)
 
  COLOR app.bgColor

  ' Fill the background with custom char
  FOR x = 1 TO app.cols
  FOR y = 1 TO app.rows
          LOCATE y, x
          PRINT CHR$(character);
  NEXT y
  NEXT x

END SUB

SUB DrawBottomBar
 
  rows = app.rows
  cols = app.cols

  DrawCustomLine rows - 1, 0, cols, 205
  DrawCustomLine rows, 0, cols, 177

  LOCATE rows, 2
  PRINT "TURN:" + STR$(cnc.turn);
  LOCATE rows, cols - 6
  PRINT "HP:" + STR$(cnc.hp);

END SUB

SUB DrawCustomLine (x, y, size, char)
 
  FOR i = 1 TO size
    LOCATE x, y + i
    PRINT CHR$(char);
  NEXT i

END SUB

SUB DrawIntro

COLOR app.mainColor
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
t = 0

DrawP1XLogo

DO
    g$ = INKEY$
   
    t = t + .05
 
    IF t MOD 1 = 0 THEN fxNoise
   
    FOR i = 1 TO 12
    FOR j = 1 TO 5
      LINE (col(surf(i, j)), row(surf(i, j)))-(col(surf(i, j + 1)), row(surf(i, j + 1))), app.bgColor
    NEXT j
    NEXT i
    
    pitch = pitch + COS(t * .1) * 6
    IF pitch > 360 THEN pitch = 0

    yaw = yaw + SIN(5 + t * .1) * 6
    IF yaw > 360 THEN yaw = 0

    rol = rol + SIN(t * .1) * 6
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

      x1 = xr3 - SIN(t) * 1.5 + xa
      y1 = yr3 - COS(t * 1.2) * 2 + ya
      z1 = zr3 - SIN(t * .75) * 12 + za

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
      LINE (col(surf(i, j)), row(surf(i, j)))-(col(surf(i, j + 1)), row(surf(i, j + 1))), 15
    NEXT j
    NEXT i

    WAIT &H3DA, 8
  LOOP UNTIL g$ <> ""

  app.state = STATEMENU
  cnc.cmd = "JMP"

  COLOR app.mainColor
  LOCATE 4, app.centerX - 5
  PRINT "P1X PRESENTS";

  LOCATE 6, app.centerX - 6
  PRINT "UNTITLED  GAME"


END SUB

SUB DrawMenu (position)

  COLOR app.mainColor
  menuX = 15
  menuY = app.centerY
  menuInit = 0
  IF app.scene = 1 THEN menuInit = 1

  FOR i = menuInit TO 3
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
      PRINT "SOUND:";
      IF app.soundEnabled > 0 THEN
        PRINT CHR$(14);
      ELSE
        PRINT CHR$(22);
      END IF
    CASE 3
      PRINT "QUIT";
    END SELECT

    IF i = position THEN
      PRINT CHR$(17) + "-";
    ELSE
      PRINT "  ";
    END IF
 
  NEXT i

  LOCATE 22, 3
  PRINT "2021 by Krzysztof Krystian Jankowski"

END SUB

SUB DrawP1XLogo

  COLOR 0
 
  ' Get center position of the window
  posx = app.centerX - 8
  posy = app.centerY - 11

  FOR y = 1 TO 22
  FOR x = 1 TO 16
    LOCATE posy + y, posx + x
    IF sprite(x, y) = 1 THEN PRINT CHR$(219);
  NEXT x
  NEXT y



END SUB

SUB DrawScene
               

  DrawTopBar
  ' Set main message
  msg$ = cnc.msg

  ' For jump command there is no message
  IF cnc.cmd = "JMP" THEN msg$ = "Loading..."
                       
  ' Draws window with title and message
  DrawWindow cnc.cmd, msg$

  ' If question command then ask for input
  IF cnc.cmd = "QST" THEN
    PlaySound
    GetUserAnswer
  ELSE
    ' Set next scene number
    cnc.JMP = cnc.jmp1
  END IF
        
END SUB

SUB DrawTopBar
  COLOR app.mainColor
  DrawCustomLine 1, 0, 40, 177
  LOCATE 1, 2
  PRINT "SDGI / V." + STR$(app.version)
  DrawCustomLine 2, 0, 40, 205

END SUB

SUB DrawWindow (title$, msg$)
       
  COLOR app.mainColor
  ptr = 1
  maxWindowWidth = app.cols - 6

  ' Trim the message and questions of empty space at the end
  TrimMsg msg$
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
  w = 5 + LEN(msg$)
  IF w > maxWindowWidth THEN w = maxWindowWidth
  IF w < 5 THEN w = 5
  rows = w - 5
  max = (LEN(msg$) / rows)
  h = INT(max + 5)
  IF cnc.cmd = "QST" THEN
    h = h + 3
    IF cnc.jmp3 > 0 THEN h = h + 1
  END IF
 
  ' Get center position of the window
  posx = INT((app.cols - w) / 2)
  posy = INT((app.rows - h) / 2)

  ' Draw background box
  FOR x = 1 TO w
  FOR y = 1 TO h
    LOCATE posy + y, posx + x
    PRINT CHR$(177);
    LOCATE posy + y, posx + x
    IF y = 1 THEN
      IF x = 1 THEN PRINT CHR$(218);
      IF x > 1 AND x < w THEN PRINT CHR$(196);
      IF x = w THEN PRINT CHR$(191);
    END IF
    IF y > 1 AND y < h THEN
      IF x = w - 1 THEN
        PRINT " ";
      ELSE
        IF x = 1 OR x = w THEN PRINT CHR$(179);
      END IF
    END IF
    IF y = h THEN
            IF x = 1 THEN PRINT CHR$(192);
            IF x > 1 AND x < w THEN PRINT CHR$(196);
            IF x = w THEN PRINT CHR$(217);
    END IF
  NEXT y
  NEXT x

  ' Draw title of the window
  LOCATE posy + 1, posx + 2
  COLOR app.secondaryColor
  PRINT title$

  ' Draw message in lines
  FOR i = 0 TO max
    LOCATE posy + 3 + i, posx + 3
    IF i < max THEN
      PRINT MID$(msg$, ptr, rows);
      ptr = ptr + rows
    ELSE
      PRINT MID$(msg$, ptr, max MOD rows);
    END IF
  NEXT i

  ' Draw questions
  IF cnc.cmd = "QST" THEN
    FOR q = 0 TO 2
      LOCATE posy + 4 + q + max, posx + 3
      IF LEN(qsts(q)) > 0 THEN
        PRINT CHR$(49 + q) + CHR$(16) + " " + qsts(q);
      END IF
    NEXT q
    app.qstPosX = posx + 1
    app.qstPosY = posy + 3 + max
 
  END IF


  ' Draw bottom message if needed
  SELECT CASE cnc.cmd
    CASE "QST"
      LOCATE posy + h, posx + (w / 2) - 4
      PRINT "[ CHOOSE ]";
    CASE "JMP"

    CASE ELSE
      LOCATE posy + h, posx + (w / 2) - 3
      PRINT "[ OK ]";
  END SELECT

END SUB

SUB fxFall

  w = app.maxWidth
  h = app.maxHeight
  l = 12
  top = 12
          
  S = RND(1)
  FOR y = top TO h STEP 8 + S
  FOR x = 0 TO w STEP 4 + S
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
  NEXT x
  NEXT y

END SUB

SUB fxNoise

  w = 320
  h = 200

  FOR y = 0 TO h STEP 4
  FOR x = 0 TO w STEP 2
    DEF SEG = &HA000
    yy = y
    IF RND(1) > .5 THEN yy = y + 2
    POKE ((yy * 320&) + x), RND(1)
    DEF SEG
  NEXT x
  NEXT y

END SUB

SUB GetUserAnswer
LET cursor = 1
LET maxPos = 2
IF cnc.jmp3 > 0 THEN maxPos = 3


DO
  ans$ = INKEY$

  FOR i = 1 TO maxPos
    LOCATE app.qstPosY + i, app.qstPosX
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
        cnc.JMP = cnc.jmp1
      CASE 2
        cnc.JMP = cnc.jmp2
      CASE 3
        cnc.JMP = cnc.jmp3
    END SELECT
    cnc.cmd = "JMP"
 
  END SELECT

' End when supported answer given
LOOP UNTIL cnc.cmd = "JMP"

END SUB

SUB HandleMenu

initPos = 0
maxPos = 3
IF app.scene = 1 THEN initPos = 1
position = initPos

DrawMenu initPos

DO
  ' Read user keyboard input
  g$ = INKEY$

  SELECT CASE RIGHT$(g$, 1)
  CASE "H"
    IF position > initPos THEN
      position = position - 1
    END IF
    DrawMenu position
    PlayMenuSound
  CASE "P"
    IF position < maxPos THEN
      position = position + 1
    END IF
    DrawMenu position
    PlayMenuSound
  END SELECT
LOOP UNTIL g$ = CHR$(13)

SELECT CASE position
  CASE 0
  app.state = STATEGAME
  DrawBackground 176
 
  CASE 1
  app.scene = 1
  app.state = STATEGAME
  DrawBackground 176
 
  CASE 2
  app.soundEnabled = app.soundEnabled * -1
  
  CASE 3
  app.state = STATEQUIT
END SELECT
cnc.cmd = "JMP"

END SUB

SUB HandleSound
  IF app.soundEnabled > 0 THEN
    IF sfx.note > 0 THEN
      SOUND sounds(sfx.bank + sfx.note), sfx.speed
      sfx.note = sfx.note - 1
    END IF
  END IF
END SUB

SUB HandleTurn

  SELECT CASE app.state
  CASE STATEINTRO
    DrawIntro

  CASE STATEMENU
    DrawTopBar
    HandleMenu

  CASE STATEGAME
    ReadScriptScene
    DrawScene
    DrawBottomBar
    SaveGame

    ' Increate turn counter but not for jump comands
    IF cnc.cmd <> "JMP" AND cnc.cmd <> "END" THEN
      cnc.turn = cnc.turn + 1
    END IF

    app.scene = cnc.JMP

  END SELECT
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

SUB InitSound
       
  sfx.bank = 0
  sfx.speed = 1 / 2
  sfx.length = 5
  sfx.note = 0
         
  sounds(0) = 44
  sounds(1) = 100
  sounds(2) = 44
  sounds(3) = 150
  sounds(4) = 100

END SUB

SUB PlayMenuSound

  IF app.soundEnabled > 0 THEN
    SOUND 250, sfx.speed
  END IF

END SUB

SUB PlaySound
 
  bank = sfx.bank
  RandomizeSound bank
  sfx.note = sfx.length

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
 
  OPEN "script.dat" FOR INPUT AS #1
  i = 1
  DO
    ' Read line of a script
    LINE INPUT #1, cmd$
                   
    ' If the line is wanted scene
    IF i = app.scene THEN
                   
      ' Check command at that line
      cnc.cmd = cmd$
      SELECT CASE cmd$
                         
      ' Message user
      CASE "MSG"
        LINE INPUT #1, cnc.msg$
        cnc.jmp1 = app.scene + 2
                         
      ' End game screen
      CASE "END"
        LINE INPUT #1, cnc.msg$
        app.scene = 1
        SaveGame

      ' Ask user for decision
      CASE "QST"
        INPUT #1, cnc.msg, cnc.q1, cnc.q2, cnc.q3, cnc.jmp1, cnc.jmp2, cnc.jmp3
                         
      ' Jump to other scene
      CASE "JMP"
        INPUT #1, cnc.jmp1
      END SELECT
    END IF
    i = i + 1
  LOOP UNTIL EOF(1) OR i > app.scene
  CLOSE #1

END SUB

SUB SaveGame

  OPEN "save.dat" FOR OUTPUT AS #2
    PRINT #2, STR$(app.scene)
  CLOSE #2

END SUB

SUB TrimMsg (m$)

  DO
    m$ = LEFT$(m$, LEN(m$) - 1)
  LOOP UNTIL RIGHT$(m$, 1) <> " "

END SUB

