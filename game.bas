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
DECLARE SUB FxNoise ()
DECLARE SUB LoadTunes ()
DECLARE SUB DrawBottomBar ()
DECLARE SUB DrawBackground (character!)
DECLARE SUB GetUserAnswer ()
DECLARE SUB DrawWindow (title$, msg$)
DECLARE SUB ReadScriptCMD
DECLARE SUB ReadMsgs ()
DECLARE SUB DrawBanner (x!, y!)
DECLARE SUB FxNnoise ()
DECLARE SUB FxFall ()
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
  turnTime AS INTEGER
  soundEnabled AS INTEGER
  state AS INTEGER
END TYPE
DIM SHARED app AS settings
app.version = 4
app.bgColor = 3
app.mainColor = 10
app.secondaryColor = 11
app.scene = 1
app.rows = 25
app.cols = 40
app.maxWidth = 320
app.maxHeight = 200
app.turnTime = 10
app.soundEnabled = -1
app.state = STATEMENU

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

turnTimer = app.turnTime

' Start of the program
' -------------------------------------------------------------------- '

SCREEN 13
COLOR 3
CLS
DrawBackground 176
DrawTopBar
DrawBottomBar
CheckSaveGame

' Main loop
DO
        ' Read user keyboard input
        g$ = INKEY$

        ' New tune requested, randomize and init play
        HandleSound

        ' Timer for turn delay
        IF turnTimer > 0 THEN
                FxNoise
                FxFall
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
        WAIT &H3DA, 8, 8
LOOP UNTIL g$ = CHR$(27) OR cnc.cmd = "END" OR app.state = STATEQUIT

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

    DrawWindow "INTRO", "Welcome to the game. V" + STR$(app.version) + ". "
    app.state = STATEMENU

END SUB

SUB DrawMenu (position)

  menuX = 15
  menuY = 4
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
      PRINT "SOUND:" + STR$(app.soundEnabled);
    CASE 3
      PRINT "QUIT";
    END SELECT

    IF i = position THEN
      PRINT CHR$(17) + "-";
    ELSE
      PRINT "  ";
    END IF
 
  NEXT i

END SUB

SUB DrawScene
                
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

  DrawCustomLine 1, 0, 40, 177
  LOCATE 1, 2
  PRINT "SDGI / V." + STR$(app.version)
  DrawCustomLine 2, 0, 40, 205

END SUB

SUB DrawWindow (title$, msg$)
       
  COLOR app.mainColor
  ptr = 1
  maxWindowWidth = app.cols - 6

  ' Trim the message of empty space at the end
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
  IF cnc.cmd = "QST" THEN h = h + 4

  ' Calculate center position of the window
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

SUB FxFall

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

SUB FxNoise

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
DO
  ans$ = INKEY$
 
  ' Play sound on not supported answer
  IF app.soundEnabled > 0 THEN
    IF ans$ <> "" THEN SOUND 500, sfx.speed
    HandleSound
  END IF
  
  SELECT CASE ans$

  ' User selected 1
  CASE CHR$(49)
    cnc.JMP = cnc.jmp1
    cnc.cmd = "JMP"

  ' User selected 2
  CASE CHR$(50)
    cnc.JMP = cnc.jmp2
    cnc.cmd = "JMP"
             
  ' User selected 3
  CASE CHR$(51)
    cnc.JMP = cnc.jmp3
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
  
  CASE 1
  app.scene = 1
  app.state = STATEGAME
  
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

  DrawTopBar
 
  SELECT CASE app.state
  CASE STATEINTRO
    DrawIntro

  CASE STATEMENU
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

