DECLARE SUB DrawScene ()
DECLARE SUB FxNoise ()
DECLARE SUB LoadTunes ()
DECLARE SUB DrawBottomBar ()
DECLARE SUB HandleTurn (scene!)
DECLARE SUB RandomizeTune ()
DECLARE SUB DrawBackground (character!)
DECLARE SUB GetUserAnswer ()
DECLARE SUB DrawWindow (title$, msg$)
DECLARE SUB ReadScriptCMD (scene!)
DECLARE SUB ReadMsgs ()
DECLARE SUB DrawBanner (x!, y!)
DECLARE SUB FxNnoise ()
DECLARE SUB FxFall ()
DECLARE SUB DrawLine ()
DECLARE SUB DrawTopBar ()
DECLARE SUB DrawCustomLine (x!, y!, size!, char!)

' codename SDGI / Story Driven Game Interpreter
' developed by Krzysztof Krystian Janowski
' QuickBASIC 4.5 / DOS 5.5
' alpha1 - 26/06/2021
' -------------------------------------------------------------------- '


' Declaring variables
' -------------------------------------------------------------------- '

' Starting scene
scene = 1

' Command & Control variables
TYPE command
 cmd AS STRING * 3
 msg AS STRING * 180
 jmp AS INTEGER
 jmp1 AS INTEGER
 jmp2 AS INTEGER
 jmp3 AS INTEGER
 turn AS INTEGER
 hp AS INTEGER
END TYPE
DIM SHARED cnc AS command
cnc.jmp = scene
cnc.hp = 10

' Sound settings
playTune = -1
tuneLength = 5
currentTune = 0
tuneTimer = tuneLength
DIM SHARED tunes(5)
LoadTunes

' Game settings
turnWaitingTime = 10
turnTimer = turnWaitingTime

' Start of the program
' -------------------------------------------------------------------- '

SCREEN 13
COLOR 3
CLS
DrawBackground 176
DrawTopBar



' Main loop
DO
        ' Read user keyboard input
        g$ = INKEY$

        ' New tune requested, randomize and init play
        IF playTune > 0 THEN
                playTune = -1
                RandomizeTune
                tuneTimer = tuneLength
        END IF
      
        ' Play tune
        IF tuneTimer > 0 THEN
            SOUND tunes(tuneTimer), 1 / 2
            tuneTimer = tuneTimer - 1
        END IF

        ' Timer for turn delay
        IF turnTimer > 0 THEN
                FxNoise
                FxFall
                turnTimer = turnTimer - 1
                IF turnTimer = 0 THEN
                        HandleTurn scene
                        scene = cnc.jmp
                        playTune = 1
                END IF
        END IF
       
        IF turnTimer < 1 AND (g$ <> "" OR cnc.cmd = "JMP") THEN
                turnTimer = turnWaitingTime
                playTune = 1
        END IF

        WAIT &H3DA, 8
        WAIT &H3DA, 8, 8
LOOP UNTIL g$ = CHR$(27) OR cnc.cmd = "END"
SLEEP

SUB DrawBackground (character)
COLOR 3

' Fill the background with custom char
FOR x = 1 TO 40
FOR y = 1 TO 25
        LOCATE y, x
        PRINT CHR$(character);
NEXT y
NEXT x

END SUB

SUB DrawBottomBar

DrawCustomLine 24, 0, 40, 205
DrawCustomLine 25, 0, 40, 177

LOCATE 25, 2
PRINT "TURN:" + STR$(cnc.turn);
LOCATE 25, 34
PRINT "HP:" + STR$(cnc.hp);

END SUB

SUB DrawCustomLine (x, y, size, char)
  FOR i = 1 TO size
    LOCATE x, y + i
    PRINT CHR$(char);
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
        GetUserAnswer
ELSE
        ' Set next scene number
        cnc.jmp = cnc.jmp1
END IF
        
END SUB

SUB DrawTopBar

DrawCustomLine 1, 0, 40, 177
LOCATE 1, 2
PRINT "SDGI / Alpha2"
DrawCustomLine 2, 0, 40, 205

END SUB

SUB DrawWindow (title$, msg$)
COLOR 10
ptr = 1
maxWindowWidth = 34

' Trim the message of empty space at the end
DO
        msg$ = LEFT$(msg$, LEN(msg$) - 1)
LOOP UNTIL RIGHT$(msg$, 1) <> " "

' Calculate size of the box
w = 5 + LEN(msg$)
IF w > maxWindowWidth THEN w = maxWindowWidth
IF w < 5 THEN w = 5
rows = w - 5
max = (LEN(msg$) / rows)
h = INT(max + 5)

' Calculate center position of the window
posx = INT((40 - w) / 2)
posy = INT((25 - h) / 2)
xx = posx
yy = posy


' Draw background box
FOR x = 1 TO w
FOR y = 1 TO h
        LOCATE yy + y, xx + x
        PRINT CHR$(177);
        LOCATE yy + y, xx + x
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
COLOR 11
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

' Draw bottom message if needed
LOCATE posy + h, posx + (w / 2) - 3
SELECT CASE cnc.cmd
        CASE "QST"
                PRINT "[ 1,2,3 ]"
        CASE "JMP"

        CASE ELSE
                PRINT "[ OK ]";
END SELECT
END SUB

SUB FxFall

w = 320
h = 200
l = 12
top = 12
  
s = RND(1)
FOR y = top TO h STEP 8 + s
FOR x = 0 TO w STEP 4 + s
IF RND(1) < .5 THEN
        DEF SEG = &HA000
        LET c = PEEK((y * 320&) + x)
        IF c > 2 THEN
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
        IF ans$ <> "" THEN SOUND 500, 1 / 2
       
        SELECT CASE ans$

        ' User selected 1
        CASE CHR$(49)
                cnc.jmp = cnc.jmp1
                cnc.cmd = "JMP"
1
        ' User selected 2
        CASE CHR$(50)
                cnc.jmp = cnc.jmp2
                cnc.cmd = "JMP"
           
        ' User selected 3
        CASE CHR$(51)
                cnc.jmp = cnc.jmp3
                cnc.cmd = "JMP"
        END SELECT

' End when supported answer given
LOOP UNTIL cnc.cmd = "JMP"

END SUB

SUB HandleTurn (scene)


DrawTopBar
ReadScriptCMD scene
DrawScene
DrawBottomBar

' Increate turn counter but not for jump comands
IF cnc.cmd <> "JMP" AND cnc.cmd <> "END" THEN
        cnc.turn = cnc.turn + 1
END IF


END SUB

SUB LoadTunes

tunes(0) = 44
tunes(1) = 100
tunes(2) = 44
tunes(3) = 150
tunes(4) = 100

END SUB

SUB RandomizeTune

ta = RND(1) * 250
tb = RND(1) * 250
tunes(0) = 44 + ta
tunes(1) = 100 + tb
tunes(2) = 44 + ta
tunes(3) = 150 + tb
tunes(4) = 100 + ta + tb

END SUB

SUB ReadScriptCMD (scene)
OPEN "script.dat" FOR INPUT AS #1
i = 1
DO
        ' Read line of a script
        LINE INPUT #1, cmd$
               
        ' If the line is wanted scene
        IF i = scene THEN
               
                ' Check command at that line
                cnc.cmd = cmd$
                SELECT CASE cmd$
               
                ' Message user
                CASE "MSG"
                        LINE INPUT #1, cnc.msg$
                        cnc.jmp1 = scene + 2
               
                ' End game screen
                CASE "END"
                        LINE INPUT #1, cnc.msg$
               
                ' Ask user for decision
                CASE "QST"
                        INPUT #1, cnc.msg, cnc.jmp1, cnc.jmp2, cnc.jmp3
               
                ' Jump to other scene
                CASE "JMP"
                        INPUT #1, cnc.jmp1
                END SELECT
        END IF
        i = i + 1
LOOP UNTIL EOF(1) OR i > scene
CLOSE #1
END SUB

