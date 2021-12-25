DECLARE SUB MakePalette ()
SCREEN 13
MakePalette

PRINT ""
COLOR 2
PRINT "  ~~~ ARNE ~~~  "
PRINT "16 COLOR PALETTE"

PRINT ""

FOR l = 0 TO 12
FOR c = 0 TO 16
  COLOR c
  PRINT CHR$(219);
NEXT c
PRINT ""
NEXT l


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
DATA 255, 0, 255

SUB MakePalette
  FOR i = 0 TO 15
    OUT &H3C8, i
    READ r, g, b
    r% = INT(r / 4)
    g% = INT(g / 4)
    b% = INT(b / 4)
    
    OUT &H3C9, r%
    OUT &H3C9, g%
    OUT &H3C9, b%
  NEXT i
END SUB

