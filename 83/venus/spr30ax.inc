//----------------------------------------------------------------------------
//[ PutSprClp ] [ABCDEFIX]                         [ 177 bytes ] [ CrASH_Man ]
//----------------------------------------------------------------------------
// Draws a sprite with an AND/XOR mask with clipping
//
// parameters: HL -> sprite, (B,C) = coordinates
// returns:    Puts sprite in GRAPH_MEM
//
PutSprClp      XOR  A
__Change_1     LD   DE, DefaultSpriteHeight      // D = 0, E = Height

                OR   C                            // If C < 0
                JP   M, _PSC_NoBotClp             // No bottom clip.

                LD   A, $3F                       // Is C is offscreen?
                SUB  C
                RET  C

__Change_2     CP   DefaultSpriteHeight-1        // If C + 7 < 64
                JR   NC, _PSC_NoVertClp           // No vertical clip.
                INC  A
                LD   E, A
                JR   _PSC_NoVertClp               // Height = 64 - C

_PSC_NoBotClp
__Change_3     CP   -(DefaultSpriteHeight-1)     // Is C is offscreen?
                RET  C

                ADD  A, E                         // Find how many lines
                LD   C, A                         // to actually draw
                SUB  E

                NEG
                LD   E, A
                ADD  HL, DE                       // Move HL down
                LD   E, C                         // by -C lines
                LD   C, D

_PSC_NoVertClp PUSH HL                           // IX -> Sprite
                POP  IX

                LD   A, $77                       // OP code for
                LD   (_PSC_OPchg_1), A            //   LD   (HL), A
                LD   (_PSC_OPchg_2), A

                XOR  A                            // Is B > 0?
                OR   B
                JP   M, _PSC_NoRightClp

                CP   89                           // Is B < 89?
                JR   C, _PSC_ClpDone
                CP   96
                RET  NC

                LD   HL, _PSC_OPchg_1             // Modify LD to NOP
                JR   _PSC_ClpModify

_PSC_NoRightClp CP   -7                           // Is B is offscreen?
                RET  C

                LD   HL, _PSC_OPchg_2             // Modify LD to NOP
_PSC_ClpModify LD   (HL), D

_PSC_ClpDone   LD   B, E                         // B = number of rows

                LD   H, A
                CPL
                AND  %00000111
                LD   (_PSC_OPchg_4 + 1), A
                XOR  %00000111
                LD   (_PSC_OPchg_3 + 1), A
                LD   E, A
                XOR  H
                LD   HL, MaskTable
                ADD  HL, DE
                LD   E, C
                LD   C, (HL)

                LD   H, D
                LD   L, E
                ADD  HL, DE                       // HL = Y * 12
                ADD  HL, DE
                ADD  HL, HL
                ADD  HL, HL

                ADD  A, 8
                RRCA
                RRCA
                RRCA

                LD   E, A                         // HL = Y*12 + X/8
                ADD  HL, DE

                LD   DE, GRAPH_MEM - 1
                ADD  HL, DE

                LD   A, C
                LD   (_PSC_OPchg_6 + 1), A        // mask
                LD   (_PSC_OPchg_7 + 1), A        // mask
                CPL
                LD   (_PSC_OPchg_8 + 1), A        // mask

_PSC_LineLoop
__Change_4     LD   A, (IX+DefaultSpriteHeight)
_PSC_OPchg_3   JR   _PSC_OPchg_3                 // modify

                RLCA
                RLCA
                RLCA
                RLCA
                RLCA
                RLCA
                RLCA
                RLCA

                LD   E, A
_PSC_OPchg_6   AND  %11110000
                LD   C, A
                XOR  E
                LD   D, A

                LD   A, (IX+0)
_PSC_OPchg_4   JR   _PSC_OPchg_4                 // modify

                RRCA
                RRCA
                RRCA
                RRCA
                RRCA
                RRCA
                RRCA

                LD   E, A
_PSC_OPchg_7   OR   %11110000
                AND  (HL)                         // AND with background
                XOR  D                            // XOR with background
_PSC_OPchg_2   LD   (HL), A                      // Write
                INC  HL                           // HL -> next 8 pixels

                LD   A, E
_PSC_OPchg_8   OR   %00001111
                AND  (HL)                         // AND with background
                XOR  C                            // XOR with background
_PSC_OPchg_1   LD   (HL), A                      // Write

                LD   DE, 11
                ADD  HL, DE                       // HL -> next row

                INC  IX                           // Increment to next data

                DJNZ _PSC_LineLoop
                RET

MaskTable      DB   %00000000
               DB   %10000000
               DB   %11000000
               DB   %11100000
               DB   %11110000
               DB   %11111000
               DB   %11111100
               DB   %11111110

	IFNDEF NO_MOD_AX
//----------------------------------------------------------------------------
//[ SetSpriteHeight ] [A]                           [ 16 bytes ] [ CrASH_Man ]
//----------------------------------------------------------------------------
// Changes the default sprite size
//
// parameters: A = New Sprite Height
// returns:    Self modified routine.
//
SetSpriteHeightLD   (__Change_1+1), A
                LD   (__Change_4+2), A
                DEC  A
                LD   (__Change_2+1), A
                NEG
                LD   (__Change_3+1), A
                RET
	ENDIF
