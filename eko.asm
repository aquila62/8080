; eko.asm - Echo ASCII keyboard characters   Version 1.0.0
; Copyright (C) 2016 aquila62 at github.com

; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License as
; published by the Free Software Foundation; either version 2 of
; the License, or (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program; if not, write to:

   ; Free Software Foundation, Inc.
   ; 59 Temple Place - Suite 330
   ; Boston, MA 02111-1307, USA.

KCIN   EQU 0006H
KCOUT  EQU 0009H
   ORG 100H
LP:
   CALL CIN
   CPI 'q'
   JZ EOJ
   CPI 01AH
   JZ EOJ
   CALL COUT
   JMP LP
EOJ:
   JMP 0H
   NOP
   NOP
   NOP
   NOP
CIN:
   PUSH B
   PUSH D
   LXI D,KCIN
   CALL IOS
   POP D
   POP B
   ; returns character in reg a
   RET
COUT:
   PUSH B
   PUSH D
   PUSH H
   MOV C,A
   LXI D,KCOUT
   CALL IOS
   POP H
   POP D
   POP B
   RET
IOS:
   LHLD 01H
   DAD D
   PCHL
   END LP

