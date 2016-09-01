/* crctbl.c - Build CRC Table  Version 1.0.0                         */
/* Copyright (C) 2016 aquila62 at github.com                         */

/* This program is free software; you can redistribute it and/or     */
/* modify it under the terms of the GNU General Public License as    */
/* published by the Free Software Foundation; either version 2 of    */
/* the License, or (at your option) any later version.               */

/* This program is distributed in the hope that it will be useful,   */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of    */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the      */
/* GNU General Public License for more details.                      */

/* You should have received a copy of the GNU General Public License */
/* along with this program; if not, write to:                        */

   /* Free Software Foundation, Inc.                                 */
   /* 59 Temple Place - Suite 330                                    */
   /* Boston, MA 02111-1307, USA.                                    */

#include <stdio.h>

#define POLY 0x1021

int main()
   {
   int i;
   unsigned short crc;
   unsigned short c;
   printf("TBL:\n");
   for (i=0; i<256; i++)
      {
      int j;
      crc = 0;
      c = ((unsigned short) i) << 8;
      for (j=0; j<8; j++)
         {
         if ((crc ^ c) & 0x8000)
            crc = (crc << 1) ^ POLY;
         else
            crc = crc << 1;
         c = c << 1;
         } /* for j = 0..7 */
      printf("   DW 0%04XH\n", crc);
      } /* for i = 0..0xff */
   return(0);
   }  /* main */
