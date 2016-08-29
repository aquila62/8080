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
