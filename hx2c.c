/* hx2c.c - Convert Intel Hex to Elf  Version 1.0.0                  */
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
#include <stdlib.h>
#include <unistd.h>

int getbyte(void)
   {
   int len;
   char buf[8];
   len = read(0,buf,1);
   if (!len)
      {
      fprintf(stderr,"getbyte: eof\n");
      fprintf(stderr,"Missing type 01 record\n");
      exit(1);
      } /* missing 01 record */
   if (len != 1)
      {
      fprintf(stderr,"getbyte: read error\n");
      exit(1);
      } /* read error */
   return(buf[0]);
   } /* getbyte */

void putblk(unsigned char *blk, int len)
   {
   int rslt;
   rslt = write(1,blk,len);
   if (rslt != len)
      {
      fprintf(stderr,"putblk: write error\n");
      exit(1);
      } /* write error */
   } /* putblk */

void getdata(int len, int ofst, unsigned char *pgm)
   {
   int i;
   unsigned char *p;
   p = (unsigned char *) pgm + ofst - 0x100;
   i = len;
   while (i--)
      {
      int ch;
      int oktet;
      oktet = 0;
      /*****************************************/
      ch = getbyte();
      ch -= 0x30;
      if (ch > 9) ch -= 7;
      if (ch < 0 || ch > 15)
         {
	 fprintf(stderr,"getdata: error ch %d oktet %02x\n",
	    ch, oktet);
	 while (1)
	    {
	    int ch;
	    ch = getbyte();
	    if (ch == 13) continue;
	    if (ch == 10) return;
	    } /* read to end of record */
	 } /* wrong length */
      oktet = ch << 4;
      /*****************************************/
      ch = getbyte();
      ch -= 0x30;
      if (ch > 9) ch -= 7;
      if (ch < 0 || ch > 15)
         {
	 fprintf(stderr,"getdata: error ch %d oktet %02x\n",
	    ch, oktet);
	 while (1)
	    {
	    int ch;
	    ch = getbyte();
	    if (ch == 13) continue;
	    if (ch == 10) return;
	    } /* read to end of record */
	 } /* wrong length */
      oktet += ch;
      /*****************************************/
      *p++ = (unsigned char) oktet;
      } /* for each byte in data */
   } /* getdata */

void getcksum(void)
   {
   while (1)
      {
      int ch;
      ch = getbyte();
      if (ch == 13) continue;
      if (ch == 10) break;
      } /* flush rest of record */
   } /* getcksum */

int main()
   {
   int len;
   int totlen;
   int ofst;
   int rcdtyp;
   int cksum;
   unsigned char *pgm;
   pgm = (unsigned char *) malloc(1024*64);
   len = totlen = ofst = rcdtyp = cksum = 0;
   while (1)
      {
      int ch;
      ch = getbyte();
      /*******************************************/
      /* read blank                              */
      /*******************************************/
      if (ch != ' ')
         {
	 while (1)
	    {
	    int ch;
	    ch = getbyte();
	    if (ch == 13) continue;
	    if (ch == 10) break;
	    } /* read to end of record */
	 continue;
	 } /* if not space */
      /*******************************************/
      /* read colon                              */
      /*******************************************/
      ch = getbyte();
      if (ch != ':')
         {
	 while (1)
	    {
	    int ch;
	    ch = getbyte();
	    if (ch == 13) continue;
	    if (ch == 10) break;
	    } /* read to end of record */
	 continue;
	 } /* if not colon */
      /*******************************************/
      /* length of data * 16                     */
      /*******************************************/
      ch = getbyte();
      ch -= 0x30;
      if (ch > 9) ch -= 7;
      if (ch < 0 || ch > 15)
         {
	 while (1)
	    {
	    int ch;
	    ch = getbyte();
	    if (ch == 13) continue;
	    if (ch == 10) break;
	    } /* read to end of record */
	 continue;
	 } /* wrong length */
      len = ch << 4;
      /*******************************************/
      /* length of data                          */
      /*******************************************/
      ch = getbyte();
      ch -= 0x30;
      if (ch > 9) ch -= 7;
      if (ch < 0 || ch > 15)
         {
	 while (1)
	    {
	    int ch;
	    ch = getbyte();
	    if (ch == 13) continue;
	    if (ch == 10) break;
	    } /* read to end of record */
	 continue;
	 } /* wrong length */
      len += ch;
      /*******************************************/
      /* offset * 4096                           */
      /*******************************************/
      ch = getbyte();
      ch -= 0x30;
      if (ch > 9) ch -= 7;
      if (ch < 0 || ch > 15)
         {
	 while (1)
	    {
	    int ch;
	    ch = getbyte();
	    if (ch == 13) continue;
	    if (ch == 10) break;
	    } /* read to end of record */
	 continue;
	 } /* wrong length */
      ofst = ch << 12;
      /*******************************************/
      /* offset * 256                            */
      /*******************************************/
      ch = getbyte();
      ch -= 0x30;
      if (ch > 9) ch -= 7;
      if (ch < 0 || ch > 15)
         {
	 while (1)
	    {
	    int ch;
	    ch = getbyte();
	    if (ch == 13) continue;
	    if (ch == 10) break;
	    } /* read to end of record */
	 continue;
	 } /* wrong length */
      ofst = ofst + (ch << 8);
      /*******************************************/
      /* offset * 16                             */
      /*******************************************/
      ch = getbyte();
      ch -= 0x30;
      if (ch > 9) ch -= 7;
      if (ch < 0 || ch > 15)
         {
	 while (1)
	    {
	    int ch;
	    ch = getbyte();
	    if (ch == 13) continue;
	    if (ch == 10) break;
	    } /* read to end of record */
	 continue;
	 } /* wrong length */
      ofst = ofst + (ch << 4);
      /*******************************************/
      /* offset                                  */
      /*******************************************/
      ch = getbyte();
      ch -= 0x30;
      if (ch > 9) ch -= 7;
      if (ch < 0 || ch > 15)
         {
	 while (1)
	    {
	    int ch;
	    ch = getbyte();
	    if (ch == 13) continue;
	    if (ch == 10) break;
	    } /* read to end of record */
	 continue;
	 } /* wrong length */
      ofst += ch;
      /***************************************/
      /* first nybble of record type = 0     */
      /***************************************/
      ch = getbyte();
      if (ch != '0')
         {
	 while (1)
	    {
	    int ch;
	    ch = getbyte();
	    if (ch == 13) continue;
	    if (ch == 10) break;
	    } /* read to end of record */
	 continue;
	 } /* wrong length */
      /***************************************/
      /* second nybble of record type        */
      /***************************************/
      ch = getbyte();
      if (ch == '1')
	 {
         break;
	 } /* end of .hex file */
      else if (ch == '0')
         {
	 totlen = ofst + len - 0x100;
	 getdata(len,ofst,pgm);
	 getcksum();
	 } /* if type zero */
      else
         {
	 while (1)
	    {
	    int ch;
	    ch = getbyte();
	    if (ch == 13) continue;
	    if (ch == 10) break;
	    } /* read to end of record */
	 } /* wrong record type */
      } /* for each record */
   putblk(pgm,totlen);
   return(0);
   } /* main */
