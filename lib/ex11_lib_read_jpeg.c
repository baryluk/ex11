#include <stdio.h>
#include <jpeglib.h>

/*
%% Copyright (C) 2004 by Joe Armstrong (joe@sics.se)
%% All rights reserved.
%% The copyright holder hereby grants the rights of usage, distribution
%% and modification of this software to everyone and for any purpose, as
%% long as this license and the copyright notice above are preserved and
%% not modified. There is no warranty for this software.
*/

/*  found on the net someplace and hacked
    compile with
    gcc -o read_jpeg read_jpeg.c -ljpeg 
*/


/* 
   For the erlang interface the protocol is 
   --->---  FileName
   ---<---  0 = ok 1 = error
   ---<---  Width:16 Ht:16
   ---<---  Line 1  (3*Width Bytes)
   ---<---  Line 2  (3*Width Bytes)
   ---<---- ...
*/

typedef unsigned char byte;

struct rgb { int r; int g; int b; }
my_buffer[256][256];

void put_scanline_someplace (JSAMPLE* ba, int row_stride)
{
  static int height;
  int i;
  byte buff[4];

  /* ba[0..row_stride] is a single line of
     data R,G,B 
     each sample is 1 byte
     so the number of samples is 3 * the width of the line */

  // for (i=0; i < row_stride; i++)printf("%d\n", ba[i]);

  // printf ("#bytes: %3d height: %3d\n", row_stride, height++);

  /* write the length */
  put_int16(row_stride+1, buff);
  buff[2] = 1;
  write_exact(buff, 3);
  write_exact(ba, row_stride);
}

int main (int argc, char* argv[])
{
  struct jpeg_error_mgr jerr;	/* "public" fields */

  struct jpeg_decompress_struct cinfo;
  FILE * infile;		/* source file */
  JSAMPARRAY buffer;		/* Output row buffer */
  int row_stride;		/* physical row width in output buffer */
  char* filename = "joeold.jpg";
  byte erlbuf[1000];
  int len;

  len = read_cmd(erlbuf);
  

  if ((infile = fopen(erlbuf, "rb")) == NULL) {
    fprintf(stderr, "can't open %s\n", erlbuf);
    return 0;
  }


  cinfo.err = jpeg_std_error(&jerr);
  jpeg_create_decompress(&cinfo);
  jpeg_stdio_src(&cinfo, infile);
  (void) jpeg_read_header(&cinfo, TRUE);
  (void) jpeg_start_decompress(&cinfo);

  /* fprintf (stderr, "width:%d height:%d\n255\n", cinfo.output_width, 
     cinfo.output_height); */

  erlbuf[0] = 1;
  put_int16(cinfo.output_width, erlbuf+1);
  put_int16(cinfo.output_height, erlbuf+3);
  write_cmd(erlbuf, 5);

  
  row_stride = cinfo.output_width * cinfo.output_components;
  buffer = (*cinfo.mem->alloc_sarray)
    ((j_common_ptr) &cinfo, JPOOL_IMAGE, row_stride, 1);

  while (cinfo.output_scanline < cinfo.output_height) {
    /* jpeg_read_scanlines expects an array of pointers to scanlines.
     * Here the array is only one element long, but you could ask for
     * more than one scanline at a time if that's more convenient.
     */
    (void) jpeg_read_scanlines(&cinfo, buffer, 1);
    /* Assume put_scanline_someplace wants a pointer and sample count. */
    put_scanline_someplace(buffer[0], row_stride);
  }
  
  /* signal eof */
  erlbuf[0] = 0;
  write_cmd(erlbuf, 1);

  (void) jpeg_finish_decompress(&cinfo);
  jpeg_destroy_decompress(&cinfo);
  fclose(infile);
  return 0;
}

