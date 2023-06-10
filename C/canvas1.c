/*
canvas1.c

First attempt at a very simple canvas drawing app

*/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

typedef struct {
  int r;
  int g;
  int b;
} colour;

colour white = {255, 255, 255};
colour black = {0, 0, 0};
colour red = {255, 0, 0};
colour green = {0, 255, 0};
colour blue = {0, 0, 255};

typedef struct {
  int w;
  int h;
  colour *pixels;
} image;


image * image_alloc(int, int);
void image_free(image *);
colour image_get(image *, int, int);
void image_set(image *, int, int, colour);
void image_write(image *, char *);


int main(int argc, char *argv[]) {
  image *im;
  im = image_alloc(10, 8);
  image_set(im, 3, 2, black);
  image_set(im, 4, 3, red);
  image_write(im, "test1.ppm");
}


image * image_alloc(int width, int height) {
  image * im;
  int i, j;
  im = malloc(sizeof(image));
  im->w = width;
  im->h = height;
  im->pixels = malloc(width*height*sizeof(colour));
  for (i=0;i<width;i++) {
    for (j=0;j<height;j++) {
      image_set(im, i, j, white);
    }
  }
  return(im);
}

void image_free(image * im) {
  free(im->pixels);
  free(im);
}

colour image_get(image * im, int x, int y) {
  return(im->pixels[y*(im->w) + x]);
}

void image_set(image * im, int x, int y, colour c) {
  im->pixels[y*(im->w) + x] = c;
}

// Output in the plain ASCII "P3" PPM format
// https://netpbm.sourceforge.net/doc/ppm.html
void image_write(image * im, char * fileName) {
  FILE *s;
  int i,j;
  colour c;
  s = fopen(fileName, "w");
  fprintf(s, "P3 %d %d 255\n", im->w, im->h);
  for (j=0;j<(im->h);j++) {
    for (i=0;i<(im->w);i++) {
      c = image_get(im, i, j);
      fprintf(s, "%d %d %d\n", c.r, c.g, c.b);
    }
  }
  fclose(s);
}


// eof

