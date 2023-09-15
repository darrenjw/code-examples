/*
statsp-bug.c

Compute the mean and standard deviation of some numbers

The number of numbers should be provided as a command line argument

The actual numbers should be entered one per line or piped in
eg. 
./statsp-bug 5 < fiveNumbers.txt

Requires math library: compile with "-lm". eg.
gcc statsp-bug.c -lm -o statsp-bug

*************************************************************
This version has a BUG deliberately induced which causes
the program to crash at runtime.

Can be used to learn about the gnu debugger, gdb - see below
*************************************************************
To use the debugger, you first need to make sure that you
compile your code with the option "-g", to ensure that
debugging information is included in the executable.
gcc statsp-bug.c -g -lm -o statsp-bug

Note that the executable is now very slightly bigger.
Load the executable into the debugger:
gdb statsp-bug

Run the program from inside the debugger with:
run
or
run 5
where 5 is the command line argument you want to pass in.
Enter the numbers one at a time until you've given it
the 5 numbers it expects.
The program will then crash with a segfault.
Note that the debugger immediately gives you the line where
the crash occurred, which is already helpful information.
Typing:
backtrace
will git a full back-trace for the call stack. Not so helpful here,
but typically is extremely useful.
info args
will give the current value of variables on the call stack at 
the time of the crash
print i
will give the current value of the counter i, which hints at the problem...
Ctrl-D to exit.
*************************************************************

If you are struggling to find the bug, do:
diff statsp.c statsp-bug.c 

from the command line to see the line of code that was changed.

*/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

double mean(double *, long);
double stddev(double *, double, long);

int main(int argc, char *argv[]) {
  double *x, *xp, mn, sd;
  long n, i;
  char *junk, *line;
  size_t len;
  if (argc != 2) {
    printf("Usage: stats <N>\n");
    exit(1);
  }
  printf("Parsing input...\n");
  n = strtol(argv[1], &junk, 0);
  printf("Read: %ld.\n", n);
  x = malloc(n * sizeof(double));
  line = NULL;
  xp = x;
  for (i=0; i<n; i++,xp++) {
    getline(&line, &len, stdin);
    *xp = strtod(line, &junk);
  }
  free(line);
  mn = mean(x, n);
  sd = stddev(x, mn, n);
  printf("Mean is: %f and stdev is %f.\n", mn, sd);
  exit(0);
}

double mean(double *x, long n) {
  long i;
  double s;
  s = 0.0;
  for (i=0; i<n; x++) {
    s += *x;
  }
  return(s/n);
}

double stddev(double *x, double m, long n) {
  long i;
  double ss;
  ss = 0.0;
  for (i=0; i<n; i++,x++) {
    ss += (*x - m)*(*x - m);
  }
  return(sqrt(ss/(n-1)));
}




    
/* eof */

