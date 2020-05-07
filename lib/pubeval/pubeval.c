#include <stdio.h>

// FIX: not thread-safe
static float wr[122], wc[122], x[122];

int pubeval_rdwts()
{
        /* read weight files into arrays wr[], wc[] */
        int i;
        FILE *fp, *fq, *fopen();
        extern float wr[], wc[];
        float x;
        fp = fopen("lib/pubeval/WT.race", "r");
        if(!fp) return 0;
        fq = fopen("lib/pubeval/WT.cntc", "r");
        if(!fq){ fclose(fp); return 0; }
        for(i=0;i<122;++i) {
                fscanf(fp,"%f",wr+i);
                fscanf(fq,"%f",wc+i);
        }
        fclose(fp);
        fclose(fq);
        return 1;
}

static void setx(int pos[])
{
        /* sets input vector x[] given board position pos[] */
        extern float x[];
        int j, jm1, n;
        /* initialize */
        for(j=0;j<122;++j) x[j] = 0.0;

        /* first encode board locations 24-1 */
        for(j=1;j<=24;++j) {
            jm1 = j - 1;
            n = pos[25-j];
            if(n!=0) {
                if(n==-1) x[5*jm1+0] = 1.0;
                if(n==1) x[5*jm1+1] = 1.0;
                if(n>=2) x[5*jm1+2] = 1.0;
                if(n==3) x[5*jm1+3] = 1.0;
                if(n>=4) x[5*jm1+4] = (float)(n-3)/2.0;
            }
        }
        /* encode opponent barmen */
        x[120] = -(float)(pos[0])/2.0;
        /* encode computer's menoff */
        x[121] = (float)(pos[26])/15.0;
}

float pubeval_eval(int race, int pos[])
{
        /* Backgammon move-selection evaluation function
           for benchmark comparisons.  Computes a linear
           evaluation function:  Score = W * X, where X is
           an input vector encoding the board state (using
           a raw encoding of the number of men at each location),
           and W is a weight vector.  Separate weight vectors
           are used for racing positions and contact positions.
           Makes lots of obvious mistakes, but provides a
           decent level of play for benchmarking purposes. */

        /* Provided as a public service to the backgammon
           programming community by Gerry Tesauro, IBM Research.
           (e-mail: tesauro@watson.ibm.com)                     */

        /* The following inputs are needed for this routine:

           race   is an integer variable which should be set
           based on the INITIAL position BEFORE the move.
           Set race=1 if the position is a race (i.e. no contact)
           and 0 if the position is a contact position.

           pos[]  is an integer array of dimension 28 which
           should represent a legal final board state after
           the move. Elements 1-24 correspond to board locations
           1-24 from computer's point of view, i.e. computer's
           men move in the negative direction from 24 to 1, and
           opponent's men move in the positive direction from
           1 to 24. Computer's men are represented by positive
           integers, and opponent's men are represented by negative
           integers. Element 25 represents computer's men on the
           bar (positive integer), and element 0 represents opponent's
           men on the bar (negative integer). Element 26 represents
           computer's men off the board (positive integer), and
           element 27 represents opponent's men off the board
           (negative integer).                                  */

        /* Also, be sure to call rdwts() at the start of your
           program to read in the weight values. Happy hacking] */

        int i;
        float score;

        if(pos[26]==15) return(99999999.);
        /* all men off, best possible move */

        setx(pos); /* sets input array x[] */
        score = 0.0;
        if(race) {  /* use race weights */
            for(i=0;i<122;++i) score += wr[i]*x[i];
        }
        else {  /* use contact weights */
            for(i=0;i<122;++i) score += wc[i]*x[i];
        }
        return(score);
}

