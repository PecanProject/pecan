#define SIGN(a,b) ((b) >= 0.0 ? fabs(a) : -fabs(a))

#ifndef _NR_UTILS_H_
#define _NR_UTILS_H_

#include <string>
using std::string;

/** timer **/

//typedef long hrtime_t;
long gettime();

/** file checking **/
int mkdir(string filename);
bool checkfileW(const char *filename);
bool checkfileR(const char *filename);

/*** STRING FUNCTIONS FOR DATA CLEANING ***/
void csv2tab(string&);
string switchNA(string,string,string);


/*** VECTORS ***/
void nrerror(char error_text[]);
int *ivector(long nl,long nh);
long *lvector(long nl,long nh);
float *avector(long nl,long nh);
double *dvector(long nl,long nh);
void free_lvector(long *v, long nl, long nh);
void free_vector(float *v, long nl, long nh);
void free_dvector(double *v, long nl, long nh);

/*** MATRICES ***/
float **matrix(long nrl, long nrh, long lcl, long nch);
void free_matrix(float **m, long nrl, long nrh, long ncl, long nch);
double **dmatrix(long nrl, long nrh, long lcl, long nch);
void free_dmatrix(double **m, long nrl, long nrh, long ncl, long nch);
int **imatrix(long  nrl, long nrh, long lcl, long nch);
void free_imatrix(int **m, long nrl, long nrh, long ncl, long nch);

/*** MIN/MAX ***/
int max(int,int);
//float max(float,float);
double max(double, double);

int min(int,int);
//float min(float,float);
double min(double,double);

/*** FUNCTION ***/
double sq(double);


char* itoc(int digit);
void itoa(int myint,char myarray[]);


#endif
