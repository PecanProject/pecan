//nr.com
#include <unistd.h>
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <iostream.h>
#include <sys/timeb.h>
#include <sys/stat.h>
#include "util.h"

#define NR_END 1
#define FREE_ARG char*

/*** STRING FUNCTIONS FOR DATA CLEANING ***/
void csv2tab(string& line){
  int loc = line.find(",");
  while(loc !=  string::npos){
    line[loc] = 9;
    loc = line.find(",");
  }
}

string switchNA(string line,string currNA,string newNA){

  int tail_len,tail_start;
  int loc = line.find(currNA);
  while(loc !=  string::npos){
    tail_start=loc+currNA.length();
    tail_len = line.length()-tail_start;
    line = line.substr(0,loc) + newNA + line.substr(tail_start,tail_len);
    loc = line.find(currNA);
  }
  return(line);

}


int mkdir(string path){
  //uses sys.stat.h mkdir
  if(path.size() > 0)  return mkdir(path.c_str(),0775);
  return 0;
}

/* check if file exists before reading */
bool checkfileR(const char *filename)
{
  if(access(filename,F_OK)!=0)
    {
      cout<<filename<<" does not exist."<<endl;
      cout<<"do you wish to continue? (y/n) : "<<endl;
      char response;
      cin>>response;
      if(!(response == 89 || response == 121))
	{
	  exit(0);
	}
      return 0;
      
    }
  return 1;
}

/* check if file exists before writing */
bool checkfileW(char const *filename)
{
  if(access(filename,F_OK)==0)
    {
      cout<<filename<<" already exists."<<endl;
      cout<<"contents will be overwritten."<<endl;
      cout<<"do you wish to continue? (y/n) : "<<endl;
      char response;
      cin>>response;
      if(!(response == 89 || response == 121))
	{
	  exit(0);
	}
      return 0;
      
    }
  return 1;
}


void nrerror(char error_text[])
{
    fprintf(stderr,"\a\a\aNumerical Recipes run-time error...\n");
    fprintf(stderr,"%s\n",error_text);
    fprintf(stderr,"....now exiting system....\n");
    exit(1);
}

int *ivector(long nl, long nh)
{
    int *v;
    v=(int *)malloc((size_t) ((nh-nl+1+NR_END)*sizeof(int)));
    if (!v) nrerror("allocation failure in ivector()");
    return v-nl+NR_END;
}

long *lvector(long nl, long nh)
{
    long *v;
    v=(long *)malloc((size_t) ((nh-nl+1+NR_END)*sizeof(long)));
    if (!v) nrerror("allocation failure in lvector()");
    return v-nl+NR_END;
}

float *avector(long nl, long nh)
{
    float *v;
    v=(float *)malloc((size_t) ((nh-nl+1+NR_END)*sizeof(float)));
    if (!v) nrerror("allocation failure in avector()");
    return v-nl+NR_END;
}

double *dvector(long nl, long nh)
{
    double *v;

    v=(double *)malloc((size_t) ((nh-nl+1+NR_END)*sizeof(double)));
    if (!v) nrerror("allocation failure in dvector()");
    return v-nl+NR_END;
}

float **matrix(long nrl, long nrh, long ncl, long nch)
{
    long i, nrow=nrh-nrl+1,ncol=nch-ncl+1;
    float **m;

    m=(float **) malloc((size_t)((nrow+NR_END)*sizeof(float*)));
    if (!m) nrerror("allocation failure 1 in matrix()");
    m += NR_END;
    m-= nrl;

    m[nrl]=(float *) malloc((size_t)((nrow*ncol+NR_END)*sizeof(float)));
    if (!m[nrl]) nrerror("allocation failure 2 in matrix()");
    m[nrl] += NR_END;
    m[nrl] -= ncl;

    for(i=nrl+1;i<=nrh;i++) m[i]=m[i-1]+ncol;

    return m;
}

int **imatrix(long nrl, long nrh, long ncl, long nch)
{
    long i, nrow=nrh-nrl+1,ncol=nch-ncl+1;
    int **m;

    m=(int **) malloc((size_t)((nrow+NR_END)*sizeof(int*)));
    if (!m) nrerror("allocation failure 1 in matrix()");
    m += NR_END;
    m-= nrl;

    m[nrl]=(int *) malloc((size_t)((nrow*ncol+NR_END)*sizeof(int)));
    if (!m[nrl]) nrerror("allocation failure 2 in matrix()");
    m[nrl] += NR_END;
    m[nrl] -= ncl;

    for(i=nrl+1;i<=nrh;i++) m[i]=m[i-1]+ncol;

    return m;
}

double **dmatrix(long nrl, long nrh, long ncl, long nch)
{
    long i, nrow=nrh-nrl+1,ncol=nch-ncl+1;
    double **m;

    m=(double **) malloc((size_t)((nrow+NR_END)*sizeof(double*)));
    if (!m) nrerror("allocation failure 1 in matrix()");
    m += NR_END;
    m-= nrl;

    m[nrl]=(double *) malloc((size_t)((nrow*ncol+NR_END)*sizeof(double)));
    if (!m[nrl]) nrerror("allocation failure 2 in matrix()");
    m[nrl] += NR_END;
    m[nrl] -= ncl;

    for(i=nrl+1;i<=nrh;i++) m[i]=m[i-1]+ncol;

    return m;
}

void free_lvector(long *v, long nl, long nh)
{
	free((FREE_ARG) (v+nl-NR_END));
}

void free_vector(float *v, long nl, long nh)
{
	free((FREE_ARG) (v+nl-NR_END));
}

void free_dvector(double *v, long nl, long nh)
{
	free((FREE_ARG) (v+nl-NR_END));
}

void free_matrix(float **m, long nrl, long nrh, long ncl, long nch)
{
	free((FREE_ARG) (m[nrl]+ncl-NR_END));
	free((FREE_ARG) (m+nrl-NR_END));
}

void free_imatrix(int **m, long nrl, long nrh, long ncl, long nch)
{
	free((FREE_ARG) (m[nrl]+ncl-NR_END));
	free((FREE_ARG) (m+nrl-NR_END));
}

void free_dmatrix(double **m, long nrl, long nrh, long ncl, long nch)
{
	free((FREE_ARG) (m[nrl]+ncl-NR_END));
	free((FREE_ARG) (m+nrl-NR_END));
}

//float max(float lhs, float rhs){
//	if (lhs>rhs) return lhs;
//	else return rhs;
//}

//float min(float lhs, float rhs){
//	if (lhs<rhs) return lhs;
//	else return rhs;
//}

int max(int lhs, int rhs){
	if (lhs>rhs) return lhs;
	else return rhs;
}

int min(int lhs, int rhs){
	if (lhs<rhs) return lhs;
	else return rhs;
}
double max(double lhs, double rhs){
	if (lhs>rhs) return lhs;
	else return rhs;
}

double min(double lhs, double rhs){
	if (lhs<rhs) return lhs;
	else return rhs;
}

double sq(double x){
	return x*x;
}

/*** TIMER ***/

long gettime(void){
  timeb timer;
  ftime(&timer);
  return timer.millitm + timer.time*1000;
}

//converts an integer to a character array
//needs to include <string.h>
//Michael Dietze 3/20/00

char* itoc(int digit){
     switch(digit){
        case 1: return "1";
                break;
        case 2: return "2";
                break;
        case 3: return "3";
                break;
        case 4: return "4";
                break;
        case 5: return "5";
                break;
        case 6: return "6";
                break;
        case 7: return "7";
                break;
        case 8: return "8";
                break;
        case 9: return "9";
                break;
        case 0: return "0";
                break;
        }
    return "X";
}

void itoa(int myint,char myarray[]){
    int temp(myint);
    int digit;
    int zeros=0;
    strcpy(myarray,"");
    if (temp<0){
        strcat(myarray,"-");
        temp=temp*-1;
    }
    if (temp>=10000){
        digit=temp/10000;
        temp=temp-digit*10000;
        strcat(myarray,itoc(digit));
        zeros=1;
    }
    if (temp>=1000||zeros==1){
        digit=temp/1000;
        temp=temp-digit*1000;
        strcat(myarray,itoc(digit));
        zeros=1;
    }
    if (temp>=100||zeros==1){
        digit=temp/100;
        temp=temp-digit*100;
        strcat(myarray,itoc(digit));
        zeros=1;
    }
    if (temp>=10||zeros==1){
        digit=temp/10;
        temp=temp-digit*10;
        strcat(myarray,itoc(digit));
        zeros=1;
    }
    digit=temp;
    strcat(myarray,itoc(digit));
}
