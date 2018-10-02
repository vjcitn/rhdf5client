#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include <stdlib.h>
#include <stdio.h>

// sdims is a vector of integers
// reslst is a list-of-list-of-list-of-vec (for 4-dimensional data) of character
// (Check that it is character)

void rsub4idx(int *D, int *Dcpr, int nd, int idx, int **sbs);
void csub4idx(int *D, int *Dcpl, int nd, int idx, int **sbs);
int ridx4sub(int *D, int *Dcpr, int nd, int *sbs);
int cidx4sub(int *D, int *Dcpl, int nd, int *sbs);

SEXP extractJSON(SEXP nd_sexp, SEXP sdims_sexp, SEXP reslst_sexp)  {

  int *ip = INTEGER(nd_sexp);
  int nd = *ip;
  int *D = (int *)malloc(nd*sizeof(int));     // dims
  memcpy(D, INTEGER(sdims_sexp), nd*sizeof(int));

  int *Dcpl = (int *)malloc(nd*sizeof(int)); 
  Dcpl[0] = 1;             // cumulative product left
  for(int i=1;i<nd;i++)  
    Dcpl[i] = D[i-1]*Dcpl[i-1];

  int na = 1;   // number of elements to extract
  for(int i=0;i<nd;i++)  
    na *= D[i];
  double *A = (double *)malloc(na*sizeof(double));
  char *buff = R_alloc(1024, sizeof(char));

  // the data is transmitted as a nested list of vectors
  int *S = (int *)malloc(nd*sizeof(int));
  for(int ia=0;ia<na;ia++)  {
    SEXP sx = reslst_sexp;
    csub4idx(D, Dcpl, nd, ia, &S);

    for(int j=0;j<nd-1;j++)  
      sx = VECTOR_ELT(sx, S[j]);

    SEXP ss;
    PROTECT(ss = AS_CHARACTER(sx));
    strcpy(buff, CHAR(STRING_ELT(ss, S[nd-1])));
    UNPROTECT(1);

    A[ia] = (double) atof(buff);
  }

  SEXP result = Rf_allocVector(REALSXP, na);
  PROTECT(result);
  memcpy(REAL(result), A, na*sizeof(double));
  UNPROTECT(1);

  free(D);
  free(Dcpl);
  free(A);
  free(S);

  return(result);
}

SEXP extractBin(SEXP nd_sexp, SEXP sdims_sexp, SEXP resvec_sexp)  {

  int *ip = INTEGER(nd_sexp);
  int nd = *ip;
  int *D = (int *)malloc(nd*sizeof(int));     // dims
  memcpy(D, INTEGER(sdims_sexp), nd*sizeof(int));

  int *Dcpl = (int *)malloc(nd*sizeof(int)); 
  Dcpl[0] = 1;             // cumulative product left
  for(int i=1;i<nd;i++)  
    Dcpl[i] = D[i-1]*Dcpl[i-1];

  int *Dcpr = (int *)malloc(nd*sizeof(int)); 
  Dcpr[nd-1] = 1;             // cumulative product right
  for(int i=nd-2;i>=0;i--)  
    Dcpr[i] = D[i+1]*Dcpr[i+1];

  int na = 1;   // number of elements to extract
  for(int i=0;i<nd;i++)  
    na *= D[i];
  double *A = (double *)malloc(na*sizeof(double));

  // the data is transmitted as a vector in row-major order
  int *rip;
  double *rdp;

  if (Rf_isReal(resvec_sexp))  
    rdp = REAL(resvec_sexp);
  else 
    rip = INTEGER(resvec_sexp);

  int *S = (int *)malloc(nd*sizeof(int));
  for(int ia=0;ia<na;ia++)  {
    csub4idx(D, Dcpl, nd, ia, &S);
    int ib = ridx4sub(D, Dcpr, nd, S);
    A[ia] = (Rf_isReal(resvec_sexp) ? rdp[ib] : rip[ib]);
  }

  SEXP result = Rf_allocVector(REALSXP, na);
  PROTECT(result);
  memcpy(REAL(result), A, na*sizeof(double));
  UNPROTECT(1);

  free(D);
  free(Dcpl);
  free(Dcpr);
  free(A);
  free(S);

  return(result);
}


void rsub4idx(int *D, int *Dcpr, int nd, int idx, int **subs)  {
  for(int j=0;j<nd;j++)  {
    int s;
    if (j == 0)  
      s = idx / Dcpr[0];
    else if (j == nd-1)  
      s = idx % D[nd-1]; 
    else  
      s = (idx / Dcpr[j]) % D[j];
    (*subs)[j] = s;
  }
}


void csub4idx(int *D, int *Dcpl, int nd, int idx, int **subs)  {
  for(int j=0;j<nd;j++)  {
    int s;
    if (j == 0)
      s = idx % D[0];
    else if (j == nd-1)  
      s = idx / Dcpl[nd-1];
    else  
      s = (idx / Dcpl[j]) % D[j];
    (*subs)[j] = s;
  }
}


int ridx4sub(int *D, int *Dcpr, int nd, int *sbs)  {
  int idx = 0;
  for(int j=0;j<nd;j++)  
    idx = idx + Dcpr[j]*sbs[j];
  return idx;
}

int cidx4sub(int *D, int *Dcpl, int nd, int *sbs)  {
  int idx = 0;
  for(int j=0;j<nd;j++)  
    idx = idx + Dcpl[j]*sbs[j];
  return idx;
}

