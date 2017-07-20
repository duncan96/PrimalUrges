Fib<-function(x, n)
{
  y = x;
  z = 0;
  if(n<=2)
  {  
    if(n<1)return();
    if(n==1)return(x);
    if(n==2)return(c(x,x));
  }
  fib = c(x,y);
  n=n-2;
  while(n>0)
  {
    fib = c(fib,x+y);
    z=y;
    y=x+y;
    x=z;
    n=n-1;
  }
  return(fib);
}
Fibs<-Fib(1,1000)
FibVar<-function(x,y,n)
{
  if(n<=2)
  {  
    if(n<1)return();
    if(n==1)return(x);
    if(n==2)return(c(x,y));
  }
  fib = c(x,y);
  n = n-2;
  while(n>0)
  {
    fib = c(fib,x+y);
    z=y;
    y=x+y;
    x=z;
    n=n-1;
  }
  return(fib);
}

FibIters<-function(n)
{
  return(Fib(1,n));
}

FibTo<-function(n)
{
  if(n<=2)
  {  
    if(n<1)return(0);
    if(n==1)return(1);
    if(n==2)return(3);
  }
  z = 0;
  x = 1;
  y = 1;
  fib = 3;
  while(x+y<n)
  {
    fib = fib+1;
    z=y;
    y=x+y;
    x=z;
  }
  return(fib);
}

isFib<-function(num, base)
{
  s<-Fib(base,num+1);
  return(any(s==num));
}

FibN<-function(n, base)
{
  s<-Fib(base, n);
  return(s[n]);
}

FibMatrix<-function(rows,cols, var=F)
{
  if(rows <= 0 || cols <= 0)return(-1)
  Y <- list(seq(1,rows,1), seq(0,cols-1,1))
  M <- matrix(nrow=rows, ncol=cols, dimnames=Y)
  for(i in 1:rows)
  {
    if(var){
      fib<-FibVar(1,i,cols)
    }
    else
    {
      fib<-Fib(i, cols)
    }
    for(j in 1:cols)
    {
      M[i,j]<-fib[j]
    }
  }
  return(M)
}

FibConverge<-function(x,n)
{
  A<-Fib(x, n);
  B<-A[1:n-1];
  for(i in 2:n)
  {
    y = A[i-1];
    z = A[i];
    B[i-1] = z/y;
  }
  return(B)
}

FibTrans<-function(digs)
{
  x<-1
  y<-1
  z<-0
  n<-2
  while(y<10^digs)
  {
    z<-y
    y<-x+y
    x<-z
    n<-n+1
  }
  print(x)
  print(y)
  return(n)
}


FibSpirals<-function(levels, ind=F)
{
  Mat<-spiralMatrix(levels)
  d<-dim(Mat)
  for(i in 1:d[1])
  {
    for(j in 1:d[2])
    {
      Mat[i,j]<-ifelse(contains(Fibs,Mat[i,j]), ifelse(ind, match(Mat[i,j],Fibs),Mat[i,j]),0)
    }
  }
  return(Mat)
}
