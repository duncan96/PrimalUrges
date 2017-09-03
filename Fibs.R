#Functions for creating, utilizing and exploring the properties of fibonacci type sequences

#Function for generating a vector of n fibonacci numbers starting with x, y
Fib<-function(n = 100, x = 1, y = 1, var = F)
{
  if(!is.numeric(n) | !is.numeric(x) | !is.numeric(y) | (y < x & var == T) |
     !all(c(length(n), length(x), length(y)) == 1))
  {
    return("error: must be numeric values with n of length 1")
  }
  y <- ifelse(var, y, x)
  z = 0;
  if(n<=2)
  {
    if(n<1)return();
    if(n==1)return(x);
    if(n==2)return(c(x,y));
  }
  else
  {
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
}

#generate fibonacci matrix of n cols and length(x) * length(y) rows
Fib_Matrix<-function(n, x = 1, y = 1)
{
  if(!is.numeric(x) | !is.numeric(y) | !is.numeric(n) | y < x | length(n) != 1)
  {
    return("error: must be numeric values for all entries n, x, y with length n = 1 and y >= x")
  }
  fibs<-c()
  for(i in x)
  {
    for(j in y)
    {
      fibs <- c(fib(n,i,j))
    }
  }
  return(matrix(fibs, ncol = n))
}

#Return the number of iterations of basic fibonacci sequence to reach n
FibTo<-function(n)
{
  if(!is.numeric(n) | length(n) != 1)
  {
    return('error: n must be numeric of length 1')
  }
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

#Shows the value to which the ratio between sequential iterations converge over n trials, starting at x
FibConverge<-function(n, x = 1, y = 1, var = F)
{
  if(n < 2 | x < 1 | y < x | !is.numeric(n) | !is.numeric(x) | !all(c(length(n), length(x), length(y)) == 1))
  {
    return("error: n and x must be numeric values of length 1, where n > 2, x > 1")
  }
  A<-Fib(n, x, y, var);
  B<-A[1:n-1];
  for(i in 2:n)
  {
    y = A[i-1];
    z = A[i];
    B[i-1] = z/y;
  }
  return(B)
}


#Basic Fibonacci Sequence of length n
Fibonacci_Seq<-function(n = 1000)
{
  return(Fib(n))
}


#return a Fibonacci matrix of size rows x cols
Fibbonaci_Matrix<-function(cols=10, rows=1, var=F)
{
  if(rows <= 0 || cols <= 0)return(-1)
  RCnames <- list(seq(1,rows,1), seq(0,cols-1,1))
  M <- matrix(nrow=rows, ncol=cols, dimnames=RCnames)
  for(i in 1:rows)
  {
    M[i,] <- ifelse(var, Fib(cols,1,i), Fib(cols,i))
  }
  return(M)
}

#Create a Spiral Matrix (2*levels x 2*levels centered at 1) showing values of the Fibonacci Sequence
#If ind, show the numeric value, otherwise show the position in the sequence
Fibonacci_Spirals<-function(levels, ind=F)
{
  Mat<-spiralMatrix(levels)
  d<-dim(Mat)
  fibSeq <- Fibonacci_Seq()
  for(i in 1:d[1])
  {
    for(j in 1:d[2])
    {
      Mat[i,j]<-ifelse(any(fibSeq == Mat[i,j]), ifelse(ind, match(Mat[i,j],fibSeq),Mat[i,j]), 0)
    }
  }
  return(Mat)
}

#Basic Spiral Matrix, values spiral out from center
spiralMatrix<-function(levels)
{
  ODDV<-levels*2-1
  FinalM<-matrix(0, nrow=ODDV,ncol=ODDV)
  mid<-ceiling(ODDV/2)
  FinalM[mid,mid]<-1
  n<-1
  m<-ODDV
  while(ODDV!=1)
  {
    coefFinal<-ODDV^2
    coefInitial<-(ODDV-2)^2+1
    outerVals<-coefInitial:coefFinal
    FinalM<-outerMat(FinalM, outerVals, n, m)
    ODDV<-ODDV-2
    n<-n+1
    m<-m-1
  }
  return(FinalM)
}

#Helper function for generating spiral matrix
outerMat<-function(Matrix, outer, currentLevel, total)
{
  final<-total
  currentR<-currentLevel+1
  currentC<-final
  a<-1
  while(currentR<final)
  {
    Matrix[currentR, currentC]<-outer[a]
    currentR<-currentR+1
    a<-a+1
  }
  while(currentC>currentLevel)
  {
    Matrix[currentR, currentC]<-outer[a]
    currentC<-currentC-1
    a<-a+1
  }
  while(currentR>currentLevel)
  {
    Matrix[currentR, currentC]<-outer[a]
    currentR<-currentR-1
    a<-a+1
  }
  while(currentC<=final)
  {
    Matrix[currentR, currentC]<-outer[a]
    currentC<-currentC+1
    a<-a+1
  }
  return(Matrix)
}

#Helper function for generating spiral matrix
corners<-function(size)
{
  levels<-ceiling(size/2)+1
  MAT<-spiralMatrix(levels)
  cornerVals<-c(MAT[1,1],MAT[1,size],MAT[size,1],MAT[size,size])
  return(cornerVals)
}
