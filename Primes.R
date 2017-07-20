prime<-function(x, int=F)
{
  a=F
  b=T
  ifelse(int, a<-0, a<-F)
  ifelse(int, b<-1, b<-T)
  if(x%%1!=0) return(a) #x<-primeDec(x)
  if(x<=0 || x==1) return(a);
  if(x<=3) return(b);
  n = sqrt(x);
  for(i in 2:n)
  {
    if(x%%i == 0) return(a);
  }
  return(b);
}



primeDec<-function(x)
{
  n=0;
  while(x%%1!=0 && n!=10)
  {
    x=x*10
    n=n+1
  }
  if(n==10) return(-1)
  return(x)
}

primeVec<-function(x, int=F)
{
  n = length(x);
  if(n == 0) return();
  reRay = c();
  for(i in 1:n)
  {
    reRay = c(reRay, prime(x[i], int));
  }
  return(as.vector(reRay));
}

primeFilter<-function(x)
{
  return(x[primeVec(x)]);
}

primeFact<-function(x)
{
  if(prime(x))return(x);
  factList = c();
  halve = x/2;
  for(i in 2:halve)
  {
    if(x%%i==0)
    {
      if(prime(i))
      {
        factList=c(factList,i);
      }
    }
  }
  return(sort(factList));
}

largestPF<-function(x)
{
  if(prime(x))return(x);
  halve = x/2;
  Y<-c()
  for(i in 2:halve)
  {
    if(x%%i==0)
    {
        Y<-c(Y,i)
    }
  }
}

primeFactList<-function(x)
{
  if(length(x)==1)return(primeFact(x));
  factList = list();
  for(i in x)
  {
    factList[[as.character(i)]] = primeFact(i);
  }
  return(factList);
}

primeView<-function(x)
{
  n = primeVec(x);
  return(plot(x,n,'h'));
}

primeRange<-function(low,hi)
{
  if(hi < 0) hi = 0;
  if(low < 0) low = 0;
  reRay = c();
  for(i in low:hi)
  {
    if(prime(i))
    {
      reRay = c(reRay, i);
    }
  }
  return(reRay);
}

primeMatrix<-function(start,cols,rows)
{
  if(rows>=1 && cols>=1 && start>=1)
  {
    dim<-list(seq(0, rows-1,1),seq(start,start+cols-1,1))
    Mat<-matrix(ncol=cols, nrow=rows, dimnames=dim)
    for(i in 1:rows)
    {
      for(j in 1:cols)
      {
        Mat[i,j]=prime(start, T)
        start = start+1
      }
    }
    return(Mat)
  }
  return(-1)
}

CircularPrimes<-function(low,hi)
{
  circPs<-c()
  a<-low:hi
  if(any(c(2,5) %in% a))circPs<-c(2,5,circPs)
  for(i in a)
  {
    digV<-digitVector(i)
    if(any(c(0,2,4,5,6,8) %in% digV)){}
    else
    {
      vec<-digitRotations(i)
      status<-primeVec(vec)
      if(all(status) & !any(vec %in% circPs))
        circPs<-c(circPs, vec)
    }
  }
  return(sort(circPs))
}

digitVector<-function(number)
{
  vec<-as.numeric(strsplit(as.character(number),"")[[1]])
  return(vec)
}
