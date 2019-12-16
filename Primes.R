
#efficient boolean primality check
### TODO implement in c++
atomic_prime<-function(x)
{
  if(x <= 1){return(F)}
  if(x <= 3){return(T)}
  n = sqrt(x)
  for(i in 2:n)
  {
    if(x %% i == 0) return(F)
  }
  return(T)
}


#basic prime function for homogenous data structures
prime <- function(x, intv = F)
{
  y <- as.array(x)
  if(is.numeric(y))
  {
    if(length(y[!y%%1]) > 0)
    {
      ret <- ifelse(!y%%1 == 0, 0, x)
      a <- ifelse(intv, 1, T)
      b <- ifelse(intv, 0, F)
      for(i in 1:length(ret))
      {
        if(ret[i] != 0)
        {
          ret[i] <- ifelse(atomic_prime(ret[i]), a, b)
        }
      }
      if(is.matrix(x))
      {
        return(matrix(ret, dim(x)[1], dim(x)[2]))
      }
      else
      {
        return(ret)
      }
    }
  }
  return("error: argument is not numeric or atomic")
}


#returns vector containing only the primes in x
primeFilter<-function(x)
{
  if(is.numeric(x))
  {
    return(x[prime(x)])
  }
  return("error: not an atomic object")
}


#low level prime factorial, returns vector of prime factors
atomic_primeFact<-function(x)
{
  if(atomic_prime(x))return(x)
  if(x <= 1) return(-1)
  factList = c()
  halve = x/2
  for(i in 2:halve)
  {
    if(x %% i == 0)
    {
      if(atomic_prime(i))
      {
        factList=c(factList,i)
      }
    }
  }
  return(sort(factList))
}


#get the largest prime factor of x
largestPrimeFactor<-function(x)
{
  if(is.numeric(x) & length(x) == 1)
  {
    if(atomic_prime(x))return(x)
    halve = x/2
    for(i in halve:2)
    {
      if(x %% i == 0 & atomic_prime(i))
      {
          return(i)
      }
    }
  }
  return("error: cannot process largest factorial")
}

#list the prime factorials of the elements of x
primeFact<-function(x)
{
  if(is.numeric(x))
  {
    if(length(x)==1)
    {
      return(list(atomic_primeFact(x)))
    }
    factList = list()
    for(i in x)
    {
      factList[[as.character(i)]] = atomic_primeFact(i)
    }
    return(factList)
  }
  return("error: cannot process prime factorial list")
}

#print a visualization of the distribution of primes in x
primeView<-function(x)
{
  n = prime(x)
  return(plot(x,n,'h'))
}


#list all primes within the range of low to hi
primeRange<-function(low, hi)
{
  if(is.numeric(low) & is.numeric(hi) & length(low) == length(hi))
  {
    if(hi < 0) hi = 0
    if(low < 0) low = 0
    reRay = c();
    for(i in low:hi)
    {
      if(atomic_prime(i))
      {
        reRay = c(reRay, i)
      }
    }
    return(reRay)
  }
  return("error: Must have numeric types of length 1")
}
