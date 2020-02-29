#Problema 3

#subpunctul a

frepcomgen <- function (n,m)
{
  A = matrix(data = 1/(m*n), nrow = n, ncol = m)
 
  if(m<n)
  for(i in 1:m) {
    A[i, i] <- -1
  }
  else
    for(i in 1:n) {
      A[i, i] <- -1
    }
    
  return (A)
}
n <- 5
m <- 6
A <- frepcomgen (n,m)
print(A)

x <- 0:(n-1)
y <- 0:(m-1)
print (x)  
print (y)

p <- rep(m/(m*n) , n)
q <- rep(n/(m*n) , m)
print(p)
print(q)



#subpunctul b
fcomplrepcom <- function(A, n, m , x, y, p, q)
{
  if(n > m){
    v <- rowSums(A) + 1
    for(i in 1:m)
      A[i,i] <- p[i] - v[i]
    
  }
  else{
      v <- colSums(A) + 1
      for(i in 1:n)
        A[i,i] <- q[i] - v[i]
  }
  
    return(A)
}

B <- fcomplrepcom(A,n,m,x,y,p,q)
print(B)



# subpunctul c
Ex <- 0
for(i in 1:n)
  Ex <- Ex + x[i]*p[i]
print(Ex)

Ey <- 0
for(i in 1:m)
  Ey <- Ey + y[i]*q[i]
print(Ey)

Exy <- 0
for(i in 1:n) {
  for(j in 1:m) {
    Exy <- Exy + B[i,j]*x[i]*y[j]
  }
}
print(Exy)

Cov <- 12 * (Exy -Ex*Ey)
print(Cov)



Pxy <- 0
Py <- 0
P <- 0
if(m > 5){
  for(i in 6:m)
    Py <- Py + q[i]
print(Py)

  for(i in  2: min(5,n))
    for(j in 6:m)
      Pxy <- Pxy + B[i,j]
  print(Pxy)

P <- Pxy / Py
}
print(P)




Pxy <- 0
if(n > 4){
  
  for(i in 5:n)
    for(j in 1: min(7,m))
      Pxy <- Pxy + B[i,j]
}
print(Pxy)



# subpunctul d

fverind <- function(B,n,m,x,y,p,q){
ok <- 1
  for( i in 1:n)
    for(j in 1:m){
      if(B[i,j] != p[i]*q[j])
        ok <- 0
    }
return (ok)
}

ok <- fverind(B,n,m,x,y,p,q)
if( ok == 0)
{ 
  print("Nu sunt independente")
}
if(ok == 1)
  print("Sunt independente")



fvernecor <- function(Cov)
{
  if(Cov == 0)
    print("Necorelate")
  if(Cov != 0)
    print("Corelate")
}

fvernecor(Cov)
  