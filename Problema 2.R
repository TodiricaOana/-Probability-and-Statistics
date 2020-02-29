#Calculul functiei gamma

fgam <- function(a){
  if(a > 1){
    return ((a-1) * fgam(a-1));
  }
  
  if(a == 1/2)
    return (sqrt(pi));
  
  if(a == 1)
    return (1);
  
  f <- function(x){
        return (x^(a-1)*exp(-x));
  }
      
  temp <- integrate(f, 0, Inf);
  
  return (temp$value); 
}

#Calculul functiei beta

fbet <- function(a, b){
  if(a+b == 1 && a > 0 && b > 0)
    return (pi/sin(a*pi));
  
  return (fgam(a)*fgam(b)/fgam(a+b));
}

#Calculul celor 9 functii

fprobgamma1 <- function(a, b){
  f <- function(x){
    (x^(a-1)*exp(-x/b))/(b^a*fgam(a));
  }
  
  integrate(f, 0, 3)$value;
}

fprobgamma2 <- function(a, b){
  f <- function(x){
    (x^(a-1)*exp(-x/b))/(b^a*fgam(a));
  }
  
  integrate(f, 2, 5)$value;
}

fprobgamma3 <- function(a, b){
  f <- function(x){
    (x^(a-1)*exp(-x/b))/(b^a*fgam(a));
  }
  
  temp <- integrate(f, 3, 4);
  temp1 <- integrate(f, 2, Inf);
  
  temp$value / temp1$value;
}

fprobbeta4 <- function(a, b){
  f <- function(x){
    (x^(a-1)*(1-x)^(b-1))/(fbet(a,b));
  }
  
  return (0);
  # Y ~ Beta(a, b) => x apartine(0,1), deci probabilitatea ca x > 2 e 0
}

fprobgamma5 <- function(a, b){
  f <- function(x){
    (x^(a-1)*exp(-x/b))/(b^a*fgam(a));
  }
  
  integrate(f, 4, 6)$value;
}

fprobgamma6 <- function(a, b){
  f <- function(x){
    (x^(a-1)*exp(-x/b))/(b^a*fgam(a));
  }
  
  temp <- integrate(f, 0, 1);
  temp1 <- integrate(f, 0, 7);
  
  temp$value / temp1$value;
}

fprob7 <- function(a, b){
  fg <- function(x){
    (x^(a-1)*exp(-x/b))/(b^a*fgam(a));
  }
  
  fb <- function(y){
    (y^(a-1)*(1-y)^(b-1))/(fbet(a,b));
  }
  
  func <- function(x, y){
    fg(x) * fb(y);
  }
  
  f <- function(x){
    min(1, 5-x);
  }
  
  integral2(func, 0, 5, 0, f)$Q;
}

fprob8 <- function(a, b){
  fg <- function(x){
    (x^(a-1)*exp(-x/b))/(b^a*fgam(a));
  }
  
  fb <- function(y){
    (y^(a-1)*(1-y)^(b-1))/(fbet(a,b));
  }
  
  func <- function(x, y){
    fg(x) * fb(y);
  }
  
  f <- function(x){
    min(1, x+0.5);
  }
  
  integral2(func, 0, 1, 0, f)$Q;
}

fprob9 <- function(a, b){
  fg <- function(x){
    (x^(a-1)*exp(-x/b))/(b^a*fgam(a));
  }
  
  fb <- function(y){
    (y^(a-1)*(1-y)^(b-1))/(fbet(a,b));
  }
  
  func <- function(x, y){
    fg(x) * fb(y);
  }
  
  left <- function(x){
    min(1, 3-x);
  }
  
  right <- function(x){
    min(1, x-0,5);
  }
  
  integral2(func, 0, 5, 0, right)$Q;
  
  
  
 # integral2(func, 0, 5, 0, right)$Q ;
}

f7 <- function(a, b){
  fg <- function(x){
    (x^(a-1)*exp(-x/b))/(b^a*gamma(a));
  }
  
  fb <- function(y){
    (y^(a-1)*(1-y)^(b-1))/(beta(a,b));
  }
  
  func <- function(x, y){
    fg(x) * fb(y);
  }
  
  f <- function(x){
    min(1, 5-x);
  }
  
  integral2(func, 0, 5, 0, f)$Q;
}

f8 <- function(a, b){
  fg <- function(x){
    (x^(a-1)*exp(-x/b))/(b^a*gamma(a));
  }
  
  fb <- function(y){
    (y^(a-1)*(1-y)^(b-1))/(beta(a,b));
  }
  
  func <- function(x, y){
    fg(x);
  }
  
  f <- function(x){
    min(1, x+0.5);
  }
  
  fct <- function(x){
    fb(x);
  }
  
  integrate(integrate(fct, 0, f)$value, 0, Inf);
}

f9 <- function(a, b){
  fg <- function(x){
    (x^(a-1)*exp(-x/b))/(b^a*gamma(a));
  }
  
  fb <- function(y){
    (y^(a-1)*(1-y)^(b-1))/(beta(a,b));
  }
  
  func <- function(x, y){
    fg(x) * fb(y);
  }
  
  f <- function(x){
    min(1, 3-x);
  }
  
  f1 <- function(x){
    min(1, x-0,5);
  }
  
  integral2(func, 0, , 0, f)$Q / integral2(func, 0, Inf, 0, f1)$Q ;
}


table <- matrix(NA, ncol = 2, nrow = 27, byrow = TRUE)
colnames(table) <- c("Functia mea", "Functia din sistem");
rownames(table) <- c("P1", "P1", "P1", "P2", "P2", "P2",
                     "P3", "P3", "P3", "P4", "P4", "P4",
                     "P5", "P5", "P5", "P6", "P6", "P6",
                     "P7", "P7", "P7", "P8", "P8", "P8",
                     "P9", "P9", "P9");
table <- as.table(table);

table[1, 1] <- fprobgamma1(1, 2);
table[1, 2] <- pgamma(3, 1, 1/2);

table[2, 1] <- fprobgamma1(2, 3);
table[2, 2] <- pgamma(3, 2, 1/3);

table[3, 1] <- fprobgamma1(2, 4);
table[3, 2] <- pgamma(3, 2, 1/4);

table[4, 1] <- fprobgamma2(1, 2);
table[4, 2] <- pgamma(5, 1, 1/2) - pgamma(2, 1, 1/2);

table[5, 1] <- fprobgamma2(2, 3);
table[5, 2] <- pgamma(5, 2, 1/3) - pgamma(2, 2, 1/3);

table[6, 1] <- fprobgamma2(2, 4);
table[6, 2] <- pgamma(5, 2, 1/4) - pgamma(2, 2, 1/4);

table[7, 1] <- fprobgamma3(1, 2);
table[7, 2] <- (pgamma(4, 1, 1/2) - pgamma(3, 1, 1/2)) / pgamma(2, 1, 1/2, lower.tail = FALSE);

table[8, 1] <- fprobgamma3(2, 3);
table[8, 2] <- (pgamma(4, 2, 1/3) - pgamma(3, 2, 1/3)) / pgamma(2, 2, 1/3, lower.tail = FALSE);

table[9, 1] <- fprobgamma3(2, 4);
table[9, 2] <- (pgamma(4, 2, 1/4) - pgamma(3, 2, 1/4)) / pgamma(2, 2, 1/4, lower.tail = FALSE);

table[10, 1] <- fprobbeta4(1, 2);
table[10, 2] <- pbeta(2, 1, 2, lower.tail = FALSE);

table[11, 1] <- fprobbeta4(2, 3);
table[11, 2] <- pbeta(2, 2, 3, lower.tail = FALSE);

table[12, 1] <- fprobbeta4(2, 4);
table[12, 2] <- pbeta(2, 2, 4, lower.tail = FALSE);

table[13, 1] <- fprobgamma5(1, 2);
table[13, 2] <- pgamma(6, 1, 1/2) - pgamma(4, 1, 1/2);

table[14, 1] <- fprobgamma5(2, 3);
table[14, 2] <- pgamma(6, 2, 1/3) - pgamma(4, 2, 1/3);

table[15, 1] <- fprobgamma5(2, 4);
table[15, 2] <- pgamma(6, 2, 1/4) - pgamma(4, 2, 1/4);

table[16, 1] <- fprobgamma6(1, 2);
table[16, 2] <- pgamma(1, 1, 1/2) / pgamma(7, 1, 1/2);

table[17, 1] <- fprobgamma6(2, 3);
table[17, 2] <- pgamma(1, 2, 1/3) / pgamma(7, 2, 1/3);

table[18, 1] <- fprobgamma6(2, 4);
table[18, 2] <- pgamma(1, 2, 1/4) / pgamma(7, 2, 1/4);

table[19, 1] <- fprob7(1, 2);
table[19, 2] <- f7(1, 2);

table[20, 1] <- fprob7(2, 3);
table[20, 2] <- f7(2, 3);

table[21, 1] <- fprob7(2, 4);
table[21, 2] <- f7(2, 4);

table[22, 1] <- fprob8(1, 2);
table[22, 2] <- f8(1, 2);

table[23, 1] <- fprob8(2, 3);
table[23, 2] <- f8(2, 3);

table[24, 1] <- fprob8(2, 4);
table[24, 2] <- f8(2, 4);

table[25, 1] <- fprob9(1, 2);
table[25, 2] <- f9(1, 2);

table[26, 1] <- fprob9(2, 3);
table[26, 2] <- f9(2, 3);

table[27, 1] <- fprob9(2, 4);
table[27, 2] <- f9(2, 4);

table