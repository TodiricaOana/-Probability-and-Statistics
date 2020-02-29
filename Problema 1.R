
#Problema 1
#Subpunctul 1

set.seed(124)
binom <- rbinom(1000, 20, 0.2)
binom[1:1000]
mean(binom)
var(binom)

pois <- rpois(1000, lambda = 3)
pois[1:1000]
mean(pois)
var(pois)

expon <- rexp(1000, rate = 1/3)
expon[1:1000]
mean(expon)
var(expon)

norm <- rnorm(1000, mean = 0, sd = sqrt(2))
norm[1:1000]
mean(norm)
var(norm)




#Subpunctul 2

colors <- c("black", "green", "blue", "magenta", "yellow")
labels <- c("0.5", "0.3", "0.6", "0.1", "0.2")
x <- seq(0,50,by = 1)
plot(x,dbinom(x,50,0.5), main = "Repartitia Binomiala", xlab="x", ylab="Masa")
x <- seq(0,45,by = 2)
lines(x,dbinom(x,50,0.3), col = "green")
x <- seq(0,60,by = 2)
lines(x,dbinom(x,30,0.6), col = "blue")
x <- seq(0,70,by = 2)
lines(x,dbinom(x,70,0.1), col = "magenta")
x <- seq(0,30,by = 1)
lines(x,dbinom(x,30,0.2), col = "yellow")
legend("topright", title="Probabilitatea", labels, lwd=2, lty=c(1, 1, 1, 1, 1), col=colors) 


colors <- c("black", "green", "blue", "magenta", "yellow")
labels <- c("lambda=5", "lambda=3", "lambda=6", "lambda=8", "lambda=3")
plot(0:30, dpois(0:30, 5), main = "Repartitia Poisson", xlab="x", ylab="Masa")
lines(0:24, dpois(0:24, 3), col = "green")
lines(0:35, dpois(0:35, 6), col = "blue")
lines(0:40, dpois(0:40, 8), col = "magenta")
lines(0:31, dpois(0:31, 3), col = "yellow")
legend("topright", title="Distributii",  labels, lwd=2, lty=c(1, 1, 1, 1, 1), col=colors) 


colors <- c("green", "blue", "magenta", "yellow", "red")
labels <- c("lambda=1/12", "lambda=1/13", "lambda=1/30", "lambda=1/4", "lambda=10")
lambda <- 1/12
t1 <- seq(-5, 100, 0.01)
plot(t1, dexp(t1,lambda), col = "green", xlab="x", ylab="Densitate", main = "Repartitia Exponentiala")
t2 <- seq(-3, 200, 0.02)
lines(t2, dexp(t2, 1/13), col = "blue")
lines(t2, dexp(t2, 1/30), col = "magenta")
t3 <- seq(-7, 50, 0.01)
lines(t3, dexp(t3, 1/4), col = "yellow")
lines(t3, dexp(t3, 10), col = "red")
legend("topright", title="Distributii", labels, lwd=2, lty=c(1, 1, 1, 1, 1), col=colors)



x <- seq(-4, 4, length=100)
hx <- dnorm(x)

degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(x, hx, type="l", lty=2, xlab="x", ylab="Densitate", main="Reparititia normala")

for (i in 1:4){
        lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Distributii", labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)




#Subpunctul 3


colors <- c("black", "green", "blue", "magenta", "yellow")
labels <- c("0.5", "0.3", "0.6", "0.1", "0.2")
x <- seq(0,50,by = 1)
plot(x,pbinom(x,50,0.5), main = "Repartitia Binomiala", xlab="x", ylab="Repartitia")
x <- seq(0,45,by = 2)
lines(x,pbinom(x,50,0.3), col = "green")
x <- seq(0,60,by = 2)
lines(x,pbinom(x,30,0.6), col = "blue")
x <- seq(0,70,by = 2)
lines(x,pbinom(x,70,0.1), col = "magenta")
x <- seq(0,30,by = 1)
lines(x,pbinom(x,30,0.2), col = "yellow")
legend("topright", title="Probabilitatea", labels, lwd=2, lty=c(1, 1, 1, 1, 1), col=colors) 


colors <- c("black", "green", "blue", "magenta", "yellow")
labels <- c("lambda=5", "lambda=3", "lambda=6", "lambda=8", "lambda=3")
plot(0:30, ppois(0:30, 5), main = "Repartitia Poisson", xlab="x", ylab="Repartitia")
lines(0:24, ppois(0:24, 3), col = "green")
lines(0:35, ppois(0:35, 6), col = "blue")
lines(0:40, ppois(0:40, 8), col = "magenta")
lines(0:31, ppois(0:31, 3), col = "yellow")
legend("topright", title="Distributii",  labels, lwd=2, lty=c(1, 1, 1, 1, 1), col=colors) 


colors <- c("green", "blue", "magenta", "yellow", "red")
labels <- c("lambda=1/12", "lambda=1/13", "lambda=1/30", "lambda=1/4", "lambda=10")
lambda <- 1/12
t1 <- seq(-5, 100, 0.01)
plot(t1, pexp(t1,lambda), col = "green", xlab="x", ylab="Repartitia", main = "Repartitia Exponentiala")
t2 <- seq(-3, 200, 0.02)
lines(t2, pexp(t2, 1/13), col = "blue")
lines(t2, pexp(t2, 1/30), col = "magenta")
t3 <- seq(-7, 50, 0.01)
lines(t3, pexp(t3, 1/4), col = "yellow")
lines(t3, pexp(t3, 10), col = "red")
legend("topright", title="Distributii", labels, lwd=2, lty=c(1, 1, 1, 1, 1), col=colors)



x <- seq(-4, 4, length=100)
hx <- pnorm(x)

degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(x, hx, type="l", lty=2, xlab="x", ylab="Reparitita", main="Reparititia normala")

for (i in 1:4){
        lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Distributii", labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)





