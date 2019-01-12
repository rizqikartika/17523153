#No 1 D
library(pracma)
Y<-c(40,46,44,55,49)
X<-c(50,51,52,53,54)

data<-lm(Y ~ X)
summary(data)

#NO 2 B
predict(data, data.frame(X = 55))

#No 3 C
library(polynom)
xi <- c(0, 1, 2, 3, 4)
yi <- c(1, 2.25, 3.75, 4.25, 5.65)

dat <- data.frame(cbind(xi, yi))
poly.calc(xi, yi)

#No 4 B
f1 <- function(x){
  1 - 0.07916667*x + 2.19375*x^2 - 0.9958333*x^3 + 0.13125*x^4
}
f1(2.75)

#No 5 C
plot(xi, yi)
curve(f1, add = TRUE)

#No 6 C
bi <- function(a, b) {
  re <- 3
  pn <- (a + b) / 2
  
  while(re >= 0.0001) {
    print(paste(a, b, pn, fx(pn), fx(a), re, sep=" "))
    p <- pn
    if (sign(g(p)) == sign(g(a))) {
      a <- p
    } else {
      b <- p
    }
    pn <- (a + b) / 2
    re <- abs(pn-p) / abs(pn)
  }
}

#NO 7 D
#NO 8 C
#NO 9 B
#NO 10 B

#No 11 A
library("pracma")
f <- function(x) x^2 - 6    
trapzfun(f, 0, 1)

#No 12 D
library("pracma")
f <- function(x) x^3 + 4*x^2- 10
trapzfun(f, 1, 2)

#No 13 A
h <- 0.1 
x<-seq(0,1, by =h)
f<-function(x){
  return(x^2)
}
f0 <-f(x[1])
fi <-sapply(x[2:10], f)
fn <-f(x[length(x)])

trap<-function(f0, fi, fn, h){
  L<-h*(f0+2*sum(fi)+fn)/2
  return(L)
}
trap(f0, fi,fn, h)

#No 14 B
#nO 15 A