# example 19
min.sq = function(x, y){
  x.bar = mean(x) 
  y.bar = mean(y)
  beta.1 = sum((x - x.bar)*(y-y.bar))/sum((x - x.bar)^2)
  beta.0 = y.bar - beta.1*x.bar
  return(list(a = beta.0, b = beta.1))
}

a = rnorm(1)
b = rnorm(1)
N = 100
x = rnorm(N)
y = a*x + b + rnorm(N)

plot(x, y)
abline(h = 0)
abline(v = 0)
abline(min.sq(x, y)$a, min.sq(x, y)$b, col = "red")

x = x - mean(x)
y = y - mean(y)

abline(min.sq(x, y)$a, min.sq(x, y)$b, col = "blue")

legend("topleft", c("BEFORE", "AFTER"), lty = 1, col = c("red", "blue"))

# example 20
n = 100 ; p = 3
beta = c(1, 2, 3)
x = matrix(rnorm(n*p), nrow = n, ncol = p)
y = beta[1] + beta[2]*x[,1] + beta[3]*x[,2] + rnorm(n)
solve(t(x)%*%x) %*% t(x) %*% y
