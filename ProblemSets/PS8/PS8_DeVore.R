library("modelsummary")
library("nloptr")

# Problem 4
set.seed(100)
ones <- matrix(data = 1, nrow = 100000, ncol = 1)
body <- matrix(rnorm(900000), nrow = 100000, ncol = 9)
X <- cbind(ones, body)
eps <- matrix(rnorm(100000, 0, 0.5), nrow = 100000)
beta <- t(matrix(c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2), nrow = 1, ncol = 10))
Y = X %*% beta + eps



# Problem 5
prob5 <- solve(t(X) %*% X) %*% (t(X) %*% Y)
print(prob5)
# My answers are insanely close to the true values in problem 4. All answers are, at the very least, 0.1 away from the true values.



# Problem 6 (With heavy help from Chatgpt)
set.seed(101)
alpha <- 0.0000003
iter <- 1000000
betagd <- rep(0, 10)

for(i in 1:iter) {
  yhat <- X %*% betagd
  res <- yhat - Y
  gradient <- t(X) %*% res
  gradient <- gradient / 100000
  betagd <- betagd - alpha * gradient
  return(betagd)
}
print(betagd)



# Problem 7 (With Chatgpt help)
    # LBFGS
eval_f <- function(betalb, X, Y) {
  yhat <- X %*% betalb
  res <- Y - yhat
  sum(res^2)
}

eval_grad_f <- function(betalb, X, Y) {
  -2 * t(X) %*% (Y - yhat)
}

lower <- rep(-Inf, 10)
upper <- rep(Inf, 10)
x0 <- rep(0, 10)

opts <- list("algorithm" = "NLOPT_LD_LBFGS", "xtol_rel" = 1.0e-8)
results1 <- nloptr(x0 = x0, eval_f = eval_f, eval_grad_f = eval_grad_f, lb = lower, ub = upper, X = X, Y = Y, opts = opts)
print(results1$solution)


    # Nelder-Mead
eval_f <- function(betanm, X, Y) {
  yhat <- X %*% betanm
  res <- Y - yhat
  sum(res^2)
}

lower <- rep(-Inf, 10)
upper <- rep(Inf, 10)
x0 <- rep(0, 10)

opts <- list("algorithm" = "NLOPT_LN_NELDERMEAD", "xtol_rel" = 1.0e-8)
results2 <- nloptr(x0 = x0, eval_f = eval_f, lb = lower, ub = upper, X = X, Y = Y, opts = opts)
print(results2$solution)
# My answers definitely differ. Based on my results, it seems that the Nelder-Mead method does not give as accurate of predictions, suggesting that the LBFGS algorithm is superior for predicting OLS.



# Problem 8
objfun <- function(theta, Y, X) {
  beta <- theta[1:length(theta)-1]
  sig <- theta[length(theta)]
  loglike <- -sum(-.5 * (log(2 * pi * (sig^2)) + ((Y - X %*% beta)/sig)^2))
  return(loglike)
}

gradient <- function (theta, Y, X) {
  grad <- as.vector(rep(0, length(theta)))
  beta <- theta[1:(length(theta)-1)]
  sig <- theta[length(theta)]
  grad [1:(length(theta)-1)] <- -t(X) %*% (Y - X %*% beta )/(sig ^2)
  grad[length(theta)] <- dim (X)[1] /sig - crossprod (Y-X%*%beta )/(sig^3)
  return (grad)
}

theta0 <- runif(dim(X)[2] + 1)

options <- list("algorithm" = "NLOPT_LD_LBFGS", "x_tol_rel" = 1.0e-8)
results3 <- nloptr(x0 = theta0, eval_f = objfun, eval_grad_f = gradient, opts = options, Y = Y, X = X)
print(results3$solution)



# Problem 9
betaols <- lm(Y ~ X -1)
print(betaols$coefficients)

modelsummary(betaols, out = "OLS_Regression.tex")

