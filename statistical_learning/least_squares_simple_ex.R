
rm(list=ls()) 

# Data generator functions
fdata <- function(t) {
  -2.50*t^2 + 1.75*t + 232.50 
}

# Generate and plot observed data
x <- 1:10
ydata <- fdata(x)
plot(x, ydata, pch=19, panel.first = grid(),
     col='red', xlab = 't in seconds', ylab = 'Observed data in Meters')
curve(fdata, from = 1, to = 10, col = 'darkred', lty=5, add = T)

# Fit Model
fmodel <- function(t) {
  c(t^2, t, 1)
}

# Pseudo Inverse compute function
pseudo.inv <- function(M){
  solve(t(M)%*%M)%*%t(M)
}

# Measurement matrix
A <- do.call(rbind, lapply(x, fmodel))

# Coefficients estimation
x.hat <- pseudo.inv(A)%*%matrix(ydata)

# Additive noise to original observed data
y.noise_obs <- ydata + rnorm(10,1,10)
A <- do.call(rbind, lapply(x, fmodel))

# Coefficients estimation
x.hat <- pseudo.inv(A)%*%matrix(y.noise_obs)
y.hat <- function(xhat, t) xhat[1]*t^2 + xhat[2]*t + xhat[3]

# View results
curve(fdata, from = 1, to = 10, col = 'darkblue', lty=5, panel.first = grid(),
      xlab = 't in seconds', ylab = 'Observed data in Meters')
curve(y.hat(x.hat, x), from = 1, to = 10, col = 'darkred', add = T)
points(x, y.noise_obs, pch=19, col='blue')
