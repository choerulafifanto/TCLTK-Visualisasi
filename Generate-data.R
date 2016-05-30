##
# Tugas APG - TclTk - Visualisasi
# Komputasi Statistik

Generate.data <- function (mu1=0, mu2=0, sig1=0.35, sig2=0.35, rho=0.5) {
  xm <- -3
  xp <- 3
  ym <- -3
  yp <- 3
  x <- seq(xm, xp, length = as.integer(( xp+abs(xm) )*10))
  y <- seq(ym, yp, length = as.integer(( xp+abs(ym) )*10))
  
  bivariate <- function(x,y){
    term1 <- 1 / (2 * pi * sig1 * sig2 * sqrt(1 - rho^2))
    term2 <- (x-mu1)^2 / sig1^2
    term3 <- -(2 * rho * (x - mu1)*(y - mu2))/(sig1*sig2)
    term4 <- (y - mu2)^2 / sig2^2
    z <- term2 + term3 + term4
    term5 <- term1 * exp((-z/(2*(1-rho^2))))
    return(term5)
  }
  
  z <- outer(x,y,bivariate)
  
  return(list(mu1 = mu1, mu2 = mu2,
              sig1 = sig1, sig2 = sig2, rho = rho,
              x = x, y = x, z = z))
}

dataq <- Generate.data(mu1=1, mu2=1, sig1=0.9, sig2=0.9, rho=0.5)

