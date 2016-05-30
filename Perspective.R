##
# Tugas APG - TclTk - Visualisasi
# Komputasi Statistik

# Multivariate_using_R pg. 31
# Edouard Tallent @ TaGoMa.Tech
# September 2012
# This code plots simulated bivariate normal distributions

# Core function
bivariate <- function(x,y){
  term1 <- 1 / (2 * pi * sig1 * sig2 * sqrt(1 - rho^2))
  term2 <- (x - mu1)^2 / sig1^2
  term3 <- -(2 * rho * (x - mu1)*(y - mu2))/(sig1 * sig2)
  term4 <- (y - mu2)^2 / sig2^2
  z <- term2 + term3 + term4
  term5 <- term1 * exp((-z / (2 *(1 - rho^2))))
  return (term5)
}

# Plot
# Color of Plot with White Background
facetcol <- function(c, z){
  ncz <- ncol(z)
  nrz <- nrow(z)
  par(bg = "white")  
  # Create a function interpolating colors in the range of specified colors
  jet.colors <- colorRampPalette(c)
  # Generate the desired number of colors from this palette
  nbcol <- 100
  color <<- jet.colors(nbcol)
  # Compute the z-value at the facet centres
  zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]  
  # Return recoded facet z-values into color indices
  # cut divides the range of x into intervals and codes the values in x according to which interval they fall.
  facetcl <<- cut(zfacet,nbcol)
  return(list(color, facetcl))
}

# Perspective Plot (Default Color: Green and Blue) *user customized color will be added later (if it is necessary)
BiVPers <- function(x,y,co,theta,phi,r,d,expand,border,ltheta,lphi,shade,box,axes,nticks,ticktype){
  ## Default S3 method:
  if(missing(co)){ co <- c('blue','green') }
  if(missing(theta)){ theta = 0 }
  if(missing(phi)){ phi = 15 }
  if(missing(r)){ r = sqrt(3) }
  if(missing(d)){ d = 1 }
  if(missing(expand)){ expand = 1 }
  if(missing(border)){ border = NULL}
  if(missing(ltheta)){ ltheta = -135 }
  if(missing(lphi)){ lphi = 0 }
  if(missing(shade)){ shade = NA }
  if(missing(box)){ box = TRUE }
  if(missing(axes)){ axes = TRUE }
  if(missing(nticks)){ nticks = 5 }
  if(missing(ticktype)){ ticktype = "simple" }
  # Computes the density values
  z <- outer(x,y,bivariate)
  f <- facetcol(co,z)
  persp(x, y, z, main = "Bivariate Normal Distribution",
        sub = bquote(bold(mu[1])==.(mu1)~", "~sigma[1]==.(sig1)~", 
        "~mu[2]==.(mu2)~", "~sigma[2]==.(sig2)~", "~rho==.(rho)),
        col = color[facetcl], theta = theta, phi = phi, r = r, d = d,
        expand = expand,border = border, ltheta = ltheta, lphi = lphi, 
        shade = shade, box = box, axes = axes,
        ticktype = ticktype, nticks=nticks) 
}
# rotate graph with theta. Default: theta: 55, phi: 30

# These variables will be obtained from the gui slider, I guess
# this is the dummy variable to try the function out
mu1 <- 0 # expected value of x
mu2 <- 0.5 # expected value of y
sig1 <- 0.5 # variance of x
sig2 <- 2 # variance of y
rho <- 0.5 # corr(x, y)

# These additional variables maybe will be obtained from the 'random data generation', I guess
# Some additional variables for x-axis and y-axis
xm <- -3
xp <- 3
ym <- -3
yp <- 3
#+/- 3 simpangan baku akan berkisar 99.7%,
xm <- mu1 - 3 * sig1 
xp <- mu1 + 3 * sig1
ym <- mu2 - 3 * sig2
yp <- mu2 + 3 * sig2

x <- seq(xm, xp, length= as.integer((xp + abs(xm)) * 10)) # vector series x
y <- seq(ym, yp, length= as.integer((yp + abs(ym)) * 10)) # vector series y
co <- c('yellow','white')
co
BiVPers(x,y,co)
BiVPers(x,y)
BiVPers(x,y,theta = 55, phi = 30, r = 40, d = 0.1,
        expand = 0.5,ltheta = 90, lphi = 180, shade = 0.4,
        ticktype = "detailed", nticks=5)

# if using rnorm

r <- sort(rnorm(1000, mu1, sig1))
n <- sort(rnorm(1000, mu2, sig2))
BiVPers(x,y,theta = 55, phi = 30, r = 40, d = 0.1,
        expand = 0.5,ltheta = 90, lphi = 180, shade = 0.4,
        ticktype = "detailed", nticks=5)

##################################################################################################################
                                                      #TKPERSP
##################################################################################################################
# Edouard Tallent @ TaGoMa.Tech
# September 2012
# This code plots simulated bivariate normal distributions
# Some variable definitions
mu1 <- 0 # expected value of x
mu2 <- 0.5 # expected value of y
sig1 <- 0.5 # variance of x
sig2 <- 2 # variance of y
rho <- 0.5 # corr(x, y)

# Some additional variables for x-axis and y-axis
xm <- -3
xp <- 3
ym <- -3
yp <- 3
#xm <- mu1 - 3 * sig1
#xp <- mu1 + 3 * sig1
#xm <- mu2 - 3 * sig2
#xp <- mu2 + 3 * sig2
x <- seq(xm, xp, length= as.integer((xp + abs(xm)) * 10)) # vector series x
y <- seq(ym, yp, length= as.integer((yp + abs(ym)) * 10)) # vector series y

# Core function
bivariate <- function(x,y){
  term1 <- 1 / (2 * pi * sig1 * sig2 * sqrt(1 - rho^2))
  term2 <- (x - mu1)^2 / sig1^2
  term3 <- -(2 * rho * (x - mu1)*(y - mu2))/(sig1 * sig2)
  term4 <- (y - mu2)^2 / sig2^2
  z <- term2 + term3 + term4
  term5 <- term1 * exp((-z / (2 *(1 - rho^2))))
  return (term5)
}

# Computes the density values
z <- outer(x,y,bivariate)

# Plot
persp(x, y, z, main = "Bivariate Normal Distribution",
      sub = bquote(bold(mu[1])==.(mu1)~", "~sigma[1]==.(sig1)~", "
                   ~mu[2]==.(mu2)~", "~sigma[2]==.(sig2)~", "~rho==.(rho)),
      col="orchid2", theta = 55, phi = 30, r = 40, d = 0.1,
      expand = 0.5,ltheta = 90, lphi = 180, shade = 0.4,
      ticktype = "detailed", nticks=5)
tt <- tktoplevel()
img <- tkrplot(tt, function() persp(x, y, z, main = "Bivariate Normal Distribution",
                         sub = bquote(bold(mu[1])==.(mu1)~", "~sigma[1]==.(sig1)~", "
                                      ~mu[2]==.(mu2)~", "~sigma[2]==.(sig2)~", "~rho==.(rho)),
                         col="orchid2", theta = 55, phi = 30, r = 40, d = 0.1,
                         expand = 0.5,ltheta = 90, lphi = 180, shade = 0.4,
                         ticktype = "detailed", nticks=5))
p <- tkpersp(x,y,z,theta = 55, phi = 30, r = 40, d = 0.1,
             expand = 0.5,ltheta = 90, lphi = 180, shade = 0.4,
             ticktype = "detailed", nticks=5)
tkpack(img)
