##
# Tugas APG - TclTk - Visualisasi
# Komputasi Statistik
# These codes create contour plots for simulated bivariate normal distributions

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

#Function to create contour
BiVContour <- function(x,y,z,co,axes,nlevels,method,add,imageOnly){
  ## Default S3 method:
  if(missing(co)){ co <- c('blue','green') }
  if(missing(axes)){ axes = TRUE }
  if(missing(nlevels)){ nlevels = 15 }
  if(missing(method)){ method = "flattest" }
  if(missing(add)){ add = TRUE }
  if(missing(imageOnly)){ imageOnly = FALSE }
  
  f <- facetcol(co,z)
  image(x, y, z)
  contour(x, y, z, nlevels = nlevels, labels = NULL, main = "Bivariate Normal Distribution",
          labcex = 0.6, drawlabels = TRUE, method = "flattest", axes = TRUE, frame.plot = axes,
          col = color[facetcl], lty = par("lty"), lwd = par("lwd"), add = add)

  if(imageOnly){image(x, y, z)}
}

############ FUNCTION TESTING ##############

# Some variables for x-axis and y-axis
xm <- -3
xp <- 3
ym <- -3
yp <- 3

x <- seq(xm, xp, length= as.integer((xp + abs(xm)) * 10)) # vector series x
y <- seq(ym, yp, length= as.integer((yp + abs(ym)) * 10)) # vector series y
# Computes the density values
z <- outer(x,y,bivariate)

BiVContour(x,y,z)
BiVContour(x,y,z,method ="edge",nlevels = 17,add = FALSE)
BiVContour(x,y,z,method ="flat",nlevels = 7,imageOnly = TRUE)

#Other example from http://www.inside-r.org/r-doc/graphics/contour
x <- -6:16
contour(outer(x, x), method = "edge", vfont = c("sans serif", "plain"))
z <- outer(x, sqrt(abs(x)), FUN = "/")
image(x, y, z)
contour(x, x, z, col = "pink", add = TRUE, method = "edge",
        vfont = c("sans serif", "plain"))
contour(x, x, z, ylim = c(1, 6), method = "simple", labcex = 1)
contour(x, x, z, ylim = c(-6, 6), nlev = 20, lty = 2, method = "simple")
