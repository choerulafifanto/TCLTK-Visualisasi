##
# Tugas APG - TclTk - Visualisasi
# Komputasi Statistik

# Multivariate_using_R pg. 31
# Edouard Tallent @ TaGoMa.Tech
# September 2012
# This code plots simulated bivariate normal distributions

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
BiVPers <- function(generate.data){
  ## Default S3 method:
  # Computes the density values
  z<-generate.data$z
  x<-generate.data$x
  y<-generate.data$y
  mu1<-generate.data$mu1
  mu2<-generate.data$mu2
  sig1<-generate.data$sig1
  sig2<-generate.data$sig2
  rho<-generate.data$rho
  co <- c('white','purple') 
  f <- facetcol(co,z)
  persp(x, y, z, 
        sub = bquote(bold(mu[1])==.(mu1)~", "~sigma[1]==.(sig1)~", "
                     ~mu[2]==.(mu2)~", "~sigma[2]==.(sig2)~", "~rho==.(rho)),
        col=color[facetcl], theta = 55, phi = 30, r = 40, d = 0.1,
        expand = 0.5,ltheta = 90, lphi = 180, shade = .4,
        ticktype = "detailed", nticks=5)
}
# rotate graph with theta. Default: theta: 55, phi: 30


