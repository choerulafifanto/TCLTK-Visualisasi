##
# Tugas APG - TclTk - Visualisasi
# Komputasi Statistik
# These codes create contour plots for simulated bivariate normal distributions

#Function to create contour
BiVContour <- function(generate.data){
  ## Default S3 method:
  co <- c('white','purple') 
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
  image(x, y, z,col=rainbow(15,start=3/6,end=4/6),sub = bquote(bold(mu[1])==.(mu1)~", "~sigma[1]==.(sig1)~", "
                                                               ~mu[2]==.(mu2)~", "~sigma[2]==.(sig2)~", "~rho==.(rho)))
  contour(x, y, z, nlevels = 15,  
          drawlabels = TRUE, 
          method = "flattest", 
          axes = TRUE,
          col = color[facetcl], 
          lty = par("lty"), lwd = par("lwd"), add = T
          )
}
