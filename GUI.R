##
# Tugas APG - TclTk - Visualisasi
# Komputasi Statistik
# 4KS1 - 4 KS2
require (tcltk)
require (tcltk2)
require (tkrplot)
BivariateGUI <- function(){
  
  tt <- tktoplevel()
  tktitle(tt)<-"Bivariate Visualization"
  fontTitle<- tkfont.create(family = "Gentium Book Basic", size = 12,
                            weight = "bold")
  fontCommand<- tkfont.create(family = "Gentium Book Basic", size = 10,
                              slant="italic")
  tkgrid(
    tk2label(
      tt, text = "Bivariate Normal Simulation", justify = "center",
      font=fontTitle
    ),
    padx = 10, pady = c(20, 5),row=0,column=0
  )
  # Frame for Plotting
  plot.frame<-tk2frame(tt,borderwidth=2,relief="flat")
  tkgrid(plot.frame,padx=0,pady=c(0,0),row=1,column=0,sticky="w")
  
  # Frame for Input 
  input.frame<-tk2frame(tt,borderwidth=2,relief="flat")
  tkgrid(input.frame,padx=20,pady=c(0,20),row=2,column=0,sticky="w")
  tkgrid(
    tk2label(
      input.frame,text="Parameter Value",font=fontCommand
    ),
    padx= 5,pady=c(5,5),row=0,column=0,sticky="w"
  )
  
  #Slider Miu
  slidermiuX <- tclVar("0.5")
  slidermiuY <- tclVar("0.5")
  
  miu<<-c(round(as.double(tclvalue(slidermiuX)),2),
         round(as.double(tclvalue(slidermiuY)),2))
  
  labelmiux <- tk2label(input.frame,
                        text = "miu x= 0.5",justify="left") 
  tkgrid(labelmiux, padx = 5, pady = c(5, 5),row=1,column=0,sticky="w")
  labelmiuY <- tk2label(input.frame,
                        text = "miu y= 0.5",justify="left") 
  tkgrid(labelmiuY, padx = 5, pady = c(5, 5),row=1,column=5,sticky="w")
  
  onChangeX <- function(...) {
    value <- round(as.double(tclvalue(slidermiuX)),2)
    label <- sprintf("miu x= %s", value)
    tkconfigure(labelmiux, text = label)
    miu<<-c(value,round(as.double(tclvalue(slidermiuY)),2))
    print(miu)
  }
  onChangeY <- function(...) {
    value <- round(as.double(tclvalue(slidermiuY)),2)
    label <- sprintf("miu y= %s", value)
    tkconfigure(labelmiuY, text = label)
    miu<<-c(round(as.double(tclvalue(slidermiuX)),2),
           value)
    print(miu)
  }
  
  miux.slider <- tk2scale(input.frame,from=-3,to=3,
                          variable = slidermiuX, 
                          orient = "horizontal", length = 100,
                          command = onChangeX)
  tkgrid(miux.slider, padx = 5, pady = c(5, 5),row=1,column=1,sticky="w")
  
  miuY.slider <- tk2scale(input.frame, from = -3, to =3,
                          variable = slidermiuY, 
                          orient = "horizontal", length = 100,
                          command = onChangeY)
  tkgrid(miuY.slider, padx = 5, pady = c(5, 5),row=1,column=6,sticky="w")
  
  #Input Variance
  varx <- tclVar("2.1")
  vary <- tclVar("2.1")
  
  varians<<-c(round(as.double(tclvalue(varx)),2),
         round(as.double(tclvalue(vary)),2))
  labelvarx <- tk2label(input.frame,
                        text = "var x= 2.1",justify="left") 
  tkgrid(labelvarx, padx = 5, pady = c(5, 5),row=1,column=2,sticky="w")
  labelvary <- tk2label(input.frame,
                        text = "var y= 2.1",justify="left") 
  tkgrid(labelvary, padx = 5, pady = c(5, 5),row=1,column=7,sticky="w")
  
  onChangeVarX <- function(...) {
    value <- round(as.double(tclvalue(varx)),2)
    label <- sprintf("var x= %s", value)
    tkconfigure(labelvarx, text = label)
    varians<<-c(round(as.double(tclvalue(varx)),2),
               round(as.double(tclvalue(vary)),2))
    print(varians)
  }
  onChangeVarY <- function(...) {
    value <- round(as.double(tclvalue(vary)),2)
    label <- sprintf("var y= %s", value)
    tkconfigure(labelvary, text = label)
    varians<<-c(round(as.double(tclvalue(varx)),2),
               round(as.double(tclvalue(vary)),2))
    print(varians)
  }
  
  var.x.slider <- tk2scale(input.frame,from=0.01,to=2.5,
                          variable = varx, 
                          orient = "horizontal", length = 100,
                          command = onChangeVarX)
  tkgrid(var.x.slider, padx = 5, pady = c(5, 5),row=1,column=3,sticky="w")
  
  var.y.slider <- tk2scale(input.frame, from = 0.01, to =2.5,
                          variable = vary, 
                          orient = "horizontal", length = 100,
                          command = onChangeVarY)
  tkgrid(var.y.slider, padx = 5, pady = c(5, 5),row=1,column=8,sticky="w")
  
  #Input CovVariance
  corxy <- tclVar(".5")
  correlation<<-round(as.double(tclvalue(corxy)),2)
  labelcorxy <- tk2label(input.frame,
                        text = "cor xy= 0.5",justify="left") 
  tkgrid(labelcorxy, padx = 5, pady = c(5, 5),row=1,column=9,sticky="w")
  onChangeCorXY <- function(...) {
    value <- round(as.double(tclvalue(corxy)),2)
    label <- sprintf("cor xy= %s", value)
    tkconfigure(labelcorxy, text = label)
    correlation<<-round(as.double(tclvalue(corxy)),2)
    print(correlation)
  }
  cor.xy.slider <- tk2scale(input.frame,from=-.99,to=.99,
                           variable = corxy, 
                           orient = "horizontal", length = 100,
                           command = onChangeCorXY)
  tkgrid(cor.xy.slider, padx = 5, pady = c(5, 5),row=1,column=10,sticky="w")
  
  # Button dan Progress Bar
  # Generate Data
  generate.data <- Generate.data(mu1=miu[1], mu2=miu[2], sig1=varians[1], 
                                 sig2=varians[2], rho=correlation)
  
  # Testing Plot
  plotBivaPers <- function() {
   BiVPers(generate.data)
   
  }
  plotBivaCon <- function() {
    mu1<-generate.data$mu1
    mu2<-generate.data$mu2
    sig1<-generate.data$sig1
    sig2<-generate.data$sig2
    rho<-generate.data$rho
    contour(generate.data$x,generate.data$y,generate.data$z,
            sub = bquote(bold(mu[1])==.(mu1)~", "~sigma[1]==.(sig1)~", "
                         ~mu[2]==.(mu2)~", "~sigma[2]==.(sig2)~", "~rho==.(rho)))
  }
  
  persp.plot<-tkrplot(plot.frame,plotBivaPers,1.25,1.25)
  cont.plot<-tkrplot(plot.frame,plotBivaCon,1.25,1.25)
  tkgrid(persp.plot,cont.plot,row=0,padx=5)
  }
BivariateGUI()
