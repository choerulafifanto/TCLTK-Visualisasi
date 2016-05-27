##
# Tugas APG - TclTk - Visualisasi
# Komputasi Statistik
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
      tt, text = "Bivariate Normal\nSimulation", justify = "center",
      font=fontTitle
    ),
    padx = 10, pady = c(5, 5),row=0,column=0
  )
  # Frame for Plotting
  plot.frame<-tk2frame(tt,borderwidth=2,relief="flat")
  tkgrid(plot.frame,padx=0,pady=c(0,0),row=1,column=0,sticky="w")
  
  # Frame for Input 
  input.frame<-tk2frame(tt,borderwidth=2,relief="flat")
  tkgrid(input.frame,padx=0,pady=c(0,0),row=2,column=0,sticky="w")
  tkgrid(
    tk2label(
      input.frame,text="Parameter Value",font=fontCommand
    ),
    padx= 5,pady=c(5,5),row=0,column=0,sticky="w"
  )
  #Slider Miu
  slidermiuX <- tclVar("0")
  slidermiuY <- tclVar("0")
  
  miu<-c(as.integer(tclvalue(slidermiuX)),as.integer(tclvalue(slidermiuY)))
  
  labelmiux <- tk2label(input.frame,
                             text = "miu x= 0",justify="left") 
  tkgrid(labelmiux, padx = 5, pady = c(5, 5),row=1,column=0,sticky="w")
  labelmiuY <- tk2label(input.frame,
                        text = "miu y= 0",justify="left") 
  tkgrid(labelmiuY, padx = 5, pady = c(5, 5),row=1,column=3,sticky="w")
  onChangeX <- function(...) {
    value <- as.integer(tclvalue(slidermiuX))
    label <- sprintf("miu x= %s", value)
    tkconfigure(labelmiux, text = label)
    miu<-c(value,as.integer(tclvalue(slidermiuY)))
    print(miu)
  }
  onChangeY <- function(...) {
    value <- as.integer(tclvalue(slidermiuY))
    label <- sprintf("miu y= %s", value)
    tkconfigure(labelmiuY, text = label)
    miu<-c(as.integer(tclvalue(slidermiuX)),value)
    print(miu)
  }
  
  miux.slider <- tk2scale(input.frame, from = -5, to = 5,
                              variable = slidermiuX, 
                              orient = "horizontal", length = 100,
                              command = onChangeX)
  tkgrid(miux.slider, padx = 5, pady = c(5, 5),row=1,column=1,columnspan=2)
  miuY.slider <- tk2scale(input.frame, from = -5, to = 5,
                          variable = slidermiuY, 
                          orient = "horizontal", length = 100,
                          command = onChangeY)
  tkgrid(miuY.slider, padx = 5, pady = c(5, 5),row=1,column=4,columnspan=4)
  
  #Input Variance
  varx <- tclVar("1")
  vary <- tclVar("1")
  covxy <- tclVar("0")
  
  var.X <<- as.integer(tclvalue(varx))
  var.Y <<- as.integer(tclvalue(vary))
  cov.XY <<- as.integer(tclvalue(covxy))
  Sigma<-matrix(c(var.X,cov.XY,cov.XY,var.Y),ncol=2)
  
  varx.entry <-tk2entry(input.frame, width = "10", textvariable = varx)
  vary.entry <-tk2entry(input.frame, width = "10", textvariable = vary)
  covxy.entry <-tk2entry(input.frame, width = "10", textvariable = covxy)
  tkgrid(tk2label(input.frame, 
                  text = "var x", 
                  justify = "left"),
         padx = 5, pady = c(5, 5), row=2,column=0,
         sticky = "w")
  tkgrid(varx.entry, padx = 10, pady = c(5, 5),row=2,column=1,sticky="w")
  tkgrid(tk2label(input.frame, 
                  text = "var y", 
                  justify = "left"),
         padx = 5, pady = c(5, 5), row=2,column=2,
         sticky = "w")
  tkgrid(vary.entry, padx = 10, pady = c(5, 5),row=2,column=3,sticky="w")
  tkgrid(tk2label(input.frame, 
                  text = "cov xy", 
                  justify = "left"),
         padx = 5, pady = c(5, 5), row=2,column=4,
         sticky = "w")
  tkgrid(covxy.entry, padx = 10, pady = c(5, 5),row=2,column=5,sticky="w")
  onEnter.varX<-function(){
    var.X <<- as.integer(tclvalue(varx))
    Sigma<-matrix(c(var.X,cov.XY,cov.XY,var.Y),ncol=2)
    print(Sigma)
  }
  onEnter.varY<-function(){
    var.Y <<- as.integer(tclvalue(vary))
    Sigma<-matrix(c(var.X,cov.XY,cov.XY,var.Y),ncol=2)
    print(Sigma)
  }
  onEnter.covXY<-function(){
    cov.XY <<- as.integer(tclvalue(covxy))
    Sigma<-matrix(c(var.X,cov.XY,cov.XY,var.Y),ncol=2)
    print(Sigma)
  }
  tkbind(varx.entry,"<Return>",onEnter.varX)
  tkbind(vary.entry,"<Return>",onEnter.varY)
  tkbind(covxy.entry,"<Return>",onEnter.covXY)
}
BivariateGUI()
