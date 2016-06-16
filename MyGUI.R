##
# Tugas APG - TclTk - Visualisasi
# Komputasi Statistik
# 4KS1 - 4 KS2
require (tcltk)
require (tcltk2)
require (tkrplot)
source("Generate-data.R")
source("Perspective.R")
source("Contour.R")
BivariateGUI <- function(){
  #create main window
  tt <- tktoplevel(bg="white", width=1080, height=650)
  #tkpack.propagate(tt,FALSE)
  tktitle(tt)<-"Bivariate Visualization"
  
  #menu
  tt$env$menu <- tk2menu(tt)           # Create a menu
  tkconfigure(tt, menu = tt$env$menu)  # Add it to the 'tt' window
  tt$env$menuFile <- tk2menu(tt$env$menu, tearoff = FALSE)
  tkadd(tt$env$menuFile, "command", label = "Close",  command = function() tkdestroy(tt))
  tkadd(tt$env$menu, "cascade", label = "File", menu = tt$env$menuFile)
  tt$env$menuAbout <- tk2menu(tt$env$menu, tearoff = FALSE)
  tkadd(tt$env$menu, "command", label = "About", #menu = tt$env$menuAbout,
        command =function() tkmessageBox(title = "About Program",
                                         message = "Tugas Analisis Peubah Ganda 4KS1-4KS2 Tahun 2016\nAchmad Fauzi Bagus F\nChoerul Afifanto\nDwi Puspita Sari\nEko Wahyu L\nImmawan Mujahid\nKhairanisa Haque\nKurnia Fauzi\nM Abdul Muhshi\nM Arif Rosyanto\nMoh Syaiful HR\nM Fikri Firmansyah\nNur Azizah\nRistika Nugraha\nShintia Afifah\nTegar Dani Pratama ", icon = "info", type = "ok"))
  #param
  varx <- tclVar("2.1")
  vary <- tclVar("2.1")
  
  varians<<-c(round(as.double(tclvalue(varx)),2),
              round(as.double(tclvalue(vary)),2))
  
  #Slider Miu
  slidermiuX <- tclVar("0.5")
  slidermiuY <- tclVar("0.5")
  
  miu<<-c(round(as.double(tclvalue(slidermiuX)),2),
          round(as.double(tclvalue(slidermiuY)),2))
  
  #Input CovVariance
  corxy <- tclVar(".5")
  correlation<<-round(as.double(tclvalue(corxy)),2)
  
  # Default Testing Plot
  plotBivaPers <- function() {
    generate.data <- Generate.data(mu1=miu[1], mu2=miu[2], sig1=varians[1], 
                                   sig2=varians[2], rho=correlation)
    BiVPers(generate.data)
    
  }
  plotBivaCon <- function() {
    generate.data <- Generate.data(mu1=miu[1], mu2=miu[2], sig1=varians[1], 
                                   sig2=varians[2], rho=correlation)
    BiVContour(generate.data)
  }
  
  # Frame for Input 
  side.frame<-tkframe(tt,borderwidth=2,relief="flat",bg="#252D3A", height=200, width=200)
  #tkpack.propagate(side.frame,FALSE)
  
  
  #adding line
  
  #frame title
  title.frame <- tkframe(tt, bg='white')
  
  
  #frame plot
  plot.frame<-tkframe(tt,bg="#E6EBEF",relief="flat", height=480, width=900)
  #tkpack.propagate(plot.frame,FALSE)
  
  plotname<-tkframe(plot.frame,bg="white",width=80,height=20)
  
  fontTitle<- tkfont.create(family = "Segoe UI",size = 18,weight = "bold")
  fontSubTitle<- tkfont.create(family = "Calibri", size = 10)
  fontplot<- tkfont.create(family = "Segoe UI",size = 14,weight = "bold")
  fontSubplot<- tkfont.create(family = "Calibri", size = 12,weight='bold')
  
  #isi title.frame
  plotlabel<-tklabel(plotname, text = "Plot", 
                     justify = "left",font=fontplot, bg='white',fg='#5D5D5F')
  
  plotlabel2<-tklabel(plotname, text = "(Perspective/Contour)", 
                      justify = "left",font=fontSubTitle, bg='white',fg='#5D5D5F')
  
  titleLabel<-tklabel(title.frame, text = "Bivariate Normal Simulation", 
                      justify = "left",font=fontTitle, bg='white',fg='#5D5D5F')
  
  #isi plot.prame
  persp.plot<-tkrplot(plot.frame,plotBivaPers,1.1,1.1)
  cont.plot<-tkrplot(plot.frame,plotBivaCon,1.1,1.1)
  ##isi input.frame
  
  birutua <-'#252D3A'
  biruTerang <-'#5AB4CE'
  
  #logo
  frame1 <- tkframe(side.frame,height=60, width=30, bg='#E4F1FE')
  frame2 <- tkframe(frame1, height=50, width=30, bg=birutua)
  logoLabel1 <- tklabel(frame2, text="Team R-Tcl/Tk",justify='center',
                        font=(tkfont.create(family = "Segoe Ui", size = 16,weight='bold')),
                        bg=birutua,fg='#E4F1FE')
  logoLabel2 <- tklabel(frame2, text="KS 54",justify='center',
                        font=(tkfont.create(family = "Calibri", size = 10)),
                        bg=birutua,fg='#E4F1FE')
  
  ##isi input.frame                    
  #parameter input
  input.frame <- tkframe(side.frame, width=30, height=50, bg=birutua)
  input.frame2 <- tkframe(input.frame, height=50, width=30, bg=birutua)
  
  fontLabel <-tkfont.create(family = "Calibri", size = 11)
  InputLabel<-tklabel(input.frame2,text="Parameter Input :", 
                      font=fontLabel,bg=birutua,fg=birutua)
  
  
  # Perform
  performed<-function(){
    #changing plot
    tkrreplot(persp.plot)
    tkrreplot(cont.plot)
  }
  
  labelmiux <- tklabel(input.frame2,
                       text = "Mean x= 0.5",justify="left", 
                       font=fontLabel,bg=birutua, fg='white') 
  labelmiuY <- tklabel(input.frame2,
                       text = "Mean y= 0.5",justify="left",
                       font=fontLabel,bg=birutua, fg='white')
  
  onChangeX <- function(...) {
    value <- round(as.double(tclvalue(slidermiuX)),2)
    label <- sprintf("Mean x= %s", value)
    tkconfigure(labelmiux, text = label)
    miu<<-c(value,round(as.double(tclvalue(slidermiuY)),2))
    performed()
    
  }
  onChangeY <- function(...) {
    value <- round(as.double(tclvalue(slidermiuY)),2)
    label <- sprintf("Mean y= %s", value)
    tkconfigure(labelmiuY, text = label)
    miu<<-c(round(as.double(tclvalue(slidermiuX)),2),
            value)
    performed()
  }
  
  miux.slider <- tk2scale(input.frame2,from=-3,to=3,
                          variable = slidermiuX, 
                          orient = "horizontal", length = 150,
                          #sliderlength=20,
                          command = onChangeX
                          #,bg=birutua, fg='white', resolution=.01
  )
  tcl("ttk::style", "configure","TScale",background=birutua,foreground='white')
  
  miuY.slider <- tk2scale(input.frame2, from = -3, to =3,
                          variable = slidermiuY, 
                          orient = "horizontal", length = 150,
                          #sliderlength=20,
                          command = onChangeY
                          #,bg=birutua, fg='white',resolution=.01
  )
  
  
  labelvar.frame <-tkframe(input.frame2,width=40, height=50,
                           bg=birutua)
  
  labelvarx <- tklabel(labelvar.frame,
                       text = "Var x\n2.1", font=fontLabel, fg='white',bg=birutua) 
  labelvary <- tklabel(labelvar.frame,
                       text = "Var y\n2.1", font=fontLabel, fg='white',bg=birutua)
  labelcorxy <- tklabel(labelvar.frame,
                        text = "Cor xy\n0.5", font=fontLabel, fg='white',bg=birutua)
  
  
  onChangeCorXY <- function(...) {
    value <- round(as.double(tclvalue(corxy)),2)
    label <- sprintf("Cor xy\n%s", value)
    tkconfigure(labelcorxy, text = label)
    correlation<<-round(as.double(tclvalue(corxy)),2)
    performed()
  }
  
  onChangeVarX <- function(...) {
    value <- round(as.double(tclvalue(varx)),2)
    label <- sprintf("Var x\n%s", value)
    tkconfigure(labelvarx, text = label)
    varians<<-c(round(as.double(tclvalue(varx)),2),
                round(as.double(tclvalue(vary)),2))
    performed()
  }
  onChangeVarY <- function(...) {
    value <- round(as.double(tclvalue(vary)),2)
    label <- sprintf("Var y\n%s", value)
    tkconfigure(labelvary, text = label)
    varians<<-c(round(as.double(tclvalue(varx)),2),
                round(as.double(tclvalue(vary)),2))
    performed()
  }
  
  
  framevar <- tkframe(input.frame2, width=40, height=50,
                      bg=birutua)
  
  
  var.x.slider <- tk2scale(framevar,from=2.5,to=.01,
                           variable = varx,orient = "vertical", 
                           length = 150,
                           #sliderlength=20,
                           #bg=birutua,fg='white',
                           command=onChangeVarX
                           #,resolution=.01
  )
  
  var.y.slider <- tk2scale(framevar, from = 2.5, to =.01,
                           variable = vary,orient = "vertical", 
                           length = 150,
                           #sliderlength=20, 
                           #bg=birutua,fg='white',
                           command=onChangeVarY
                           #,resolution=.01
  )
  
  cor.xy.slider <- tk2scale(framevar,from=.99,to=-.99,
                            variable = corxy,orient = "vertical",
                            length = 150,
                            #sliderlength=20, bg=birutua,fg='white',
                            command=onChangeCorXY
                            #,resolution=0.01
  )
  
  tkpack(side.frame, side="left",fill="y")
  tkpack(tkframe(tt,bg='#E6EBEF',width=40,height=2),fill='x',side='top')
  tkpack(title.frame, side='top', anchor='ne',fill='both',pady=10)
  tkpack(tkframe(tt,bg='#E6EBEF',width=40,height=4),fill='x',side='top')
  tkpack(plot.frame, side="bottom",fill="both",anchor='se',pady=c(0,20),expand=TRUE)
  tkpack(plotname,side='top',fill='x',padx=8,pady=2)
  tkpack(plotlabel,plotlabel2,side='top',padx=10,anchor='w',fill='x',pady=5)
  tkpack(titleLabel, side='top', padx=10, anchor='w')
  tkpack(tklabel(title.frame, text= "A Visualization of Bivariate Normal Data in Contour and Perspective Plot",
                 justify='left', font=fontSubTitle,bg='white',fg='#A8B0BB'),
         side='top', anchor='w',padx=10)
  tkpack(cont.plot,side="right",padx=c(0,6))
  tkpack(persp.plot,side='left',padx=c(8,0))
  tkpack(frame1, side='top', fill='x')
  tkpack(frame2, fill='both',pady=c(1,1))
  tkpack(logoLabel1,side='top',pady=c(10,0))
  tkpack(logoLabel2,side='top',pady=c(0,10))
  tkpack(input.frame, side='top', anchor='n', fill='both',padx=5,pady=10)
  tkpack(input.frame2, fill='both',padx=2,pady=2)
  tkpack(InputLabel, side='top',anchor='w',pady=c(5,10))
  tkpack(tkframe(input.frame2,bg='#7f8c8d',width=40,height=2),fill='x',side='top')
  tkpack(labelmiux,side='top',anchor='w')
  tkpack(miux.slider,side='top',anchor='w',padx=15,pady=c(0,5))
  tkpack(labelmiuY,side='top',anchor='w')
  tkpack(miuY.slider,side='top',anchor='w',padx=15,pady=c(0,15))
  tkpack(tkframe(input.frame2,bg='#7f8c8d',width=40,height=2),fill='x',side='top')
  tkpack(tkframe(input.frame2,bg='#7f8c8d',width=40,height=2),fill='x',side='top',pady=c(30,0))
  tkpack(labelvar.frame, side='top',fill='both',padx=10)
  tkpack(labelvarx,labelvary,labelcorxy, side='left',padx=7,pady=c(10,5))
  tkpack(var.x.slider,var.y.slider,cor.xy.slider, 
         side='left'
         ,padx=15
         ,pady=c(0,10)
  )
  tkpack(framevar,side='top',padx=5,fill='both')
  tkpack(tkframe(input.frame2,bg='#7f8c8d',width=40,height=2),fill='x',side='top')
  
}
BivariateGUI()

