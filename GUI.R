##
# Tugas APG - TclTk - Visualisasi
# Komputasi Statistik
require (tcltk)
welcomeDialogue <- function(){
  
  tt <- tktoplevel()
  tkwm.title(tt,"Visualisasi")
  tes<-"This is tes page gui for visualisasi."
  tkgrid(tklabel(tt,text=tes), pady = 10)
  
}

welcomeDialogue()
