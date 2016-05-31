#only for test
require(tcltk)
require(tcltk2)
addScrollbars <- function(parent, widget) {
  xscr <- ttkscrollbar(parent, orient = "horizontal",
                       command = function(...) tkxview(widget, ...))
  tkconfigure(widget, xscrollcommand = function(...) tkset(xscr,...))
  
  yscr <- ttkscrollbar(parent, command = function(...) tkyview(widget,...))
  tkconfigure(widget, yscrollcommand = function(...) tkset(yscr,...))
  sg <- ttksizegrip(parent)
  
  ## Pack into a grid, from tkFAQ 10.1
  tkgrid(widget,row = 0,column = 0, sticky = "news")
  tkgrid(xscr,row = 1,column = 0, sticky = "ew")
  tkgrid(yscr,row = 0,column = 1, sticky = "ns")
  
  tkgrid.columnconfigure(parent, 0, weight = 1)
  tkgrid.rowconfigure(parent, 0, weight = 1)
  tkgrid(sg,row=1,column=1,sticky = "se")
  }

scrollable_frame <- function(parent, width=300, height=300) {
  canvas_widget <- 
    tkcanvas(parent,
             borderwidth = 0, highlightthickness = 0,
             width = width, height = height)
  addScrollbars(parent, canvas_widget)
  #
  frame <- ttkframe(canvas_widget, padding = c(0,0,0,0))
  frame_id <- tkcreate(canvas_widget, "window", 0, 0, 
                       anchor = "nw", window = frame)
  tkitemconfigure(canvas_widget, frame_id, width = width)
  ## update scroll region
  tkbind(frame, "<Configure>", function() {  
    bbox <- tcl(canvas_widget, "bbox", "all")
    tcl(canvas_widget, "config", scrollregion = bbox)
  })
  ## adjust "window" width when canvas is resized.
  tkbind(canvas_widget, "<Configure>", function(W) {
    width <- as.numeric(tkwinfo("width", W))
    frame_width <- as.numeric(tkwinfo("width", frame))
    if(frame_width < width)
      tkitemconfigure(canvas_widget, frame_id, width = width)
  })
  return(frame)
}
tt  <- tktoplevel()
#addScrollbars(tt,tt)
frame <- ttkframe(tt)
tkpack(frame, expand = TRUE, fill = "both")
scroll_frame <- scrollable_frame(frame,width =400, height =400 )