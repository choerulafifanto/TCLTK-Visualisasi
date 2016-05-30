#another scrollable--unused
##tes file for scrollable
tt  <- tktoplevel()
# textbox with scroll bars
textbox <- tk2frame(tt)
txt <- tktext(textbox, bg="white", font="courier", wrap="word", yscrollcommand=function(...)tkset(scr,...))
scr <- tkscrollbar(textbox, repeatinterval=5, command=function(...) tkyview(txt,...))

tkmark.set(txt,"insert","0.0")

# Set up the geometry for the stuff inside the "textbox" frame.

# The text and scrollbar widgets live on the same row.
tkgrid(txt, scr)

# The text widget should stick to all four sides of its parcel.
tkgrid.configure(txt, sticky="nsew")

# The scrollbar should stick to the top and bottom of its parcel, it need not stick to the
# left and right.
tkgrid.configure(scr, sticky="ns")

# When the window is resized, we want surplus space to be allocated to the text widget,
# which is in the top left corner of this frame.
tkgrid.columnconfigure(textbox,0,weight=1)
tkgrid.rowconfigure(textbox,0,weight=1)

# status bar and size grip
statusText <- tclVar("")
l <- tk2label(tt, textvariable=statusText,relief="sunken")
sg <- ttksizegrip(tt)

# Set up the geometry for the stuff inside the "tt" window.

# First row is just the textbox frame...
tkgrid(textbox)

# Second row is the status label and the resize gadget
tkgrid(l, sg)

# The textbox widget should span 2 colums, and stick to all four sides of its parcel.
tkgrid.configure(textbox,columnspan=2,sticky="nsew")

# The status label should stick to all four sides of its parcel too
tkgrid.configure(l,sticky="nsew")

# The resize gadget should only stick to the bottom right of its parcel
tkgrid.configure(sg,sticky="se")

# When the window is resized, we want surplus space to go to the textbox frame (and from there
# to the text widget itself, which it will do thanks to the grid weights we set up above).  The
# textbox frame is in the top left corner of its parent window.
tkgrid.columnconfigure(tt,0,weight=1)
tkgrid.rowconfigure(tt,0,weight=1)