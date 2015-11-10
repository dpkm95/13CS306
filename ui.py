from tkinter import *

# define global variables for window dimensions
window_length = "600"
window_width = "600"

def makeWindow () :
    global nameVar, phoneVar, select
    win = Tk()
    win.geometry(window_length+"x"+window_width)
    win.title("CD Project - Semantic Analysis of a Function")

    frame1 = Frame(win)
    frame1.pack()

    Label(frame1, text="Enter C Program").grid(row=0, column=0, sticky=W)
    nameVar = StringVar()
    name = Entry(frame1, textvariable=nameVar)
    name.grid(row=0, column=1, sticky=W)

    """Label(frame1, text="Phone").grid(row=1, column=0, sticky=W)
    phoneVar= StringVar()
    phone= Entry(frame1, textvariable=phoneVar)
    phone.grid(row=1, column=1, sticky=W)
    """
    
    frame2 = Frame(win)       # Row of buttons
    frame2.pack()
    b1 = Button(frame2,text="Perform Semantic Analysis",command=print("Hello Button1"))
    b2 = Button(frame2,text="Load Semantic Analyzer Pg",command=print("Hello Button2"))
    ## b3 = Button(frame2,text="Delete",command=print("Hello Button3"))
    ## b4 = Button(frame2,text=" Load ",command=print("Hello Button4"))
    b1.pack(side=LEFT); b2.pack(side=LEFT)
    ## b3.pack(side=LEFT); b4.pack(side=LEFT)

    frame3 = Frame(win)
    frame3.pack()
    scroll = Scrollbar(frame3, orient=VERTICAL)
    select = Listbox(frame3, yscrollcommand=scroll.set, height=6)
    select.insert(END, "Hello World")
    scroll.config (command=select.yview)
    scroll.pack(side=RIGHT, fill=Y)
    select.pack(side=LEFT,  fill=BOTH, expand=1)
    return win

if __name__=="__main__":

    win = makeWindow()
    # setSelect ()
    win.mainloop()