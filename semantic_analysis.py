from Tkinter import * #tkinter module
import subprocess as s #subprocess
from tkFileDialog import askopenfilename #file dialog
import os
import main as semantic_analyzer

global_output_filename = "C:\\Users\\Aditya\\Desktop\\13CS306_D\\sample_output_example.txt" # Path of the intermediate file name for storing the processed output
global_symbol_filename = "C:\\Users\\Aditya\\Desktop\\13CS306_D\\symbol_table.txt"

# The path for the python program
file_name = "C:\\Users\\Aditya\\Desktop\\13CS306_D\\main.py"
f = open(file_name, 'r')
# print(f.read())

#creating a main frame for 3-address gui input field-----------
root = Tk() #root variable which creates tkinter module object
gframe = Frame(root,name = "3-Address Code Generation") #Frame object for main window
gframe.grid(row = 0, column = 0 ) #Align the window on monitor screen
gtext = Text(gframe,height = "20", bd = "10") #Create text dialog to import text into it
gscroll = Scrollbar(gframe) #Create a  scrollbar to scroll text up and down as required
gscroll.pack(side=RIGHT, fill=Y) #Position scrollbar to the right of the textbox
gtext.pack(side=LEFT, fill=Y) #Position textbox to the left of the Scrollbox and the left of the Frame window
gscroll.config(command=gtext.yview) #Scroll text up and down as in when scroll bar is pressed 
gtext.config(yscrollcommand=gscroll.set) #Move text screen up and down based on scroll command
gtext.pack(expand = True)

name=None #global variable name for input file path

def callback():  # callback() function 
    global name #calling name in function callback()
    name = askopenfilename() #open file explorer to select file

"""def script(): #script() function:
    gtext.delete(1.0,END) #clear text area in textbox
    s.call("./makefile") #call script file makefile to perform UNIX scripting commands
    f = open("tcode",'r') #open output file tcode of makefile in python
    text = f.read() #read entire file into string text
    gtext.insert(END,text) #display this string by inserting it on the text dialog in the output
    """
def script(): # Runs the python program and provides the C Program as input
	global name
	print('The name inside script method is', name)
	gtext.delete(1.0, END)
	if name is None:
		gtext.insert(END, "Please enter the C file for reading first")
	else:
		gtext.delete(1.0, END)
		semantic_analyzer.called_main(name, global_output_filename)
		f_open = open(global_output_filename, 'r')
		readOutput = f_open.read()
		for line in readOutput:
			gtext.insert(END, line)
		f_open.close()

def q_table(): #script() function:
    gtext.delete(1.0,END) #clear text area in textbox
    s.call("./quadfile") #call script file makefile to perform UNIX scripting commands
    f = open("tcode",'r')
    gtext.insert(END,'op\t|arg1\t|arg2\t|result\t|')
    gtext.insert(END,'\n')
    gtext.insert(END,'--------------------------------')
    gtext.insert(END,'\n')
    for line in f:
    	line = line.strip()
    	rows = line.split('|')
	st =""
    	for x in range(len(rows)):
		st+=rows[x]+"\t|"
    	gtext.insert(END,st)
	gtext.insert(END,'\n')
     #display this string by inserting it on the text dialog in the output

def disscreen(): #disscreen function:
    gtext.delete(1.0, END)
    if name is None:
        gtext.insert(END, "Please provide the file path for C Program first")
    f = open(name,'r') #open global file path name as f in python
    quote = f.read() #read the entire file into the string text
    gtext.insert(END, quote)
    f.seek(0,0)
    f1 = open("test",'w')
    for line in f:
        f1.writelines(line)
    f.close()
    f1.close() #display this string by inserting it on the text dialog in the output

def symbolTableDisplay(): #displays the symbol table
	gtext.delete(1.0, END)
	if name is None:
		print("Please enter the file for reading the C Program")
		gtext.insert(END, "Please enter the file for reading the C Program first")
	else:
		f_symbol = open(global_symbol_filename, 'r')
		f_read_all = f_symbol.read()
		for line in f_read_all:
			gtext.insert(END, line)
		f_symbol.close()
	
def displayProgram(): # displays the python program
	gtext.delete(1.0, END)
	f = open(file_name, 'r')
	quote= f.read()
	gtext.insert(END, quote)
	f.seek(0, 0)
	f2 = open("open", 'w')
	for line in f:
		f2.writelines(line)
	f.close()
	f2.close() # Display the string

# a new frame for buttons (3)------------------------------

bframe = Frame(root) #frame for the supporting buttons
bframe.grid(row = 0, column = 1) #Align the frame adjacent to text dialog

errmsg = 'Error!'

displayProgram= Button(bframe, width="50", text="Display the Python Program", command = displayProgram) # Display the python file program
displayProgram.pack(fill=X)
displayProgram.grid(row=0, column=0)# position of the 0th button in bframe

fileButton = Button(bframe, width = "50",text = 'Open the File for Reading', command=callback) #fileButton defines bframe frame and name as 'File Open'. On clicking it calls callback() function
# fileButton.pack(fill=X) 
fileButton.grid(row=1,column=0) #position button first in the bframe

displayButton = Button(bframe, width = "50", text = "Display the C Program",command=disscreen) #fileButton defines bframe frame and name as 'Display Input'. On clicking it calls callback() function
displayButton.grid(row=2,column=0) #position button second in the bframe

ExportButton = Button(bframe, width = "50" , text ="Run Semantic Analyzer", command= script) #fileButton defines bframe frame and name as 'Runs the semantic analyzer'. On clicking it calls callback() function
ExportButton.grid(row = 3, column = 0) #position button third in the bframe

SymbolButton = Button(bframe, width = "50", text = "Display global symbol table", command = symbolTableDisplay) # displays the symbol table
SymbolButton.grid(row = 4, column = 0)

exitButton = Button(bframe, width = "50" , text ="EXIT", command=root.destroy) #fileButton defines bframe frame and name as 'File Open'. On clicking it exits the frame windows and program comes to an end
exitButton.grid(row = 5, column = 0) #position button fourth in the bframe


root.mainloop() # execute the main frame window to display on screen
