# This program is written in Python 3
from pyswip import Prolog

prolog = Prolog()
prolog.consult("project.pl")

# ===========================================================
# Main loop:
# 1. Repeat "input-response" cycle until input starts with "bye"
#    Each "input-response" cycle consists of:
# 		1.1 Reading an input string and convert it to a tokenized list
# 		1.2 Processing tokenized list
# ===========================================================
def chat():
    while(True):
        userInput = readinput()
        if userInput[0].startswith("bye"):
        	break
        process(userInput)

# ===========================================================
# Read input:
# 1. Read a string from keyboard. 
# 3. Convert the string to lower case.
# 4. Tokenize (based on spaces).
# ===========================================================
def readinput():
    lineInput = input()
    lineInput = lineInput.lower()
    lineInput = lineInput.split()
    return lineInput
 
def process(line):
	print(line)
	parsedLine = parse(line)
	print(parsedLine)
	
def parse(line):
	toReturn = []
	for word in line:
		toPrint = list(prolog.query("tag(" + word + ",Y,Z)"))
		toReturn.append([toPrint[0]["Y"],toPrint[0]["Z"]])
	return toReturn
chat()
