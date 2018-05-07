# This program is written in Python 3
from pyswip import Prolog

prolog = Prolog()
prolog.consult("project.pl")

plmode = False

# ===========================================================
# Main loop:
# 1. Repeat "input-response" cycle until input starts with "bye"
#    Each "input-response" cycle consists of:
# 		1.1 Reading an input string and convert it to a tokenized list
# 		1.2 Processing tokenized list
# ===========================================================
def chat():
	global plmode
	while(True):
		if plmode:
			userInput = input()
		else:
			userInput = readinput()
		if userInput[0].startswith("bye"):
			print("Bye!")
			break
		elif userInput[0].startswith("plmode"):
			plmode = True
			continue
		elif userInput[0].startswith("plend"):
			plmode = False
		print(userInput[0])
		if plmode:
			print(list(prolog.query(userInput)))
		else:
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
		toPutIn = list(prolog.query("uninflect0(" + word +",Y)"))
		toPrint = list(prolog.query("lex(X," + toPutIn[0]["Y"] +")"))
		toReturn.append([toPrint[0]["X"]])
	return toReturn
chat()
