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
		print(userInput)
		if plmode:
			if userInput.startswith("parse"):
				userInput = userInput[7:][:-5]
				print(parse(userInput.split(',')))
			else:
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
	toReturn = list(prolog.query("parse(" + line +",X)"))[0][X]
	return toReturn
chat()
