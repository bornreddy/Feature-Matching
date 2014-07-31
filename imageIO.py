import scipy as sp
from PIL import Image
import os

def writeSMI(filename="sample_image.jpg"):
	im = Image.open(filename)
	im = sp.array(im.convert('L'))
	im = im.tolist()
	myString = '['
	for r,row in enumerate(im):
		if r == 0:
			myString+='['
		else:
			myString+=', ['
		for v,val in enumerate(row):
			if v < len(row)-1:
				myString+=str(val)+', '
			else:
				myString+=str(val)
		myString+=']'
	myString+=']'
 
	with open('sample_image.smi','wb') as outFile: 
		outFile.write(myString)		

def readSMO(filename="sample_image.smo"):
	im = Image.open(filename)
	
	



if __name__=='__main__':
	writeSMI() 
	os.system("./harris")
	

