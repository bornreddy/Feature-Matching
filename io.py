import scipy as sp
import Image
import os

def image_IO(filename):

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
 
	with open('imlist.txt','wb') as outFile: 
		outFile.write(myString)		
	

if __name__=='__main__':
	filename="notredame.jpg"
	image_IO(filename) 
		
