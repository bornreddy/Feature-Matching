import scipy as sp
from scipy import misc
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
	imageList = []
	tempList = []
	with open(filename,'rb') as inFile:
		stringArray = inFile.read().split(",")
	
	#import pdb; pdb.set_trace()

	for char in stringArray:
		if char[0] == '[':	
			tempList = [int(sp.around(float(char[1:])))]
		elif char[-1] == ']':
			imageList.append(tempList)
		elif char == '\n':
			pass
		else:
			tempList.append(int(sp.around(float(char))))

	imageList = sp.array(imageList)
	misc.imsave("final_converted.jpg",imageList)
	


def main():
	#writeSMI() 
	writeSMI("notredame.jpg")
	os.system("./harris")
	readSMO()

if __name__=='__main__':
	main()
