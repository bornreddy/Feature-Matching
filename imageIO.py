import scipy as sp
from scipy import misc
from PIL import Image
import math
import os

def makeSquare(image):
	'''For images that aren't square, this funtion will identify the closest power of 2 
	to the smaller dimension and decide whether to crop or pad. If it needs to be padded, 
	this is done in harris.hs'''
 	dim = min(sp.shape(image)[0],sp.shape(image)[1])
	potential_crop = int(2**math.floor(math.log(dim,2)))
	if potential_crop**2 >= 0.9*sp.shape(image)[0]*sp.shape(image)[1]:
		return sp.misc.imresize(image,(potential_crop,potential_crop))

	return image

def writeSMI(filename="paw.jpg"):
	im = Image.open(filename)
	im = makeSquare(sp.array(im.convert('L')))
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

	for elem in stringArray:
		if elem[0] == '[':	
			tempList = [int(sp.around(float(elem[1:])))]
		elif elem[-1] == ']':
			imageList.append(tempList)
		elif elem == '\n':
			pass
		else:
			tempList.append(int(sp.around(float(elem))))

	imageList = sp.array(imageList)
	misc.imsave("final_converted.jpg",imageList)
        
        


def main():
	writeSMI() 
	os.system("ghc harris.hs")
	os.system("./harris")
	readSMO()

if __name__=='__main__':
	main()
