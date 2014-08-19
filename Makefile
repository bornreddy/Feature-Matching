make:
	python imageIO.py
	open final_converted.jpg
	open paw.jpg
clean:
	rm *~ 
	rm .swp
im:
	python generate_images.py
