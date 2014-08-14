make:
	python imageIO.py
	gnome-open final_converted.jpg
	gnome-open small_image.jpg
clean:
	rm *~ 
	rm .swp
im:
	python generate_images.py
