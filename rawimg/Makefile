# Smaller images
D1=../www/img
WIDTH=700

# Larger images
D2=../www/imginfo

all: ${D1}/mn.png ${D2}/mnopq.html ${D2}/mnopq.png \
		${D1}/bridge.png ${D2}/bridge.html ${D2}/bridge.png \
		${D1}/redbug.png ${D2}/redbug.html ${D2}/redbug.png \
		${D1}/lightbulb.png ${D2}/lightbulb.html ${D2}/lightbulb.png \
		${D1}/lightning.png ${D2}/lightning.html ${D2}/lightning.png \
		${D1}/arches.png ${D2}/arches.html ${D2}/arches.png \
		${D1}/carnival.png ${D2}/carnival.html ${D2}/carnival.png \
		${D1}/robots.png ${D2}/robots.html ${D2}/robots.png \
		${D1}/tomato.png ${D2}/tomato.html ${D2}/tomato.png \
		${D1}/start_server.png \
		${D1}/oop.png \
		${D1}/cqrs.png \
		${D1}/add_a_name.png

./mn.jpg: mnopq.jpg
	convert -modulate 110 -level 10% -crop 1250x880+0+820 $? $@

${D1}/oop.png: oop.png
	convert -interlace NONE -resize ${WIDTH} -border 6x6 -bordercolor black  $? $@
${D1}/cqrs.png: cqrs.png
	convert -interlace NONE -resize ${WIDTH} -border 6x6 -bordercolor black  $? $@
${D1}/start_server.png: start_server.png
	convert -interlace NONE -resize ${WIDTH} -border 6x6 -bordercolor black  $? $@
${D1}/add_a_name.png: add_a_name.png
	convert -interlace NONE -resize ${WIDTH} -border 6x6 -bordercolor black  $? $@
${D1}/%.png: %.jpg
	convert -interlace NONE -resize ${WIDTH} -border 6x6 -bordercolor black  $? $@

${D2}/%.html: info.% template.mustache
	mkdir -p ${D2}
	./html.sh $@ > $@

${D2}/%.png: %.jpg
	mkdir -p ${D2}
	convert -interlace NONE -resize 1280 -border 8x8 -bordercolor black  $? $@

#------------------------------------------------------------------------------
#
#                     D O W N L O A D   R A W   I M A G E S 
#
#------------------------------------------------------------------------------

lightning.jpg:
	curl -L https://unsplash.com/photos/vmvlzJz1lHg/download -o t
	mv t $@
bridge.jpg:
	curl -L https://unsplash.com/photos/eOpewngf68w/download -o t
	mv t $@
lightbulb.jpg:
	curl -L https://unsplash.com/photos/c3Nu56OdLl0/download -o t
	mv t $@
mnopq.jpg:
	curl -L https://unsplash.com/photos/g2QOdjF0h9c/download -o t
	mv t $@
redbug.jpg:
	curl -L https://unsplash.com/photos/fAMG3PDk830/download -o t
	mv t $@
arches.jpg:
	curl -L https://unsplash.com/photos/JoSbjuOLBz8/download -o t
	mv t $@
carnival.jpg:
	curl -L https://unsplash.com/photos/J1bAgl8E8uQ/download -o t
	mv t $@
robots.jpg:
	curl -L https://farm8.staticflickr.com/7279/7408451314_0bb1cca4c2_o_d.jpg -o t
	mv t $@
tomato.jpg:
	curl -L https://unsplash.com/photos/uVPDAwgqLXY/download -o t
	mv t $@

