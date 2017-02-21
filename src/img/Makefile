# Smaller images
D1=../../www/img
# Larger images
D2=../../www/imginfo
WIDTH=700
FULLWIDTH=1200

# 150K --> 32K.  png8 does the trick; see http://stackoverflow.com/a/11408390
256_DEPTH=convert -interlace NONE -resize ${WIDTH} -border 6 -bordercolor black $? png8:$@
FULL_DEPTH=convert -interlace NONE -resize ${WIDTH} -border 6 -bordercolor black $? $@


all: unsplash_images full_images 256_images svg_drawings

# Images used to introduce blog; have an imginfo page.
unsplash_images: \
	${D1}/full/mn.png         ${D2}/mnopq.html      ${D2}/mnopq.png      \
	${D1}/full/service.png    ${D2}/service.html    ${D2}/service.png    \
	${D1}/full/bridge.png     ${D2}/bridge.html     ${D2}/bridge.png     \
	${D1}/full/redbug.png     ${D2}/redbug.html     ${D2}/redbug.png     \
	${D1}/full/lightbulb.png  ${D2}/lightbulb.html  ${D2}/lightbulb.png  \
	${D1}/full/lightning.png  ${D2}/lightning.html  ${D2}/lightning.png  \
	${D1}/full/arches.png     ${D2}/arches.html     ${D2}/arches.png     \
	${D1}/full/carnival.png   ${D2}/carnival.html   ${D2}/carnival.png   \
	${D1}/full/tomato.png     ${D2}/tomato.html     ${D2}/tomato.png     \
	${D1}/full/fire.png       ${D2}/fire.html       ${D2}/fire.png       \
	${D1}/full/nest.png       ${D2}/nest.html       ${D2}/nest.png       \
	${D1}/full/letters.png    ${D2}/letters.html    ${D2}/letters.png    \
	${D1}/full/blackboard.png ${D2}/blackboard.html ${D2}/blackboard.png \
	${D1}/full/rsquo.png      ${D2}/rsquo.html      ${D2}/rsquo.png      \
	${D1}/full/thanku.png     ${D2}/thanku.html     ${D2}/thanku.png     \
	${D1}/full/elm.png        ${D2}/elm.html        ${D2}/elm.png

# Full color images that don't come from unsplash.
full_images: ${D1}/full/headshot.png

${D1}/full/headshot.png: headshot.jpg
	${FULL_DEPTH}

# Normal images that appear lower down in blog entry. 
256_images: \
		${D1}/start_server.png \
		${D1}/add_a_name.png \
                ${D1}/prometheus_chunks.png \
                ${D1}/prometheus_erlang_vm_memory.png \
                ${D1}/prometheus_http_requests.png \
                ${D1}/prometheus_response_time.png \
		${D1}/oop.png \
		${D1}/cqrs.png \
		${D1}/erlang-aagh.png \
		${D1}/javascript-closure.png \
		${D1}/add_a_name.png

svg_drawings: ${D1}/line-wrap-arrow.svg


#------------------------------------------------------------------------------
#
#                        C U S T O M   H A N D L I N G 
#
#------------------------------------------------------------------------------

# Crop a piece out of a bigger image.
./work/mn.jpg: work/mnopq.jpg
	@# geometry: <width>x<height>{+-}<xoffset>{+-}<yoffset>
	convert -modulate 110 -level 10% -crop 1250x880+0+820 $? $@

# Crop grey border off
${D1}/full/elm.png: work/elm.jpg
	${FULL_DEPTH}
work/elm.jpg: work/elm.tiff
	convert -modulate 110 -level 10% -crop 1500x1172+22+44 $? $@

#------------------------------------------------------------------------------
#
#                      S T A N D A R D   H A N D L I N G 
#
#------------------------------------------------------------------------------


${D1}/%.png: %.jpg
	${256_DEPTH}

${D1}/full/%.png: work/%.jpg
	${FULL_DEPTH}

${D1}/%.svg: %.svg
	cp $? $@

${D2}/%.html: info.% template.mustache
	mkdir -p ${D2}
	./html.sh $@ > $@

${D2}/%.png: work/%.jpg
	mkdir -p ${D2}
	convert -interlace NONE -resize 1280 -border 8 -bordercolor black  $? $@

#------------------------------------------------------------------------------
#
#                     D O W N L O A D   R A W   I M A G E S 
#
#------------------------------------------------------------------------------

work/lightning.jpg:
	curl -L https://unsplash.com/photos/vmvlzJz1lHg/download -o t
	mv t $@
work/bridge.jpg:
	curl -L https://unsplash.com/photos/eOpewngf68w/download -o t
	mv t $@
work/lightbulb.jpg:
	curl -L https://unsplash.com/photos/c3Nu56OdLl0/download -o t
	mv t $@
work/mnopq.jpg:
	curl -L https://unsplash.com/photos/g2QOdjF0h9c/download -o t
	mv t $@
work/redbug.jpg:
	curl -L https://unsplash.com/photos/fAMG3PDk830/download -o t
	mv t $@
work/arches.jpg:
	curl -L https://unsplash.com/photos/JoSbjuOLBz8/download -o t
	mv t $@
work/carnival.jpg:
	curl -L https://unsplash.com/photos/J1bAgl8E8uQ/download -o t
	mv t $@
work/tomato.jpg:
	curl -L https://unsplash.com/photos/uVPDAwgqLXY/download -o t
	mv t $@
work/service.jpg:
	curl -L https://unsplash.com/photos/2zLEAIZi92M/download -o t
	mv t $@
work/fire.jpg:
	curl -L http://unsplash.com/photos/2O2h72YF76A/download -o t
	mv t $@
work/nest.jpg:
	curl -L http://unsplash.com/photos/SastSwWtap0/download?force=true -o t
	mv t $@
work/letters.jpg:
	curl -L http://unsplash.com/photos/HYNOp2CUcbY/download?force=true -o t
	mv t $@
work/blackboard.jpg:
	curl -L http://unsplash.com/photos/5mZ_M06Fc9g/download?force=true -o t
	mv t $@
work/rsquo.jpg:
	curl -L http://unsplash.com/photos/kXDHR_bXIZo/download?force=true -o t
	mv t $@
work/thanku.jpg:
	curl -L http://unsplash.com/photos/oxjo1IQBK7M/download?force=true -o t
	mv t $@
work/elm.tiff:
	curl -L http://cdn.loc.gov/master/pnp/cph/3b30000/3b33000/3b33500/3b33525u.tif -o t
	mv t $@

#------------------------------------------------------------------------------
#
#                                  C L E A N
#
#------------------------------------------------------------------------------

.PHONY: clean
clean:
	find ${D1} -type f | xargs rm 
	rm -f ${D2}/*
