all: www rss idx sitemap
	(cd img ; make all)


#===============================================================================
#	For each new blog post, create:
#
#		1.  info.YYYY-MM-DD_<slug_with_underscores>
#
#			Mustache variables (copy existing info file)
#
#		2.  body.YYYY-MM-DD_<slug_with_underscores>
#
#			Body of post.
#
#		3.  add as dependency of www
#
#		4.  add as dependency of ../www/feed.rss
#
#		5.  add as dependency of index.html
#
#		6.  add as dependency of ../www/sitemap.xml
#
#===============================================================================

.PHONY: www
www: \
		../www/2016-10-27_cqrs_versus_oop.html \
		../www/2016-09-14_simple_gen_event_example.html \
		../www/2016-08-31_why_so_many_lambda_tshirts.html \
		../www/2016-08-17_erlang_error_handling_primitives.html \
		../www/2016-08-03_the_essence_of_otp.html \
		../www/2016-07-20_my_first_erlang_patch.html \
		../www/2016-07-06_what_is_cqrs.html \
		../www/2016-06-22_parsing_text_with_erlang_pattern_matching_and_guards.html

../www/feed.rss: work/rss.header templates/rss.footer \
		work/2016-10-27_cqrs_versus_oop.rss \
		work/2016-09-14_simple_gen_event_example.rss \
		work/2016-08-31_why_so_many_lambda_tshirts.rss \
		work/2016-08-17_erlang_error_handling_primitives.rss \
		work/2016-08-03_the_essence_of_otp.rss \
		work/2016-07-20_my_first_erlang_patch.rss \
		work/2016-07-06_what_is_cqrs.rss \
		work/2016-06-22_parsing_text_with_erlang_pattern_matching_and_guards.rss
	cat work/rss.header > t
	ls -r work/*.rss | xargs cat >> t
	cat templates/rss.footer >> t
	mv t $@

../www/sitemap.xml: templates/sitemap.header templates/sitemap.footer \
		work/2016-10-27_cqrs_versus_oop.xml \
		work/2016-09-14_simple_gen_event_example.xml \
		work/2016-08-31_why_so_many_lambda_tshirts.xml \
		work/2016-08-17_erlang_error_handling_primitives.xml \
		work/2016-08-03_the_essence_of_otp.xml \
		work/2016-07-20_my_first_erlang_patch.xml \
		work/2016-07-06_what_is_cqrs.xml \
		work/2016-06-22_parsing_text_with_erlang_pattern_matching_and_guards.xml
	cat templates/sitemap.header > t
	ls -r work/*.xml | xargs cat >> t
	cat templates/sitemap.footer >> t
	mv t $@

../www/index.html: templates/idx.header templates/idx.footer \
		work/2016-10-27_cqrs_versus_oop.idx \
		work/2016-09-14_simple_gen_event_example.idx \
		work/2016-08-31_why_so_many_lambda_tshirts.idx \
		work/2016-08-17_erlang_error_handling_primitives.idx \
		work/2016-08-03_the_essence_of_otp.idx \
		work/2016-07-20_my_first_erlang_patch.idx \
		work/2016-07-06_what_is_cqrs.idx \
		work/2016-06-22_parsing_text_with_erlang_pattern_matching_and_guards.idx
	cat templates/idx.header > t
	ls -r work/*.idx | xargs cat >> t
	cat templates/idx.footer >> t
	mv t $@

#-------------------------------------------------------------------------------
#
#                                    W W W
#
#-------------------------------------------------------------------------------


../www/%.html: work/%.header work/%.body work/%.footer
	cat $^ > $@
	@#echo $@

work/%.header: info.% templates/www.header
	mkdir -p work
	(source $< && \
		source ../rawimg/mustache.sh && \
		mustache < templates/www.header > $@)

# Footer is currently static data.
work/%.footer: templates/www.footer
	mkdir -p work
	cp $? $@

# Use sed to convert some simple things, but double quotes are too hard
# (code, links, and img src's).  Do double-quotes in editor.
work/%.body: body.% quotify.sh
	mkdir -p work
	cat $< | ./quotify.sh > $@

#-------------------------------------------------------------------------------
#
#                                    R S S
#
#-------------------------------------------------------------------------------

rss: ../www/feed.rss

info.feedheader: info.2*
	ls -1 info.2* | tail -1 | xargs grep PUBDATE_ISO > t
	mv t $@

work/rss.header: info.feedheader templates/rss.header
	rm -f work/*
	(source $< && \
		source ../rawimg/mustache.sh && \
		mustache < templates/rss.header > $@)

work/%.rss: info.% templates/rss.item
	(source $< && \
		source ../rawimg/mustache.sh && \
		mustache < templates/rss.item > $@)


#-------------------------------------------------------------------------------
#
#                                S I T E M A P 
#
#-------------------------------------------------------------------------------

sitemap: ../www/sitemap.xml

work/%.xml: info.% templates/sitemap.item
	(source $< && \
		source ../rawimg/mustache.sh && \
		mustache < templates/sitemap.item > $@)

#-------------------------------------------------------------------------------
#
#                                  I N D E X
#
#-------------------------------------------------------------------------------

idx: ../www/index.html

work/%.idx: info.% templates/idx.item
	(source $< && \
		source ../rawimg/mustache.sh && \
		mustache < templates/idx.item > $@)


.PHONY: clean
clean:
	rm -f work/*
