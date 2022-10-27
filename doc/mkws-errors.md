October 23, 2022

Buffer Overflow
==============================

MAXSIZE in pp.c


    markbucciarelli@Marks-MacBook-Air markbucciarelli.com % make
    mkdir -p www/posts
    (cd www/posts && MKWSTHEMEDIR=../../share ../../bin/mkws http://markbucciarelli.com ../../posts)
    Making 2016-06-22_parsing_text_with_erlang_pattern_matching_and_guards.html
    Making 2016-07-06_what_is_cqrs.html
    Making 2016-07-20_my_first_erlang_patch.html
    Making 2016-08-03_the_essence_of_otp.html
    Making 2016-08-17_erlang_error_handling_primitives.html
    Making 2016-08-31_why_so_many_lambda_tshirts.html
    Making 2016-09-14_simple_gen_event_example.html
    Making 2016-10-27_cqrs_versus_oop.html
    Making 2016-11-09_a_simple_erlang_application.html
    Making 2016-11-23_a_simple_erlang_application_with_prometheus.html
    Making 2016-12-07_a_closure_with_erlang.html
    Making 2016-12-21_a_production_gen_event_application.html
    Making 2017-01-04_how_to_return_json_from_an_erlang_web_service.html
    Making 2017-01-18_how_to_read_utf8_encoded_file_with_erlang.html
    Making 2017-02-01_so_long_erlang_its_been_great.html
    Making 2017-02-21_hello_elm_and_haskell.html
    Making 2017-03-07_how_to_build_snap_with_stack.html
    Making 2017-03-22_some_cqrs_notes.html
    Making 2017-04-05_haskell_on_alpine_linux.html
    Making 2017-04-19_haskell_operators.html
    Making 2017-12-16_how-to-create-custom-nixpkg-for-haskell-application.html
    pp: Buffer overflow
    Making 2019-01-26_how-to-push-to-github-from-travis-ci.html
    Making 2019-05-05_install-alpine-on-raspberry-pi-using-macos.html
    Making 2019-05-20_set-current-time-with-chrony.html
    Making 2019-05-23_how-to-list-tarsnap-archive.html
    Making 2019-06-08_carbon_savings_with_tesla.html
    Making 2019-11-09_kotlin_value_object.html
    Making 2019-11-10_kotlin_consumer.html
    Making 2019-11-15_event-storning-notes.html
    Making 2020-01-04_kotlin_functional_gold_mine.html
    Making index.html
    Making sitemap.xml
    markbucciarelli@Marks-MacBook-Air markbucciarelli.com %






Error 137
===========================


    markbucciarelli@Marks-MacBook-Air markbucciarelli.com % make 
    mkdir -p www/posts
    (cd www/posts && MKWSTHEMEDIR=../../share ../../bin/mkws http://markbucciarelli.com ../../posts)
    Making 2016-06-22_parsing_text_with_erlang_pattern_matching_and_guards.html
    ../../bin/mkws: line 21: 36627 Killed: 9               pp "${SHARE}"/l.upphtml "${t}" "$1" > "$(basename "${t%.upphtml}".html)"
    make: *** [posts] Error 137
    markbucciarelli@Marks-MacBook-Air markbucciarelli.com %A

Solution: sign the app!

codesign -s - ./bin/pp

or, create a cert in keystore

can't find the link now.
