October 15, 2022


This is the source for my blog 
[markbucciarelli.com](http://markbucciarelli.com),
and is built using the static site generator mkws.sh. 

It has some shell script processing that converts Common markdown into the DOM
required by the Tufte CSS; see the notes in the doc directory for more info on
that.

    .
    ├── bin                 mkws and awk scripts
    ├── doc                 Spec for how I markdown to Tufte DOM
    ├── scripts
    ├── src
    │   ├── css
    │   ├── img
    │   ├── imginfo
    │   └─ posts
    │       └── share       mkws templates
    ├── test
    └── work                Build local version for testing & deploy


To test site locally:

    make
    ./scripts/start-server.sh
    curl http://localhost:8080/

See Makefile and [mkws.sh](https://mkws.sh) for more information.
