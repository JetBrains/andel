#!/bin/sh

lein clean && lein cljsbuild once min && cp -R resources/public/* target/cljsbuild/public && cd target/cljsbuild/public && python -m SimpleHTTPServer 8000
