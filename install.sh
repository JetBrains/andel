#!/bin/sh
lein jar && mvn install:install-file -Dfile=target/andel-0.1.0-SNAPSHOT.jar -DgroupId=andel -DartifactId=andel -Dversion=0-1-0-SNAPSHOT -Dpackaging=jar
