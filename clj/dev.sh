#!/bin/sh
cd "$( dirname "${BASH_SOURCE[0]}" )"
clj -Odev -Rdev -m nrepl.cmdline
