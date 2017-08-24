#!/bin/bash
git checkout gh-pages
git reset master --hard
lein clean
lein cljsbuild once
git add target -f
git commit -m "Deploy to GitHub Pages"
git push --force origin gh-pages
git checkout master
