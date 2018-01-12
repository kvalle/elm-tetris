#!/usr/bin/env bash
set -e

npm run build

git checkout gh-pages
for file in $(ls -1 build/)
do
  cp "build/$file" .
  git add $file
  echo "updated $file"
done

if git diff-index --quiet HEAD -- ; then
  echo
  echo '! No changes to deploy.'
  echo
  git checkout master
  exit 0
fi

git commit -m "Deploy updated version to GitHub Pages"
git push -f origin gh-pages
git checkout master
