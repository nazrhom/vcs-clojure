#!/bin/bash

old_branch=$(git symbolic-ref --short HEAD)
for commit in `git rev-list --merges HEAD`
do
  # find the parents of the merge commit
  parents=$(git log -1 --format=%P $commit)
  fst=${parents%% *}
  rest=${parents#* }
  # check out the first parent
  git checkout -q $fst
  # merge with the rest of them
  git merge --no-commit $rest >/dev/null 2>&1
  # if there are any conflicts, print the commit and abort the merge
  if git ls-files --unmerged | grep -q '^'; then
    echo $commit
    git merge --abort
  fi
  # get rid of changes so the next checkout doesnt complain
  git reset -q --hard
  git clean -fdxq
done
git checkout -q $old_branch