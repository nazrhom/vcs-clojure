#!/bin/bash
cd ../test/repos
for D in ./*; do
  if [ -d "$D" ]; then
    cd "$D"
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
        echo "Found conflict in $D - $commit"
        for objID in `git ls-files --unmerged | sed -E 's/.+(([a-z]|[0-9]){40}) ([1|2|3]).+/\1-\3/'`
          do
            echo "$objID"
            obj=${objID:0:39}
            role=${objID:41:42}
            if [ "$role" -eq "1" ]; then
              targetDir="../../conflicts/mined/$D-$commit-$obj"
              mkdir -p "$targetDir"
              fname="O.clj"
            fi
            if [ "$role" -eq "2" ]; then
              fname="A.clj"
            fi
            if [ "$role" -eq "3" ]; then
              fname="B.clj"
            fi

            git cat-file -p $obj > "$targetDir/$fname"

          done
        git merge --abort
      fi
      # get rid of changes so the next checkout doesnt complain
      git reset -q --hard
      git clean -fdxq
    done
    git checkout -q $old_branch
    cd ..
  fi
done
