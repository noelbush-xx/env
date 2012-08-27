alias lr='ls -ralt --color=auto'

# Remove python bytecode files.
alias rmpyc="find -name '*.pyc' -exec rm {} \;"

# Remove those ".orig" files leftover from a manual merge
alias rmorig="find -name '*.orig' -exec rm {} \;"

# "fnl" == "find named like"
function fnl() {
  find -name "*$@*"
}

# "pyg" == "python grep -- grep in python files"
function pyg() {
  find -name '*.py' -exec egrep --color --with-filename "$@" {} \;
}

# "fig" == "file grep -- grep in all files"
function fig() {
  find -exec egrep --color --with-filename "$@" {} \;
}
