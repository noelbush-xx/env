alias lr='ls -ralt --color=auto'
alias rmpyc="find -name '*.pyc' -exec rm {} \;"

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

# switch_to --> switch virtualenv and git branch *AND* go to the right directory
function now_on() {
  venv_name=$1
  proj_name=`echo $venv_name | sed -e 's/_.\+//' -`
  branch_name=`echo $venv_name | sed -e 's/^[^_]\+_//' -`
  shopt -q expand_aliases
  workon $venv_name
  cd $HOME/projects/$proj_name
  git checkout $branch_name
}

complete -o nospace -F _virtualenvs now_on
