alias lr='ls -ralt'
alias rmpyc="find -name '*.pyc' -exec rm {} \;"

# "fnl" == "find named like"
function fnl() {
  find -name "*$@*"
}

# "pyg" == "grep in python files"
function pyg() {
  find -name '*.py' -exec egrep --color --with-filename "$@" {} \;
}

# switch_to --> switch virtualenv *AND* go to the right directory
function now_on() {
  venv_name=$1
  proj_name=`echo $venv_name | sed -e 's/_.\+//' -`
  shopt -q expand_aliases
  workon $venv_name
  cd $HOME/projects/$proj_name
}

complete -o nospace -F _virtualenvs now_on
