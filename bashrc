# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# History size... ITS OVER 9000!
HISTSIZE=9500
HISTFILESIZE=$HISTSIZE
HISTCONTROL=ignorespace:ignoredups
# append to the history file, don't overwrite it
shopt -s histappend
shopt -u interactive_comments

# BLACK="\[\033[0;30m\]"
# BLACKBOLD="\[\033[1;30m\]"
# RED="\[\033[0;31m\]"
# REDBOLD="\[\033[1;31m\]"
GREEN="\[\033[0;32m\]"
# GREENBOLD="\[\033[1;32m\]"
YELLOW="\[\033[0;33m\]"
# YELLOWBOLD="\[\033[1;33m\]"
# BLUE="\[\033[0;34m\]"
# BLUEBOLD="\[\033[1;34m\]"
# PURPLE="\[\033[0;35m\]"
# PURPLEBOLD="\[\033[1;35m\]"
# CYAN="\[\033[0;36m\]"
# CYANBOLD="\[\033[1;36m\]"
# WHITE="\[\033[0;37m\]"
# WHITEBOLD="\[\033[1;37m\]"
RESET="\[\033[00m\]"

colorfularrows="$GREEN>$YELLOW>$GREEN>$YELLOW>$RESET"

function timer_start
{
    timer=${timer:-$SECONDS}
}

function timer_stop
{
    timer_show=$(($SECONDS - $timer))
    unset timer
}

trap 'timer_start' DEBUG

if [ "$PROMPT_COMMAND" == "" ]; then
    PROMPT_COMMAND="timer_stop"
else
    PROMPT_COMMAND="$PROMPT_COMMAND; timer_stop"
fi

export PS1="${colorfularrows} \u@\h [\${timer_show}s](\$?) \w\n\$ "

export EDITOR='emacs -nw'
export VISUAL='emacs -nw'
export GIT_EDITOR="emacs -nw"

alias e='emacsclient -n'

export s=~/src/

PATH=$HOME/bin/:$PATH
PATH=$HOME/.local/bin:$PATH
export PATH

export GOPATH=~/src/go

