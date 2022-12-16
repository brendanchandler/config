# If not running interactively, don't do anything
[ -z "$PS1" ] && return

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
#export PS1="$colorfularrows \u@\h [\t](\$?) \w\n\$ "

export EDITOR='vim'
export VISUAL='gvim --nofork'
export GIT_EDITOR="vim"
export TERM=xterm-256color

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

alias gvim_diff='gvimdiff'
alias less='less -R'
alias gdb='EDITOR=gvimr gdb'
alias mc='. /usr/share/mc/bin/mc-wrapper.sh'
alias e='emacsclient -n'

export s=~/src/
export l=/local/bchandler/
export c=~/C2/iocs/

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi


#export EPICS_BASE=${HOME}/src/epics-base
#export EPICS_HOST_ARCH=$(${EPICS_BASE}/startup/EpicsHostArch)
#export PATH=${EPICS_BASE}/bin/${EPICS_HOST_ARCH}:${PATH}
#export PATH=/C2/conda/linux-x86_64/bin:$PATH
#export PATH=/C2/conda/linux-x86_64/envs/gui/bin:$PATH
#PATH="$HOME/.cargo/bin:$PATH"
#PATH=/usr/local/go/bin:$PATH
#PATH=$HOME/src/sh/:$PATH
PATH=$HOME/bin/:$PATH
export PATH=$HOME/.local/bin:$PATH
#PATH=$HOME/src/epics-base/bin/linux-x86_64/:$PATH
#export PATH=/C2/conda/linux-x86_64/envs/ioc/bin:$PATH
export PATH

#export SUMOCONFIG=/C2/ioc-modules

export GOPATH=~/src/go

if [ -n "$DISPLAY" ]; then
    xset b off
fi

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/C2/conda/linux-x86_64/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/C2/conda/linux-x86_64/etc/profile.d/conda.sh" ]; then
        . "/C2/conda/linux-x86_64/etc/profile.d/conda.sh"
    else
        export PATH="/C2/conda/linux-x86_64/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<


