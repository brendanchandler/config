#!/bin/bash

sessionname="myproj"
srcdir="~/src/$sessionname"

tmux has-session -t "$sessionname"
if [[ $? == 0 ]]; then
    echo "Session $sessionname already exists"
    exit 1
fi

# EDITOR window
tmux new-session -s $sessionname -n editor -d
tmux send-keys -t $sessionname "cd $srcdir" C-m
tmux send-keys -t $sessionname "$EDITOR" C-m

tmux split-window -v -t $sessionname
tmux select-layout -t $sessionname main-horizontal
tmux send-keys -t $sessionname:1.2 "cd $srcdir" C-m

tmux split-window -h -t $sessionname
tmux send-keys -t $sessionname:1.3 "cd $srcdir" C-m

# Documentation window
tmux new-window -n documentation -t $sessionname
tmux send-keys -t $sessionname:2 "cd $srcdir" C-m
tmux send-keys -t $sessionname:2 'nvim ../README.md' C-m

# Finishing stuff
tmux select-window -t $sessionname 1
tmux attach -t $sessionname
