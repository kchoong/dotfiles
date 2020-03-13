#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1="[\u@\H] \w $ "

#
# alias
#

# list
alias ls='ls --color=auto'
alias ll='ls -la --color=auto'

# service
alias start='sudo systemctl start'
alias stop='sudo systemctl stop'
alias restart='sudo systemctl restart'
alias status='systemctl status'

# utils
alias auto-screenlayout='~/.scripts/auto-screenlayout.sh'
alias activate='source env/bin/activate'
