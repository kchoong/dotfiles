#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1="[\u@\H] \w \[\e[38;5;161m\]$\[\e[m\] "
#PS1="\[\e[38;5;161m\][\u@\H]\[\e[m\] \w \[\e[38;5;161m\]$\[\e[m\] "

# powerline-daemon -q
# POWERLINE_BASH_CONTINUATION=1
# POWERLINE_BASH_SELECT=1
# . /usr/share/powerline/bindings/bash/powerline.sh

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

# documents
alias sync-drive='rclone sync -P drive: ~/Drive'
alias pdf='chromium --new-window --incognito'
