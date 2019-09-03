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

# fix backlight temp
alias flight='sudo chmod 666 /sys/class/backlight/intel_backlight/brightness'

# documents
alias sync-drive='rclone sync -P drive: ~/Drive'
alias pdf='chromium --new-window --incognito'
