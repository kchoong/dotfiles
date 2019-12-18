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

# login/lock
alias unlock-session='sudo loginctl unlock-session'

# service
alias start='sudo systemctl start'
alias stop='sudo systemctl stop'
alias restart='sudo systemctl restart'
alias status='systemctl status'

# fix backlight temp
alias fix-light='sudo chmod 666 /sys/class/backlight/intel_backlight/brightness'

# documents
alias google-drive-sync='rclone sync -P drive: ~/google-drive'
alias pdf='chromium --new-window --incognito'

# projects
alias cd-bibsonomy='cd ~/Work/bibsonomy'
alias cd-bibsonomy-plugins='cd ~/Work/bibsonomy-plugins'
alias cd-anikan='cd ~/Projects/anikan'
