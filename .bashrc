# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples
export TERM="xterm-256color"
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
HISTCONTROL=$HISTCONTROL${HISTCONTROL+:}ignoredups
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoreboth

export PATH="$HOME/.config/emacs/bin:$PATH"

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# ls aliases
alias ls='lsd'
alias lsa='lsd -a' # all
alias lsaa='lsd -A' # almost all
alias lsr='lsd -R' # recursive
alias lsm='lsd -t' # sort by time modified

# fzf aliases
alias ffx='fzf -x' # extended search
alias ffe='fzf -e' # exact search

# cd aliases
alias ..="cd .."
alias ...="cd ../.."
alias .3="cd ../../.."
alias .4="cd ../../../.."
alias .5="cd ../../../../.."

# Miscellaneous aliases
alias grep="grep --color=auto"
alias emacs="emacsclient -c -a 'emacs'"
alias cmus=" -T cmus & 'cmus'"

# Config (git)
alias config='/usr/bin/git --git-dir=$HOME/dotfiles --work-tree=$HOME'

# Scripts (git)
alias scripts='/usr/bin/git --git-dir=$HOME/scripts --work-tree=$HOME/Documents/scripts'

# System
alias shutdown='shutdown -h -P now'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# Default parameter to send to the "less" command
# -R: show ANSI colors correctly; -i: case insensitive search
LESS="-R -i"

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# Add sbin directories to PATH.  This is useful on systems that have sudo
echo $PATH | grep -Eq "(^|:)/sbin(:|)"     || PATH=$PATH:/sbin
echo $PATH | grep -Eq "(^|:)/usr/sbin(:|)" || PATH=$PATH:/usr/sbin

# Starship Prompt
eval "$(starship init bash)"
