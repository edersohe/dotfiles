# .bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='[\u@\h \W]\$ '

alias ls='ls --color=auto -h'
alias ll='ls -l --group-directories-first'
alias la='ll -a'
alias lt='ls -lart'
alias emacs='emacs -nw'

export XKB_DEFAULT_LAYOUT=us
export XKB_DEFAULT_VARIANT=altgr-intl
export LC_ALL=en_US.UTF-8
export PATH=$HOME/.bin:$PATH
export TERMINAL=footclient

[ -f $HOME/.swayrc ] && . $HOME/.swayrc

