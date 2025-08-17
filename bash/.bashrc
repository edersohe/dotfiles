# .bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='[\u@\h \W]\$ '

alias ls='ls --color=auto -h'
alias ll='ls -l --group-directories-first'
alias la='ll -a'
alias lt='ls -lart'
alias emacs='emacs -nw'

[ -f $HOME/.swayrc ] && . $HOME/.swayrc
