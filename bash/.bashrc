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

[ -f $HOME/.ezarc ] && . $HOME/.ezarc
[ -f $HOME/.footrc ] && . $HOME/.footrc
[ -f $HOME/.starshiprc ] && . $HOME/.starshiprc
[ -f $HOME/.fzfrc ] && . $HOME/.fzfrc
[ -f $HOME/.swayrc ] && . $HOME/.swayrc

