# .bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='[\u@\h \W]\$ '

bind '"\ez": "fg\n"'

alias ls='ls --color=auto -h'
alias ll='ls -l --group-directories-first'
alias la='ll -a'
alias lt='ls -lart'
alias ssh="TERM=xterm-256color ssh"

export XKB_DEFAULT_LAYOUT=us
export XKB_DEFAULT_VARIANT=altgr-intl
export LC_ALL=en_US.UTF-8

[ -f $HOME/.env ] && . $HOME/.env
[ -f $HOME/.miserc ] && . $HOME/.miserc
[ -f $HOME/.ezarc ] && . $HOME/.ezarc
[ -f $HOME/.footrc ] && . $HOME/.footrc
[ -f $HOME/.starshiprc ] && . $HOME/.starshiprc
[ -f $HOME/.fzfrc ] && . $HOME/.fzfrc
[ -f $HOME/.swayrc ] && . $HOME/.swayrc
[ -f $HOME/.helixrc ] && . $HOME/.helixrc
[ -f $HOME/.emacsrc ] && . $HOME/.emacsrc
[ -f $HOME/.tmuxrc ] && . $HOME/.tmuxrc
[ -f $HOME/.zellijrc ] && . $HOME/.zellijrc
[ -f $HOME/.lazygitrc ] && . $HOME/.lazygitrc
[ -f $HOME/.opencoderc ] && . $HOME/.opencoderc
