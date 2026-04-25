# .bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

HISTCONTROL=ignoreboth
shopt -s histappend
HISTSIZE=1000
HISTFILESIZE=2000
shopt -s checkwinsize

if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

bind '"\ez": "fg\n"'

alias ls='ls --color=auto -h'
alias ll='ls -l --group-directories-first'
alias la='ll -a'
alias lt='ls -lart'
alias ssh="TERM=xterm-256color ssh"
alias grep="grep --color"

export XKB_DEFAULT_LAYOUT=us
export XKB_DEFAULT_VARIANT=altgr-intl
export LC_ALL=en_US.UTF-8
export ERL_AFLAGS="-kernel shell_history enabled"
export EDITOR="nvim"

[[ -f $HOME/.env ]] && . "$HOME"/.env
[[ -f $HOME/.miserc ]] && . "$HOME"/.miserc
[[ -f $HOME/.ezarc ]] && . "$HOME"/.ezarc
[[ -f $HOME/.footrc ]] && . "$HOME"/.footrc
[[ -f $HOME/.starshiprc ]] && . "$HOME"/.starshiprc
[[ -f $HOME/.fzfrc ]] && . "$HOME"/.fzfrc
[[ -f $HOME/.ripgreprc ]] && . "$HOME"/.ripgreprc
[[ -f $HOME/.swayrc ]] && . "$HOME"/.swayrc
[[ -f $HOME/.qtilerc ]] && . "$HOME"/.qtilerc
[[ -f $HOME/.helixrc ]] && . "$HOME"/.helixrc
[[ -f $HOME/.emacsrc ]] && . "$HOME"/.emacsrc
[[ -f $HOME/.tmuxrc ]] && . "$HOME"/.tmuxrc
[[ -f $HOME/.zellijrc ]] && . "$HOME"/.zellijrc
[[ -f $HOME/.lazygitrc ]] && . "$HOME"/.lazygitrc
[[ -f $HOME/.opencoderc ]] && . "$HOME"/.opencoderc

[ -d $HOME/.local/bin ] && export PATH="$HOME/.local/bin:$PATH"
[ -d $HOME/.bin ] && export PATH="$HOME/.bin:$PATH"
[ -f $HOME/.npmrc ] && [ -d $HOME/.npm/bin ] && export PATH="$HOME/.npm/bin:$PATH"
[ -d $HOME/.bun ] && export BUN_INSTALL="$HOME/.bun" && export PATH="$BUN_INSTALL/bin:$PATH"
[ -d $HOME/.cargo/bin ] && export PATH="$HOME/.cargo/bin:$PATH"
