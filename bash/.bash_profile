# .bash_profile
export PATH=$HOME/.bin:$HOME/.local/bin:$PATH

[ -f $HOME/.npmrc ] && export PATH="$HOME/.npm/bin:$PATH"

[ -d $HOME/.bun ] && export PATH="$HOME/.bun/bin:$PATH"

# Get the aliases and functions
[ -f $HOME/.bashrc ] && . $HOME/.bashrc

. "$HOME/.cargo/env"
