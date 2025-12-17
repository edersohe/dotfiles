# .bash_profile
export PATH=$HOME/.bin:$HOME/.local/bin:$PATH

[ -f $HOME/.npmrc ] && export PATH="$HOME/.npm/bin:$PATH"

# Get the aliases and functions
[ -f $HOME/.bashrc ] && . $HOME/.bashrc

