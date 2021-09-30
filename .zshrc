export ZSH=~/.oh-my-zsh

ZSH_THEME="robbyrussell"

plugins=(git gpg-agent lein history-substring-search)

autoload -U compinit && compinit

zstyle :omz:plugins:ssh-agent agent-forwarding on
zstyle :omz:plugins:ssh-agent identities id_rsa id_github

source $ZSH/oh-my-zsh.sh

setopt noflowcontrol

alias grep='grep --color=always'

export LESS='-R'
export LESSOPEN='|~/bin/lessfilter %s'
export PATH="$HOME/bin:$HOME/.emacs.d/bin:$PATH"
export EDITOR=emacsclient
