export ZSH=~/.oh-my-zsh

ZSH_THEME="robbyrussell"

plugins=(git gpg-agent lein history-substring-search zsh-syntax-highlighting zsh-completions)

autoload -U compinit && compinit

zstyle :omz:plugins:ssh-agent agent-forwarding on
zstyle :omz:plugins:ssh-agent identities id_rsa id_github

source $ZSH/oh-my-zsh.sh

setopt noflowcontrol

alias grep='grep --color=always'

export LESS='-R'
export LESSOPEN='|~/bin/lessfilter %s'
