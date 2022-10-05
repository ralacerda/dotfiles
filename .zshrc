LANG=en_US.UTF-8

base_path="$HOME/.zsh"
fpath=( "$HOME/.zsh/pure" $fpath )

autoload -U promptinit; promptinit
prompt pure

autoload -U compinit
compinit

setopt autocd
setopt extendedglob
setopt NO_NOMATCH

CLICOLOR=1

bindkey -e

REPORTTIME=-1

bindkey '^[p' up-line-or-history
bindkey '^[n' down-line-or-history
bindkey '^p' up-line-or-search
bindkey '^n' down-line-or-search
bindkey '^[h' backward-delete-word
bindkey '^[m' run-help 

bindkey -s '^[l' "ls\n"
bindkey -s '^[-' "cd -\n"

DIRSTACKSIZE=8
setopt autopushd pushdminus pushdsilent pushdtohome
alias dh='dirs -v'

setopt correct_all
alias man='nocorrect man'
alias mv='nocorrect mv'
alias mkdir='nocorrect mkdir'
alias gist='nocorrect gist'
alias sudo='nocorrect sudo'
alias ls='exa -l --group-directories-first'
alias la='ls -a'
alias lt='ls -T -L=2'
alias cat='bat'

alias lg='lazygit'

alias mv='mv -v'
alias cp='cp -v'

alias rsync='rsync --progress'

setopt no_correct_all

source $base_path/history.zsh
source $base_path/completion.zsh
source $base_path/fast-syntax-highlighting/F-Sy-H.plugin.zsh

PATH=$PATH:$HOME/.config/emacs/bin
