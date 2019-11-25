base_path="$HOME/.zsh/"
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
alias ls="ls --color=auto --group-directories-first -lah"
alias lls="ls | less"

alias mv='mv -v'
alias cp='cp -v'

alias rsync='rsync --progress'

setopt no_correct_all

source $base_path/history.zsh
source $base_path/completion.zsh
source $base_path/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
