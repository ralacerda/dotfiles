LANG=en_US.UTF-8

base_path="$HOME/.zsh"

setopt autocd
setopt extendedglob
setopt NO_NOMATCH
autoload -U compinit; compinit

CLICOLOR=1

# Use emacs bindings
bindkey -e

# Don't show how long a command took
REPORTTIME=-1

# History and search keybindings
bindkey '^[p' up-line-or-history
bindkey '^[n' down-line-or-history
bindkey '^p' up-line-or-search
bindkey '^n' down-line-or-search

# Stack options to easily navigate between files
DIRSTACKSIZE=8
setopt autopushd pushdminus pushdsilent pushdtohome
alias dh='dirs -v'

# Use rust command-line alternatives
alias ls='exa -l --group-directories-first'
alias la='ls -a'
alias lt='ls -T -L=2'
alias cat='bat'
alias du='dust'
alias top='btm'

# Alias for common commands
alias lg='lazygit'

# Always be verbose when copying and moving
# Confirmation when deleting
# Always show progress with rsync
alias mv='mv -v'
alias cp='cp -v'
alias rm='rm -I'
alias rsync='rsync --progress'

eval "$(zoxide init zsh)"
eval "$(starship init zsh)"

source $base_path/history.zsh
source $base_path/completion.zsh
source $base_path/fast-syntax-highlighting/F-Sy-H.plugin.zsh
