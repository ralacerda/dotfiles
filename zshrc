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

# sets the TERM variable when I'm using ssh
alias ssh='TERM=xterm-256color ssh'

# Use rust command-line alternatives
alias ls='exa -l --group-directories-first --icons --git'
alias la='ls -a'
alias lt='ls -T -L=2'
alias cat='bat --style=grid,header-filename'
alias du='dust'
alias top='btm'

# ls for folders only
alias lsd='ls -d */'

# Create dir and cd into it
take() {
  mkdir $1
  cd $1
}

# open current folder and redirects output
open() {
  xdg-open "${1:-.}" &>/dev/null
}

# Alias for common commands
alias lg='lazygit'
alias ld='lazydocker'
alias gc='git clone'
alias gs='git status'

# Alias for gh commands
## Use fzf to select a repository to clone
alias ghc='gh repo list -L 100 --json name --jq ".[].name" | fzf --prompt="Select a repository: " | xargs -I {} gh repo clone {}'

# Alias for Zoxide
## Run zoxide in interactive mode passing folders that start with the current folder
## Basically a way to run "zi" but only for the current folder
alias cdz='zi "$(pwd)" /'

# Pipe find with maxdepth 1 into sort into fzf and cd into the selected folder
cdl() {
    local target
    target="$(find . -maxdepth 1 -type d -printf "%f\n" | LC_COLLATE=C sort -r | fzf)" || return
    cd "$target" || return
}

# Always be verbose
alias mv='mv -v'
alias cp='cp -v'
# Ask before removing files
alias rm='rm -Iv'
# Create parent directories if they don't exist
alias mkdir='mkdir -vp'
# Show rsync progress
alias rsync='rsync --progress'

# Start zoxide and starship
eval "$(zoxide init zsh)"
eval "$(starship init zsh)"

# Load history, completion and plugins
source $base_path/history.zsh
source $base_path/completion.zsh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/fzf/key-bindings.zsh

# Set pnpm variables
export PNPM_HOME="$HOME/.local/share/pnpm"
export PATH="$PNPM_HOME:$PATH"

# Set nvm variables and load the plugin
export NVM_LAZY_LOAD=true
export NVM_COMPLETION=true
export NVM_LAZY_LOAD_EXTRA_COMMANDS=('nr')
source ~/.zsh-nvm/zsh-nvm.plugin.zsh

