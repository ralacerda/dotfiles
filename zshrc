# Add deno completions to search path
if [[ ":$FPATH:" != *":/home/ralacerda/.zsh/completions:"* ]]; then export FPATH="/home/ralacerda/.zsh/completions:$FPATH"; fi
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

# ls piped to less
alias lls="ls --color='always' | less -R"

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

# Move to the root of the git project
alias gr='cd $(git rev-parse --show-toplevel)'

# Alias for gh commands
## Use fzf to select a repository to clone
alias ghc='gh repo list -L 100 --json name --jq ".[].name" | fzf --prompt="Select a repository: " | xargs -I {} gh repo clone {}'
## Open repo in the browser
alias ghw='gh repo view -w'
## Create git repo, set as origen and push
ghp() {
    local name="$1"
    gh repo create "$name" --private --source=. --remote=upstream --push
}

alias ghl='gh repo list skylar-ai -L 100 --json name --jq ".[].name" | rofi -dmenu | xargs -I {} gh browse -R skylar-ai/{}'

# Alias for Zoxide
## Run zoxide in interactive mode passing folders that start with the current folder
## Basically a way to run "zi" but only for the current folder
alias cdz='zi "$(pwd)" /'

# Bind ctrl+z to "zi"
bindkey -s "^Z" "zi^M" 

# Pipe find with maxdepth 1 into sort into fzf and cd into the selected folder
cdl() {
    local target
    target="$(find . -maxdepth 1 -type d -not -name '.' -printf "%T@ %f\n" | sort -nr | cut -d' ' -f2- | fzf)" || return
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
export PATH="$HOME/.local/bin:$PATH"
eval "$(zoxide init zsh)"
eval "$(starship init zsh)"

# Load history, completion and plugins
source $base_path/history.zsh
source $base_path/completion.zsh
source $base_path/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source $base_path/zsh-autosuggestions/zsh-autosuggestions.zsh

# Set pnpm variables
export PNPM_HOME="$HOME/.local/share/pnpm"
export PATH="$PNPM_HOME:$PATH"

# bun completions
[ -s "$HOME/.bun/_bun" ] && source "$HOME/.bun/_bun"

# zoxide

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

# Turso
export PATH="$HOME/.turso:$PATH"

# fnm
export PATH="$HOME/.local/share/fnm:$PATH"
eval "`fnm env`"

# go
export PATH="/usr/local/go/bin:$PATH"
export PATH="/$HOME/go/bin:$PATH"


# cargo
export PATH="$HOME/.cargo/bin:$PATH"

# fzf
# disable CTRL+T keybind
export FZF_CTRL_T_COMMAND=""
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# neovim
export PATH="$PATH:/opt/nvim-linux64/bin"

eval "$(atuin init zsh --disable-up-arrow)"

export ANDROID_HOME=$HOME/Android/Sdk
export PATH=$PATH:$ANDROID_HOME/emulator
export PATH=$PATH:$ANDROID_HOME/platform-tools

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/ralacerda/Repos/sklippy-backend/google-cloud-sdk/path.zsh.inc' ]; then . '/home/ralacerda/Repos/sklippy-backend/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/home/ralacerda/Repos/sklippy-backend/google-cloud-sdk/completion.zsh.inc' ]; then . '/home/ralacerda/Repos/sklippy-backend/google-cloud-sdk/completion.zsh.inc'; fi
. "/home/ralacerda/.deno/env"

. "$HOME/.cargo/env"
