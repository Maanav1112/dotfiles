colorscript -r &

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/maanav/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# zsh plugins
source ~/.zplug/init.zsh

zplug "zsh-users/zsh-syntax-highlighting", defer:2

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

# Then, source plugins and add commands to $PATH
zplug load

# Aliases
# ls aliases
alias ls="exa --icons"
alias v="nvim"

source ~/.zsh/catppuccin_mocha-zsh-syntax-highlighting.zsh
eval "$(starship init zsh)"
