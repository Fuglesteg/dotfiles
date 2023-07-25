# Neovim
alias "neovide=neovide --multigrid"

# Docker
alias "dps=docker ps"
alias "dcu=docker compose up"
alias "dcd=docker compose down"

# ls
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# GCC
alias cc=gcc

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls -lh --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

if [[ -x /usr/bin/exa || -x $HOME/.guix-profile/bin/exa ]]; then
   alias ls='exa -l --icons'
fi

# SSH
alias sshf='ssh andy@fuglesteg.mywire.org'

# Apt
alias apt='sudo apt'
alias apti='sudo apt install'
