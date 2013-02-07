# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' expand prefix suffix
zstyle ':completion:*' file-sort modification
zstyle ':completion:*' group-name ''
zstyle ':completion:*' ignore-parents parent pwd
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' list-suffixes true
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._-]=** r:|=**' 'r:|[._-]=** r:|=** l:|=*'
zstyle ':completion:*' max-errors 2
zstyle ':completion:*' menu select=1
zstyle ':completion:*' original true
zstyle ':completion:*' preserve-prefix '//[^/]##/'
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' use-compctl false
zstyle :compinstall filename '/home/nav/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=10000
setopt appendhistory autocd
unsetopt extendedglob nomatch
bindkey -e
# End of lines configured by zsh-newuser-install
autoload -U colors && colors
export PS1="%{$fg[green]%}%n%{$reset_color%} %{$fg[blue]%}%~ %#%{$reset_color%}"

alias ls='ls -F --color=always'
alias cp='cp -i'
alias mv='mv -i'

bindkey "\e[1~" beginning-of-line # Home
bindkey "\e[4~" end-of-line # End
bindkey "\e[5~" beginning-of-history # PageUp
bindkey "\e[6~" end-of-history # PageDown
bindkey "\e[2~" quoted-insert # Ins
bindkey "\e[3~" delete-char # Del
bindkey "\e[1;5C" forward-word
bindkey "\eOc" emacs-forward-word
bindkey "\e[1;5D" backward-word
bindkey "\eOd" emacs-backward-word
bindkey "\e\e[C" forward-word
bindkey "\e\e[D" backward-word
bindkey "\e[Z" reverse-menu-complete # Shift+Tab
# for rxvt
bindkey "\e[7~" beginning-of-line # Home
bindkey "\e[8~" end-of-line # End

export PATH=$PATH:~/.gem/ruby/1.9.1/bin
alias emacs='/usr/local/bin/emacs'
alias emacsclient='/usr/local/bin/emacsclient'

export EDITOR="emacsclient -t"
export VISUAL="emacsclient -c"
alias e='emacsclient -t'
alias ec='emacsclient -c'
alias sudo='sudo -E '

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
export http_proxy=http://10.3.100.212:8080/ 
export ftp_proxy=http://10.3.100.212:8080/ 
export https_proxy=http://10.3.100.212:8080/ 
eval `dircolors /etc/DIR_COLORS`
