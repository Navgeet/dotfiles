set fish_greeting
set -x TERM xterm
set -x GOPATH $HOME/go
set -x PATH $PATH /usr/local/go/bin $GOPATH/bin $HOME/.local/bin
source ~/.asdf/asdf.fish

set -x NVM_DIR $HOME/.nvm
# [ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh"  # This loads nvm

fish_ssh_agent
