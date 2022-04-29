set -U fish_greeting

set -gx EDITOR emacsclient -tca  ''
set -gx CDPATH . $HOME $HOME/src
set -gx GOOS linux
set -gx GOARCH amd64
set -gx GOBIN $HOME/bin
set -gx FZF_DEFAULT_COMMAND fd --type file --follow --hidden --exclude .git
set -gx FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND

fish_add_path $HOME/bin
fish_add_path $HOME/bin/aliases

if status is-interactive
    # Commands to run in interactive sessions can go here
end
