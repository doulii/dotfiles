#!/bin/bash

set -e

cd $(dirname $0)

# link source target
link() {
    echo "mkdir -p $(dirname $2)"
    mkdir -p $(dirname $2)

    echo "ln -sf $1 $2"
    ln -sfn $1 $2
}

# TODO: use XDG cofig variable
link $PWD $HOME/.config/kitty

