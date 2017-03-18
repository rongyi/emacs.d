#!/bin/zsh

# reverse action
if [[ $# -gt 0 ]]
then                            # overwrite local settings! for install new machine mostly
    cp init.el ~/.emacs.d/
    mkdir -p ~/.emacs.d/elisp
    cp -r elisp/* ~/.emacs.d/elisp/
    cp -r ./snippets ~/.emacs.d/snippets
    echo "sync repo to .emacs.d done"
else # sync local to this repo
    cp ~/.emacs.d/init.el .
    cp ~/.emacs.d/elisp/* elisp/
    cp -r ~/.emacs.d/snippets/* ./snippets
    echo "copy local setting to repo done"
fi
