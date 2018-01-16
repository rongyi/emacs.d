#!/bin/zsh

# reverse action
if [[ $# -gt 0 ]]
then                            # overwrite local settings! for install new machine mostly
    cp init.el ~/.emacs.d/
    mkdir -p ~/.emacs.d/elisp
    cp -r elisp/* ~/.emacs.d/elisp/
    echo "sync . ==> .emacs.d done"
else # sync local to this repo
    cp ~/.emacs.d/init.el .
    cp ~/.emacs.d/elisp/*.el elisp/
    echo "copy ~/.emacs.d/ ==> here done"
fi
