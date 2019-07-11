#!/bin/zsh

# reverse action
if [[ $# -gt 0 ]]
then                            # overwrite local settings! for install new machine mostly
    mkdir -p ~/.emacs.d/elisp
    cp init.el ~/.emacs.d/
    cp -r elisp/* ~/.emacs.d/elisp/
    # ycmd global_conf file
    cp ./global_conf.py  ~/.emacs.d/
    echo "sync . ==> .emacs.d done"
else # sync local to this repo
    cp ~/.emacs.d/init.el .
    cp ~/.emacs.d/elisp/*.el elisp/
    echo "copy ~/.emacs.d/ ==> here done"
fi
