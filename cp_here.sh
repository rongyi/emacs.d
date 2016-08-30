#!/bin/zsh


if [[ $# -gt 0 ]]
then
    cp init.el ~/.emacs.d/
    cp -r elisp/* ~/.emacs.d/elisp/
    echo "copy repo to emacs.d done"
else
    cp ~/.emacs.d/init.el .
    cp ~/.emacs.d/elisp/* elisp/
    echo "copy current setting to here done"
fi
