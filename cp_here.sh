#!/bin/zsh

# reverse action
if [[ $# -gt 0 ]]
then
    cp init.el ~/.emacs.d/
    cp -r elisp/* ~/.emacs.d/elisp/
    cp -r ./snippets ~/.emacs.d/snippets
    echo "sync repo to .emacs.d done"
else # backup action
    cp ~/.emacs.d/init.el .
    cp ~/.emacs.d/elisp/* elisp/
    cp -r ~/.emacs.d/snippets/* ./snippets
    echo "copy local setting to repo done"
fi
