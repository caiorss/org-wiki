#!/usr/bin/env sh

USER_DIR="sandbox"

# INIT_FILE=$USER_DIR/init.el
INIT_FILE=$(pwd)/init.el

PACKAGE_DIR=$USER_DIR/elpa
    

ELISP_BOOTSTRAP=" (progn  (setq user-directory \"$USER_DIR\")  (setq user-emacs-directory  \"$USER_DIR\")  (setq user-init-file        \"$INIT_FILE\") (setq package-user-dir      \"$PACKAGE_DIR\") (add-to-list 'load-path     \"$USER_DIR\") )"

# echo $ELISP_BOOTSTRAP
# echo ""
# echo emacs -Q -l init.el --eval "$ELISP_BOOTSTRAP"

# emacs -Q -l init.el --eval "$ELISP_BOOTSTRAP"

BASEDIR=$(pwd)/sandbox

# env USER_DIRECTORY=$BASEDIR \
#     PACKAGE_DIR=$BASEDIR/elpa \
#     DEFAULT_DIR=$(pwd) \
#     emacs -Q -l init.el 

export USER_DIRECTORY=$BASEDIR 
export PACKAGE_DIR=$BASEDIR/elpa 
export DEFAULT_DIR=$(pwd) 
export ORG_WIKI_LOCATION=$BASEDIR/wiki

echo "USER_DIRECTORY = "$USER_DIRECTORY
echo "PACKAGE_DIR = "$PACKAGE_DIR


emacs -Q -l init.el 
