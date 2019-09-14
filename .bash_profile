# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile
umask 022

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# SHELL=/bin/bash
EDITOR=mg
PAGER=less
JAVA_HOME="/usr/lib/jvm/default"

LESS="-RMiq"

export SHELL EDITOR PAGER LESS
export LESSOPEN DICTIONARY
CVSEDITOR=emacs
CVS_RSH=ssh
LC_CTYPE="en_US.utf8"
export CVSROOT CVSEDITOR CVS_RSH LC_CTYPE


export JAVA_HOME CLASSPATH

export PATH=/usr/local/sbin:$PATH
export PATH=/usr/local/bin:$PATH
# export PATH=$RUBYPATHS:$PATH
export PATH=/usr/texbin/:$PATH

# if running bash
#if [ -n "$BASH_VERSION" ]; then
#    # include .bashrc if it exists
#    if [ -f "$HOME/.bashrc" ]; then
#       . "$HOME/.bashrc"
#    fi
#fi

# Tell ls to be colourful
export CLICOLOR=1

# Tell grep to highlight matches

export XDG_RUNTIME_DIR=/run/user/`id -u`

export PATH=$HOME/.cargo/bin:$PATH
