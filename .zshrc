# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="mytheme"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git git-extras git-prompt github mvn archive archlinux colorize scala docker fasd emoji thefuck)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
autoload -U z

export JAVA_HOME="/usr/lib/jvm/default/"
export _JAVA_OPTIONS="-DJINTEGRA_NATIVE_MODE -DJINTEGRA_COINIT_VALUE=0 -Dsun.io.useCanonCaches=false -XX:ThreadPriorityPolicy=42 -XX:CompileThreshold=1500 -XX:+TieredCompilation -XX:+UnlockExperimentalVMOptions -XX:+UseBiasedLocking -Xverify:none -XX:UseSSE=3 -XX:+UseThreadPriorities -XX:-UseLargePages -XX:+OptimizeStringConcat -XX:+UseFastAccessorMethods -XX:+UseCompressedOops -XX:+AggressiveOpts -XX:ReservedCodeCacheSize=512m -XX:LargePageSizeInBytes=2m -XX:+CMSClassUnloadingEnabled -Xms512M -Xmx7168M -Xss1M -Dawt.useSystemAAFontSettings=lcd -Dsun.java2d.xrender=true"
export GNOME_DESKTOP_SESSION_ID=this-is-deprecated

# Locale settings (utf-8)
export LC_CTYPE=en_US.UTF-8
export LANG=en_US.UTF-8

source $HOME/.aliases

# DISABLE_UPDATE_PROMPT=true

autoload bashcompinit
bashcompinit

export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"

export GPG_TTY=$(tty)
export JAVA_HOME=/usr/lib/jvm/default

# GO config
export GOPATH=$HOME/golang
export GOROOT=
export PATH=$GOPATH/bin:$PATH
export PATH=$GOROOT/bin:$PATH

[[ -d '/usr/facebook/ops/rc' ]] && export LOCAL_ADMIN_SCRIPTS='/usr/facebook/ops/rc'

[[ ! -z $LOCAL_ADMIN_SCRIPTS ]] && source "${LOCAL_ADMIN_SCRIPTS}/master.zshrc"

export EDITOR=mg
# Start the gpg-agent if not already running
if ! pgrep -x -u "${USER}" gpg-agent >/dev/null 2>&1; then
        gpg-connect-agent /bye >/dev/null 2>&1
        gpg-connect-agent updatestartuptty /bye >/dev/null
fi
# use a tty for gpg
# solves error: "gpg: signing failed: Inappropriate ioctl for device"
GPG_TTY=$(tty)
export GPG_TTY
# Set SSH to use gpg-agent
unset SSH_AGENT_PID

SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
export SSH_AUTH_SOCK

# add alias for ssh to update the tty
alias ssh="gpg-connect-agent updatestartuptty /bye >/dev/null; ssh"

# OPAM configuration
. $HOME/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# Rust configuration
SSL_CERT_DIR=/etc/ssl/certs/
[ -f ~/.cargo/bin/sccache ] && export RUSTC_WRAPPER=sccache
[ -f ~/.cargo/bin/sccache ] && [ -z SCCACHE_GCS_BUCKET ] && export SCCACHE_CACHE_SIZE=2G

# Keybindings for fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
[ -f /usr/share/fzf/key-bindings.zsh ] && source /usr/share/fzf/key-bindings.zsh
[ -f /usr/share/fzf/completion.zsh ] && source /usr/share/fzf/completion.zsh

# really important or emacs will freak out
export TERM=xterm

export TERMINFO="$HOME/.terminfo"
infocmp -I -r -T rxvt-unicode-256color > $TERMINFO
export TERMCAP="$HOME/.termcap"
infocmp -C -r -T rxvt-unicode-256color > $TERMCAP

eval $(opam config env)
