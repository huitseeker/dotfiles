# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="tonotdo"

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
plugins=(git git-extras git-prompt github mvn archive archlinux rails ruby colorize scala gem docker fasd emoji)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
autoload -U z

export JAVA_HOME="/usr/lib/jvm/default/"
export _JAVA_OPTIONS="-DJINTEGRA_NATIVE_MODE -DJINTEGRA_COINIT_VALUE=0 -Dsun.io.useCanonCaches=false -XX:ThreadPriorityPolicy=42 -XX:CompileThreshold=1500 -XX:+TieredCompilation -XX:+UnlockExperimentalVMOptions -XX:+UseBiasedLocking -Xverify:none -XX:UseSSE=3 -XX:+UseThreadPriorities -Djava.net.preferIPv4Stack=true -XX:-UseLargePages -XX:+OptimizeStringConcat -XX:+UseFastAccessorMethods -XX:+UseCompressedOops -XX:+AggressiveOpts -XX:ReservedCodeCacheSize=512m -XX:LargePageSizeInBytes=2m -XX:+CMSClassUnloadingEnabled -Xms512M -Xmx7168M -Xss1M -Dawt.useSystemAAFontSettings=lcd -Dsun.java2d.xrender=true"
export GNOME_DESKTOP_SESSION_ID=this-is-deprecated

# Locale settings (utf-8)
export LC_CTYPE=en_US.UTF-8
export LANG=en_US.UTF-8

# export PERL_LOCAL_LIB_ROOT="/home/huitseeker/perl5";
# export PERL_MB_OPT="--install_base /home/huitseeker/perl5";
# export PERL_MM_OPT="INSTALL_BASE=/home/huitseeker/perl5";
# export PERL5LIB="/home/huitseeker/perl5/lib/perl5/x86_64-linux-thread-multi:/home/huitseeker/perl5/lib/perl5";
# export PATH="/home/huitseeker/perl5/bin:$PATH";
export PATH="/home/huitseeker/.gem/ruby/2.4.0/bin:$PATH"

source $HOME/.aliases
source $HOME/.profile
export JAVA_HOME=/usr/lib/jvm/default/
DISABLE_UPDATE_PROMPT=true

# PATH="/home/huitseeker/perl5/bin${PATH:+:${PATH}}"; export PATH;
# PERL5LIB="/home/huitseeker/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
# PERL_LOCAL_LIB_ROOT="/home/huitseeker/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
# PERL_MB_OPT="--install_base \"/home/huitseeker/perl5\""; export PERL_MB_OPT;
# PERL_MM_OPT="INSTALL_BASE=/home/huitseeker/perl5"; export PERL_MM_OPT;

autoload bashcompinit
bashcompinit
export LIBND4J_HOME=~/DL4J/libnd4j
eval "$(ntfy shell-integration)"

export GPG_TTY=$(tty)
export SKIL_HOME=/opt/skil
export SKIL_CLASS_PATH="/opt/skil/lib/*"
export JAVA_HOME=/usr/lib/jvm/default
export TERM=xterm

PATH="/home/huitseeker/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/home/huitseeker/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/huitseeker/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/huitseeker/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/huitseeker/perl5"; export PERL_MM_OPT;

# GO config
export GOPATH=$HOME/golang
export GOROOT=/lib/go
export PATH=$GOPATH/bin:$PATH
export PATH=$GOROOT/bin:$PATH

# OPAM configuration
. /home/huitseeker/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# dotfiles config
alias config='/usr/bin/git --git-dir=/home/huitseeker//.cfg/ --work-tree=/home/huitseeker/'
