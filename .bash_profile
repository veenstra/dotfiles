# .bash_profile
#
# based on a chunk of jpb's bash profile.
#
# Load aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs

PATH=$HOME/bin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/local/sbin:$PATH:.
PATH=$PATH:$HOME/packages/rsense-0.3/bin

# Conditional additions
if [ -d /Developer/Tools ];then
    export PATH=$PATH:/Developer/Tools:/Developer/usr/bin:/Developer/usr/sbin
fi

BASH_ENV=$HOME/.bashrc
ENV=$HOME/.bashrc

if [ -d ~/.bash_completion.d ]; then
    for c in ~/.bash_completion.d/*; do
        . "$c"
    done
fi
. ~/bin/z.sh
#export CDPATH=".:~:~/repos/ooyala/ofe:~/repos"

export BASH_ENV ENV PATH PS1 DISPLAY

# from macosxhints.com
cdf() # cd's to frontmost window of Finder
{
    cd "`osascript -e 'tell application "Finder"' \
        -e 'set myname to POSIX path of (target of window 1 as alias)' \
        -e 'end tell' 2>/dev/null`"
}


if [ $(ssh-add -l | grep -c "The agent has no identities." ) -eq 1 ]; then
    if [ -f /mach_kernel ]; then
        ssh-add -k
    fi
fi

# Setup bash history options
# export HISTCONTROL=erasedups
#export HISTCONTROL='ignoreboth'
export HISTIGNORE="&:ls:[bf]g:exit"
export HISTSIZE=10000
export HISTTIMEFORMAT='%b %d %H:%M:%S: '
shopt -s histappend
set cmdhist

bind "set completion-ignore-case on"
shopt -s cdspell

if [ -f /opt/local/etc/bash_completion ]; then
  . /opt/local/etc/bash_completion
fi

export OOYALA_CODE_ROOT=$HOME/repos/ooyala
export BACKLOT_CODE_ROOT=$HOME/repos/backlot
# hadoop/youtube_exporter/dist/Capfile needs OOYALA_REPO set
export OOYALA_REPO=$OOYALA_CODE_ROOT
export OO_DEPLOY_DIR=$HOME/ooyala_private
# Don't need this anymore
#export RUBYLIB=${OOYALA_CODE_ROOT}/vendor/thrift/lib/rb/lib:${OOYALA_CODE_ROOT}/vendor/thrift/lib/rb/ext-darwin-i686
export RUBYOPT="-I. -rubygems"
PATH=$OOYALA_CODE_ROOT/ofe/tools/smithy/smithy-bin:$PATH
export FLEX_HOME=/Applications/Adobe\ Flex\ Builder\ 3/sdks/flex_sdk_4.1.0.16076
PATH=$FLEX_HOME/bin:$PATH

# ====================
# EDITOR and  PAGER
# ====================
export EDITOR=vim
export PAGER="less -eiMRswX"
export LESS="-eiMRswX"
export MANPAGER=$PAGER

# ====================
# LS Colors
# ====================
export LSCOLORS=exfxcxdxbxegedabagacad
export LS_COLORS="ex=31:di=36:ln=35"
export LS_COMMON=-G

# ====================
# Bash Completion
# ====================
if [ -f /opt/local/etc/bash_completion ]; then
  . /opt/local/etc/bash_completion
fi

# ====================
# PROMPT
# ====================
WHITE="\[\033[0;38m\]"
GREEN="\[\033[0;32m\]"
CYAN="\[\033[0;36m\]"
GRAY="\[\033[0;37m\]"
BLUE="\[\033[0;34m\]"
BLACK="\[\033[0;30m\]"
PURPLE="\[\033[0;35m\]"
RED="\[\033[0;31m\]"

DARK_GREEN="\[\033[1;32m\]"
DARK_CYAN="\[\033[1;36m\]"
DARK_BLUE="\[\033[1;34m\]"

#PS1='\[\033[1;34m\]\w:\[\033[0m\]\n$ '

prompt_simple() {
  unset PROMPT_COMMAND
  PS1="\W\\$ "
  PS2="> "
}

function prompt_fancy {
  # Shows user@host in the title
  unset PROMPT_COMMAND

  # To change the title bar, uncomment the following line
  #TITLEBAR="\[\033]0;\u@\h\007\]"

  # Shows a "*" next to the branch name if you have un-staged local changes
  # Shows a "+" next to the branch name if you have staged local changes
  export GIT_PS1_SHOWDIRTYSTATE=1
  # Shows a "$" next to the branch name if you have stashed changes
  export GIT_PS1_SHOWSTASHSTATE=1
  # Shows a "%" next to the branch name if you have untracked files
  export GIT_PS1_SHOWUNTRACKEDFILES=1
  # Put it all together
  if type -p __git_ps1; then
    PS1="${DARK_BLUE}\w${DARK_GREEN}"'$(__git_ps1 "(%s)")'"${WHITE}\n\\$ "
  else
    PS1="${DARK_BLUE}\w$${WHITE}\n\\$ "
  fi
  PS2="> "
}
prompt_fancy

# for rvm
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

