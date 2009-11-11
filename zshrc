#------------------------------------------------------------------#
# File:     .zshrc   ZSH resource file                             #
# Version:  0.1.16                                                 #
# Author:   Øyvind "Mr.Elendig" Heggstad <mrelendig@har-ikkje.net> #
#------------------------------------------------------------------#

#------------------------------
# History stuff
#------------------------------
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

#------------------------------
# Variables
#------------------------------
export EDITOR="vim"
export PAGER="vimpager"
export PATH="${PATH}:${HOME}/bin"

#-----------------------------
# Dircolors
#-----------------------------
LS_COLORS='rs=0:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:tw=30;42:ow=34;42:st=37;44:ex=01;32:';
export LS_COLORS

#------------------------------
# Keybindings
#------------------------------
bindkey -v
typeset -g -A key
#bindkey '\e[3~' delete-char
bindkey '\e[1~' beginning-of-line
bindkey '\e[4~' end-of-line
#bindkey '\e[2~' overwrite-mode
bindkey '^?' backward-delete-char
bindkey '^[[1~' beginning-of-line
bindkey '^[[5~' up-line-or-history
bindkey '^[[3~' delete-char
bindkey '^[[4~' end-of-line
bindkey '^[[6~' down-line-or-history
bindkey '^[[A' up-line-or-search
bindkey '^[[D' backward-char
bindkey '^[[B' down-line-or-search
bindkey '^[[C' forward-char 
# for rxvt
bindkey "\e[8~" end-of-line
bindkey "\e[7~" beginning-of-line
# for gnome-terminal
bindkey "\eOH" beginning-of-line
bindkey "\eOF" end-of-line

#------------------------------
# Alias stuff
#------------------------------
alias ls="ls -h --group-directories-first --color=always"
alias ll="ls --color -lh"
alias mkdir="mkdir -p"
alias ..="cd .."
alias nrestart="sudo /etc/rc.d/network stop && sudo rm /var/run/dhcpcd.pid && sudo /etc/rc.d/network start && ping google.com"
alias torrent="cd ~/torrents && rtorrent"
alias config="vim ~/.xmonad/xmonad.hs"
alias spm="sudo pacman"
alias spmc="sudo pacman-color"
extract () {
	if [ -f $1 ]; then
		case $1 in
			*.tar.bz2)	tar xjf $1	;;
			*.tar.gz)	tar xzf $1	;;
			*.bz2)		bunzip2 $1	;;
			*.gz)		gzip -d $1	;;
			*.rar)		unrar e $1	;;
			*.tar)		tar xf $1	;;
			*.tbz2)		tar xjf $1	;;
			*.tgz)		tar xzf $1	;;
			*.zip)		unzip $1	;;
			*.7z)		7z x $1		;;
			*)		echo "'$1' cannot be extracted via extract()" ;;
		esac
	else
		echo "'1' is not a valid file"
	fi
}

#------------------------------
# Comp stuff
#------------------------------
zmodload zsh/complist 
autoload -Uz compinit
compinit
zstyle :compinstall filename '${HOME}/.zshrc'

zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'   force-list always

zstyle ':completion:*:*:killall:*' menu yes select
zstyle ':completion:*:killall:*'   force-list always

#------------------------------
# Window title
#------------------------------
case $TERM in
    
*xterm*|rxvt|rxvt-unicode|rxvt-256color|(dt|k|E)term)
		precmd () { print -Pn "\e]0; [%~]\a" } 
		preexec () { print -Pn "\e]0; [%~] ($1)\a" }
	;;
    screen)
    	precmd () { 
			print -Pn "\e]83;title \"$1\"\a" 
			print -Pn "\e]0;$TERM - (%L) [%n@%M]%# [%~]\a" 
		}
		preexec () { 
			print -Pn "\e]83;title \"$1\"\a" 
			print -Pn "\e]0;$TERM - (%L) [%n@%M]%# [%~] ($1)\a" 
		}
	;; 
esac

#------------------------------
# Prompt
#------------------------------
setprompt () {
	# load some modules
	autoload -U colors zsh/terminfo # Used in the colour alias below
	colors
	setopt prompt_subst

	# make some aliases for the colours: (coud use normal escap.seq's too)
	for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
		eval PR_$color='%{$fg[${(L)color}]%}'
	done
	PR_NO_COLOR="%{$terminfo[sgr0]%}"

	# Check the UID
	#if [[ $UID -ge 1000 ]]; then # normal user
	#	eval PR_USER='${PR_WHITE}%#${PR_NO_COLOR}'
	#	eval PR_USER_OP='${PR_WHITE}%#${PR_NO_COLOR}'
	#elif [[ $UID -eq 0 ]]; then # root
	#	eval PR_USER='${PR_RED}%n${PR_NO_COLOR}'
	#	eval PR_USER_OP='${PR_RED}%#${PR_NO_COLOR}'
	#fi	

	# Check if we are on SSH or not  --{FIXME}--  always goes to |no SSH|
	if [[ -z "$SSH_CLIENT"  ||  -z "$SSH2_CLIENT" ]]; then 
		eval PR_HOST='${PR_GREEN}%M${PR_NO_COLOR}' # no SSH
	else 
		eval PR_HOST='${PR_YELLOW}%M${PR_NO_COLOR}' #SSH
	fi
	# set the prompt
	#PS1=$'${PR_CYAN}[${PR_USER}${PR_CYAN}@${PR_HOST}${PR_CYAN}][${PR_BLUE}%~${PR_CYAN}]${PR_USER_OP}'
	PS1=$'${PR_WHITE}[${PR_RED}%~${PR_WHITE}] ${PR_USER_OP}'
	PS2=$'%_>'
}
setprompt

