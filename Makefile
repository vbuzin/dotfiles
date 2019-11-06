GRAY    := '\033[1;90m'
NC      := '\033[0m'

message  = $(GRAY)">>> ===== "$(1)" ====="$(NC)

all: _brew _apps _git _emacs _rust _haskell _zsh
.PHONY: all list $(MAKECMDGOALS)

# Installing utils and apps via Homebrew
# ==============================================================================
APPS_SOURCE_DIR := $(abspath ./apps)

GPG_SOURCE_DIR := $(abspath ./gnupg)
GPG_TARGET_DIR := $(abspath $(HOME)/.gnupg)

HTOP_SOURCE_DIR := $(abspath ./htop)
HTOP_TARGET_DIR := $(abspath $(HOME)/.config/htop/)

SCREEN_SOURCE_DIR := $(abspath ./screen)
SCREEN_TARGET_DIR := $(HOME)

_apps: _brew | $(GPG_TARGET_DIR) $(HTOP_TARGET_DIR)
	@echo $(call message,"Installing utils and apps via Homebrew")
	@brew bundle --file=$(APPS_SOURCE_DIR)/Brewfile

	@echo $(call message,"Setting up configuration files for GnuPG")
	ln -sf $(GPG_SOURCE_DIR)/gpg.conf       $(addsuffix /,$(GPG_TARGET_DIR))
	ln -sf $(GPG_SOURCE_DIR)/gpg-agent.conf $(addsuffix /,$(GPG_TARGET_DIR))

	@echo $(call message,"Setting up configuration files for htop")
	ln -sf $(HTOP_SOURCE_DIR)/htoprc $(addsuffix /,$(HTOP_TARGET_DIR))

	@echo $(call message,"Setting up configuration files for screen")
	ln -sf $(SCREEN_SOURCE_DIR)/screenrc $(addsuffix /.screenrc,$(SCREEN_TARGET_DIR))

$(GPG_TARGET_DIR):
	@echo "Creating directory $(GPG_TARGET_DIR)"
	@mkdir -p $@
	@chmod 700 $(GPG_TARGET_DIR)

$(HTOP_TARGET_DIR):
	@echo "Creating directory $(HTOP_TARGET_DIR)"
	@mkdir -p $@

# installing Homebrew if not installed, otherwise updating
# ==============================================================================
_brew:
ifneq (,$(shell which brew))
	@echo $(call message,"Updating Homebrew")
	@brew update && brew upgrade && brew cask upgrade
else
	@echo $(call message,"Installing Homebrew")
	@ruby -e "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
endif

# Emacs
# ==============================================================================
EMACS_SOURCE_DIR := $(abspath ./emacs)
EMACS_TARGET_DIR := $(abspath $(HOME)/.emacs.d)

_emacs: _brew | $(EMACS_TARGET_DIR)
	@echo $(call message,"Installing required packages via Homebrew")
	@brew bundle --file=$(EMACS_SOURCE_DIR)/Brewfile

	@echo $(call message,"Setting up configuration files for emacs")
	ln -sf $(EMACS_SOURCE_DIR)/init-frames.el $(addsuffix /,$(EMACS_TARGET_DIR))
	ln -sf $(EMACS_SOURCE_DIR)/init-keys.el   $(addsuffix /,$(EMACS_TARGET_DIR))
	ln -sf $(EMACS_SOURCE_DIR)/init-misc.el   $(addsuffix /,$(EMACS_TARGET_DIR))
	ln -sf $(EMACS_SOURCE_DIR)/init-org.el    $(addsuffix /,$(EMACS_TARGET_DIR))
	ln -sf $(EMACS_SOURCE_DIR)/init-pkg0.el   $(addsuffix /,$(EMACS_TARGET_DIR))
	ln -sf $(EMACS_SOURCE_DIR)/init-pkg1.el   $(addsuffix /,$(EMACS_TARGET_DIR))
	ln -sf $(EMACS_SOURCE_DIR)/init.el        $(addsuffix /,$(EMACS_TARGET_DIR))
	ln -sf $(EMACS_SOURCE_DIR)/snippets       $(addsuffix /,$(EMACS_TARGET_DIR))

$(EMACS_TARGET_DIR):
	@echo "Creating directory $(EMACS_TARGET_DIR)"
	@mkdir -p $@

# Git
# ==============================================================================
GIT_SOURCE_DIR := $(abspath ./git)
GIT_TARGET_DIR := $(HOME)
HOSTNAME       := $(shell hostname)

_git:
	@echo $(call message,"Configuring git")
ifeq ($(HOSTNAME),v8vmacp)
	ln -sf $(GIT_SOURCE_DIR)/gitconfig.pers $(addsuffix /.gitconfig,$(GIT_TARGET_DIR))
endif
	ln -sf $(GIT_SOURCE_DIR)/gitignore $(addsuffix /.gitignore,$(GIT_TARGET_DIR))

# Haskell
# ==============================================================================
HASKELL_SOURCE_DIR := $(abspath ./haskell)
GHC_TARGET_DIR     := $(abspath $(HOME)/.ghc)
STACK_TARGET_DIR   := $(abspath $(HOME)/.stack)

_haskell: _brew | $(GHC_TARGET_DIR) $(STACK_TARGET_DIR)
	@echo $(call message,"Installing/configuring haskell stack and utils")
	@brew install haskell-stack
	@stack setup #install the GHC compiler
	@stack install hindent hlint hoogle
	ln -sf $(HASKELL_SOURCE_DIR)/ghci.conf   $(addsuffix /,$(GHC_TARGET_DIR))
	ln -sf $(HASKELL_SOURCE_DIR)/config.yaml $(addsuffix /,$(STACK_TARGET_DIR))

$(GHC_TARGET_DIR):
	@echo "Creating directory $(GHC_TARGET_DIR)"
	@mkdir -p $@

$(STACK_TARGET_DIR):
	@echo "Creating directory $(STACK_TARGET_DIR)"
	@mkdir -p $@

# Mu for brave and true
# ==============================================================================
MU_SOURCE_DIR := $(abspath ./mu)

MAIL_TARGET_DIR1 := $(abspath $(HOME)/Mail/v8v.buzin@gmail.com)
MAIL_TARGET_DIR2 := $(abspath $(HOME)/Mail/v.buzin@icloud.com)

_mu: _emacs | $(MAIL_TARGET_DIR1) $(MAIL_TARGET_DIR2)
	@echo $(call message,"Installing isync and mu")
	@brew install mu isync

	ln -sf $(MU_SOURCE_DIR)/mbsyncrc $(addsuffix /.mbsyncrc,$(HOME))

	@echo "Synchronising mailbox(es) and indexing via mu"
	@mbsync --all --verbose
	@mu index --maildir=$(MAIL_TARGET_DIR1)
	@mu index --maildir=$(MAIL_TARGET_DIR2)

$(MAIL_TARGET_DIR1):
	@echo "Creating directory $(MAIL_TARGET_DIR1)"
	@mkdir -p $@

$(MAIL_TARGET_DIR2):
	@echo "Creating directory $(MAIL_TARGET_DIR2)"
	@mkdir -p $@

# Rust
# ==============================================================================
_rust: _brew
	@echo $(call message,"Installing/configuring rust and utils")
	@brew install rustup-init
	@rustup-init -y --no-modify-path --default-toolchain stable
	@rustup toolchain add nightly
	@cargo +nightly install racer
	@rustup component add rust-src # required for racer
	@rustup component add rustfmt

# zsh
# ==============================================================================
CSHELL         := $(shell dscl . -read ~/ UserShell | sed 's/.*\/\(.*\)$$/\1/')
ZSH_SOURCE_DIR := $(abspath ./zsh)
ZSH_TARGET_DIR := $(HOME)
ZPREZTO_DIR    := $(ZSH_SOURCE_DIR)

_zsh:
	@echo $(call message,"Configuring zsh")
ifneq ($(CSHELL),zsh)
	@echo "Changing your shell to zsh"
	@chsh -s /bin/zsh
endif

	@echo $(call message,"Updating https://github.com/sorin-ionescu/prezto and submodules")
	git submodule update --init --recursive --force zsh/zprezto

	@echo $(call message,"Setting up configuration files for zsh")
	ln -sf $(ZSH_SOURCE_DIR)/zprezto   $(addsuffix /.zprezto,$(ZSH_TARGET_DIR))
	ln -sf $(ZSH_SOURCE_DIR)/zprofile  $(addsuffix /.zprofile,$(ZSH_TARGET_DIR))
	ln -sf $(ZSH_SOURCE_DIR)/zshrc     $(addsuffix /.zshrc,$(ZSH_TARGET_DIR))
	ln -sf $(ZSH_SOURCE_DIR)/zpreztorc $(addsuffix /.zpreztorc,$(ZSH_TARGET_DIR))
