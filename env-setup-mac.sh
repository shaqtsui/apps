# Homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"


# Xcode (contains git)
# currently only in app store


# Homebrew Commands:
brew cask install lantern
# Not recommand, as safri provide more functionality & power efficient in mac
#brew cask install google-chrome
brew cask install java
brew install clojure
## used to download java sources via: mvn dependency:sources, which can be used by cider to look for java sources
brew install maven
# boot need to copy src to tmp each time it run, I don't want too much useless io, will harm my mac
#brew install boot-clj
# used by luminus
brew install leiningen

# db
brew install postgresql

brew install emacs --with-cocoa

# benchmark, geekbench is not free
brew cask install cinebench

# logitech mouse advanced driver & setting tool
brew cask install homebrew/cask-drivers/logitech-options

# flash plugin for safri
brew cask install flash-npapi

# xun lei
brew cask install thunder
# qq for remote assist
brew cask install qq
# for remote assist
brew cask install teamviewer

# system mornitor
## not a good solution, as it keep running & show in menu
brew cask install istat-menus
brew cask install intel-power-gadget

# serial crackers
## serial box 2
### iserial reader - font is blur
### serialseeker - seems a good serial box 2 reader

## KCNScrew - different databse with serial box 2, also support patches


# network location
# mac connect to mi wifi via samba, steps to do this: 1, enable samba service in wifi. 2, grant whole disk access permission in wifi manage client. 3, if not auto detected & connected, connect via command-k input path: smb://miwifi.com



