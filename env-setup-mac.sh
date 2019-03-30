# Homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"


# Xcode (contains git)
# currently only in app store


# Homebrew Commands:
brew cask install lantern
# Not recommand, as safri provide more functionality & power efficient in mac
#brew cask install google-chrome
# for video downloadhelper, as this plugin is blocked in chrome
brew cask install firefox
# stream downloader, downloadhelper addionaly need external apps installed in MAC, youtube-dl is complex than you-get

# usage, list formates: youtube-dl -F https://www.youtube.com/playlist?list=PLun8-Z_lTkC5HAjzXCLEx0gQkJZD4uCtJ
# usage, only show info without download: youtube-dl --get-filename https://www.youtube.com/playlist?list=PLun8-Z_lTkC5HAjzXCLEx0gQkJZD4uCtJ
# usage, only show info without download: youtube-dl --get-format https://www.youtube.com/playlist?list=PLun8-Z_lTkC5HAjzXCLEx0gQkJZD4uCtJ
# usage: youtube-dl --download-archive archive.txt -f 'bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best' https://www.youtube.com/playlist?list=PLun8-Z_lTkC5HAjzXCLEx0gQkJZD4uCtJ
# prefer mp4, as quicktime doesn't support webm
# usage: youtube-dl --download-archive ../archive.txt -f 'bestvideo[ext=mp4]+bestaudio[ext=m4a]/bestvideo+bestaudio/best[ext=mp4]/best' https://www.youtube.com/playlist?list=PLun8-Z_lTkC6qJF1sVh3_Hx7aL6FPd0IN
brew install youtube-dl
# merge video & audio, as youtube seprate video & audio for movies better quality than 1080p, it will invoked by youtube-dl
brew install ffmpeg

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

# download manager
brew install aria2
# xun lei (Afraid of hack)
# brew cask install thunder

# qq for remote assist(Can not control qq in win, and afraid of hack)
# brew cask install qq
# for remote assist
brew cask install teamviewer
# media player as quickplayer support limited media types
brew cask install vlc

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



