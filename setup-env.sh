# openssh-server for remove connection
sudo apt-get install openssh-server

# curl
sudo apt-get install curl

# add-apt-repository
sudo apt-get install software-properties-common

# git
sudo add-apt-repository ppa:git-core/ppa
sudo apt-get update
apt-cache policy git
sudo apt-get install git
git config --global user.email "xfcjscn@gmail.com"
git config --global user.name "Shark"

# apps
git clone https://github.com/xfcjscn/apps.git
# used by clatrix which useing native BLAS
sudo apt-get install libgfortran3
# gpg default key used by remote deploy
gpg --gen-key

# used by emacs installation
sudo apt-get install make
sudo apt-get install gcc
sudo apt-get install libncurses-dev
# emacs X11 dependency
sudo apt-get install libgtk-3-dev libjpeg-dev libgif-dev libtiff-dev libxpm-dev

# emacs
curl -O https://mirrors.ustc.edu.cn/gnu/emacs/emacs-25.3.tar.gz
tar -xf emacs-25.3.tar.gz
cd emacs-25.3
./config
make
sudo make install
emacs -batch -f package-refresh-contents
cd ~/.emacs.d
ln -s /home/shark/apps/resources/emacs/init.el 

# jdk8
sudo add-apt-repository ppa:openjdk-r/ppa
sudo apt-get update
sudo apt-get install openjdk-8-jdk
# fix ssl connection bug: java.security.InvalidAlgorithmParameterException: the trustAnchors parameter must be non-empty
sudo /var/lib/dpkg/info/ca-certificates-java.postinst configure

# boot
mkdir ~/bin
sudo bash -c 'cd ~/bin && curl -fLo boot https://github.com/boot-clj/boot-bin/releases/download/latest/boot.sh && chmod 755 boot && ./boot'

# leiningen for build 3rd part project
sudo bash -c 'cd ~/bin && curl -fLO https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein && chmod 755 lein && ./lein'

# racket
sudo add-apt-repository ppa:plt/racket
sudo apt-get update
sudo apt-get install racket

# mysql
sudo apt-get install mysql-server

# lantern
curl -fLO https://raw.githubusercontent.com/getlantern/lantern-binaries/master/lantern-installer-64-bit.deb
sudo dpkg -i lantern-installer-64-bit.deb


# tty unicode support (show chinese char) via wrapper
sudo apt-get install language-pack-zh-hans
sudo apt-get install fbterm
sudo sh -c "echo 'zh_CN.UTF-8 UTF-8' >> /var/lib/locales/supported.d/local"
sudo apt-get install ttf-wqy-zenhei ttf-wqy-microhei fonts-arphic-ukai fonts-arphic-uming
sudo fbterm

# gui
## xserver
sudo apt-get install xserver-xorg
## xclient
echo 'export DISPLAY=:0' >> ~/.profile

# firefox
sudo apt-get install firefox

# desktop
sudo apt-get install ubuntu-desktop


# input method (WARNING: NOT WORK)
sudo apt-get install fcitx
sudo apt-get install fcitx-frontend-fbterm
sudo apt-get install fcitx-table-wbpy
sudo sed -i.sedbk '/input-method=/ s/=.*/=fcitx-fbterm/' ~/.fbtermrc
sudo sed -i.sedbk '/text-encodings=/ s/=.*/=utf-8/' ~/.fbtermrc 
sudo apt-get install im-switch
sudo apt-get install dbus-x11
