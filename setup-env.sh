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

# curl
sudo apt-get install curl

# used by emacs installation
sudo apt-get install make
sudo apt-get install gcc
sudo apt-get install libncurses-dev

# emacs
curl -O https://mirrors.ustc.edu.cn/gnu/emacs/emacs-25.3.tar.g
tar -xf emacs-25.3.tar.gz
cd emacs-25.3
./config
make
sudo make install
cd ~/.emacs.d
ln -s /home/shark/apps/resources/emacs/init.el 

# tty unicode support (show chinese char) via wrapper
sudo apt-get install language-pack-zh-hans
sudo apt-get install fbterm
sudo sh -c "echo 'zh_CN.UTF-8 UTF-8' >> /var/lib/locales/supported.d/local"
sudo apt-get install ttf-wqy-zenhei ttf-wqy-microhei fonts-arphic-ukai fonts-arphic-uming
sudo fbterm


# input method
sudo apt-get install fcitx
sudo apt-get install fcitx-frontend-fbterm
sudo apt-get install fcitx-table-wbpy
sed -i.sedbk '/input-method=/ s/=.*/=fcitx-fbterm/' ~/.fbtermrc 
im-config


# gui
## xserver
sudo apt-get install xserver-xorg
## xclient
echo 'export DISPLAY=:0' >> ~/.profile

# firefox
sudo apt-get install firefox

# openssh-server for remove connection
sudo apt-get install openssh-server


# desktop
sudo apt-get install --no-install-recommends ubuntu-desktop

# jdk8
sudo add-apt-repository ppa:openjdk-r/ppa
sudo apt-get update
sudo apt-get install openjdk-8-jdk

# boot
sudo bash -c 'cd ~/bin && curl -fLo boot https://github.com/boot-clj/boot-bin/releases/download/latest/boot.sh && chmod 755 boot'

# lantern
curl -fLO https://raw.githubusercontent.com/getlantern/lantern-binaries/master/lantern-installer-64-bit.deb
sudo dpkg -i lantern-installer-64-bit.deb


