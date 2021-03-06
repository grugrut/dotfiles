#!/bin/bash

INSTALL_PACKAGES="
build-essential
fonts-migmix
fonts-powerline
autoconf
texi2html
texinfo
libgtk-3-dev
libwebkit2gtk-4.0-dev
libxpm-dev
libjpeg-dev
libgif-dev
libtiff-dev
gnutls-bin
libgnutls28-dev
libtinfo-dev
libncurses5-dev
mailutils
cmigemo
kubectl
python3-pip
x11-xserver-utils
jq
ruby
ruby-dev
"

apt update
apt upgrade -y 
apt dist-upgrade -y
apt install -y ${INSTALL_PACKAGES} 
