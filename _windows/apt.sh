#!/bin/bash

INSTALL_PACKAGES="
build-essential
fonts-migmix
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
"

apt update
apt upgrade -y 
apt dist-upgrade -y
apt install -y ${INSTALL_PACKAGES} 
