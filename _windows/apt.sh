#!/bin/bash

INSTALL_PACKAGES="
build-essential
fonts-migmix
"


apt update
apt upgrade -y 
apt dist-upgrade -y
apt install -y ${INSTALL_PACKAGES} 
