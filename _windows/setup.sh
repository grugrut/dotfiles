#!/bin/bash

NOW=${PWD}
cd ~
## make directory scaffold
mkdir -p bin
mkdir -p src/github.com/grugrut

## apt
curl -s https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key add -
echo "deb https://apt.kubernetes.io/ kubernetes-xenial main" | sudo tee -a /etc/apt/sources.list.d/kubernetes.list

## install packages
sudo ${NOW}/apt.sh

## golang setting
go get -u golang.org/x/lint/golint
go get -u golang.org/x/tools/cmd/gopls
go get -u github.com/golang/dep/cmd/dep

## tools
go get -u github.com/motemen/ghq
git config --global ghq.root ~/src
pip3 install percol
