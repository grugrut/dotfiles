#!/bin/bash

NOW=${PWD}
cd ~
## make directory scaffold
mkdir -p bin
mkdir -p src/github.com/grugrut

## install packages
sudo ${NOW}/apt.sh

## golang setting
go get -u golang.org/x/lint/golint
go get -u golang.org/x/tools/cmd/gopls

## tools
go get -u github.com/motemen/ghq
git config --global ghq.root ~/src
pip install percol
