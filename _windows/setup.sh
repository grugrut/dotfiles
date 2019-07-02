#!/bin/bash

NOW=${PWD}
cd ~
## make directory scaffold
mkdir -p bin
mkdir -p src/github.com/grugrut

## install packages
sudo ${NOW}/apt.sh

## golang setting
go get -u github.com/golang/lint/golint
go get -u golang.org/x/tools/cmd/gopls

