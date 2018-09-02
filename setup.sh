#!/bin/bash

$(cd $(dirname $0) && pwd)
ln -s .emacs.d ${HOME}/.emacs.d
ln -s .config ${HOME}/.config
ln -s .bashrc ${HOME}/.bashrc
