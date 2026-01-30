#!/bin/bash

MYUSER=`whoami`

# Docker - https://docs.docker.com/engine/install/ubuntu/
sudo apt-get update
sudo apt-get install \
    ca-certificates \
    curl \
    gnupg \
    lsb-release

sudo mkdir -m 0755 -p /etc/apt/keyrings
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg
echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
  $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt-get update
sudo apt-get -y install docker-ce docker-ce-cli containerd.io
sudo adduser $MYUSER docker

sudo apt -y install openvswitch-switch make

# Go 1.20.8
cd ~/
wget https://golang.org/dl/go1.20.8.linux-amd64.tar.gz
tar xf go1.20.8.linux-amd64.tar.gz
mkdir gopath
GOROOT=`pwd`/go
GOPATH=`pwd`/gopath
echo "" >> /home/$MYUSER/.bashrc
echo "export GOROOT=$GOROOT" >> /home/$MYUSER/.bashrc
echo "export GOPATH=$GOPATH" >> /home/$MYUSER/.bashrc
echo "export PATH=$PATH:$GOROOT/bin" >> /home/$MYUSER/.bashrc

echo "*** Please log out or reboot your system!"