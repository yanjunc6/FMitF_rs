#!/bin/bash

dir=$(dirname "$0")
echo ${dir}

if ! [ -f "${dir}/id_rsa.pub" ]; then
    if ! [ -f "$HOME/.ssh/id_rsa.pub" ]; then
        echo "You do not have a public SSH key. Please generate one! (ssh-keygen)"
        exit 1
    fi
    cp $HOME/.ssh/id_rsa.pub ${dir}/../
fi

docker build -f ${dir}/Dockerfile -t icnode ${dir}/../

rm -rf ${dir}/../id_rsa.pub
