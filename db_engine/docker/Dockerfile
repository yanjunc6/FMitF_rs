#Dockerfile for ICNODES
FROM ubuntu:22.04
RUN apt update && apt-get -y install tzdata
RUN apt update && apt -y install build-essential git jq libc6 python3-pip \
    iperf wget iputils-ping net-tools libcurl4-openssl-dev libtool m4 automake openssh-server iproute2 psmisc \
    vim python2 dstat unzip nodejs npm curl autoconf make g++  protobuf-compiler redis \
    && pip3 install solc-select \
    && solc-select install 0.4.24 \
    && solc-select use 0.4.24

ENV HOME /root

RUN ssh-keygen -t rsa -N "" -f /root/.ssh/id_rsa && cd /root/.ssh && cp id_rsa.pub authorized_keys
ADD id_rsa.pub /
RUN cat /id_rsa.pub >> ~/.ssh/authorized_keys
RUN echo "StrictHostKeyChecking no" > /root/.ssh/config
RUN echo "PermitUserEnvironment yes" >> /etc/ssh/sshd_config
RUN echo "root:newpass" | chpasswd
RUN service ssh start
CMD ["/usr/sbin/sshd","-D"]

RUN wget https://golang.org/dl/go1.20.8.linux-amd64.tar.gz
RUN rm -rf /usr/local/go && tar -C /usr/local -xzf go1.20.8.linux-amd64.tar.gz

COPY ./go.mod ${HOME}/go.mod
WORKDIR ${HOME}
RUN /usr/local/go/bin/go mod download

COPY . ${HOME}/ShardDB1/
WORKDIR ${HOME}/ShardDB1
RUN /usr/local/go/bin/go run startServer.go > tmp.txt 2>&1 || true