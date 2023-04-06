FROM ubuntu:18.04

USER root
ENV TZ=Etc/UTC
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && \
  echo $TZ > /etc/timezone

RUN apt-get update && \
  apt-get install -y --no-install-recommends \
  gnupg \
  dirmngr \
  apt-transport-https \
  ca-certificates \
  net-tools \
  software-properties-common \
  sudo \
  curl \
  htop \
  wget \
  zip \
  unzip \
  locales \
  less \
  netcat \
  lftp

RUN locale-gen en_US.UTF-8

# [LOCAL] Adding Ubuntu' & Postgres' GPG keys to the keyring.
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 7FCC7D46ACCC4CF8 && \
  apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 561F9B9CAC40B2F7 && \
  apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 7FCC7D46ACCC4CF8 && \
  add-apt-repository ppa:git-core/ppa

# [LOCAL] Updating local apt cache
RUN apt-get clean && apt-get update && apt-get -y install --no-install-recommends \
  git \
  libyaml-dev \
  make clang gcc  \
  libicu-dev libblas-dev liblapack-dev libgmp3-dev xz-utils \
  libbz2-dev \
  nano \
  openssh-client \
  passenger \
  time \
  rsync

RUN addgroup --gid 1001 vl && \
    adduser  --disabled-password --gecos "" --uid 1001 --gid 1001 vl && \
    echo 'vl ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

USER vl

RUN mkdir -p /home/vl/.ghcup/bin && \
  cd /home/vl/.ghcup/bin && \
  wget -O ghcup https://downloads.haskell.org/~ghcup/0.1.18.0/x86_64-linux-ghcup-0.1.18.0 && \
  chmod +x ghcup
RUN /home/vl/.ghcup/bin/ghcup install ghc 8.10.7 && \
  /home/vl/.ghcup/bin/ghcup install cabal && \
  /home/vl/.ghcup/bin/ghcup install stack && \
  /home/vl/.ghcup/bin/ghcup install hls
ENV PATH="$PATH:/home/vl/.ghcup/bin"