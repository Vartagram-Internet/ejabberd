FROM debian:bullseye

ENV DEBIAN_FRONTEND=noninteractive

# Install system tools and dependencies
RUN apt-get update && apt-get install -y \
    curl git unzip build-essential autoconf libtool \
    libssl-dev libexpat1-dev libyaml-dev libsqlite3-dev \
    libncurses-dev ca-certificates gnupg \
    zlib1g-dev procps \
    sudo vim locales



# Set up locale
RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen && locale-gen

# Create non-root user
RUN useradd -ms /bin/bash vscode && echo "vscode ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

USER vscode
WORKDIR /home/vscode

# Install ASDF version manager
RUN git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.14.0

ENV PATH="/home/vscode/.asdf/bin:/home/vscode/.asdf/shims:$PATH"
RUN echo '. "$HOME/.asdf/asdf.sh"' >> ~/.bashrc

# Preinstall Erlang & Rebar3 plugins
RUN bash -c ". $HOME/.asdf/asdf.sh && \
    asdf plugin add erlang https://github.com/asdf-vm/asdf-erlang.git && \
    asdf plugin add rebar && \
    asdf install erlang 26.2.2 && \
    asdf global erlang 26.2.2 && \
    asdf install rebar latest && \
    asdf global rebar latest"

