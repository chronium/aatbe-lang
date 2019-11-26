FROM rustlang/rust:nightly

ARG USERNAME=aatbe
ARG USER_UID=1000
ARG USER_GID=$USER_UID

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update

RUN apt-get install -y --no-install-recommends apt-utils dialog 2>&1
RUN apt-get install -y git iproute2 procps lsb-release
RUN apt-get install -y lldb
RUN apt-get install -y libllvm6.0 llvm-6.0 llvm-6.0-dev llvm-6.0-runtime llvm-6.0-tools

RUN ln -s /usr/bin/llvm-config-6.0 /usr/local/bin/llvm-config

RUN rustup update
RUN rustup component add rls rust-analysis rust-src rustfmt

RUN groupadd --gid $USER_GID $USERNAME
RUN useradd -s /bin/bash --uid $USER_UID --gid $USER_GID -m $USERNAME

RUN apt-get install -y sudo
RUN echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME
RUN chmod 0440 /etc/sudoers.d/$USERNAME

RUN apt-get autoremove -y
RUN apt-get clean -y
RUN rm -rf /var/lib/apt/lists/*

ENV DEBIAN_FRONTEND=dialog
