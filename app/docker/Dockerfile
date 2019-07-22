FROM ubuntu:19.04

RUN apt-get update && apt-get install --assume-yes wget unixodbc-dev
COPY app/docker/odbcinst.ini /etc/odbcinst.ini
RUN cd /tmp && \
    wget https://dev.mysql.com/get/Downloads/Connector-ODBC/8.0/mysql-connector-odbc-8.0.16-linux-ubuntu19.04-x86-64bit.tar.gz && \
    tar -xzf mysql-connector-odbc-8.0.16-linux-ubuntu19.04-x86-64bit.tar.gz && \
    cp mysql-connector-odbc-8.0.16-linux-ubuntu19.04-x86-64bit/lib/* /usr/lib/x86_64-linux-gnu/odbc/ && \
    rm -rf /tmp/*

COPY app/ /opt/app/app/
COPY .stack-work/install/x86_64-linux-tinfo6/lts-13.28/8.6.5/bin/nlidb-bo-exe /opt/app/server
WORKDIR /opt/app

ENTRYPOINT ["/opt/app/server"]
