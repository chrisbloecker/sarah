FROM debian:jessie

RUN apt-get update && \
    apt-get install -y libgmp10 libmysqlclient-dev && \
    apt-get clean autoclean

ADD sarah-persist-exe /

EXPOSE 80

CMD ["/sarah-persist-exe"]
