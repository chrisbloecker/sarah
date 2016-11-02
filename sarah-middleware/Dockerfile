FROM resin/rpi-raspbian:jessie-20161026

RUN apt-get update && \
    apt-get install libgmp10 && \
    apt-get clean autoclean

ADD sarah-middleware-exe /

CMD ["/sarah-middleware-exe"]
