FROM nicferrier/elnode-and-nodejs
MAINTAINER nic@ferrier.me.uk
USER emacs
WORKDIR /home/emacs
RUN git clone https://github.com/nicferrier/elnode-linky.git
WORKDIR /home/emacs/elnode-linky
# RUN npm install .   ### don't need node yet!
EXPOSE 8005
VOLUME /home/emacs/elnode-linky/db
ENV ETAG 20140816213427254185764
CMD /usr/local/emacs/bin/emacs -daemon -l linky.el ; tail -f /dev/null
