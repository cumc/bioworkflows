FROM node:12.18.2-alpine
LABEL maintainer "Gao Wang"

RUN apk update && apk upgrade && \
    echo @edge http://nl.alpinelinux.org/alpine/edge/community >> /etc/apk/repositories && \
    echo @edge http://nl.alpinelinux.org/alpine/edge/main >> /etc/apk/repositories && \
    echo @edge http://nl.alpinelinux.org/alpine/edge/testing >> /etc/apk/repositories && \
    apk add --no-cache \
      grep \
      chromium@edge \
      freetype@edge \
      libstdc++@edge \
      harfbuzz@edge \
      wqy-zenhei@edge \
      ttf-liberation@edge \
      font-noto-devanagari@edge \
      font-noto-arabic@edge \
      font-noto-bengali@edge \
      font-ipa@edge \
      nss@edge

RUN mkdir -p /opt/marp/app
ADD https://github.com/marp-team/marp-cli/archive/v0.21.0.tar.gz /opt/marp/v0.21.0.tar.gz
RUN cd /opt/marp && tar zxvf v0.21.0.tar.gz && mv marp-cli-0.21.0 /opt/marp/.cli && rm -f v0.21.0.tar.gz

RUN addgroup -S marp && adduser -S -g marp marp \
    && chown -R marp:marp /opt/marp

USER marp
ENV IS_DOCKER true

WORKDIR /opt/marp/.cli
RUN yarn add puppeteer-core@chrome-$(chromium-browser --version | sed -r 's/^Chromium ([0-9]+).+$/\1/')
RUN yarn install && yarn build && rm -rf ./src ./node_modules && yarn install --production && yarn cache clean

WORKDIR /opt/marp/app
CMD ["sh"]
