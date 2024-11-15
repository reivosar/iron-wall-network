ARG NGINX_VERSION
FROM nginx:${NGINX_VERSION}

RUN apt-get update && \
    apt-get install -y git build-essential libpcre3 libpcre3-dev libssl-dev zlib1g-dev \
    libxml2 libxml2-dev libyajl-dev libgeoip-dev wget \
    libtool automake autoconf pkg-config curl && \
    apt-get clean

RUN curl -L -o /usr/local/bin/mkcert https://github.com/FiloSottile/mkcert/releases/download/v1.4.4/mkcert-v1.4.4-linux-amd64 && \
    chmod +x /usr/local/bin/mkcert

RUN mkdir -p /etc/ssl/certs /etc/ssl/private && \
    mkcert -install && \
    mkcert -cert-file /etc/ssl/certs/nginx-selfsigned.crt -key-file /etc/ssl/private/nginx-selfsigned.key localhost

RUN git clone --depth 1 -b v3/master --single-branch https://github.com/SpiderLabs/ModSecurity /opt/ModSecurity && \
    cd /opt/ModSecurity && \
    git submodule init && \
    git submodule update && \
    ./build.sh && \
    ./configure && \
    make && \
    make install

RUN git clone --depth 1 https://github.com/SpiderLabs/ModSecurity-nginx.git /opt/ModSecurity-nginx

RUN git clone --depth 1 https://github.com/coreruleset/coreruleset.git /usr/local/modsecurity-crs && \
    mv /usr/local/modsecurity-crs/crs-setup.conf.example /usr/local/modsecurity-crs/crs-setup.conf

RUN wget http://nginx.org/download/nginx-${NGINX_VERSION}.tar.gz && \
    tar zxvf nginx-${NGINX_VERSION}.tar.gz && \
    cd nginx-${NGINX_VERSION} && \
    ./configure --with-compat --add-dynamic-module=/opt/ModSecurity-nginx && \
    make modules && \
    cp objs/ngx_http_modsecurity_module.so /etc/nginx/modules/

COPY modsecurity/modsecurity.conf /etc/modsecurity/modsecurity.conf
COPY nginx/nginx.conf /etc/nginx/nginx.conf

RUN echo "load_module modules/ngx_http_modsecurity_module.so;" > /etc/nginx/modules.conf

COPY wait-for-backend.sh /wait-for-backend.sh
RUN chmod +x /wait-for-backend.sh

CMD ["sh", "-c", "/wait-for-backend.sh && nginx -g 'daemon off;'"]
