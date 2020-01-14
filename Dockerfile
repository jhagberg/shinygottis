FROM ubuntu:bionic AS myr-base

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
	locales \
	wget \
	ca-certificates \
	gsfonts \
	gnupg2 \
	&& echo "deb http://ppa.launchpad.net/marutter/rrutter3.5/ubuntu bionic main" > /etc/apt/sources.list.d/marutter-ubuntu-rrutter3_5-bionic.list \
	&& apt-key adv --keyserver keyserver.ubuntu.com --recv-keys C9A7585B49D51698710F3A115E25F516B04C661B \
        && apt-get purge -y gnupg2 \
        && apt autoremove -y \        
	&& rm -rf /var/lib/apt/lists/* 

# Configure default locale, see https://github.com/rocker-org/rocker/issues/19
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
	&& locale-gen en_US.utf8 \
	&& /usr/sbin/update-locale LANG=en_US.UTF-8

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8

ENV R_BASE_VERSION 3.6.2

RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
        r-base=${R_BASE_VERSION}* \
	&& rm -rf /var/lib/apt/lists/*

FROM myr-base AS R-build

RUN apt-get update \
        && apt-get install -y --no-install-recommends \
        r-base-dev=${R_BASE_VERSION}* \
	pandoc-citeproc \
	libxml2-dev \
	libcairo2-dev \
	libxt-dev \
	libssl-dev \
	libssh2-1-dev \
	libssl1.0.0 \
	libglu1-mesa-dev \
	libcurl4-openssl-dev \
        && rm -rf /var/lib/apt/lists/*

RUN MAKEFLAGS='-j4' R -e "options(rgl.useNULL = TRUE);install.packages(c('shiny', 'rmarkdown', 'optiSel', 'shinyjs','readxl', 'anytime', 'shinydashboard','data.table','devtools'), clean=TRUE, Ncpus=8); \
          library(devtools);devtools::install_github(c('datastorm-open/visNetwork','luansheng/visPedigree'))"

FROM myr-base AS gottis
COPY --from=R-build /usr/local/lib/R/site-library /usr/local/lib/R/site-library

RUN apt-get update \
        && apt-get install -y --no-install-recommends \
	libglu1-mesa \
	libxml2 \
        pandoc-citeproc \
        pandoc \
        && rm -rf /var/lib/apt/lists/*

CMD R -e "shiny::runApp('/root/gottis', port="$PORT", host='0.0.0.0')"

