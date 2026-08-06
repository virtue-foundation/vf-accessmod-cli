ARG GRASS_VERSION=8.4.2

FROM ubuntu:26.04 AS main

RUN export DEBIAN_FRONTEND=noninteractive && apt-get update && apt-get install -y \
        bash \
        bzip2 \
        gettext \
        libicu78 \
        libfftw3-double3 \
        libjpeg-turbo8 \
        libjsoncpp26 \
        libgdal38 \
        libgeos3.14.1 \
        libgeotiff5 \
        liblaszip8 \
        libncurses6 \
        libopenblas0 \
        libopenjp2-7 \
        libpng16-16t64 \
        libpq5 \
        libtool \
        libtiff6 \
        libunwind8 \
        locales \
        python3 \
        python3-pip \
	r-base \
	r-cran-remotes \
	sqlite3 \
	zstd \
	&& \
    rm -rf /var/lib/apt/lists/*

FROM main AS build_r_env

RUN export DEBIAN_FRONTEND=noninteractive && apt-get update && apt-get install -y \
      libgdal-dev \
      && \
    rm -rf /var/lib/apt/lists/*

FROM build_r_env AS build_grass

ARG GRASS_VERSION

RUN export DEBIAN_FRONTEND=noninteractive && apt-get update && apt-get install -y \
      wget \
      && \
    rm -rf /var/lib/apt/lists/*
      
WORKDIR /build

# Download and extract sources
RUN echo "Download grass version ${GRASS_VERSION}" && \
    wget -O grass.tar.gz \
    https://github.com/OSGeo/grass/archive/refs/tags/${GRASS_VERSION}.tar.gz && \
    tar --extract --file grass.tar.gz --strip-components 1


# GRASS dev packages
RUN export DEBIAN_FRONTEND=noninteractive && apt-get update && apt-get install -y \
      grass-dev \
      bison \
      flex \
      build-essential \
      libbz2-dev \
      libcairo2-dev \
      libfftw3-dev \
      libfreetype-dev \
      libgdal-dev \
      g++ \
      gcc \
      libgeos-dev \
      gnutls-dev \
      libc6 \
      libjpeg-turbo8-dev \
      libpng-dev \
      make \
      libopenjp2-7-dev \
      libopenblas-dev \
      python3-dev \
      python3-setuptools \
      python3-numpy \
      libsqlite3-dev \
      libtiff-dev \
      libzstd-dev \
      && \
    rm -rf /var/lib/apt/lists/*
       
# set configuration options, without wxGUI
ENV GRASS_CONFIG="\
      --enable-largefile \
      --with-cxx \
      --with-proj-share=/usr/share/proj \
      --with-gdal=/usr/bin/gdal-config \
      --with-geos \
      --with-sqlite \
      --with-bzlib \
      --with-zstd \
      --with-cairo --with-cairo-ldflags=-lfontconfig \
      --with-fftw \
      --without-x \
      --without-pdal \
      --without-postgres \
      --without-openmp \
      --without-freetype \
      --without-opengl \
      --without-nls \
      --without-mysql \
      --without-odbc \
      "

# Build r.walk.accessmod
COPY modules/r.walk.accessmod r.walk.accessmod

# Patch GRASS
COPY patches .

# Configure compile and install GRASS GIS
RUN echo "Configure and compile GRASS" && \
    CFLAGS_X86="--march=x86-64 -m64" && \
    CFLAGS_ARM="--march=armv8.5-a -mcpu=apple-a14" && \
    CFLAGS="-Ofast -std=gnu99 " && \
    if test $(arch) = "aarch64"; then \
    CFLAGS="$CFLAGS $CFLAGS_ARM"; \
    else \ 
    CFLAGS="$CFLAGS $CFLAGS_X86"; \
    fi &&\
    LDFLAGS="-s -Wl,--no-undefined -lblas" &&\
    LD_LIBRARY_PATH="/usr/local/lib" && \
    ./configure $GRASS_CONFIG && \
    make -j $(nproc --all) && \
    make install && \
    cd r.walk.accessmod && \
    make MODULE_TOPDIR="$(grass --config path)"

# Reduce the image size - Remove unnecessary grass files
RUN G="$(grass --config path)"; \
    cp "$G/gui/wxpython/xml/module_items.xml" module_items.xml; \
    rm -rf "$G/demolocation"; \
    rm -rf "$G/fonts"; \
    rm -rf "$G/gui"; \
    rm -rf "$G/share"; \
    mkdir -p "$G/gui/wxpython/xml/"; \
    mv module_items.xml "$G/gui/wxpython/xml/module_items.xml"; \
    apt-get remove -y grass-dev

FROM build_r_env AS build_r_packages

# Fix for locales
RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen
ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8

# Copy GRASS GIS from build image
COPY --from=build_grass /usr/local/bin/grass* /usr/local/bin/grass
COPY --from=build_grass /usr/local/grass* /usr/local/grass/

# Link grass /usr/local/grass <-> /usr/local/grass84
RUN ln -sf /usr/local/grass `grass --config path`

RUN echo "\
options(repos = c(CRAN = \"https://packagemanager.posit.co/cran/2024-12-31\")); \
install = function(pkg){ \
  install.packages(pkg); \
  tryCatch(library(pkg,character.only=T), \
      error = function(e){ \
      print(e);\
      quit('no',status=1);\
      })\
}" > .Rprofile

COPY requirements_r.txt /build/requirements_r.txt
COPY build_r_packages.sh /build/build_r_packages.sh

RUN chmod +x /build/build_r_packages.sh && \
  /build/build_r_packages.sh "/build/requirements_r.txt"

# uv pinned by digest for reproducible builds; bump deliberately.
COPY --from=ghcr.io/astral-sh/uv@sha256:93b61e21202b1dab861092748e46bbd6e0e41dd84f59b9174efd2353186e1b47 /uv /usr/local/bin/uv
COPY pyproject.toml uv.lock /build/

WORKDIR /build

# PEP 668: 26.04's system python is externally-managed, so install into a
# venv (not --system) and put it on PATH for the app image + test job.
RUN uv venv /opt/venv && \
    uv pip install --python /opt/venv/bin/python --no-cache .

# appended to existing ones.
ENV AM5_PORT_HTTP=5000 \
    AM5_PORT_HTTP_PUBLIC=5080 \
    AM5_PORT_APP=3000 \
    AM5_PORT_APP_PUBLIC=3080 \
    AM5_DB_PATH="/data/dbgrass" \
    AM5_LOGS_PATH="/data/logs" \
    AM5_CACHE_PATH="/data/cache" \
    AM5_TMP_PATH="/tmp" \
    AM5_APP_PATH="/app" \
    TZ=UTC \
    _R_SHLIB_STRIP_="true" \
    R_ZIPCMD="/usr/bin/zip" \
    SHELL="/bin/bash" \
    PROJ_NETWORK="ON" \
    GISRC="" \
    GRASSBIN="/usr/local/bin/grass" \
    GRASS_SKIP_MAPSET_OWNER_CHECK=1 \
    GRASS_PYTHON="python3"

# composed env
ENV GISDBASE="${AM5_DB_PATH}"
ENV GISBASE="/usr/local/grass"
ENV GRASS_INSTALLATION="$GISBASE"
ENV LD_LIBRARY_PATH="$GISBASE/lib"
ENV GRASS_LD_LIBRARY_PATH="$LD_LIBRARY_PATH"
ENV PYTHONPATH="$GISBASE/etc/python:$PYTHONPATH"
ENV VIRTUAL_ENV=/opt/venv
ENV PATH=/opt/venv/bin:$GISBASE/bin:$GISBASE/scripts:$PATH

RUN mkdir -p "$GISDBASE"

WORKDIR /app

CMD /bin/bash
