ARG GRASS_VERSION=7.8.7

FROM ubuntu:22.04 as main

RUN export DEBIAN_FRONTEND=noninteractive && apt-get update && apt-get install -y \
        bash \
        bzip2 \
        fftw2 \
        gettext \
        gnutls-bin \
        libexecs0 \
        libicu70 \
        libjpeg-turbo8 \
        libjsoncpp25 \
        libgeos3.10.2 \
        libgeotiff5 \
        liblaszip8 \
        libncurses5 \
        libopenblas0 \
        libopenjp2-7 \
        libpng16-16 \
        libpq5 \
        libtool \
        libtiff5 \
        libunwind8 \
        musl \
        musl-tools \
        python3 \
        python3-pip \
        python3-six \
	r-base \
	r-cran-remotes \
	sqlite3 \
	zstd \
	&& \
    rm -rf /var/lib/apt/lists/*

FROM main as build_grass

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
      fftw-dev \
      libfreetype-dev \
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
      python3-numpy-dev \
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
      --without-wxwidgets \
      --without-openmp \
      --without-freetype \
      --without-opengl \
      --without-nls \
      --without-mysql \
      --without-ffmpeg \
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
    CXXFLAGS="$MYCXXFLAGS" &&\
    ./configure $GRASS_CONFIG && \
    make -j $(nproc --all) && \
    make install && \
    ldconfig /etc/ld.so.conf.d && \
    cd r.walk.accessmod && \
    make MODULE_TOPDIR=/usr/local/grass78

# Reduce the image size - Remove unnecessary grass files
RUN cp /usr/local/grass78/gui/wxpython/xml/module_items.xml module_items.xml; \
    rm -rf /usr/local/grass78/demolocation; \
    rm -rf /usr/local/grass78/fonts; \
    rm -rf /usr/local/grass78/gui; \
    rm -rf /usr/local/grass78/share; \
    mkdir -p /usr/local/grass78/gui/wxpython/xml/; \
    mv module_items.xml /usr/local/grass78/gui/wxpython/xml/module_items.xml; \
    apt-get remove -y grass-dev
    
FROM main as final

ENV LC_ALL="en_US.UTF-8"

# Copy GRASS GIS from build image
COPY --from=build_grass /usr/local/bin/grass* /usr/local/bin/grass
COPY --from=build_grass /usr/local/grass* /usr/local/grass/

# Link grass /usr/local/grass <-> /usr/local/grass78
RUN ln -sf /usr/local/grass `grass --config path`

RUN echo "\
install = function(pkg){ \
  install.packages(pkg); \
  tryCatch(library(pkg,character.only=T), \
      error = function(e){ \
      print(e);\
      quit('no',status=1);\
      })\
}" > .Rprofile

COPY requirements_r.txt /build/requirements_r.txt
COPY requirements.txt /build/requirements.txt
COPY build_r_packages.sh /build/build_r_packages.sh

RUN chmod +x /build/build_r_packages.sh && \
  /build/build_r_packages.sh "/build/requirements_r.txt"
RUN pip3 install --no-cache-dir --no-input -r "/build/requirements.txt"

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
    LC_ALL="en_US.UTF-8" \
    _R_SHLIB_STRIP_="true" \
    R_ZIPCMD="/usr/bin/zip" \
    SHELL="/bin/bash" \
    PROJ_NETWORK="ON" \
    GISRC="" \
    GRASSBIN="/usr/local/bin/grass" \
    GRASS_SKIP_MAPSET_OWNER_CHECK=1 \
    GRASS_PYTHON="python3" \
    GISBASE="/usr/local/grass"

# composed env
ENV GISDBASE="${AM5_DB_PATH}"
ENV LD_LIBRARY_PATH="$GISBASE/lib"
ENV GRASS_LD_LIBRARY_PATH="$LD_LIBRARY_PATH"
ENV PYTHONPATH="$GISBASE/etc/python:$PYTHONPATH"
ENV PATH=$GISBASE/bin:$GISBASE/scripts:$PATH

WORKDIR /app

CMD /bin/bash
