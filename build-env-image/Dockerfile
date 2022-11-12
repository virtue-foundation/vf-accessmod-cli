FROM mundialis/grass-py3-pdal:stable-ubuntu as build

EXPOSE 5000

RUN export DEBIAN_FRONTEND=noninteractive && apt-get update && apt-get install -y \
        bash \
        libtool \
        python3 \
	r-base \
	r-cran-remotes \
	&& \
    rm -rf /var/lib/apt/lists/*

WORKDIR /build

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
  
ENTRYPOINT /bin/bash
