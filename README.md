# vf-accessmod-cli
# Docker images to run Accessmod core functions on Heroku 

The contents of this folder contain what is needed to build the python and r environment container image. It is based on an older version of the codebase and will be updated to a more compact size.

This directory contains a few other files necessary to build this image. The random walk script that accessmod uses needs to be built using GRASS so that has been packaged into "modules" seperate from other implentations of the code. "build_r_packages.sh" is a custom script that the dockerfile will use to allow for seperately tracking r dependencies using a text file.
