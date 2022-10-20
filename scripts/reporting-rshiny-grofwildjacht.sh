#!/bin/bash
# Make a tar.gz from the R-package from the code
cd /home/ubuntu/reporting-rshiny-grofwildjacht
# Build the docker image
sudo DOCKER_BUILDKIT=1 docker build --progress=plain --no-cache -t openanalytics/wildapp .
