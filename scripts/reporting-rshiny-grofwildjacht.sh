#!/bin/bash
# Make a tar.gz from the R-package from the code
cd /home/ubuntu/reporting-rshiny-grofwildjacht
# Build the docker image
sudo DOCKER_BUILDKIT=1 docker build --progress=plain --build-arg GIT_SHA=$(git rev-parse HEAD) --no-cache -t openanalytics/wildapp .
