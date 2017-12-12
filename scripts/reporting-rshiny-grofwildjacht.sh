#!/bin/bash
# Make a tar.gz from the R-package from the code
cd /home/ubuntu/reporting-rshiny-grofwildjacht
if [ -f reporting-grofwild.tar.gz ]; then
    rm reporting-grofwild.tar.gz
fi
tar -zcvf reporting-grofwild.tar.gz reporting-grofwild
# Build the docker image
sudo docker build --no-cache -t openanalytics/wildapp .