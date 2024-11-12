# Reporting Rshiny grofwildjacht

This repo contains all the required scripts to run the affiliated Rshiny app of the R package *reportingGrofwild*. For most users, this will be appropriate. Besides the R package, some deploy scripts are included in this repo to support the incorporation of the Rshiny app within the shinyproxy environment.

## R package

As the R code to create the visualisations is implemented as an R package, users for the R code itself should check the manuals in the [reporting-grofwild subdirectory](https://github.com/inbo/reporting-rshiny-grofwildjacht/tree/master/reporting-grofwild). Important to know is that the function `runWildApp()` of the package is the central piece to start the Rshiny app, as this is also used by the [shinyproxy application](http://www.shinyproxy.io/). 

## Deployment of the application

For a detailed description of the [shinyproxy application](http://www.shinyproxy.io/) and in-depth knowledge of the setup and [Docker](https://github.com/inbo/reporting-rshiny-grofwildjacht/blob/master/Dockerfile) settings, the user is referred to the [documentation of shinyproxy]((http://www.shinyproxy.io/)). The  full [deployment on the AWS infrastructure](https://www.milieuinfo.be/confluence/pages/viewpage.action?spaceKey=INBOAWS&title=Shiny-Proxy) is out of scope for this manual and the configuration and setup is provided in a [private repo](https://github.com/inbo/shinyproxy). However, the following elements are present here to support the deployment and should be taken into account when creating new Rshiny packages/application to be handled by the INBO instance of shinyproxy:

* [Dockerfile](https://github.com/inbo/reporting-rshiny-grofwildjacht/blob/master/Dockerfile) to put the R package in a container as required by shinyproxy.
* [Rprofile.site](https://github.com/inbo/reporting-rshiny-grofwildjacht/blob/master/Rprofile.site), an additional port forwarding feature for the Docker handling
* [appspec.yml](https://github.com/inbo/reporting-rshiny-grofwildjacht/blob/master/appspec.yml) providing the required settings for the AWS codedeploy
* `scripts/reporting-rshiny-grofwildjacht.sh` providing the minimal actions to enable the usage of this R package and R shiny application wihtin the shinyproxy environment, i.e. build a Docker with the package inside.

**Remark**: You can create and test the Docker locally as well, when [Docker is installed](https://docs.docker.com/engine/installation/). Execute `docker build -t openanalytics/wildapp .` inside the main repo folder to prepare the Docker image.

## Troubleshooting

If you want to check on the EC2 or locally how the Rshiny App inside the Docker is running (without the shinyproxy wrap).

Build docker image.

```
sudo docker build --build-arg GIT_SHA=$(git rev-parse HEAD) -t inbo/wildapp .
```

Configure connection to [S3 data buckets](https://docs.aws.amazon.com/cli/latest/userguide/cli-chap-configure.html).

Run docker image, public app. 

```
sudo docker run -it -v ~/.aws:/root/.aws -p 3001:3838 inbo/wildapp R -e "reportingGrofwild::setupS3(); reportingGrofwild::runWildApp(public=TRUE)" 
```

Browse to `localhost:3001`.

Run docker image, private app for specific KBO.

```
sudo docker run -it -v ~/.aws:/root/.aws -p 3001:3838 inbo/wildapp R -e "reportingGrofwild::setupS3(); reportingGrofwild::runWildApp(public=FALSE, kbo = xxx)" 
```

In a similar way, an R session can be started to run specific functions of the reportingGrofwild R package.

```
sudo docker run -it -p 3001:3838 inbo/wildapp R
```


### Acknowledgements
We would like to thank [openanalytics](https://www.openanalytics.eu/) to open source their shinyproxy application, which enabled us to bring the Rshiny application to the web. 
