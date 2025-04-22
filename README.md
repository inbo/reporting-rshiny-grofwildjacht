[![Test Website Reachability](https://github.com/inbo/reporting-rshiny-grofwildjacht/actions/workflows/prd_reachable.yml/badge.svg)](https://github.com/inbo/reporting-rshiny-grofwildjacht/actions/workflows/prd_reachable.yml)

# Reporting Rshiny grofwildjacht

This repo contains all the required scripts to run the affiliated Rshiny app of the R package _reportingGrofwild_. For most users, this will be appropriate. Besides the R package, some deploy scripts are included in this repo to support the incorporation of the Rshiny app within the shinyproxy environment.

## R package

As the R code to create the visualisations is implemented as an R package, users for the R code itself should check the manuals in the [reporting-grofwild subdirectory](https://github.com/inbo/reporting-rshiny-grofwildjacht/tree/master/reporting-grofwild). Important to know is that the function `runWildApp()` of the package is the central piece to start the Rshiny app, as this is also used by the [shinyproxy application](http://www.shinyproxy.io/).

## Deployment of the application

For a detailed description of the [shinyproxy application](http://www.shinyproxy.io/) and in-depth knowledge of the setup and [Docker](https://github.com/inbo/reporting-rshiny-grofwildjacht/blob/master/Dockerfile) settings, the user is referred to the [documentation of shinyproxy](<(http://www.shinyproxy.io/)>). The full [deployment on the AWS infrastructure](https://www.milieuinfo.be/confluence/pages/viewpage.action?spaceKey=INBOAWS&title=Shiny-Proxy) is out of scope for this manual and the configuration and setup is provided in a [private repo](https://github.com/inbo/shinyproxy). However, the following elements are present here to support the deployment and should be taken into account when creating new Rshiny packages/application to be handled by the INBO instance of shinyproxy:

- [Dockerfile](https://github.com/inbo/reporting-rshiny-grofwildjacht/blob/master/Dockerfile) to put the R package in a container as required by shinyproxy.
- [Rprofile.site](https://github.com/inbo/reporting-rshiny-grofwildjacht/blob/master/Rprofile.site), an additional port forwarding feature for the Docker handling
- [appspec.yml](https://github.com/inbo/reporting-rshiny-grofwildjacht/blob/master/appspec.yml) providing the required settings for the AWS codedeploy
- `scripts/reporting-rshiny-grofwildjacht.sh` providing the minimal actions to enable the usage of this R package and R shiny application wihtin the shinyproxy environment, i.e. build a Docker with the package inside.

### GitHub Actions Deployment

The application is automatically deployed using GitHub Actions workflows:

#### UAT Deployment

The UAT (User Acceptance Testing) environment is deployed automatically when:

- Code is pushed to the `uat` branch
- The workflow is manually triggered via the GitHub UI

The workflow performs the following steps:

1. Checks out the code
2. Sets up AWS credentials
3. Logs in to Amazon ECR (Elastic Container Registry)
4. Builds the Docker image with the current Git SHA
5. Tags the image with both the Git SHA and `uat` tag
6. Pushes the Docker image to ECR

#### Production Deployment

The Production environment is deployed automatically when:

- A new release is created in GitHub

The workflow performs similar steps as the UAT deployment, but tags the Docker image with `prod` instead of `uat`.

Both workflows build a Docker image with the application code, tag it appropriately, and push it to Amazon ECR where it can be deployed to the respective environments.

**Remark**: You can create and test the Docker locally as well, when [Docker is installed](https://docs.docker.com/engine/installation/). Execute `docker build -t openanalytics/wildapp .` inside the main repo folder to prepare the Docker image.

## Troubleshooting

If you want to check on the EC2 or locally how the Rshiny App inside the Docker is running (without the shinyproxy wrap).

Build docker image.

```
docker build --build-arg GIT_SHA=$(git rev-parse HEAD) -t inbo/wildapp .
```

Configure connection to [S3 data buckets](https://docs.aws.amazon.com/cli/latest/userguide/cli-chap-configure.html).

Run docker image, public app.

```
docker run -it -v ~/.aws:/root/.aws -p 3001:3838 inbo/wildapp R -e "reportingGrofwild::setupS3(); reportingGrofwild::runWildApp()"
```

Browse to `localhost:3001`.

Run docker image, private app for specific KBO.

```
docker run -it -v ~/.aws:/root/.aws -p 3001:3838 inbo/wildapp R -e "reportingGrofwild::setupS3(); reportingGrofwild::runWildApp(public = FALSE, kbo = xxx)"
```

In a similar way, an R session can be started to run specific functions of the reportingGrofwild R package.

```
docker run -it -p 3001:3838 inbo/wildapp R
```

### Acknowledgements

We would like to thank [openanalytics](https://www.openanalytics.eu/) to open source their shinyproxy application, which enabled us to bring the Rshiny application to the web.
