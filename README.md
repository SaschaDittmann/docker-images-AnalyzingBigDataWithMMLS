# Workshop: Analyzing Big Data with Microsoft Machine Learning Server

[![](https://img.shields.io/github/license/mashape/apistatus.svg)](https://github.com/SaschaDittmann/docker-images-AnalyzingBigDataWithMMLS)
[![](https://img.shields.io/github/tag/SaschaDittmann/docker-images-rstudio.svg)](hhttps://github.com/SaschaDittmann/docker-images-AnalyzingBigDataWithMMLS)

This Docker Image is for development & testing purposes ONLY.

It is based on the [rocker/rstudio](https://hub.docker.com/r/rocker/rstudio-stable/) image, and adds the Microsoft Machine Learning Server platform on top of it.

## Docker Images

[![](https://img.shields.io/docker/pulls/bytesmith/analyzing-big-data-with-mmls.svg)](https://hub.docker.com/r/bytesmith/analyzing-big-data-with-mmls)
[![](https://img.shields.io/docker/automated/bytesmith/analyzing-big-data-with-mmls.svg)](https://hub.docker.com/r/bytesmith/analyzing-big-data-with-mmls/builds)

version          | description                               | size 
---------------- | ----------------------------------------- | ------
[![](https://images.microbadger.com/badges/version/bytesmith/rstudio.svg)](https://hub.docker.com/r/bytesmith/analyzing-big-data-with-mmls) | Latest build from the [GitHub Repo](https://github.com/SaschaDittmann/docker-images-AnalyzingBigDataWithMMLS) | [![](https://images.microbadger.com/badges/image/bytesmith/rstudio.svg)](https://microbadger.com/images/bytesmith/analyzing-big-data-with-mmls)

## Quickstart
```
docker run --rm -p 8787:8787 -e PASSWORD=yourpasswordhere bytesmith/rstudio
```

Visit *localhost:8787* in your browser and log in with username *rstudio* and the password you set. 

**Note:** Setting a password is now REQUIRED. Container will error otherwise.

## Give the user root permissions (add to sudoers)
```
docker run -d -p 8787:8787 -e ROOT=TRUE -e PASSWORD=yourpasswordhere bytesmith/rstudio
```

Link a local volume (in this example, the current working directory, $(pwd)) to the rstudio container:

```
docker run -d -p 8787:8787 -v $(pwd):/home/rstudio -e PASSWORD=yourpasswordhere bytesmith/rstudio
```

## Bypassing the authentication step

**Warning:** use only in a secure environment. Do not use this approach on an
Azure or other cloud machine with a publicly accessible IP address.

Simply set the environmental variable *DISABLE_AUTH=true*, e.g.

```
docker run --rm \
  -p 127.0.0.1:8787:8787 \
  -e DISABLE_AUTH=true \
  bytesmith/rstudio
```

Navigate to *http://localhost:8787* and you should be logged into RStudio as
the *rstudio* user without needing a password.

## Access a root shell for a running rstudio container instance
First, determine the name or id of your container (unless you provided a --name to docker run) using docker ps. You need just enough of the hash id to be unique, e.g. the first 3 letters/numbers. Then exec into the container for an interactive session:

```
docker exec -ti <CONTAINER_ID> bash
```

You can now perform maintenance operations requiring root behavior such as apt-get, adding/removing users, etc.

Or, simply enable root as shown above and use the RStudio bash terminal.

## About the Microsoft Machine Learning Server

Microsoft Machine Learning Server is your flexible enterprise platform for analyzing data at scale, building intelligent apps, and discovering valuable insights across your business with full support for Python and R.

Machine Learning Server meets the needs of all constituents of the process – from data engineers and data scientists to line-of-business programmers and IT professionals. It offers a choice of languages and features algorithmic innovation that brings the best of open-source and proprietary worlds together.

R support is built on a legacy of Microsoft R Server 9.x and Revolution R Enterprise products. Significant machine learning and AI capabilities enhancements have been made in every release. Python support was added in the previous release. Machine Learning Server supports the full data science lifecycle of your Python-based analytics.

Additionally, Machine Learning Server enables operationalization support so you can deploy your models to a scalable grid for both batch and real-time scoring.

## Trademarks ##

RStudio is a registered trademark of RStudio, Inc.  The use of the trademarked term RStudio and the distribution of the RStudio binaries through the images hosted on [hub.docker.com](https://registry.hub.docker.com/) has been granted by explicit permission of RStudio.  Please review [RStudio's trademark use policy](http://www.rstudio.com/about/trademark/) and address inquiries about further distribution or other questions to [permissions@rstudio.com](mailto:permissions@rstudio.com).
