FROM bytesmith/rstudio:9.3.0
LABEL maintainer="info@bytesmith.de"

RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
		libxml2-dev \
		libgeos-dev \
		libjpeg-dev \
		zlibc \
		zlib1g \
		zlib1g-dev \
	&& rm -rf /tmp/* \
	&& apt-get autoremove -y \
	&& apt-get autoclean -y \
	&& rm -rf /var/lib/apt/lists/*

# install required R packages
# install devtools incl. the certificate error workaround
# install ggmap from github
RUN Revo64 -e 'install.packages(c("devtools", "tidyverse", "lubridate", "stringr", "rgeos", "maptools", "gridExtra", "ggrepel", "seriation"))' --no-save \
	&& Revo64 -e 'remove.packages(c("curl","httr"))' --no-save \
	&& Revo64 -e 'install.packages(c("curl","httr"))' --no-save \
	&& Revo64 -e 'devtools::install_github("dkahle/ggmap", ref = "tidyup")' --no-save \
	&& rm -rf /tmp/*

RUN cd /home/rstudio \
	&& wget https://hdinsightresources.blob.core.windows.net/nyctaxi/NYC_taxi.zip \
	&& unzip NYC_taxi.zip -d data/ \
	&& chown -R rstudio:rstudio /home/rstudio/data \
	&& rm -rf /home/rstudio/NYC_taxi.zip

ADD scripts /home/rstudio/scripts/
