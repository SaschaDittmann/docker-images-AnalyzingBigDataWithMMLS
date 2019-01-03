FROM bytesmith/rstudio:9.3.0
LABEL maintainer="info@bytesmith.de"

RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
		libxml2-dev \
		libgeos-dev \
		libjpeg-dev \
	&& rm -rf /tmp/* \
	&& apt-get autoremove -y \
	&& apt-get autoclean -y \
	&& rm -rf /var/lib/apt/lists/*

# install required R packages
RUN Revo64 -e 'install.packages(c("tidyverse", "lubridate", "stringr", "rgeos", "maptools", "ggmap", "gridExtra", "ggrepel", "seriation"))' --no-save \
	&& rm -rf /tmp/*

RUN cd /home/rstudio \
	&& wget https://hdinsightresources.blob.core.windows.net/nyctaxi/NYC_taxi.zip \
	&& unzip NYC_taxi.zip -d data/ \
	&& chown -R rstudio:rstudio /home/rstudio/data \
	&& rm -rf /home/rstudio/NYC_taxi.zip

ADD scripts /home/rstudio/scripts/
