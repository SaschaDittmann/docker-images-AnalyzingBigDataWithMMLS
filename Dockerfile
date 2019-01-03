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

ADD https://hdinsightresources.blob.core.windows.net/nyctaxi/examples/nyctaxi/yellow_tripsample_2016-01.csv https://hdinsightresources.blob.core.windows.net/nyctaxi/examples/nyctaxi/yellow_tripsample_2016-02.csv https://hdinsightresources.blob.core.windows.net/nyctaxi/examples/nyctaxi/yellow_tripsample_2016-03.csv https://hdinsightresources.blob.core.windows.net/nyctaxi/examples/nyctaxi/yellow_tripsample_2016-04.csv https://hdinsightresources.blob.core.windows.net/nyctaxi/examples/nyctaxi/yellow_tripsample_2016-05.csv https://hdinsightresources.blob.core.windows.net/nyctaxi/examples/nyctaxi/yellow_tripsample_2016-06.csv /home/rstudio/data/

ADD https://hdinsightresources.blob.core.windows.net/nyctaxi/examples/nyctaxi/ZillowNeighborhoods-NY/ZillowNeighborhoods-NY.dbf https://hdinsightresources.blob.core.windows.net/nyctaxi/examples/nyctaxi/ZillowNeighborhoods-NY/ZillowNeighborhoods-NY.prj https://hdinsightresources.blob.core.windows.net/nyctaxi/examples/nyctaxi/ZillowNeighborhoods-NY/ZillowNeighborhoods-NY.shp https://hdinsightresources.blob.core.windows.net/nyctaxi/examples/nyctaxi/ZillowNeighborhoods-NY/ZillowNeighborhoods-NY.shx /home/rstudio/data/ZillowNeighborhoods-NY/

#ADD scripts /home/rstudio/scripts/
