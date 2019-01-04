@echo off
docker run -d --rm^
	-p 8787:8787^
	-e DISABLE_AUTH=true^
	--name rstudio-rclient^
	-t bytesmith/analyzing-big-data-with-mmls:latest
