FROM pecan/models:develop

RUN mkdir /data

COPY ./model_info.json /work

COPY ./inst/biocro.Rscript /work

WORKDIR /data

ENTRYPOINT ["Rscript", "/work/biocro.Rscript"]

