FROM alpine

# name to use in the machines table FQDN when registering the data files
ENV FQDN="" \
    PSQL="psql -U bety -h postgres -d bety -q -t -c"

WORKDIR /work

RUN apk --no-cache add bash curl rsync postgresql-client unzip \
    && curl -s -o sites.tgz http://isda.ncsa.illinois.edu/~kooper/EBI/sites.tgz \
    && curl -s -o inputs.tgz http://isda.ncsa.illinois.edu/~kooper/EBI/inputs.tgz \
    && curl -s -o cru_1901_2006.bin http://isda.ncsa.illinois.edu/~kooper/PEcAn/data/cru_1901_2006.bin \
    && curl -s -o plot.tgz http://isda.ncsa.illinois.edu/~kooper/EBI/plot.tgz \
    && curl -s -o Santarem_Km83.zip http://isda.ncsa.illinois.edu/~kooper/EBI/Santarem_Km83.zip \
    && curl -s -o testrun.s83.zip http://isda.ncsa.illinois.edu/~kooper/EBI/testrun.s83.zip \
    && curl -s -o ed2ws.harvard.tgz http://isda.ncsa.illinois.edu/~kooper/EBI/ed2ws.harvard.tgz \
    && curl -s -o testrun.PDG.zip http://isda.ncsa.illinois.edu/~kooper/EBI/testrun.PDG.zip \
    && curl -s -o create_met_driver.tar.gz http://isda.ncsa.illinois.edu/~kooper/EBI/create_met_driver.tar.gz

COPY add-data.sh add.util.sh /work/
CMD ["bash", "/work/add-data.sh"]
