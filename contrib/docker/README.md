# Dockerfiles for Models

To create a docker image, copy the appropriate Dockerfile and build using the following command:

```
docker build -t pecan/<model>:<version> -f Dockerfile.<model> .
```

For example to build sipnet v136

```
curl -o sipnet_r136.tar.gz http://isda.ncsa.illinois.edu/~kooper/EBI/sipnet_r136.tar.gz
tar zxf sipnet_r136.tar.gz
rm sipnet_r136.tar.gz
cp Dockerfile.sipnet sipnet_r136
docker build -t pecan/sipnet:136 -f Dockerfile.sipnet sipnet_r136
```
