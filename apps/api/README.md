# PEcAn RESTful API Server

This folder contains the code & tests for PEcAn's RESTful API server. The API allows users to remotely interact with the PEcAn servers and leverage the functionalities provided by the PEcAn Project. It has been designed to follow common RESTful API conventions. Most operations are performed using the HTTP methods: `GET` (retrieve) & `POST` (create).

#### For the most up-to-date documentation, you can visit the [PEcAn API Documentation](http://pecan-dev.ncsa.illinois.edu/swagger/).

## Starting the PEcAn server:

Follow the following steps to spin up the PEcAn API server locally:

```bash
$ cd R
$ ./entrypoint.R
```

## Running the tests:

```bash
$ ./test_pecanapi.sh
```
