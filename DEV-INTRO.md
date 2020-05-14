# PEcAn Development

This is a minimal guide to getting started with PEcAn development.

## Git Repository and Workflow

The recommended workflow is gitflow which is described in the PEcAn documentation. For the repository we recommend using a [fork of the PEcAn repsitory](https://help.github.com/en/github/getting-started-with-github/fork-a-repo). In the scripts folder there is a script called [syngit.sh](scripts/syncgit.sh) that will help with synchronizing your fork with the official repository.

## Developing in Docker

To get started with development in docker we need to bring up the docker stack first. In the main pecan folder you will find the [docker-compose.yml](docker-compose.yml) file that can be used to bring up the pecan stack. There is also the [docker-compose.dev.yaml](docker-compose.dev.yaml) file that adds additional containers, and changes some services to make it easier for development.

You can copy the `docker-compose.dev.yaml` to `docker-compose.override.yml`. Once that is done the `docker-compose` program will automatically use the `docker-compose.yml` and `docker-compose.override.yml` files, and you don't have to explicitly load them in the commands below.

### First time setup

You can copy the [`env.example`](docker/env.example) file as .env in your pecan folder. The variables we want to modify are:

* `COMPOSE_PROJECT_NAME` set this to pecan, the prefix for all containers
* `PECAN_VERSION` set this to develop, the docker image we start with

Next we will create the folders that will hold all the data for the docker containers using: `mkdir -p volumes/{lib,pecan,portainer,postgres,rabbitmq,traefik}`. The `volumes` folder will be ignored by git.

First we bring up postgresql (we will start RabbitMQ as well since it takes some time to start): `docker-compose -f docker-compose.yml -f docker-compose.dev.yml up -d postgres rabbitmq`. This will start postgresql and rabbitmq. We need to wait for a few minutes (you can look at the logs using `docker-compose logs postgres`) to see if it is ready.

During the time it takes for the database to start we will copy the R packages from a container to our `volumes/lib` folder. This folder contain the compiled R packages. Later we will put our newly compiled code here as well. You can copy all the data using `docker run -ti --rm -v ${PWD}/volumes/lib:/rlib pecan/base:develop cp -r /usr/local/lib/R/site-library/* /rlib/`. This will copy all compiled packages to your local machine. This only needs to be done once (or if the PEcAn base image changes drastically, for example a new version of R).

At this point postgresql should have started and is ready to load the database. This is done using `docker run -ti --rm --network pecan_pecan pecan/db`. This only needs to be done once (unless the volumes folder is removed).

Once the database is loaded we can bring up the rest of the docker stack using `docker-compose -f docker-compose.yml -f docker-compose.dev.yml up -d`. At this point you have PEcAn running in docker.

### PEcAn Development

The current folder is mounted in some containers as `/pecan`. You can see which containers exactly in `docker-compose.dev.yaml`. You can now modify the code on your local machine, or you can use [rstudio](http://localhost:8000) in the docker stack. Once you made changes to the code you can compile the code either in the terminal of rstudio (`cd pecan && make`) or using `./scripts/compile.sh` from your machine (latter is nothing more than a shell script that runs `docker-compose exec executor sh -c 'cd /pecan && make'`. 

If you submit new workflows through the webinterface it will use this new code in the executor, and any other containers that have `volumes/lib` mounted inside the containers (done by `docker-compose.dev.yaml`).

### Workflow Submission

A better way of doing this is developed as part of GSOC. You can leverage of the API folder (specifically submit_workflow.R).

# Directory Structure

Following are the main folders inside the pecan repository. 

### base (R packages)

These are the core packages of PEcAn. Most other packages will depend on the packages in this folder.

### models (R packages)

Each subfolder contains the required pieces to run the model in PEcAn

### modules (R packages)

Contains packages that either do analysis, or download and convert different data products.

### web (PHP + javascript)

The Pecan web application

### shiny (R + shiny)

Each subfolder is its own shiny application.

### book_source (RMarkdown)

The PEcAn documentation that is compiled and uploaded to the PEcAn webpage.

### docker

Some of the docker build files. The Dockerfiles for each model are placed in the models folder.

### scripts

Small scripts that are used as part of the development and installation of PEcAn.
