# PEcAn Development

This is a minimal guide to getting started with PEcAn development under Docker. You can find more information about docker in the [pecan documentation](https://pecanproject.github.io/pecan-documentation/master/docker-index.html).

## Git Repository and Workflow

The recommended workflow is gitflow which is described in the PEcAn documentation. For the repository we recommend using a [fork of the PEcAn repsitory](https://help.github.com/en/github/getting-started-with-github/fork-a-repo). In the `/scripts` folder there is a script called [syncgit.sh](scripts/syncgit.sh) that will help with synchronizing your fork with the official repository.

## Developing in Docker

If running on a linux system it is recommended to add your user to the docker group. This will prevent you from having to use `sudo` to star the docker containers, and makes sure that any file that is written to a mounted volume is owned by you. This can be done using `sudo adduser ${USER} docker`.

To get started with development in docker we need to bring up the docker stack first. In the main pecan folder you will find the [docker-compose.yml](docker-compose.yml) file that can be used to bring up the pecan stack. There is also the [docker-compose.dev.yaml](docker-compose.dev.yaml) file that adds additional containers, and changes some services to make it easier for development.

### First time setup

The steps in this section only need to be done the fist time you start working with the stack in docker. After this is done you can skip these steps. You can find more detail about the docker commands in the [pecan documentation](https://pecanproject.github.io/pecan-documentation/master/docker-index.html).

#### .env file
You can copy the [`env.example`](docker/env.example) file as .env in your pecan folder. The variables we want to modify are:

* `COMPOSE_PROJECT_NAME` set this to pecan, the prefix for all containers
* `PECAN_VERSION` set this to develop, the docker image we start with

#### folders

Next we will create the folders that will hold all the data for the docker containers using: `mkdir -p volumes/{lib,pecan,portainer,postgres,rabbitmq,traefik}`. The `volumes` folder will be ignored by git. You can create these at any location, however you will need to update the `docker-compose.dev.yml` file. The subfolders are used for the following:

- **lib** holds all the R packages for the specific version of PEcAn and R. This folder will be shared amongst all other containers, and will contain the compiled PEcAn code.
- **pecan** this holds all the data, such as workflows and any downloaded data.
- **portainer** if you enabled the portainer service this folder is used to hold persistent data for this service
- **postgres** holds the actual database data. If you want to backup the database, you can stop the postgres container, zip up the folder.
- **rabbitmq** holds persistent information of the message broker (rabbitmq). 
- **traefik** holds persisent data for the web proxy, that directs incoming traffic to the correct container.

These folders will hold all the persistent data for each of the respective containers and can grow. For example the postgres database is multiple GB. The pecan folder will hold all data produced by the workflows, including any downloaded data, and can grow to many giga bytes.

#### postgresql database

First we bring up postgresql (we will start RabbitMQ as well since it takes some time to start): `docker-compose -f docker-compose.yml -f docker-compose.dev.yml up -d postgres rabbitmq`. This will start postgresql and rabbitmq. We need to wait for a few minutes (you can look at the logs using `docker-compose logs postgres`) to see if it is ready.

Once the database has finished starting up we will initialize the database using: `docker run --rm --network pecan_pecan pecan/db`. Once that is done we create two users for BETY:

```
# guest user
docker-compose run --rm bety user guestuser guestuser "Guest User" guestuser@example.com 4 4

# example user
docker-compose run --rm bety user carya illinois "Carya Demo User" carya@example.com 1 1
```

#### copy R packages

The final step is to copy the R packages from a container to your local machine as the `volumes/lib` folder. This is not really needed, but will speed up the process of the first compilation. Later we will put our newly compiled code here as well. 

You can copy all the data using `docker run -ti --rm -v ${PWD}/volumes/lib:/rlib pecan/base:develop cp -r /usr/local/lib/R/site-library/* /rlib/`. This will copy all compiled packages to your local machine.

This only needs to be done once (or if the PEcAn base image changes drastically, for example a new version of R). You can also always delete all files in the `volumes/lib` folder, and recompile PEcAn from scratch.

### PEcAn Development

To begin development we first have to bring up the full PEcAn stack. This assumes you have done once the steps above. You don't need to stop any running containers, you can use the following command to start all containers: `docker-compose -f docker-compose.yml -f docker-compose.dev.yml up -d`. At this point you have PEcAn running in docker.

The current folder (most likely your clone of the git repository) is mounted in some containers as `/pecan`, and in the case of rstudio also in your home folder as `pecan`. You can see which containers exactly in `docker-compose.dev.yaml`.

You can now modify the code on your local machine, or you can use [rstudio](http://localhost:8000) in the docker stack. Once you made changes to the code you can compile the code either in the terminal of rstudio (`cd pecan && make`) or using `./scripts/compile.sh` from your machine (latter is nothing more than a shell script that runs `docker-compose exec executor sh -c 'cd /pecan && make'`. 

The compiled code is written to `/usr/local/lib/R/site-library` which is mapped to `volumes/lib` on your machine. This same folder is mounted in many other containers, allowing you to share the same PEcAn modules in all containers. Now if you change a module, and compile all other containers will see and use this new version of your module.

To compile the PEcAn code you can use the make command in either the rstudio container, or in the executor container. The script [`compile.sh`](sripts/compile.sh) will run make inside the executor container.

To make it easier to start the containers and not having to remember to use `docker-compose -f docker-compose.yml -f docker-compose.dev.yml` when you want to use the docker-compose comannd, you can rename `docker-compose.dev.yml` to `docker-compose.override.yml`. The docker-compose command will automatically use the `docker-compose.yml`, `docker-compose.override.yml` and the `.env` files to start the right containers with the correct parameters.

### Workflow Submission

You can submit your workflow either in the executor container or in rstudio container. For example to run the `docker.sipnet.xml` workflow located in the tests folder you can use: 

```
docker-compose exec executor baseh
# inside the container
cd /pecan/tests
R CMD ../web/workflow.R docker.sipnet.xml
```

A better way of doing this is developed as part of GSOC, in which case you can leverage of the restful interface defined, or using the new R PEcAn API package.

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
