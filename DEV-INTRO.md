# PEcAn Development

This is a minimal guide to getting started with PEcAn development under Docker. You can find more information about docker in the [pecan documentation](https://pecanproject.github.io/pecan-documentation/master/docker-index.html).

## Git Repository and Workflow

We recommend following the the [gitflow](https://nvie.com/posts/a-successful-git-branching-model/) workflow and working in your own [fork of the PEcAn repsitory](https://help.github.com/en/github/getting-started-with-github/fork-a-repo). See the [PEcAn developer guide](book_source/02_demos_tutorials_workflows/05_developer_workflows/02_git/01_using-git.Rmd) for further details. In the `/scripts` folder there is a script called [syncgit.sh](scripts/syncgit.sh) that will help with synchronizing your fork with the official repository.

## Developing in Docker

If running on a linux system it is recommended to add your user to the docker group. This will prevent you from having to use `sudo` to start the docker containers, and makes sure that any file that is written to a mounted volume is owned by you. This can be done using `sudo adduser ${USER} docker`.

To get started with development in docker we need to bring up the docker stack first. In the main pecan folder you will find the [docker-compose.yml](docker-compose.yml) file that can be used to bring up the pecan stack. There is also the [docker-compose.dev.yaml](docker-compose.dev.yaml) file that adds additional containers, and changes some services to make it easier for development.

By default docker-compose will use the files `docker-compose.yml` and `docker-compose.override.yml`. We will use the default `docker-compose.yml` file from PEcAn. The `docker-compose.override.yml` file can be used to configure it for your specific environment, in our case we will use it to setup the docker environment for development. Copy the `docker-compose.dev.yml` file to `docker-compose.override.yml` to start working with your own override file, i.e. `cp docker-compose.dev.yml docker-compose.override.yml`. You can now use the command `docker-compose` to launch all the containers setup for development.

If you in the past had loaded some of the data, but would like to start from scratch you can simply remove the `volumes` folder (and all the subfolders) and start with the "First time setup" section again.

### First time setup

The steps in this section only need to be done the fist time you start working with the stack in docker. After this is done you can skip these steps. You can find more detail about the docker commands in the [pecan documentation](https://pecanproject.github.io/pecan-documentation/master/docker-index.html).

Before doing anything it is recommended to make sure you have the lastest docker images ready. You can do a `docker-compose pull` to get the latest images.

* setup .env file
* create folders to hold the data
* load the postgresql database
* load some test data
* copy all R packages (optional but recommended) 
* setup for web folder development (optional)

#### .env file

You can copy the [`env.example`](docker/env.example) file as .env in your pecan folder. The variables we want to modify are:

* `COMPOSE_PROJECT_NAME` set this to pecan, the prefix for all containers
* `PECAN_VERSION` set this to develop, the docker image we start with
Both of these variables should also be uncommented by removing the # preceding them. 
At the end you should see the following if you run the following command `egrep -v '^(#|$)' .env`

```
COMPOSE_PROJECT_NAME=pecan
PECAN_VERSION=develop
```

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

Once the database has finished starting up we will initialize the database. Before we run the container we want to make sure we have the latest database information, you can do this with `docker pull pecan/db`, which will make sure you have the latest version of the database ready. Now you can load the database using: `docker run --rm --network pecan_pecan pecan/db` (in this case we use the latest image instead of develop since it refers to the actual database data, and not the actual code). Once that is done we create two users for BETY:

```
# guest user
docker-compose run --rm bety user guestuser guestuser "Guest User" guestuser@example.com 4 4

# example user
docker-compose run --rm bety user carya illinois "Carya Demo User" carya@example.com 1 1
```

#### load example data

Once the database is loaded we can add some example data, some of the example runs and runs for the ED model, assume some of this data is available. To do this we first again make sure we have the latest code ready using `docker pull pecan/data:develop` and run this image using `docker run -ti --rm --network pecan_pecan --volume pecan_pecan:/data --env FQDN=docker pecan/data:develop`. This can take some time, but all the data needed will be copied to the `/data` folder in the pecan containers (which is mounted from `volumes/pecan` in your current folder.

#### copy R packages (optional but recommended)

Next copy the R packages from a container to your local machine as the `volumes/lib` folder. This is not really needed, but will speed up the process of the first compilation. Later we will put our newly compiled code here as well. 

You can copy all the data using `docker run -ti --rm -v ${PWD}/volumes/lib:/rlib pecan/base:develop cp -a /usr/local/lib/R/site-library/. /rlib/`. This will copy all compiled packages to your local machine.

This only needs to be done once (or if the PEcAn base image changes drastically, for example a new version of R). You can also always delete all files in the `volumes/lib` folder, and recompile PEcAn from scratch.

#### copy web config file (optional)

The `docker-compose.override.yml` file has a section that will enable editing the web application. This is by default commented out. If you want to uncoment it you will need to first copy the config.php from the docker/web folder. You can do this using `cp docker/web/config.docker.php web/config.php`.

### PEcAn Development

To begin development we first have to bring up the full PEcAn stack. This assumes you have done once the steps above. You don't need to stop any running containers, you can use the following command to start all containers: `docker-compose -f docker-compose.yml -f docker-compose.dev.yml up -d`. At this point you have PEcAn running in docker.

The current folder (most likely your clone of the git repository) is mounted in some containers as `/pecan`, and in the case of rstudio also in your home folder as `pecan`. You can see which containers exactly in `docker-compose.override.yml`.

You can now modify the code on your local machine, or you can use [rstudio](http://localhost:8000) in the docker stack. Once you made changes to the code you can compile the code either in the terminal of rstudio (`cd pecan && make`) or using `./scripts/compile.sh` from your machine (latter is nothing more than a shell script that runs `docker-compose exec executor sh -c 'cd /pecan && make'`. 

The compiled code is written to `/usr/local/lib/R/site-library` which is mapped to `volumes/lib` on your machine. This same folder is mounted in many other containers, allowing you to share the same PEcAn modules in all containers. Now if you change a module, and compile all other containers will see and use this new version of your module.

To compile the PEcAn code you can use the make command in either the rstudio container, or in the executor container. The script [`compile.sh`](sripts/compile.sh) will run make inside the executor container.

### Workflow Submission

You can submit your workflow either in the executor container or in rstudio container. For example to run the `docker.sipnet.xml` workflow located in the tests folder you can use: 

```
docker-compose exec executor bash
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

# Advanced Development Options

## Linux and User permissions

(On Mac OSX and Windows files should automatically be owned by the user running the docker-compose commands)

This will leverage of NFS to mount the file system in your local docker image, changing the files to owned by the user specified in the export file. Try to limit this to only your PEcAn folder since this will allow anybody on this system to get access to the exported folder as you!

First install nfs server:

```
apt-get install nfs-kernel-server
```

Next export your home directory:

```
echo -e "$PWD\t127.0.0.1(rw,no_subtree_check,all_squash,anonuid=$(id -u),anongid=$(id -g))" | sudo tee -a /etc/exports
```

And export the filesystem.

```
sudo exportfs -va
```

At this point you have exported your home directory, only to your local machine. All files written to that exported filesystem will be owned by you (`id -u`) and your primary group (`id -g`).

Finally we can modify the docker-compose.dev.yaml file to allow for writing files to your PEcAn folder as you:

```
volumes:
  pecan_home:
    driver_opts:
      type: "nfs"
      device: ":${PWD}"
      o: "addr=127.0.0.1"
```
