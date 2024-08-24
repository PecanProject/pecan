# PEcAn Development

This is a minimal guide to getting started with PEcAn development under Docker. You can find more information about docker in the [pecan documentation](https://pecanproject.github.io/pecan-documentation/master/docker-index.html).

## Requirements and Recommendations

Docker is the primary software requirement; it handles all of the other software dependencies. This has been tested on Ubuntu 18.04 and above, MacOS Sonoma, and Windows 10 with Windows Subsystem for Linux 2 (following the Linux instructions).

- Software (installation instructions below):
  - Docker version 26
  - Docker Compose version 2.27
  - Git (optional until you want to make major changes)
- Hardware
  - 100 GB storage (minimum 50 GB)
  - 16 GB RAM (minimum 8 GB)

## Git Repository and Workflow

We recommend following the the [gitflow](https://nvie.com/posts/a-successful-git-branching-model/) workflow and working in your own [fork of the PEcAn repsitory](https://help.github.com/en/github/getting-started-with-github/fork-a-repo). See the [PEcAn developer guide](book_source/02_demos_tutorials_workflows/05_developer_workflows/02_git/01_using-git.Rmd) for further details. In the `/scripts` folder there is a script called [syncgit.sh](scripts/syncgit.sh) that will help with synchronizing your fork with the official repository.

To clone the PEcAn repository:

```sh
git clone git@github.com:pecanproject/pecan
cd pecan
# alternatively, if you haven't set up ssh keys with GitHub
# git clone https://github.com/PecanProject/pecan
```

## Developing in Docker

The use of Docker in PEcAn is described in detail in the [PEcAn documentation](https://pecanproject.github.io/pecan-documentation/master/docker-index.html). This is intended as a quick start.

### Installing Docker

To install Docker and Docker Compose, see the docker documentation:

- Docker Desktop (includes Docker Compose) in [MacOS](https://docs.docker.com/desktop/install/mac-install/) or [Windows](https://docs.docker.com/desktop/install/windows-install/) or [Linux](https://docs.docker.com/desktop/install/linux-install/)
- On Linux, you can also choose to separately install the [Docker engine](https://docs.docker.com/engine/install/) and [Docker Compose](https://docs.docker.com/compose/install/).

_Note for Linux (including Windows WSL2) users:_ add your user to the docker group. This will prevent you from having to use `sudo` to start the docker containers, and makes sure that any file that is written to a mounted volume is owned by you. This can be done using

```sh
# for linux users
sudo adduser ${USER} docker.
```

### Deploying PEcAn in Docker

To get started with development in docker we need to bring up the docker stack first. In the main pecan folder you will find the [docker-compose.yml](docker-compose.yml) file that can be used to bring up the pecan stack. There is also the [docker-compose.dev.yaml](docker-compose.dev.yaml) file that adds additional containers, and changes some services to make it easier for development.

By default Compose will use the files `docker-compose.yml` and `docker-compose.override.yml`. We will use the default `docker-compose.yml` file from PEcAn. The `docker-compose.override.yml` file can be used to configure it for your specific environment, in our case we will use it to setup the docker environment for development. Copy the `docker-compose.dev.yml` file to `docker-compose.override.yml` to start working with your own override file, i.e. :

For Linux/MacOSX

```sh
cp docker-compose.dev.yml docker-compose.override.yml
```

You can now use the command `docker compose` to work with the containers setup for development. **The rest of this document assumes you have done this step.**

### First time setup

The steps in this section only need to be done the first time you start working with the stack in docker. After this is done you can skip these steps. You can find more detail about the docker commands in the [pecan documentation](https://pecanproject.github.io/pecan-documentation/master/docker-index.html).

- setup .env file
- create folders to hold the data
- load the postgresql database
- load some test data
- copy all R packages (optional but recommended)
- setup for web folder development (optional)

#### .env file

You can copy the [`docker/env.example`](docker/env.example) file as .env in your pecan folder.

```sh
cp docker/env.example .env
```

The variables we want to modify are:

- `COMPOSE_PROJECT_NAME`, the prefix for all containers. Set this to "pecan".
- `PECAN_VERSION`, the docker image we start with. Set this to "develop".

Both of these variables should also be uncommented by removing the # preceding them.

At the end you should see the following if you run the command  `egrep -v '^(#|$)' .env`:

```sh
COMPOSE_PROJECT_NAME=pecan
PECAN_VERSION=develop
```

If you have a Linux system you will need to set UID and GID (these are needed by rstudio when sharing files between host and container):

```sh
echo "UID=$(id -u)" >> .env
echo "GID=$(id -g)" >> .env
```

Later you may wish to modify other variables in `.env`, but for this intro please confirm that the system is working with this minimal configuration first.

Once you have setup `docker-compose.override.yml` and the `.env` files, it is time to pull all docker images that will be used. Doing this will make sure you have the latest version of those images on your local system.

```sh
docker compose pull
```

#### Folders (optional)

The goal of the development is to share the development folder with your container, whilst minimizing the latency. What this will do is setup the folders to allow for your pecan folder to be shared, and keep the rest of the folders managed by docker. Some of this is based on Dave Scott's DockerCon 2020 presentation ["New Docker Desktop Filesharing Features"](https://www.youtube.com/watch?v=gyddZyc8r48&t=512s). In this talk it is recommended to keep the database on the filesystem managed by docker, as well as any other folders that are not directly modified on the host system (not using the docker managed volumes could lead to a large speed loss when reading/writing to the disk). The `docker-compose.override.yml` can be modified to copy all the data to the local filesystem, by uncommenting the appropriate blocks. If you are sharing more than the pecan home directory you will need to make sure that these folder exist. As from the video, it is recommended to keep these folders outside of the actual pecan folder to allow for better caching capabilities of the docker system.

If you have uncommented the volumes in `docker-compose.override.yml` you will need to create the folders. Assuming you have not modified the values, you can do this with:

```sh
mkdir -p $HOME/volumes/pecan/{R_library,pecan,portainer,postgres,rabbitmq,traefik}
```

The following volumes are specified:

- **pecan_home** : is the checked out folder of PEcAn. This is shared with the executor and rstudio container allowing you to share and compile PEcAn. (defaults to current folder)
- **pecan_web** : is the checked out web folder of PEcAn. This is shared with the web container allowing you to share and modify the PEcAn web app. (defaults to web folder in the current folder)
- **R_library** : holds all the R packages for the specific version of PEcAn and R. This folder will be shared amongst all other containers, and will contain the compiled PEcAn code. (defaults to managed by docker, or $HOME/volumes/pecan/R_library)
- **pecan** this holds all the data, such as workflows and any downloaded data.  (defaults to managed by docker, or $HOME/volumes/pecan/pecan)
- **traefik** holds persisent data for the web proxy, that directs incoming traffic to the correct container. (defaults to managed by docker, or $HOME/volumes/pecan/traefik)
- **postgres** holds the actual database data. If you want to backup the database, you can stop the postgres container, zip up the folder. (defaults to managed by docker, or $HOME/volumes/pecan/postgres)
- **rabbitmq** holds persistent information of the message broker (rabbitmq). (defaults to managed by docker, or $HOME/volumes/pecan/rabbitmq)
- **portainer** if you enabled the portainer service this folder is used to hold persistent data for this service. You will need to enable this service. (defaults to managed by docker, or $HOME/volumes/pecan/portainer)

These folders will hold all the persistent data for each of the respective containers and can grow. For example the postgres database is multiple GB. The pecan folder will hold all data produced by the workflows, including any downloaded data, and can grow to many giga bytes.

Note that the volume names shown here are the ones that appear in the compose file. When examining volumes in Docker, each name will have an additional `pecan_` prefixed: `pecan_pecan_home`, `pecan_traefik`, and so on.

#### Postgresql database

First we bring up postgresql (we will start RabbitMQ as well since it takes some time to start):

```sh
docker compose up -d postgres rabbitmq
```

This will start postgresql and rabbitmq. We need to wait for a few minutes (you can look at the logs using `docker compose logs postgres`) to see if it is ready.

Once the database has finished starting up we will initialize the database. Now you can load the database using the following commands. The first command will make sure we have the latest version of the image, the second command will actually load the information into the database.

```sh
docker pull pecan/db
docker run --rm --network pecan_pecan pecan/db
```

Once that is done we create two users for BETY,  first user is the guest user that you can use to login in the BETY interface. The second user is a user with admin rights.

```sh
docker compose run --rm bety user guestuser guestuser "Guest User" guestuser@example.com 4 4
docker compose run --rm bety user carya illinois "Carya Demo User" carya@example.com 1 1
```

#### Load example data

Once the database is loaded we can add some example data, some of the example runs and runs for the ED model, assume some of this data is available. This can take some time, but all the data needed will be copied to the `/data` folder in the pecan containers. As with the database we first pull the latest version of the image, and then execute the image to copy all the data:

```sh
docker pull pecan/data:develop
docker run -ti --rm --network pecan_pecan --volume pecan_pecan:/data --env FQDN=docker pecan/data:develop
```

Linux & Mac

```bash
# Change ownership of /data directory in pecan volume to the current user
docker run -ti --rm --network pecan_pecan --volume pecan_pecan:/data pecan/data:develop chown -R "$(id -u).$(id -g)" /data

docker run -ti --user="$(id -u)" --rm --network pecan_pecan --volume pecan_pecan:/data --env FQDN=docker pecan/data:develop
```

#### Copy R packages (optional but recommended)

Next copy the R packages from a container to volume `pecan_lib`. This is not really needed, but will speed up the process of the first compilation. Later we will put our newly compiled code here as well. This folder is shared with all PEcAn containers, allowing you to compile the code in one place, and have the compiled code available in all other containers. For example modify the code for a model, allows you to compile the code in rstudio container, and see the results in the model container.

You can copy all the data using the following command. This will copy all compiled packages to your local machine.

```bash
docker run -ti --rm -v pecan_R_library:/rlib pecan/base:develop cp -a /usr/local/lib/R/site-library/. /rlib/
```

If you have set a custom UID or GID in your `.env`, change ownership of these files as described above for the data volume. E.g. if you use the same UID in the containers as on your host machine, run:

```bash
docker run -ti --rm -v pecan_R_library:/rlib pecan/base:develop chown -R "$(id -u):$(id -g)" /rlib/
```

#### Copy web config file (optional)

If you want to use the web interface, you will need to:

1. Uncomment the web section from the `docker-compose.override.yml` file. This section includes three lines at the top of the file, just under the `services` section. Uncomment the lines that start `web:`, `volumes:`, and `- pecan_web:`.
2. Then copy the config.php from the docker/web folder. You can do this using

For Linux/MacOSX

```sh
cp docker/web/config.docker.php web/config.php
```

For Windows

```sh
copy docker\web\config.docker.php web\config.php
```

## PEcAn Development Setup

To begin development we first have to bring up the full PEcAn stack. This assumes you have done once the steps above. You don\'t need to stop any running containers, you can use the following command to start all containers. At this point you have PEcAn running in docker.

```sh
docker compose up -d
```

The current folder (most likely your clone of the git repository) is mounted in some containers as `/pecan`, and in the case of rstudio also in your home folder as `pecan`. You can see which containers exactly in `docker-compose.override.yml`.

You can now modify the code on your local machine, or you can use [rstudio](http://pecan.localhost) in the docker stack. Once you made changes to the code you can compile the code either in the terminal of rstudio (`cd pecan && make`) or using `./scripts/compile.sh` from your machine (latter is nothing more than a shell script that runs `docker compose exec executor sh -c 'cd /pecan && make'`.

The compiled code is written to `/usr/local/lib/R/site-library` which is mapped to `volumes/pecan/R_library` on your machine. This same folder is mounted in many other containers, allowing you to share the same PEcAn modules in all containers. Now if you change a module, and compile all other containers will see and use this new version of your module.

To compile the PEcAn code you can use the make command in either the rstudio container, or in the executor container. The script [`compile.sh`](scripts/compile.sh) will run make inside the executor container.

### Workflow Submission

You can submit your workflow either in the executor container or in rstudio container. For example to run the `docker.sipnet.xml` workflow located in the tests folder you can use:

```sh
docker compose exec executor bash
# inside the container
cd /pecan/tests
R CMD ../web/workflow.R --settings docker.sipnet.xml
```

A better way of doing this is developed as part of GSOC, in which case you can leverage of the restful interface defined, or using the new R PEcAn API package.

## PEcAn URLs

You can check the RabbitMQ server used by pecan using <https://rabbitmq.pecan.localhost> on the same server that the docker stack is running on. You can use rstudio either with <http://server/rstudio> or at <http://rstudio.pecan.localhost>. To check the traefik dashboard you can use <http://traefik.pecan.localhost>.

If the stack is running on a remote machine, you can use ssh and port forwarding to connect to the server. For example `ssh -L 8000:localhost:80` will allow you to use <http://rabbitmq.pecan.localhost:8000/> in your browser to connect to the remote PEcAn server RabbitMQ.

## Directory Structure

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

## Advanced Development Options

### Reset all containers/database

If you want to start from scratch and remove all old data, but keep your pecan checked out folder, you can remove the folders where you have written the data (see `folders` below). You will also need to remove any of the docker managed volumes. To see all volumes you can do `docker volume ls -q -f name=pecan`. If you are sure, you can either remove them one by one, or remove them all at once using the command below. **THIS DESTROYS ALL DATA IN DOCKER MANAGED VOLUMES.**.

```sh
docker volume rm $(docker volume ls -q -f name=pecan)
```

If you changed the docker-compose.override.yml file to point to a location on disk for some of the containers (instead of having them managed by docker) you will need to actually delete the data on your local disk, docker will NOT do this.

## Reset the R_library folder

If you want to reset the R library folder that is mounted across all machines, for example when there is a new version of PEcAn or a a new version of R, you will need to delete the volume pecan_R_library, and repopulate it. To delete the volume use the following command, and then look at "copy R packages" to copy the data again.

```sh
docker compose down
docker volume rm pecan_R_library
```

## Linux and User permissions

(On Mac OSX and Windows files should automatically be owned by the user running the docker compose commands).

If you use mounted folders, make sure that these folders are writable by the containers. Docker on Linux will try to preserve the file permissions. To do this it might be necessary for the folders to have rw permissions. This can be done by using `chmod 777 $HOME/volumes/pecan/{lib,pecan,portainer,postgres,rabbitmq,traefik}`.

This will leverage of NFS to mount the file system in your local docker image, changing the files to owned by the user specified in the export file. Try to limit this to only your PEcAn folder since this will allow anybody on this system to get access to the exported folder as you!

First install nfs server:

```sh
apt-get install nfs-kernel-server
```

Next export your home directory:

```sh
echo -e "$PWD\t127.0.0.1(rw,no_subtree_check,all_squash,anonuid=$(id -u),anongid=$(id -g))" | sudo tee -a /etc/exports
```

And export the filesystem.

```sh
sudo exportfs -va
```

At this point you have exported your home directory, only to your local machine. All files written to that exported filesystem will be owned by you (`id -u`) and your primary group (`id -g`).

Finally we can modify the `docker-compose.override.yml` file to allow for writing files to your PEcAn folder as you:

```sh
volumes:
  pecan_home:
    driver_opts:
      type: "nfs"
      device: ":${PWD}"
      o: "addr=127.0.0.1"
```
