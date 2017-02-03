Following is information on how to update the software and data to the latest version.

## Update Build and Check PEcAn

The [`build.sh`](https://github.com/PecanProject/pecan/blob/master/scripts/build.sh) script has options that make it easy to to update PEcAn, compile your local changes, and use `R CMD check` on all of the packages  the most recent versions. 

Here are the options (see `./scripts/build.sh -h`)

```
./scripts/build.sh <options>
 -c, --check         : check the R packages before install
 -d, --documentation : (re)generates all Rd files
 -f, --force         : force a build
 -g, --git           : do a git pull
 -h, --help          : this help text
 -i, --install       : install all R packages (default=yes)
 -n, --noinstall     : do not install all R packages
 -t, --test          : run tests
```

The best method to update PEcAn is to use `./scripts/build.sh -t -g`. This will get the latest version from GitHub, compile and run the local test cases.

## Updating BETY database

For more, in-depth information about BETY check out this [gitbook](https://dlebauer.gitbooks.io/betydb-documentation/content/).

BETYdb can be hosted anywhere, forming a network of instances. Data marked as public that has not failed QA/QC can be synced across different servers. For more details, see the BETYdb wiki page [Distributed BETYdb](https://github.com/PecanProject/bety/wiki/Distributed-BETYdb). 

This feature enables you to update a local instance of BETYdb (e.g. on a PEcAn Virtual Machine or version of the database used for development) without losing any local changes. To update the database and keep your changes, all you have to do is run the following:

```
cd
pecan/scripts/load.bety.sh
```

For more information see the [Updating BETYdb](https://github.com/PecanProject/bety/wiki/Updating-BETY) documentation.

## Updating BETY code

BETY uses rails to update. To update BETY and not loose any of your data the following steps can be used:

```
cd <bety installation folder>

# dump database
sudo -u postgres pg_dump -d bety > ~kooper/bety.sql

# update BETY and database
# become user that installed bety
sudo -u <owner of Rakefile> -s
git checkout master
git pull
export PATH=/usr/local/rbenv/versions/2.1.5/bin:${PATH}
bundle install
bundle install --deployment
RAILS_ENV="production" bundle exec rake db:migrate
touch tmp/restart.txt
exit
```

