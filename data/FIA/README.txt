This document describes the code to download and install the FIA
database locally in a mysql database. The code leverages of a single
script (called mirror.sh to download all the new files from
apps.fs.fed.us. If any new files are downloaded it will reload the
database using the new data.

The script will also load the latest version of the access database.
This database will need to be converted to MySQL before it can be
used. The installation will have the latest database converted as of
Jan 29, 2012. The mirror script will check if a newer version is
available and exit after downloading this newer version. To convert
this access database to mysql I have used windows software. The
software is free and available from
http://www.bullzip.com/products/a2m/info.php. When converting the
access database to MySQL make sure to use the same database name as
in the script.

The mirror.sh script has 4 variables at the top that will determine
in which database all the data is written. Please set these before
running the script.
DB_USER is the name of the user for MySQL.
DB_PASS is the password used with the user.
DB is the database in which the data wil be loaded.
VER is the version of the database (currently 5.1)

To create the database and the right permissions the following
commands can be used, replacing <DB>, <DB_USER> and <DB_PASS> with
the values described above:

mysql -u root -p --execute="CREATE DATABASE <DB>; GRANT ALL ON <DB>.* to '<DB_USER>' identified by '<DB_PASS>';"
