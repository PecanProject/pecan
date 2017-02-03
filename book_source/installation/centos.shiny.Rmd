---
title: "Installing and configuring Shiny for PEcAn"
author: 
    - Alexey Shiklomanov
    - Rob Kooper
toc: true
---

**NOTE: Instructions are only tested for CentOS 6.5.**
**NOTE: Pretty much every step here requires root access.**

# Install the Shiny R package and Shiny server

The `shiny` R package can be installed directly in R via `install.packages("shiny")`.

Download and install the Shiny server binary ([link](https://www.rstudio.com/products/shiny/download-server)).

## CentOS

```
wget https://download3.rstudio.org/centos5.9/x86_64/shiny-server-1.4.2.786-rh5-x86_64.rpm
sudo yum install --nogpgcheck shiny-server-1.4.2.786-rh5-x86_64.rpm
```

## Ubuntu 

NOTE: The additional `gdebi` dependence may be optional and `dpkg` may work just fine, but I haven't tested this out.

```
$ sudo apt-get install gdebi-core
$ wget https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.4.2.786-amd64.deb
$ sudo gdebi shiny-server-1.4.2.786-amd64.deb
```

# Modify the shiny configuration file

The Shiny configuration file is located in `/etc/shiny-server/shiny-server.conf`. Comment out the entire file and add the following, replacing `<username>` with your user name and `<location>` with the URL location you want for your app. This will allow you to run Shiny apps from your web browser at https://your.server.edu/shiny/your-location

```
run as shiny;
server {
    listen 3838;
    location /<location>/ {
        run as <username>;
        site_dir /path/to/your/shiny/app;
        log_dir /var/log/shiny-server;
        directory_index on;
    }
}
```

For example, my configuration on the old test-pecan looks like this.

```
run as shiny;
server {
    listen 3838;
    location /ashiklom/ {
        run as ashiklom;
        site_dir /home/ashiklom/fs-data/pecan/shiny/;
        log_dir /var/log/shiny-server;
        directory_index on;
    }
}
```

...and I can access my Shiny apps at, for instance, https://test-pecan.bu.edu/shiny/ashiklom/workflowPlots.

You can add as many `location <loc> { ... }` fields as you would like.

```
run as shiny;
server {
    listen 3838;
    location /ashiklom/ {
        ...
    }
    location /bety/ {
        ...
    }
}
```

# Set the httpd proxy

Create a file `/etc/httpd/conf.d/shiny.conf` containing the following proxy settings:

```
ProxyPass           /shiny/ http://localhost:3838/
ProxyPassReverse    /shiny/ http://localhost:3838/
RedirectMatch permanent ^/shiny$ /shiny/
```

# Create a symbolic link to the shiny server service

```
sudo ln -s /opt/shiny-server/config/init.d/redhat/shiny-server /etc/init.d
```

# Start the shiny server and restart httpd

```
sudo service shiny-server stop
sudo service shiny-server start
sudo service httpd restart
```

You can check that Shiny is running with `service shiny-server status`.

# Troubleshooting

Refer to the log files for shiny (`/var/log/shiny-server.log`) and httpd (`/var/log/httpd/error-log`).



# Further reading

* [Shiny server configuration reference](http://docs.rstudio.com/shiny-server/)

