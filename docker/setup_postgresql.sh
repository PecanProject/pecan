echo "######################################################################"
echo "POSTGRES"
echo "######################################################################"
#    ADD export PATH=${PATH}:/usr/pgsql-9.5/bin
#    ADD exclude=postgresql* to /etc/yum.repos.d/CentOS-Base.repo or /etc/yum/pluginconf.d/rhnplugin.conf
#    SEE https://wiki.postgresql.org/wiki/YUM_Installation#Configure_your_YUM_repository
case "$OS_VERSION" in
  RH_5)
    echo "No PostgreSQL configuration (yet) for RedHat 5"
    exit 1
    ;;
  RH_6)
    sudo service postgresql-9.5 initdb
    sudo sh -c 'if ! grep -Fq "bety" /var/lib/pgsql/9.5/data/pg_hba.conf ; then
      sed -i "/# TYPE/ a\
local   all             bety                                    trust\n\
host    all             bety            127.0.0.1/32            trust\n\
host    all             bety            ::1/128                 trust" /var/lib/pgsql/9.5/data/pg_hba.conf
    fi'
    chkconfig postgresql-9.5 on
    sudo service postgresql-9.5 start
    ;;
  RH_7)
    sudo /usr/pgsql-9.5/bin/postgresql95-setup initdb
    sudo sh -c 'if ! grep -Fq "bety" /var/lib/pgsql/9.5/data/pg_hba.conf ; then
      sed -i "/# TYPE/ a\
local   all             bety                                    trust\n\
host    all             bety            127.0.0.1/32            trust\n\
host    all             bety            ::1/128                 trust" /var/lib/pgsql/9.5/data/pg_hba.conf
    fi'
    sudo systemctl enable postgresql-9.5.service
    sudo systemctl start postgresql-9.5.service
    ;;
  Ubuntu)
    sudo sh -c 'if ! grep -Fq "bety" /etc/postgresql/9.5/main/pg_hba.conf ; then
      sed -i "/# TYPE/ a\
local   all             bety                                    trust\n\
host    all             bety            127.0.0.1/32            trust\n\
host    all             bety            ::1/128                 trust" /etc/postgresql/9.5/main/pg_hba.conf
fi'
    sudo service postgresql restart
    ;;
esac
