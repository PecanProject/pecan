#!/bin/bash

# Slack token for notifications
SLACK_TOKEN=${SLACK_TOKEN:-""}
SLACK_CHANNEL=${SLACK_CHANNEL:-"#github"}
SLACK_USER=${SLACK_USER:-"NCSA Build"}

post_message() {
  printf "$1\n"
  if [ "${SLACK_TOKEN}" != "" -a "${SLACK_CHANNEL}" != "" ]; then
    url="https://hooks.slack.com/services/${SLACK_TOKEN}"
    txt=$( printf "$1\n" | tr '\n' "\\n" | sed 's/"/\\"/g' )
    payload="payload={\"channel\": \"${SLACK_CHANNEL}\", \"username\": \"${SLACK_USER}\", \"text\": \"${txt}\"}"
    result=$(curl -s -X POST --data-urlencode "${payload}" $url)
  fi
}

# make sure there is only 1 running process
if [ -e /tmp/running ]; then
  echo "Already running"
  exit 0
fi
touch /tmp/running

# ----------------------------------------------------------------------
# PECAN
# ----------------------------------------------------------------------

# update pecan
cd ${HOME}/src/pecan
BRANCH="$(git rev-parse --abbrev-ref HEAD)"
git pull &>/tmp/pecan-pull.log
if [ $? != 0 ]; then
  post_message "*ERROR*   [PEcAn] : git checkout (BRANCH=${BRANCH})
\`\`\`
$(tail /tmp/pecan-pull.log)
\`\`\`"
  rm -f /tmp/running
  exit 1
fi

# only run following if not up-to-date
if ! grep -Fxq "Already up-to-date." /tmp/pecan-pull.log ; then
  COMMIT="$(git rev-parse --short HEAD)"

  # new version of PEcAn detected
  post_message "*SUCCESS* [PEcAn] : git checkout (BRANCH=${BRANCH} COMMIT=<https://github.com/PecanProject/pecan/commit/${COMMIT}|${COMMIT}>)"

  # docker build
  ./docker.sh &>/tmp/pecan-docker.log
  if [ $? != 0 ]; then
    post_message "*ERROR*   [PEcAn] : Building docker containers
\`\`\`
$(tail /tmp/pecan-docker.log)
\`\`\`"
    rm -f /tmp/running
    exit 1
  fi
  post_message "*SUCCESS* [PEcAn] : Building docker containers"

  # docker push
  ./release.sh &>/tmp/pecan-hub.log
  if [ $? != 0 ]; then
    post_message "*ERROR*   [PEcAn] : Pushing docker containers to <https://hub.docker.com/u/pecan/dashboard/|DockerHub>
\`\`\`
$(tail /tmp/pecan-hub.log)
\`\`\`"
    rm -f /tmp/running
    exit 1
  fi
  post_message "*SUCCESS* [PEcAn] : Pushing docker containers to <https://hub.docker.com/u/pecan/dashboard/|DockerHub>"

  # start new version of docker
  cd ${HOME}/docker
  /usr/local/bin/docker-compose up --no-color --detach &>/tmp/pecan-compose.log
  if [ $? != 0 ]; then
    post_message "*ERROR*   [PEcAn] : Deploying docker containers to http://pecan-dev.ncsa.illinois.edu/pecan/
\`\`\`
$(tail /tmp/pecan-compose.log)
\`\`\`"
    rm -f /tmp/running
    exit 1
  fi
  post_message "*SUCCESS* [PEcAn] : Deploying docker containers to http://pecan-dev.ncsa.illinois.edu/pecan/"
fi

# ----------------------------------------------------------------------
# BETY
# ----------------------------------------------------------------------

# update bety
cd ${HOME}/src/bety
BRANCH="$(git rev-parse --abbrev-ref HEAD)"
git pull &>/tmp/bety-pull.log
if [ $? != 0 ]; then
  post_message "*ERROR*   [BETY]  : git checkout (BRANCH=${BRANCH} COMMIT=<https://github.com/PecanProject/bety/commit/${COMMIT}|${COMMIT}>)
\`\`\`
$(tail /tmp/bety-pull.log)
\`\`\`"
  rm -f /tmp/running
  exit 1
fi

# only run following if not up-to-date
if ! grep -Fxq "Already up-to-date." /tmp/bety-pull.log ; then
  COMMIT="$(git rev-parse --short HEAD)"

  # new version of BETY detected
  post_message "*SUCCESS* [BETY]  : git checkout (BRANCH=${BRANCH} COMMIT=<https://github.com/PecanProject/bety/commit/${COMMIT}|${COMMIT}>)"

  # docker build
  if [ $? != 0 ]; then
    post_message "*ERROR*   [BETY]  : Building docker containers
\`\`\`
$(tail /tmp/bety.log)
\`\`\`"
    rm -f /tmp/running
    exit 1
  fi
  post_message "*SUCCESS* [BETY]  : Building docker containers"

  # docker push

  # start new version of docker
  cd ${HOME}/docker
  /usr/local/bin/docker-compose up --no-color --detach &>/tmp/bety-compose.log
  if [ $? != 0 ]; then
    post_message "*ERROR*   [BETTY]  : Deploying docker containers to http://pecan-dev.ncsa.illinois.edu/bety/
\`\`\`
$(tail /tmp/bety-compose.log)
\`\`\`"
    rm -f /tmp/running
    exit 1
  fi
  post_message "*SUCCESS* [BETY]  : Deploying docker containers to http://pecan-dev.ncsa.illinois.edu/bety/"
fi

# ----------------------------------------------------------------------
# STATUS PAGE
# ----------------------------------------------------------------------

# updating status page
# Rscript -e "PEcAn.visualization::create_status_page('/home/kooper/pecan/web/config.php', '/home/kooper/pecan/web/status')" &>/tmp/status.log
# if [ $? != 0 ]; then
#   post_message "UPDATING <http://pecan-dev.ncsa.illinois.edu/pecan/status.html|STATUS> = **ERROR**
# \`\`\`
# $(tail /tmp/status.log)
# \`\`\`"
#   rm -f /tmp/running
#   exit 1
# fi

rm -f /tmp/running
