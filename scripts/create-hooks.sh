#!/bin/bash

# change to right folder
cd $(dirname $0)/..


hooks=$( git rev-parse --show-toplevel )/.git/hooks

if [ ! -e ${hooks}/pre-commit ]; then
	cat > ${hooks}/pre-commit << EOF
#!/bin/bash

username=\$( git config user.name )
if [[ "\$username" == "PEcAn Demo User" || "\$username" == "" ]]; then
  echo "ERROR : please set your user.name, see https://help.github.com/articles/setting-your-username-in-git"
  exit 1
fi

email=\$( git config user.email )
if [[ "\$email" =~ "carya@pecan" || "\$email" == "" ]]; then
  echo "ERROR : please set your user.email, see https://help.github.com/articles/setting-your-email-in-git"
  exit 1
fi	
EOF
	chmod 755 ${hooks}/pre-commit
else
	if ! grep 'PEcAn Demo User' ${hooks}/pre-commit >/dev/null ; then
		cat >> ${hooks}/pre-commit << EOF

username=\$( git config user.name )
if [[ "\$username" == "PEcAn Demo User" || "\$username" == "" ]]; then
  echo "ERROR : please set your user.name, see https://help.github.com/articles/setting-your-username-in-git"
  exit 1
fi
EOF
	fi

	if ! grep 'carya@pecan' ${hooks}/pre-commit >/dev/null ; then
		cat >> ${hooks}/pre-commit << EOF

email=\$( git config user.email )
if [[ "\$email" =~ "carya@pecan" || "\$email" == "" ]]; then
  echo "ERROR : please set your user.email, see https://help.github.com/articles/setting-your-email-in-git"
  exit 1
fi
EOF
	fi
fi
