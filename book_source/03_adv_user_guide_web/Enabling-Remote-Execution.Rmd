## Remote execution

PEcAn can be configured to run workflows on remote machines (with or without qsub). This does assume PEcAn is installed on the remote machine. To execute the workflows PEcAn will use SSH to connect to the remote machine. To allow for password less connections you can either setup a ssh keypair, or setup a shared tunnel.

The easiest way to create the keypair is to use `ssh-key-gen` and `ssh-key-copy`. The first command will create the key, and the second will copy it to the remote host. `ssh-key-gen` will ask for a password, leaving this blank will allow you to connect to the remote host without using a password. Once done, you should be able to login without typing your password.

The shared tunnel requires you to login once to the remote host and keep this ssh connection alive. This works well in case of an additional security request (such as a one time password), or if you have do not want to store your password less key on the machine. To setup the shared tunnel you will need to add the following to your ~/.ssh/config

```
Host *
  ControlMaster auto
  ControlPath /tmp/%r@%h:%p
```

You can add the following to your .ssh/config as well, which will make it so when you login to the remote machine it will use XYZ as your login name.

```
Host remotehost
  User XYZ
```

For example the following will set this up for you with the right permissions.

```bash
mkdir ~/.ssh
chmod 700 ~/.ssh
cat > ~/.ssh/config << EOF
Host *
  ControlMaster auto
  ControlPath /tmp/%r@%h:%p
EOF
chmod 600 ~/.ssh/config
```
