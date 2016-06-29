#!/usr/bin/expect -f

if {[llength $argv] != 3} {
    puts "${argv0} hostname username folder"
    puts "folder should contain a file called password with the actual password"
    exit -1
}

# set Variables
set sshHostname  [lrange $argv 0 0]
set sshUsername  [lrange $argv 1 1]
set sshTunnelDir [lrange $argv 2 2]
set sshPassFile  [file join $sshTunnelDir password]
set sshTunnel    [file join $sshTunnelDir tunnel]
set sshPID       [file join $sshTunnelDir pid]

# check if tunnel already exists
if {[file exists $sshTunnel]} {
    puts "${sshTunnel} already exists"
    exit -1
}

# load the file
if {! [file exists $sshPassFile]} {
    puts "missing ${sshPassFile} file"
    exit -1
}

# read password and delete file
set f [open $sshPassFile]
set sshPassword [string trim [read $f]]
close $f
file delete $sshPassFile

# start ssh
spawn ssh -nN -o ControlMaster=yes -o ControlPath="$sshTunnel" -l $sshUsername $sshHostname
set f [open $sshPID "w"]
puts $f [exp_pid]
close $f

# answer some questions (like password) and just wait
set timeout -1
expect {
    "yes/no" { 
        send "yes\r"
        exp_continue
    }
    "*?assword" {
        puts "got password prompt"
        send "${sshPassword}\r"
        exp_continue
    }
    eof {
        file delete $sshPID
        exit -1
    }
}
