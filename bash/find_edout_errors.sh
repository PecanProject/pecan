cd ~/EDBRAMS/ED/run/
for i in *log
do if tail -n 1 $i | grep error > /dev/null
    then
	echo $i
    fi
done