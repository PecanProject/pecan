curl -v -L -X POST -H "Host: pecan.localhost" \
    -F 'hostname=docker' \
    -F 'modelid=1000000014' \
    -F 'sitegroupid=1' \
    -F 'siteid=756' \
    -F 'sitename=Duke Forest - loblolly pine (US-Dk3)' \
    -F 'pft[]=temperate.coniferous' \
    -F 'start=2004/01/01' \
    -F 'end=2004/12/31' \
    -F 'input_met=CRUNCEP' \
    -F 'email=' \
    -F 'notes=' \
'http://172.17.0.1/pecan/04-runpecan.php'

sleep 200

curl -v -L -H "Host: pecan.localhost" \
'http://172.17.0.1/pecan/dataset.php?workflowid=99000000001&type=file&name=out/99000000001/sipnet.out'
