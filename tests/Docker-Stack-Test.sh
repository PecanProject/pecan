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


# -----------------------------------------------
#               cURL Request Template
#-----------------------------------------------

# Be sure to use the -H header and also use the bridge IP address of the Docker host (172.17.0.1) and not localhost


# curl -v -L -X POST -H "Host: pecan.localhost" \
#     -F 'hostname=' \
#     -F 'modelid=' \
#     -F 'sitegroupid=' \
#     -F 'siteid=' \
#     -F 'sitename=' \
#     -F 'pft[]=' \
#     -F 'start=' \
#     -F 'end=' \
#     -F 'input_met=' \
#     -F 'email=' \
#     -F 'notes=' \
# 'http://172.17.0.1/pecan/04-runpecan.php'

# Adjust the sleep time according to the model you are running

# Append the workflowid and ID after `out/` by 1 for every consecutive run

# curl -v -L -H "Host: pecan.localhost" \
# 'http://172.17.0.1/pecan/dataset.php?workflowid=(ID)&type=file&name=out/(ID)/sipnet.out'