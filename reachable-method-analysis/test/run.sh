cd ./jpass
mvn clean package -P reachability-analysis --no-transfer-progress
mkdir /data/jpass
cp *.txt /data/jpass