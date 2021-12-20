cd ./jpass
mvn clean package -P reachability-analysis-compositional --no-transfer-progress
mkdir /data/jpass/compositional
cp *.txt /data/jpass/compositional
rm *.txt
mvn clean package -P reachability-analysis-regular --no-transfer-progress
mkdir /data/jpass/regular
cp *.txt /data/jpass/regular
