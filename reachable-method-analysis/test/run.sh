cd ./jpass
mvn clean package -P reachability-analysis-compare --no-transfer-progress
mkdir /data/jpass
cp *.txt /data/jpass
#rm *.txt
#mvn clean package -P reachability-analysis-regular --no-transfer-progress
#mkdir /data/jpass/regular
#cp *.txt /data/jpass/regular
