sudo apt-get install -y default-jre
java -version

wget http://www-eu.apache.org/dist//jmeter/binaries/apache-jmeter-4.0.tgz
tar xzvf apache-jmeter-4.0.tgz

JAVA_HOME=/usr/lib/jvm/default-java JVM_ARGS="-Xms256m -Xmx512m" ./apache-jemter-4.0/bin/jmeter -v
