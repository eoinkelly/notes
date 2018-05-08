
```
Don't use GUI mode for load testing, only for Test creation and Test debugging !
For load testing, use NON GUI Mode:
   jmeter -n -t [jmx file] -l [results file] -e -o [Path to output folder]
& adapt Java Heap to your test requirements:
   Modify HEAP="-Xms512m -Xmx512m" in the JMeter batch file
```


For load testing, you must run JMeter in this mode (Without the GUI) to get the optimal results from it. To do so, use the following command options:

-n This specifies JMeter is to run in non-gui mode
-t [name of JMX file that contains the Test Plan].
-l [name of JTL file to log sample results to].
-j [name of JMeter run log file].
-r Run the test in the servers specified by the JMeter property "remote_hosts"
-R [list of remote servers] Run the test in the specified remote servers
-g [path to CSV file] generate report dashboard only
-e generate report dashboard after load test
-o output folder where to generate the report dashboard after load test. Folder must not exist or be empty

The script also lets you specify the optional firewall/proxy server information:

-H [proxy server hostname or ip address]
-P [proxy server port]

Example
jmeter -n -t my_test.jmx -l log.jtl -H my.proxy.server -P 8000


```
================================================================================
Don't use GUI mode for load testing !, only for Test creation and Test debugging.
For load testing, use NON GUI Mode:
   jmeter -n -t [jmx file] -l [results file] -e -o [Path to web report folder]
& increase Java Heap to meet your test requirements:
   Modify current env variable HEAP="-Xms1g -Xmx1g -XX:MaxMetaspaceSize=256m" in the jmeter batch file
Check : https://jmeter.apache.org/usermanual/best-practices.html
================================================================================
```
