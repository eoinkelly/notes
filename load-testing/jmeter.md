# JMeter

Q: What is the optimal JVM params for running jmeter on Ubuntu?


For load testing, you must run JMeter in this mode (Without the GUI) to get the
optimal results from it. To do so, use the following command options:

```
jmeter -n -t [jmx file] -l [results file] -e -o [Path to output folder]


-n This specifies JMeter is to run in non-gui mode
-t [name of JMX file that contains the Test Plan].
-l [name of JTL file to log sample results to].
-j [name of JMeter run log file].
-r Run the test in the servers specified by the JMeter property "remote_hosts"
-R [list of remote servers] Run the test in the specified remote servers
-g [path to CSV file] generate report dashboard only
-e generate report dashboard after load test
-o output folder where to generate the report dashboard after load test. Folder must not exist or be empty
```
