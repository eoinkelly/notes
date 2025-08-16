# Cloudwatch Metrics

- Cost is 30 cents per metric/month
- Basic monitoring
    - Data available in 5 min periods
- Detailed monitoring
    - Data available in 1 min periods
    - Enables you to get aggregated data across groups of similar instances
    - detailed monitoring adds 7 metrics per instance
- Each instance sends the same metrics to CW in both basic and detailed
    > The default namespace for metrics collected by the CloudWatch agent is
    > CWAgent, although you can specify a different namespace when you configure
    > the agent
- CloudWatch retains metric data as follows:
    - Data points with a period of less than 60 seconds are available for 3
      hours. These data points are high-resolution custom metrics.
    - Data points with a period of 60 seconds (1 minute) are available for 15
      days
    - Data points with a period of 300 seconds (5 minute) are available for 63
      days
    - Data points with a period of 3600 seconds (1 hour) are available for 455
      days (15 months)
- if metrics block is empty the agent seems to send through
    - mem_used_percent
    - disk_used_percent for each mounted filesystem
- conclusions
    - use temp namespace names to isolate my experiments
