# Streaming Data Using Apache Spark

The quality of streaming framework can be determined by its data sources, which require a powerful messaging platform to provide solid performance.

Spark Streaming is reinforced from various data sources, sensors and devices such as live streams from Apache Kafka, Apache Flume, Amazon Kinesis, Twitter. Additionaly, data can also be flew from storage services such as HDFS and AWS (Spark Streaming, 2017). Spark Streaming processes data by applying numerous algorithms and advanced data processing functions like map, reduce, join, which then transfered to a range of external file systems, or to feed the live dashboards.

![](https://github.com/mpinar/MachineLearning/blob/master/Big%20Data/Screen%20Shot%202017-12-06%20at%2014.16.53.png?raw=true)
Figure implemented from MapR Data Technologies

By reason, a stream of input data represented by Spark as a discretized stream, then stored and processed as a series of RDDs (Resilient Distributed Dataset), (MapR Data Technologies, 2015). Spark is able to operate batch processing on data by using these RDDs, which contains snapshots of the processed data up to a certain time period.

![](https://github.com/mpinar/MachineLearning/blob/master/Big%20Data/Screen%20Shot%202017-12-06%20at%2014.18.26.png?raw=true)
Figure implemented from MapR Data Technologies

## Why Spark?
Spark operates on the concept of micro-batches those are composed of series of events batched together over a period of time (Spark Streaming, 2017). This means that Spark Streaming should not be considered as a real-time stream processing engine. The batch interval is defined in between 500ms to about 5,000ms that can be higher. Little batch intervals like 500ms, makes system to be closer to real time, and also causes more indirect computations (Overhead) (MapR Data Technologies, 2015). However the comparison between Spark Streaming and other engines is a challenging process due to differences and complexity among systems, its micro-batched infrastructure gives an upper hand on the concept of speed. Due to its micro-batched infrastructure, Spark is suitable for data processing, streaming, and machine learning on a very large scale.

Sakaki and his colleagus state the effectiveness of social media in their experiment, by showing that it is faster to inform people about earthquake by using social media than Japan Meteorological Agency (Sakaki et al, 2010). So by this example, we can put Spark in use for our case, that requires dealing with the velocity, variety and volume of Big Data. Information could be clustered or filtered, then results could be merged with other data sources (e.g. customer comments, reviews), could be used as suggestions for company to adapt new trends.


# References

 Spark Streaming. Spark.apache.org. (2017)- Spark Streaming Programming Guide 2.2.0 Documentation. [online] Available at: http://spark.apache.org/docs/latest/streaming-programming-guide.html [Accessed 7 Dec. 2017].
 
 MapR Data Technologies (2015). Getting Started with Apache Spark. [online] Available at: https://mapr.com/ebooks/spark/06-apache-spark-streaming-framework.html [Accessed 7 Dec. 2017].
 
 Sakaki, T., Okazaki, M. and Matsuo, Y. (2010) Earthquake Shakes Twitter Users: Real-time Event Detection by Social Sensors. World-Wide Web Conference Committee (IW3C2).p: 851-860. Raleigh, North Carolina, USA.
