# Streaming Data Using Apache Spark

[![N|Solid](https://cldup.com/dTxpPi9lDf.thumb.png)](https://nodesource.com/products/nsolid)

The quality of streaming framework can be determined by its data sources, which require a powerful messaging platform to provide solid performance.

Spark Streaming is reinforced from various data sources, sensors and devices such as live streams from Apache Kafka, Apache Flume, Amazon Kinesis, Twitter. Additionaly, data can also be flew from storage services such as HDFS and AWS. Spark Streaming processes data by applying numerous algorithms and advanced data processing functions like map, reduce, join, which then transfered to a range of external file systems, or to feed the live dashboards.

By reason, a stream of input data represented by Spark as a discretized stream, then stored and processed as a series of RDDs (Resilient Distributed Dataset). Spark is able to operate batch processing on data by using these RDDs, which contains snapshots of the processed data up to a certain time period.

## Why Spark?
Spark operates on the concept of micro-batches those are composed of series of events batched together over a period of time. This means that Spark Streaming should not be considered as a real-time stream processing engine. The batch interval is defined in between 500ms to about 5,000ms that can be higher. Little batch intervals like 500ms, makes system to be closer to real time, and also causes more indirect computations (Overhead). However the comparison between Spark Streaming and other engines is a challenging process due to differences and complexity among systems, its micro-batched infrastructure gives an upper hand on the concept of speed.

