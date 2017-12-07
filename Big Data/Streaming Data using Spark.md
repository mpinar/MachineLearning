# Streaming Data Using Apache Spark

[![N|Solid](https://cldup.com/dTxpPi9lDf.thumb.png)](https://nodesource.com/products/nsolid)

A streaming framework is only as good as its data sources. A strong messaging platform is the best way to ensure solid performance for any streaming system.

Spark Streaming supports the ingest of data from a wide range of data sources, including live streams from Apache Kafka, Apache Flume, Amazon Kinesis, Twitter, or sensors and other devices connected via TCP sockets. Data can also be streamed out of storage services such as HDFS and AWS S3. Data is processed by Spark Streaming, using a range of algorithms and high-level data processing functions like map, reduce, join and window. Processed data can then be passed to a range of external file systems, or used to populate live dashboards.

Logically, Spark Streaming represents a continuous stream of input data as a discretized stream. Internally, Spark actually stores and processes this stream as a sequence of RDDs. Each of these RDDs is a snapshot of all data ingested during a specified time period, which allows Spark's existing batch processing capabilities to operate on the data.

## Why Spark?
Spark streaming operates on the concept of micro-batches. This means that Spark Streaming should not be considered a real-time stream processing engine. A micro-batch consists of a series of events batched together over a period of time. The batch interval generally ranges from as little as 500ms to about 5,000ms (can be higher). The shorter the time frame (500ms), the closer to real time, and also the more overhead the system will endure. Spark Streaming is fast, but to make comparisons between Spark Streaming and other stream processing engines, such as Apache Storm, is a difficult task. 

