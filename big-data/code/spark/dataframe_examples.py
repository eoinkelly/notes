from pyspark.sql import SparkSession

spark = (
    SparkSession.builder.appName("EoinDataFrameExamples")
    # .config("spark.jars.packages", "org.apache.spark:spark-avro_2.12:3.5.1")
    .getOrCreate()
)

spark.sparkContext.setLogLevel("ERROR")

schema = "State STRING NOT NULL, Color STRING NOT NULL, Count BIGINT NOT NULL"

print("Starting")
my_file_path = "../mnm_dataset.csv"
my_df = (
    spark.read.format("csv").option("header", "true").load(my_file_path, schema=schema)
)

# print(type(spark.read))
# print(type(my_df.write))

print(my_df.printSchema())

# These work out of the box
# they don't just write a single file, they write a directory containing
# 1. The file(s) in the format specified
# 2. A _SUCCESS file
# 2. A _SUCCESS.crc file
# 3. A _common_metadata file
# For example, writing CSV generates:
#
# $ ls ./mnm_dataset.csv/
#        8 Apr  1 09:48 ._SUCCESS.crc
#     9.1K Apr  1 09:48 .part-00000-4b850d77-0926-4355-9b30-d50a9a345876-c000.csv.crc
#        0 Apr  1 09:48 _SUCCESS
#     1.2M Apr  1 09:48 part-00000-4b850d77-0926-4355-9b30-d50a9a345876-c000.csv
#
# $ ls ./mnm_dataset.json/
#        8 Apr  1 09:48 ._SUCCESS.crc
#     9.1K Apr  1 09:48 .part-00000-4b850d77-0926-4355-9b30-d50a9a345876-c000.json.crc
#        0 Apr  1 09:48 _SUCCESS
#     1.2M Apr  1 09:48 part-00000-4b850d77-0926-4355-9b30-d50a9a345876-c000.json
#
# $ ls ./mnm_dataset.parquet/
#        8 Apr  1 09:48 ._SUCCESS.crc
#     9.1K Apr  1 09:48 .part-00000-4b850d77-0926-4355-9b30-d50a9a345876-c000.snappy.parquet.crc
#        0 Apr  1 09:48 _SUCCESS
#     1.2M Apr  1 09:48 part-00000-4b850d77-0926-4355-9b30-d50a9a345876-c000.snappy.parquet
#
my_df.write.format("parquet").mode("overwrite").save("mnm_dataset.parquet")
my_df.write.format("csv").mode("overwrite").save("mnm_dataset.csv")
my_df.write.format("json").mode("overwrite").save("mnm_dataset.json")
my_df.write.format("orc").mode("overwrite").save("mnm_dataset.orc")

# Writing to arvo requires you to have the arvo jar on the classpath
# I'm not sure how to add the arvo jar for spark installed via pip (TODO)
# my_df.write.format("arvo").mode("overwrite").save("mnm_dataset.arvo")

# Writing a DatFrame to text format in Spark SQL requires:
#
# 1. you to convert all types to string and
# 2. to only have one column
#
# (my_df
# 	.withColumn("Count", my_df.Count.cast("string"))
#  	.write
# 	.format("text")
# 	.mode("overwrite")
# 	.save("mnm_dataset.text")
# )

# You can write to JDBC but it's a bit more involved
# https://spark.apache.org/docs/latest/sql-data-sources-jdbc.html

print("Done")
