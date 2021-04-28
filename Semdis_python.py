#!/usr/bin/env python
# coding: utf-8

# In[179]:


import pyspark
import csv


# In[180]:


from pyspark import SparkContext
from pyspark.sql import SparkSession
from pyspark.sql.types import StructField, StructType, StringType, LongType, IntegerType, FloatType
from pyspark.sql.functions import lower, col, column, regexp_replace, concat_ws, udf
from pyspark.sql.functions import expr, size
from pyspark.sql.functions import split, trim
from pyspark.sql import Row


# In[181]:


ss=SparkSession.builder.appName("Semdis").getOrCreate()


# In[182]:


text_DF = ss.read.csv("/storage/home/sqs6406/StoryResults.csv", header=True, inferSchema=True)
text_DF.printSchema()
text_DF.first()


# In[183]:


text_DF.select("Story").show(1)


# In[184]:


Story_DF = text_DF.select("Story")
Story_DF.show(2)


# In[185]:


def lower_clean_str(x):
  punc = '!"#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~'
  lowercased_str = x.lower()
  for ch in punc:
    lowercased_str = lowercased_str.replace(ch, '')
  return lowercased_str
def removePunctuation(column):
     return trim(lower(regexp_replace("Story",'[^\sa-zA-Z]', ''))).alias('Story')
Trim_DF = Story_DF.withColumn("Story", trim(col("Story")))
Strip_DF = Trim_DF.select(removePunctuation(col('Story')))
#Strip_DF = Trim_DF.rdd.map(lower_clean_str)
Strip_DF.show()


# In[186]:


Story_DF2 = Strip_DF.select(split(col("Story")," ").alias("Story"))
Story_DF2.show()


# In[187]:


Count_DF = Story_DF2.select('*', size("Story").alias("Story_Length"))
Count_DF.show()
max_value = Count_DF.agg({"Story_Length": "max"}).collect()[0][0]
print(max_value)


# In[188]:


Split_DF = Count_DF.select([Count_DF["Story"][k] for k in range(max_value)])


# In[189]:


Split_DF.show()


# In[194]:


Split_DF.write.csv('Words.csv')


# In[ ]:




