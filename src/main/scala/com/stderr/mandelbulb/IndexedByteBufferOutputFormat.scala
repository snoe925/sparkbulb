package com.stderr.mandelbulb

import java.io.DataOutputStream

import org.apache.hadoop.fs.{FileSystem, Path}
import org.apache.hadoop.io.compress.GzipCodec
import org.apache.hadoop.io.compress.CompressionCodecFactory
import org.apache.hadoop.io.compress.CompressionCodec
import org.apache.hadoop.io.{BytesWritable, NullWritable}
import org.apache.hadoop.mapred.lib.MultipleOutputFormat
import org.apache.hadoop.mapred.{FileOutputFormat, JobConf, RecordWriter, Reporter}
import org.apache.hadoop.util.Progressable

class IndexedByteBufferOutputFormat extends MultipleOutputFormat[Any, Any] {

  override def getBaseRecordWriter(fileSystem: FileSystem, jobConf: JobConf, name: String, progressable: Progressable): RecordWriter[Any, Any] = {
    new ByteRecordWriter[Any, Any](fileSystem, jobConf)
  }
}

class ByteRecordWriter[K, V](fileSystem: FileSystem, jobConf: JobConf) extends RecordWriter[K, V] {

  def write(key: K, value: V): Unit = {
    val nullValue = value == null || value.getClass.equals(classOf[NullWritable])
    if (nullValue) return

    val outputPath = FileOutputFormat.getOutputPath(jobConf)
    val ii = key.asInstanceOf[Integer].intValue()
    val name = "%08d.vp8.gz".format(ii)
    val path = fileSystem.create(new Path(outputPath, name))
    val ccf: CompressionCodecFactory = new CompressionCodecFactory(jobConf)
    val codec: CompressionCodec = ccf.getCodecByClassName(classOf[GzipCodec].getName());
    val out = new DataOutputStream(codec.createOutputStream(path))
    val bw = value.asInstanceOf[BytesWritable]
    out.write(bw.copyBytes())
    out.close()
  }

  override def close(reporter: Reporter): Unit = {}
}