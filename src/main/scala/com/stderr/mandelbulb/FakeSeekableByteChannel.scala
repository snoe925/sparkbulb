package com.stderr.mandelbulb

import org.apache.hadoop.fs.FSDataOutputStream
import java.nio.channels.Channels

/**
 * Wrap the Channel methods used by the codec to write into a file.
 */
class FakeSeekableByteChannel(out: FSDataOutputStream) extends org.jcodec.common.SeekableByteChannel {
  private var openFlag: Boolean = true
  private val channel = Channels.newChannel(out)
  
  // Members declared in java.nio.channels.Channel
  override def close(): Unit =  {
    channel.close()
    out.close()
    openFlag = false
    }
  
  override def isOpen(): Boolean = { openFlag }
  
  // Members declared in java.nio.channels.ReadableByteChannel
  override def read(dst: java.nio.ByteBuffer): Int = {
    throw new UnsupportedOperationException() 
    }
  
  // Members declared in org.jcodec.common.SeekableByteChannel
  override def position(pos: Long): org.jcodec.common.SeekableByteChannel = {
    throw new UnsupportedOperationException() 
    }
  
  override def position(): Long = {
    throw new UnsupportedOperationException() 
    }
  override def size(): Long = {
    throw new UnsupportedOperationException() 
    }
  override def truncate(pos: Long): org.jcodec.common.SeekableByteChannel = {
    throw new UnsupportedOperationException() 
    }
  
  // Members declared in java.nio.channels.WritableByteChannel
  override def write(src: java.nio.ByteBuffer): Int = {
    channel.write(src)
  }
}