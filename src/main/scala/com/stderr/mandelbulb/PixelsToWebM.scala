package com.stderr.mandelbulb

import java.nio.ByteBuffer
import java.util
import java.util.zip.GZIPInputStream

import org.apache.commons.io.FileUtils
import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.{FileSystem, Path}
import org.jcodec.codecs.vpx.VP8Encoder
import org.jcodec.common.SeekableByteChannel

import org.jcodec.common.NIOUtils
import org.jcodec.common.model.{Size, ColorSpace, Picture}
import org.jcodec.containers.mkv.muxer.{MKVMuxerTrack, MKVMuxer}
import org.jcodec.scale.RgbToYuv420p

import scala.collection.mutable.ListBuffer

object PixelsToWebM {
  def toYUV420(pair: (Int, Iterable[(Scene, Point, Pixel)])): (Int, Int, Int, Array[Array[Int]]) = {
    val bits = pair._2
    val scene = bits.head._1
    // A red, green and blue color plane
    val data: Array[Array[Int]] = new Array[Array[Int]](1)
    data(0) = new Array[Int](scene.imageHeight * scene.imageWidth * 3)
    for ((_, point, pixel) <- bits) {
      val offset = 3 * (point.x + point.y * scene.imageWidth);
      // Use offset colors
      data(0)(offset) = pixel.red.toInt
      data(0)(offset + 1) = pixel.green.toInt
      data(0)(offset + 2) = pixel.blue.toInt
    }
    val rgb = new Picture(scene.imageWidth, scene.imageHeight, data, ColorSpace.RGB)
    val transform: RgbToYuv420p = new RgbToYuv420p(0, 0);
    val yuv = Picture.create(rgb.getWidth(), rgb.getHeight(), ColorSpace.YUV420)
    transform.transform(rgb, yuv)
    (scene.frame, scene.imageWidth, scene.imageHeight, yuv.getData)
  }

  def toVP8(pair: (Int, Int, Int, Array[Array[Int]])): (Int, Int, Int, Array[Byte]) = {
    val frameId = pair._1
    val imageWidth = pair._2
    val imageHeight = pair._3
    val data = pair._4
    val encoder: VP8Encoder = new VP8Encoder(10)
    val buffer: ByteBuffer = ByteBuffer.allocate(imageWidth * imageHeight * 3);
    val pic = new Picture(imageWidth, imageHeight, data, ColorSpace.YUV420)
    encoder.encodeFrame(pic, buffer)
    val encBuf = encoder.encodeFrame(pic, buffer)
    val array = buffer.array()
    val arrayOffset = encBuf.arrayOffset()
    val outArray = util.Arrays.copyOfRange(array,
      arrayOffset + buffer.position(),
      arrayOffset + buffer.limit())
    (frameId, imageWidth, imageHeight, outArray)
  }

  /*
   * Transform from pixels to VP8 compressed frame.
   */
  def rgbToVP8(pair: (Int, Iterable[(Scene, Point, Pixel)])): (Int, Int, Int, Array[Byte]) = {
    val yuv = toYUV420(pair)
    toVP8(yuv)
  }

  /*
   * Convert multiple VP8 encoding into webm
   */
  def combineFiles(width: Int, height: Int, directory: String, outputFile: String, hadoopConfig: Configuration): Unit = {
    val encoder = new PixelsToWebM(outputFile, width, height, hadoopConfig)
    val path = new Path(directory)
    val fs = path.getFileSystem(hadoopConfig)
    val files = new ListBuffer[Path]()
    val iter = fs.listFiles(path, false)
    while (iter.hasNext()) {
      val p = iter.next().getPath()
      if (p.getName.endsWith(".vp8.gz")) files += p
    }
    val sorted = files.sortWith(_.getName < _.getName)
    for (file <- sorted) {
      val is = new GZIPInputStream(fs.open(file))
      val bytes = org.apache.commons.io.IOUtils.toByteArray(is)
      is.close()
      encoder.writeFile(bytes)
    }
    encoder.finish
  }
}

class PixelsToWebM(fileName: String, width: Int, height: Int, hadoopConfig: Configuration) {

  val encoder: VP8Encoder = new VP8Encoder(10);
//  val sink: SeekableByteChannel = NIOUtils.writableFileChannel(new java.io.File(fileName))
  val path = new Path(fileName)
  val out = path.getFileSystem(hadoopConfig).create(path)
  val sink: SeekableByteChannel = new FakeSeekableByteChannel(out)
  val muxer: MKVMuxer = new MKVMuxer();
  val videoTrack: MKVMuxerTrack = muxer.createVideoTrack(new Size(width, height), "V_VP8")
  var ii: Int = 0

  def encodeFrame(bits: Array[Array[Int]], width: Int, height: Int): ByteBuffer = {
    val buf: ByteBuffer = ByteBuffer.allocate(width * height * 3);
    val yuv = new Picture(width, height, bits, ColorSpace.YUV420)
    encoder.encodeFrame(yuv, buf);
  }

  def writeFile(bytes: Array[Byte]) = {
      val buffer = ByteBuffer.wrap(bytes)
      videoTrack.addSampleEntry(buffer, ii)
      ii = ii + 1
  }

  def writeFile(buffers: Iterable[ByteBuffer], fileName: String, width:Int, height: Int) = {
    for (buffer <- buffers) {
      videoTrack.addSampleEntry(buffer, ii)
      ii = ii + 1
    }
  }

  def finish = {
    muxer.mux(sink)
    sink.close()
  }
}


