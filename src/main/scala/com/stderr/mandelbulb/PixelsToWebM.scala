package com.stderr.mandelbulb

import java.nio.ByteBuffer

import org.jcodec.codecs.vpx.VP8Encoder
import org.jcodec.common.SeekableByteChannel

import org.jcodec.common.NIOUtils
import org.jcodec.common.model.{Size, ColorSpace, Picture}
import org.jcodec.containers.mkv.muxer.{MKVMuxerTrack, MKVMuxer}
import org.jcodec.scale.RgbToYuv420p

object PixelsToWebM {
  def toPicture(pair:(Int, Iterable[(Scene, Point, Pixel)])): (Int, Array[Array[Int]]) = {
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
    (pair._1, yuv.getData)
  }
}

class PixelsToWebM(fileName: String, width: Int, height: Int) {

  val encoder: VP8Encoder = new VP8Encoder(10);
  val sink: SeekableByteChannel = NIOUtils.writableFileChannel(new java.io.File(fileName))
  val muxer: MKVMuxer = new MKVMuxer();
  val videoTrack: MKVMuxerTrack = muxer.createVideoTrack(new Size(width, height), "V_VP8")
  var ii: Int = 0

  def encodeFrame(bits: Array[Array[Int]], width: Int, height: Int): ByteBuffer = {
    val buf: ByteBuffer = ByteBuffer.allocate(width * height * 3);
    val yuv = new Picture(width, height, bits, ColorSpace.YUV420)
    encoder.encodeFrame(yuv, buf);
  }

  def writeFile(buffer: ByteBuffer) = {
      videoTrack.addSampleEntry(buffer, ii *  10)
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

  /*
  private static void png2webm(String pattern, String out) throws IOException {
    FileChannelWrapper sink = null;
    try {
      sink = NIOUtils.writableFileChannel(new File(out));
      VP8Encoder encoder = new VP8Encoder(10); // qp
      RgbToYuv420p transform = new RgbToYuv420p(0, 0);

      MKVMuxer muxer = new MKVMuxer();
      MKVMuxerTrack videoTrack = null;

      int i;
      for (i = 0; i < 10000; i++) {
      File nextImg = new File(String.format(pattern, i));
      if (!nextImg.exists())
        continue;

      BufferedImage rgb = ImageIO.read(nextImg);

      if (videoTrack == null)
        videoTrack = muxer.createVideoTrack(new Size(rgb.getWidth(), rgb.getHeight()), "V_VP8");

      Picture yuv = Picture.create(rgb.getWidth(), rgb.getHeight(), ColorSpace.YUV420);
      transform.transform(AWTUtil.fromBufferedImage(rgb), yuv);
      ByteBuffer buf = ByteBuffer.allocate(rgb.getWidth() * rgb.getHeight() * 3);

      ByteBuffer ff = encoder.encodeFrame(yuv, buf);

      videoTrack.addSampleEntry(ff, i - 1);
    }
    if (i == 1) {
      System.out.println("Image sequence not found");
      return;
    }
    muxer.mux(sink);
    } finally {
      IOUtils.closeQuietly(sink);
    }
  }
  */
}
