import java.awt.Font
import java.io.File
import java.nio.file.{Files, Paths}

import com.sksamuel.scrimage.{Color, Image}
import com.sksamuel.scrimage.canvas.WatermarkFilter
import com.typesafe.config.{ConfigException, ConfigFactory}

object Main extends App {
  val conf = ConfigFactory.parseFile(new File("application.conf"))
  def opt[T](value : => T) : Option[T] = {
    try {
      Some(value)
    } catch {
      case _: ConfigException.Missing => None
    }
  }
  val ratio = opt(conf.getDouble("ratio")).getOrElse(0.1)
  val mark = {
    val text = conf.getString("text")
    val h = 50
    val filter = new WatermarkFilter(text, 0, h, Color.White, size = h, antiAlias = true, alpha = 0.4)
    Image.filled(h * text.length, h, Color.Transparent)
      .filter(filter)
  }

  def processImage(path : String): Unit = {
    val inPath: File = new File(path)
    val input = Image.fromFile(inPath)
    val dims = {
      val d = input.dimensions
      List(d._1, d._2)
    }
    val patch = dims.map(_ * ratio).map(_.toInt)
    val (mw, mh) = (patch(0), patch(1))
    val outfile = new File(inPath.getPath.replaceAll("\\.[^.]*$", "-copy$0"))
    val output = input.overlay(mark.fit(mw, mh), input.width - mw, input.height - mh)
    output.output(outfile)
  }

  args.foreach(processImage)
}
