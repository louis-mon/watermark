import java.io.File

import com.sksamuel.scrimage.canvas.WatermarkFilter
import com.sksamuel.scrimage.{Color, Image, Position}
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
    val filter = new WatermarkFilter(text, 0, h * 2, Color.Black, size = 50, antiAlias = false, alpha = 0.4)
    val background = Color.White.copy(alpha = 1)
    Image.filled(h * 4, h * 4, background)
      .filter(filter)
      .autocrop(background)
      .pad(10)
  }

  val outDir = opt(conf.getString("outDir"))

  def processImage(path : String): Unit = {
    val inPath: File = new File(path)
    val input = Image.fromFile(inPath)
    val dims = {
      val d = input.dimensions
      List(d._1, d._2)
    }
    val patch = dims.map(_ * ratio).map(_.toInt)
    val List(mw, mh) = patch
    outDir.foreach(new File(_).mkdirs())
    val outfile = outDir
      .map(dir => new File(dir, inPath.getName))
      .getOrElse(new File(inPath.getPath.replaceAll("\\.[^.]*$", "-copy$0")))
    val markFit = mark.fit(mw, mh, position = Position.BottomRight)
    val output = input.overlay(markFit, input.width - markFit.width, input.height - markFit.height)
    output.output(outfile)
  }

  args.foreach(processImage)
}
