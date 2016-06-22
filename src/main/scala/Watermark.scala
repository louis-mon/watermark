import java.io.File

import com.sksamuel.scrimage.canvas.WatermarkFilter
import com.sksamuel.scrimage.{Color, Image, Position, RGBColor}
import com.typesafe.config.{ConfigException, ConfigFactory}

class Watermark {
  def run(files: Traversable[String]): Unit = {
    val conf = ConfigFactory.parseFile(new File("application.conf"))
    def opt[T](value: => T): Option[T] = {
      try {
        Some(value)
      } catch {
        case _: ConfigException.Missing => None
      }
    }
    val ratio = opt(conf.getDouble("ratio")).getOrElse(0.1)
    val alpha = opt(conf.getDouble("alpha")).getOrElse(0.4)

    def toList(c: RGBColor): List[Long] = List(c.red, c.green, c.blue, c.alpha)

    def toColor(l: List[Long]) = l.map(_.toInt) match {
      case List(r, g, b, a) => RGBColor(r, g, b, a)
    }

    def mark(patch: Image) = {
      val color = {
        val mean = patch.iterator.foldLeft(List.fill(4)(0L))((acc, c) =>
          acc.zip(toList(c.toColor)).map { case (a, b) => a + b })
          .map(_ / patch.count)
        val hsv = toColor(mean).toHSV
        hsv.copy(hue = (hsv.hue + 180) % 360, value = 1 - hsv.value)
      }
      val text = conf.getString("text")
      val h = 50
      val filter = new WatermarkFilter(text, 0, h * 2, color, size = 50, antiAlias = false, alpha = alpha)
      val background = Color.White.copy(alpha = 1)
      Image.filled(h * 4, h * 4, background)
        .filter(filter)
        .autocrop(background)
        .pad(10)
        .fit(patch.width, patch.height, position = Position.BottomRight)
    }

    val outDir = opt(conf.getString("outDir"))

    def processImage(path: String): Unit = {
      val inPath: File = new File(path)
      val input = Image.fromFile(inPath)
      val patch = input.resize(ratio, Position.BottomRight)
      outDir.foreach(new File(_).mkdirs())
      val outfile = outDir
        .map(dir => new File(dir, inPath.getName))
        .getOrElse(new File(inPath.getPath.replaceAll("\\.[^.]*$", "-copy$0")))
      val markFit = mark(patch)
      val output = input.overlay(markFit, input.width - markFit.width, input.height - markFit.height)
      output.output(outfile)
    }

    files.foreach(processImage)
  }
}
