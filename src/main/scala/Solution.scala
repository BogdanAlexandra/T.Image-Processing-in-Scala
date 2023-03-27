import util.Pixel
import util.Util.getNeighbors
import util.Util.toGrayScale


// Online viewer: https://0xc0de.fr/webppm/
object Solution {
  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]

  def fromStringPPM(image: List[Char]): Image = {

    val image_to_string = image.mkString
    val lines = image_to_string.split("\n").toList

    val (width, height) = lines match {
      case _ :: dimensions :: tail if lines.length >= 4 => {
        val Array(w, h) = dimensions.split(' ').map(_.toInt)
              (w, h)
          }
      case _ => throw new IllegalArgumentException("Please give a correct width and height format. Thank you!")
          }


    def splitinPixels(current_line: String, acc: List[List[Pixel]]): List[List[Pixel]] = {
      val pixelValues = current_line.split(' ').map(_.toInt)
      val pixel = Pixel(pixelValues(0), pixelValues(1), pixelValues(2))
      acc match {
        case Nil => List(List(pixel))
        case x :: xs =>
          if (x.length % width == 0) (pixel :: Nil) :: acc else (pixel :: x) :: xs
      }
    }

    val pixel_matrix = lines.drop(3).foldRight(Nil: List[List[Pixel]])(splitinPixels)
    pixel_matrix
  }


  // def toStringPPM(image: Image): List[Char] = ???

  def toStringPPM(image: Image): List[Char] = {
    val format = "P3"
    val w = image.head.length
    val h = image.length
    val max_c = "255"
    val first_part = List(format, s"$w $h", max_c).mkString("\n")
    val pixel_matrix = image.map(row => row.map(p => s"${p.red.toInt} ${p.green.toInt} ${p.blue.toInt}\n"))
      .foldRight("")((rowPixels, acc) => rowPixels.mkString + acc)
    val str = (first_part + "\n" + pixel_matrix.mkString).toList
    str
  }

  // ex 1
  def verticalConcat(image1: Image, image2: Image): Image = {

    val img = image1 ++ image2
    img
  }

  // ex 2
  def horizontalConcat(image1: Image, image2: Image): Image = {

    val extracted_image = image1.lazyZip(image2)
    val img = extracted_image.map(_ ++ _)
    img
  }

  // ex 3
  def rotate(image: Image, degrees: Integer): Image = {

    degrees.toInt match {
      case 360 => image
      case 90 => image.transpose.reverse
      case 180 => image.reverse.map(_.reverse)
      case 270 => image.reverse.transpose
    }
  }


  // ex 4
  val gaussianBlurKernel: GrayscaleImage = List[List[Double]](
    List(1, 4, 7, 4, 1),
    List(4, 16, 26, 16, 4),
    List(7, 26, 41, 26, 7),
    List(4, 16, 26, 16, 4),
    List(1, 4, 7, 4, 1)
  ).map(_.map(_ / 273))

  val Gx: GrayscaleImage = List(
    List(-1, 0, 1),
    List(-2, 0, 2),
    List(-1, 0, 1)
  )

  val Gy: GrayscaleImage = List(
    List(1, 2, 1),
    List(0, 0, 0),
    List(-1, -2, -1)
  )

  def toGrayScaleImage(image: Image): GrayscaleImage = {
    image.map(row => row.map(pixel => toGrayScale(pixel)))
  }

  def edgeDetection(image: Image, threshold: Double): Image = {
    val img = toGrayScaleImage(image)
    val blurred = applyConvolution(img, gaussianBlurKernel)
    val mx = applyConvolution(blurred, Gx)
    val my = applyConvolution(blurred, Gy)
    val combined = mx.zip(my).map { case (row1, row2) => row1.zip(row2).map { case (x, y) => x.abs + y.abs } }
    val thresholded = combined.map(row => row.map(pixel => if (pixel < threshold) Pixel(0, 0, 0) else Pixel(255, 255, 255)))
    thresholded
  }

  def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage): GrayscaleImage = {

    val kernel_Rows = kernel.length
    val kernel_Colomns = kernel.head.length
    val convolvedValues = (0 until image.length - kernel_Rows + 1).map { rowStart =>
      (0 until image(0).length - kernel_Colomns + 1).map { colStart =>
        val chunk = (rowStart until rowStart + kernel_Rows).map { i =>
          image(i).slice(colStart, colStart + kernel_Colomns)
        }
        chunk.flatten.zip(kernel.flatten).map { case (a, b) => a * b }.sum
      }.toList
    }.toList
   convolvedValues

  }

  // ex 5

  def moduloPascal(m: Integer, funct: Integer => Pixel, size: Integer): Image = {

    def f(n: Integer): Int = {

        if (n == 0) 1
        else 0

    }

    val initial_matrix = List(List.tabulate(size)(n => f(n)))

    val triangle = (1 until size).foldRight(initial_matrix) { (row, acc) =>
      val newRow = (0 :: acc.head).zip(acc.head ++ List(row - 1)).map { case (a, b) => (a + b) % m }
      newRow.dropRight(1) :: acc
    }
    val triangle_reverse = triangle.reverse
    val final_Triangle = List.tabulate(triangle_reverse.length, triangle_reverse.length) { (rowIndex, colIndex) =>
      val value = triangle_reverse(rowIndex)(colIndex)
      if (rowIndex >= colIndex) value else -1
    }

    final_Triangle.map(_.map(x => funct(x)))
  }


}
