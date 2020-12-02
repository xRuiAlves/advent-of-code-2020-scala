package utils

import scala.io.Source.fromFile

object FileReader {
    def readFile(file_path: String) : Iterator[String] = {
        fromFile(file_path).getLines
    }
}
