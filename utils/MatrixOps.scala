package utils

import scala.reflect.ClassTag

object MatrixOps {
    def flipMatrix[T:ClassTag](m: Array[Array[T]]): Array[Array[T]] = {
        m.reverse
    }

    def rotateMatrix[T:ClassTag](m: Array[Array[T]]): Array[Array[T]] = {
        val rotated_img = Array.ofDim[T](m.length, m.head.length)
        val num_levels = math.floor(m.length / 2).toInt

        for (level <- 0 until num_levels) {
            for (i <- 0 until (m.length - 2*level - 1)) {
                val x = level + i
                val rx = m.length - x - 1
                val rlevel = m.length - level - 1

                rotated_img(level)(x) = m(rx)(level)
                rotated_img(rx)(level) = m(rlevel)(rx)
                rotated_img(rlevel)(rx) = m(x)(rlevel)
                rotated_img(x)(rlevel) = m(level)(x)
            }
        }
        rotated_img
    }
}
