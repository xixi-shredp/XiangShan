package xiangshan.frontend.bpu.hint

import chisel3._
import chisel3.util._

/** Shared fixed-wiring folds. No context rotation or dynamic shifter. */
object HintHash {
  def fold(x: UInt, width: Int): UInt = {
    val count = (x.getWidth + width - 1) / width
    val padded = x.pad(count * width)
    def tree(xs: Seq[UInt]): UInt = if (xs.size == 1) xs.head else {
      val (left, right) = xs.splitAt(xs.size / 2)
      tree(left) ^ tree(right)
    }
    tree((0 until count).map(i => padded((i + 1) * width - 1, i * width)))
  }
  def component(x: UInt): UInt = {
    val tag = fold(x, 17) ^ (fold(x, 16) << 2)(16, 0)
    Cat(tag, fold(x, 11))
  }
}
