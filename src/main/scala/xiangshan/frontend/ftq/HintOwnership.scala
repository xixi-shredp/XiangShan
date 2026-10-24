package xiangshan.frontend.ftq

import chisel3._
import org.chipsalliance.cde.config.Parameters

/** Pure views of the existing FTQ lifetime. No Hint identity register is added. */
object HintOwnership {
  def residentPtr(index: UInt, oldest: FtqPtr)(implicit p: Parameters): FtqPtr =
    FtqPtr(oldest.flag ^ (index < oldest.value), index)

  def active(ptr: FtqPtr, oldest: FtqPtr, newest: FtqPtr): Bool =
    ptr >= oldest && ptr < newest

  def killed(ptr: FtqPtr, redirectValid: Bool, redirectNewPtr: FtqPtr,
      s3Override: Bool, s3Ptr: FtqPtr): Bool =
    Mux(redirectValid, ptr >= redirectNewPtr, s3Override && ptr > s3Ptr)
}
