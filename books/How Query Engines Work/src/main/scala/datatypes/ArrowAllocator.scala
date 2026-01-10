package datatypes

import org.apache.arrow.memory.BufferAllocator
import org.apache.arrow.memory.RootAllocator

/** Shared Arrow memory allocator for the entire application. */
object ArrowAllocator {
  val rootAllocator: BufferAllocator = new RootAllocator(Long.MaxValue)
}
