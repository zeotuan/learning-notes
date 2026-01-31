package datasource.parquet

import datatypes.{ArrowAllocator, ArrowFieldVector, RecordBatch, Schema}
import org.apache.arrow.vector.ipc.ArrowReader

import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.{Success, Try}

/**
 * Based on blog post at https://www.arm64.ca/post/reading-parquet-files-java/
 */
class ParquetScan(
  private val reader: ArrowReader,
  private val customSchema: Schema
) extends Iterable[RecordBatch] with AutoCloseable {
  override def iterator(): Iterator[RecordBatch] = ParquetIterator(reader, customSchema)

  override def close(): Unit = reader.close()
}

case class ParquetIterator(
  private val reader: ArrowReader,
  private val schema: Schema
) extends Iterator[RecordBatch] {

  var batch: Try[Option[RecordBatch]] = Success(None)

  override def hasNext(): Boolean = {
    batch = nextBatch()
    batch.get.isDefined
  }

  override def next(): RecordBatch = {
    val next = batch
    batch = Success(None)
    next.get.get
  }

  private def nextBatch(): Try[Option[RecordBatch]] = Try {
    val nextBatch = reader.loadNextBatch()
    if (nextBatch) {
      //TODO: Support casting to custom schema types
      val fieldsVector = reader
        .getVectorSchemaRoot
        .getFieldVectors
        .asScala
        .filter(vector => schema.fields.map(_.name).contains(vector.getName))
        .map(ArrowFieldVector)
      val recordBatch = new RecordBatch(schema, fieldsVector.toIndexedSeq)
      Some(recordBatch)
    } else {
      None
    }
  }
}
