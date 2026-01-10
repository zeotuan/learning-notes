package datasource.csv

import datatypes.{ArrowAllocator, ArrowFieldVector, RecordBatch, Schema}
import de.siegmar.fastcsv.reader.{CsvReader, NamedCsvRecord, NamedCsvRecordHandler}
import org.apache.arrow.vector._

import scala.jdk.CollectionConverters.{CollectionHasAsScala, IteratorHasAsScala}

case class CsvReaderAsIterator(
  schema: Schema,
  reader: CsvReader[NamedCsvRecord],
  batchSize: Int
) extends Iterable[RecordBatch] {
  override def iterator(): Iterator[RecordBatch] = CsvReaderIterator(schema, reader, batchSize)
}

object CsvReaderAsIterator {
  def apply(
    schema: Schema,
    filename: String,
    hasHeader: Boolean,
    batchSize: Int
  ): CsvReaderAsIterator = {
    val filePath = new java.io.File(filename).toPath
    val reader = if (hasHeader) {
      CsvReader.builder().ofNamedCsvRecord(filename)
    } else {
      val callbackHandler = NamedCsvRecordHandler.builder()
        .header(schema.fields.map(_.name): _*)
        .build()

      CsvReader.builder().build(callbackHandler, filePath)
    }
    CsvReaderAsIterator(schema, reader, batchSize)
  }
}


case class CsvReaderIterator(
  schema: Schema,
  reader: CsvReader[NamedCsvRecord],
  batchSize: Int
) extends Iterator[RecordBatch] {
  private val csvIterator = reader.iterator().asScala.grouped(batchSize)

  override def hasNext: Boolean = csvIterator.hasNext

  override def next(): RecordBatch = createBatch(csvIterator.next())

  private def createBatch(rows: Seq[NamedCsvRecord]): RecordBatch = {
    val arrowSchema = schema.toArrow
    val root = VectorSchemaRoot.create(arrowSchema, ArrowAllocator.rootAllocator)
    val fieldVectors = root.getFieldVectors.asScala.toIndexedSeq
    fieldVectors.foreach(_.setInitialCapacity(rows.size))
    root.allocateNew()

    fieldVectors.foreach {
      vector =>
        val rowWithIndex = rows.zipWithIndex
        val vectorName = vector.getName
        vector match {
          case v: VarCharVector =>
            rowWithIndex.foreach { case (value, index) =>
              v.setSafe(index, value.getField(vectorName).getBytes)
            }
          case v: TinyIntVector =>
            rowWithIndex.foreach { case (value, index) =>
              val strVal = value.getField(vectorName)
              if (strVal.isEmpty) {
                v.setNull(index)
              } else {
                v.setSafe(index, strVal.toByte)
              }
            }

          case v: SmallIntVector =>
            rowWithIndex.foreach { case (value, index) =>
              val strVal = value.getField(vectorName)
              if (strVal.isEmpty) {
                v.setNull(index)
              } else {
                v.setSafe(index, strVal.toShort)
              }
            }

          case v: IntVector =>
            rowWithIndex.foreach { case (value, index) =>
              val strVal = value.getField(vectorName)
              if (strVal.isEmpty) {
                v.setNull(index)
              } else {
                v.setSafe(index, strVal.toInt)
              }
            }

          case v: BigIntVector =>
            rowWithIndex.foreach { case (value, index) =>
              val strVal = value.getField(vectorName)
              if (strVal.isEmpty) {
                v.setNull(index)
              } else {
                v.setSafe(index, strVal.toLong)
              }
            }

          case v: Float4Vector =>
            rowWithIndex.foreach { case (value, index) =>
              val strVal = value.getField(vectorName)
              if (strVal.isEmpty) {
                v.setNull(index)
              } else {
                v.setSafe(index, strVal.toFloat)
              }
            }

          case v: Float8Vector =>
            rowWithIndex.foreach { case (value, index) =>
              val strVal = value.getField(vectorName)
              if (strVal.isEmpty) {
                v.setNull(index)
              } else {
                v.setSafe(index, strVal.toDouble)
              }
            }

          case _ =>
            throw new IllegalStateException(s"Unsupported vector type: ${vector.getClass.getSimpleName}")
        }
    }
    new RecordBatch(schema, fieldVectors.map(ArrowFieldVector))
  }
}
