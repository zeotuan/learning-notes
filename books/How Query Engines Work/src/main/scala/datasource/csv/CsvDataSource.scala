package datasource.csv

import datasource.DataSource
import datatypes.{ArrowAllocator, ArrowFieldVector, ArrowTypes, Field, RecordBatch, Schema}
import de.siegmar.fastcsv.reader.{CsvReader, NamedCsvRecord, NamedCsvRecordHandler}
import org.apache.arrow.vector._
import org.apache.arrow.vector.types.pojo.ArrowType

import scala.jdk.CollectionConverters.{CollectionHasAsScala, IterableHasAsJava, IteratorHasAsScala}
import scala.util.Try

case class CsvDataSource(
  filename: String,
  customSchema: Option[Schema],
  private val hasHeader: Boolean,
  private val batchSize: Int
) extends DataSource {
  lazy val finalSchema: Schema = customSchema.getOrElse {
    //TODO: infer types other than StringType
    val fields = CsvReader.builder().ofNamedCsvRecord(filename).iterator().next()
      .getHeader
      .asScala
      .map(Field(_, ArrowTypes.StringType))
      .toSeq
    Schema(fields)
  }
  def schema: Schema = finalSchema
  def scan(projection: Seq[String]): Iterable[RecordBatch] = {
    val readSchema = if (projection.isEmpty) finalSchema else finalSchema.select(projection)
    CsvReaderAsIterator(readSchema, filename, hasHeader, batchSize)
  }
}