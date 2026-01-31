package datasource.parquet

import datasource.DataSource
import datatypes.{Field, RecordBatch, Schema}
import org.apache.arrow.dataset.file.{FileFormat, FileSystemDatasetFactory}
import org.apache.arrow.dataset.jni.NativeMemoryPool
import org.apache.arrow.dataset.scanner.ScanOptions
import org.apache.arrow.memory.RootAllocator

import scala.jdk.CollectionConverters.CollectionHasAsScala

case class ParquetDataSource(
  private val filename: String,
  customSchema: Option[Schema]
) extends DataSource {
  private lazy val reader =  {
    val datasetFactory = new FileSystemDatasetFactory(
      new RootAllocator(),
      NativeMemoryPool.getDefault,
      FileFormat.PARQUET,
      filename
    )
    val dataset = datasetFactory.finish()
    val scanner = dataset.newScan(new ScanOptions(1024 * 1024))
    scanner.scanBatches()
  }

  def schema: Schema = Schema(
    reader
      .getVectorSchemaRoot
      .getSchema
      .getFields
      .asScala
      .filter(f => customSchema.map(s => s.fields.map(_.name)).exists(_.contains(f.getName)))
      .map(f => Field(f.getName, f.getType))
      .toSeq
  )

  def scan(projection: Seq[String]): Iterable[RecordBatch] = {
    val readSchema = if (projection.isEmpty) schema else schema.select(projection)
    new ParquetScan(reader, readSchema)
  }
}
