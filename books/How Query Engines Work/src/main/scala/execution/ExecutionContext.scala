package execution

import datasource.{CsvDataSource, ParquetDataSource}
import logicalplan.{DataFrame, DataFrameImpl, Scan}

class ExecutionContext(settings: Map[String, String] = Map.empty) {
  def csv(fileName: String): DataFrame = DataFrameImpl(Scan(fileName, CsvDataSource()))
  def parquet(fileName: String): DataFrame = DataFrameImpl(Scan(fileName, ParquetDataSource()))
}
