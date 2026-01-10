package execution

import datasource.ParquetDataSource
import datasource.csv.CsvDataSource
import logicalplan.{DataFrame, DataFrameImpl, Scan}

class ExecutionContext(settings: Map[String, String] = Map.empty) {
  def csv(fileName: String): DataFrame = DataFrameImpl(Scan(fileName, CsvDataSource()))
  def parquet(fileName: String): DataFrame = DataFrameImpl(Scan(fileName, ParquetDataSource()))
}
