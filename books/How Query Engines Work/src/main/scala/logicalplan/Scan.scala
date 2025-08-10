package logicalplan

import datasource.DataSource

class Scan(
  path: String,
  datasource: DataSource,
  projection: Seq[String]
) extends LogicalPlan {
  override def schema: datatypes.Schema = datasource.schema.select(projection)

  override def children: Seq[LogicalPlan] = Seq.empty // Scan has no children

  override def toString: String = if (projection.isEmpty) {
    s"Scan(path: $path, datasource: ${datasource.getClass.getSimpleName}, projection: [])"
  } else {
    s"Scan(path: $path, datasource: ${datasource.getClass.getSimpleName}, projection: ${projection.mkString(", ")})"
  }
}
