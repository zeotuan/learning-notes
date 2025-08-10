package datatypes

case class Schema(fields: Seq[Field]) {
  def select(projection: Seq[String]): Schema = {
    if (projection.isEmpty) {
      this
    } else {
      if (projection.distinct.length != projection.length) {
        throw new IllegalArgumentException("Projection contains duplicate column names")
      }
      val selectFields = projection.map { name =>
        fields.filter(_.name == name) match {
          case head::Nil                => head
          case Nil                      => throw new IllegalArgumentException(s"column not found in schema: $name")
          case _::tail if tail.nonEmpty => throw new IllegalArgumentException(s"Ambiguous column name: $name")
        }
      }
      Schema(selectFields)
    }
  }
}
