package logicalplan

import datasource.InMemoryDataSource
import execution.ExecutionContext
import logicalplan.expression.{Column, Eq}
import logicalplan.expression.Literal.lit
import logicalplan.expression.functions.col
import org.scalatest.flatspec.AnyFlatSpec

class LogicalPlanSpec extends AnyFlatSpec {
  it should "generate a logical plan" in {
    val plan = Projection(
      Selection(
        Scan(
          path = "test_path",
          datasource = InMemoryDataSource(),
          projection = Seq("col1", "col2")
        ),
        Eq(lit("col1"), lit("value1"))
      ),
      Seq(Column("col1"), Column("col2"))
    )

    println(plan.pretty())
  }

  it should "generate logical plan from dataframe" in {
    val ctx = new ExecutionContext()
    val df = ctx.csv("test.csv")
      .select(col("col1"), col("col2"))
      .filter(col("col1") === lit("value1"))

    println(df.logicalPlan.pretty())
  }

}
