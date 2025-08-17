package logicalplan

import datatypes.Schema
import logicalplan.expression.{AggregateExpr, LogicalExpr}

trait DataFrame {
  def select(expr: Seq[LogicalExpr]): DataFrame
  def filter(expr: Seq[LogicalExpr]): DataFrame
  def groupBy(groupExpr: Seq[LogicalExpr], aggExpr: Seq[AggregateExpr]): DataFrame
  def schema: Schema
  def explain(): String
  def logicalPlan: LogicalPlan

  def select(expr: LogicalExpr*): DataFrame = select(expr)
  def filter(expr: LogicalExpr*): DataFrame = filter(expr)
}

case class DataFrameImpl(private val plan: LogicalPlan) extends DataFrame {
  def select(expr: Seq[LogicalExpr]): DataFrame = DataFrameImpl(Projection(plan, expr))

  def filter(expr: Seq[LogicalExpr]): DataFrame = DataFrameImpl(Selection(plan, expr))

  def groupBy(groupExpr: Seq[LogicalExpr], aggExpr: Seq[AggregateExpr]): DataFrame = DataFrameImpl(Aggregate(plan, groupExpr, aggExpr))

  def schema: Schema = logicalPlan.schema

  def explain(): String = logicalPlan.toString

  def logicalPlan: LogicalPlan = plan
}