package optimizer

trait OptimizerRule {
  def optimize(logicalPlan: logicalplan.LogicalPlan): logicalplan.LogicalPlan
}

case class ProjectionPushdownRule() extends OptimizerRule {
  override def optimize(logicalPlan: logicalplan.LogicalPlan): logicalplan.LogicalPlan = pushDown(logicalPlan, Seq.empty)

  private def pushDown(plan: logicalplan.LogicalPlan, requiredColumns: Seq[String]): logicalplan.LogicalPlan = plan match {
    case logicalplan.Projection(input, expr) =>
      val newRequiredColumns = requiredColumns ++ Optimizer.extractColumns(expr, input)
      val newInput = pushDown(input, newRequiredColumns.distinct)
      logicalplan.Projection(newInput, expr)
    case logicalplan.Selection(input, expr) =>
      val newRequiredColumns = requiredColumns ++ Optimizer.extractColumns(Seq(expr), input)
      val newInput = pushDown(input, newRequiredColumns.distinct)
      logicalplan.Selection(newInput, expr)
    case logicalplan.Aggregate(input, groupExpr, aggExpr) =>
      val newRequiredColumns = requiredColumns ++ Optimizer.extractColumns(groupExpr, input) ++ Optimizer.extractColumns(aggExpr, input)
      val newInput = pushDown(input, newRequiredColumns)
      logicalplan.Aggregate(newInput, groupExpr, aggExpr)

    case logicalplan.Join(left, right, joinType, condition) =>
      val leftRequiredColumns = requiredColumns ++ Optimizer.extractColumns(Seq(condition), left)
      val rightRequiredColumns = requiredColumns ++ Optimizer.extractColumns(Seq(condition), right)
      val newLeft = pushDown(left, leftRequiredColumns.distinct)
      val newRight = pushDown(right, rightRequiredColumns.distinct)
      logicalplan.Join(newLeft, newRight, joinType, condition)
    case logicalplan.Scan(path, datasource, projection) => logicalplan.Scan(path, datasource, requiredColumns)
    case _ => throw new IllegalStateException("Unsupported logical plan type: " + plan.getClass)
  }
}