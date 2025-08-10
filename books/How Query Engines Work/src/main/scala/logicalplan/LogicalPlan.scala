package logicalplan

import datatypes.Schema
import logicalplan.LogicalPlan.format

trait LogicalPlan {
  def schema: Schema
  def children: Seq[LogicalPlan]
  def pretty(): String = format(this)
}

object LogicalPlan {
  def format(plan: LogicalPlan, indent: Int = 0): String = {
    val sb = new StringBuilder()
    sb.append("\t" * indent)
    sb.append(plan.toString).append("\n")
    plan.children.foreach { child =>
      sb.append(format(child, indent + 1))
    }
    sb.toString()
  }
}