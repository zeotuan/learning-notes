package physicalplan.aggregate

import datatypes.{ArrowAllocator, ArrowFieldVector, RecordBatch, Schema}
import org.apache.arrow.vector.VectorSchemaRoot
import physicalplan.PhysicalPlan
import physicalplan.expression.Expression
import physicalplan.expression.aggregate.{Accumulator, AggregateExpression}

import scala.jdk.CollectionConverters.CollectionHasAsScala

class HashAggregateExec(
  input: PhysicalPlan,
  groupExpressions: Seq[Expression],
  aggregateExpressions: Seq[AggregateExpression],
  physicalSchema: Schema
) extends PhysicalPlan {
  override def schema: Schema = physicalSchema

  override def children: Seq[PhysicalPlan] = Seq(input)

  override def execute: Iterable[RecordBatch] = {
    val map = scala.collection.mutable.HashMap[Seq[Any], Seq[Accumulator]]()
    input.execute.iterator.foreach { batch =>
      val groupKeys = groupExpressions.map(_.evaluate(batch))
      val aggrInputs = aggregateExpressions.map(_.inputExpression.evaluate(batch))

      (0 until batch.rowCount).foreach { rowIndex =>
        val key = groupKeys.map(_.getValue(rowIndex) match {
          case ba: Array[Byte] => new String(ba)
          case other           => other
        })

        map
          .getOrElseUpdate(key, aggregateExpressions.map(_.createAccumulator))
          .zipWithIndex
          .foreach { case (accumulator, aggrIndex) =>
            val inputValue = aggrInputs(aggrIndex).getValue(rowIndex)
            accumulator.accumulate(inputValue)
          }
      }
    }

    val root = VectorSchemaRoot.create(schema.toArrow, ArrowAllocator.rootAllocator)
    root.allocateNew()
    root.setRowCount(map.size)

    val builders = root.getFieldVectors.asScala.map(ArrowFieldVector).toIndexedSeq
    map.zipWithIndex.foreach { case ((groupingKey, accumulator), rowIndex) =>
      groupingKey.zipWithIndex.foreach { case (keyPart, keyIndex) =>
        builders(keyIndex).setValue(rowIndex, keyPart)
      }

      accumulator.zipWithIndex.foreach { case (acc, accIndex) =>
        builders(groupingKey.length + accIndex).setValue(rowIndex, acc.finalValue())
      }
    }

    Iterable(new RecordBatch(schema, builders))
  }
}
