package immutable

import scala.annotation.tailrec

case class Trie[A](value: Option[A], children: List[Option[Trie[A]]]) {
  def insert(key: String, value: A): Trie[A] = Trie.insert(this, key, value, 0)
  def search(key: String): Option[A] = Trie.search(this, key, 0)
  def delete(key: String): Trie[A] = Trie.delete(this, key, 0)
}

object Trie {
  def indexInChilden(key: String, step: Int): Int = key.charAt(step) - 'a'
  def empty[A]: Trie[A] = Trie(None, List.fill(26)(None))
  def apply[A](value: A): Trie[A] = Trie(Some(value), List.fill(26)(None))
  def insert[A](node: Trie[A], key: String, value: A, step: Int): Trie[A] = {
    if (step == key.length) {
      node.copy(value = Some(value))
    } else {
      // get new next item, create empty if not exist yet
      val index = indexInChilden(key, step)
      val nextItem = node.children(index).getOrElse(empty[A])
      // rebuild tree from this point
      val newNextNode = insert(nextItem, key, value, step + 1)
      val newNext = node.children.updated(index, Some(newNextNode))
      node.copy(children = newNext)
    }
  }

  def delete[A](node: Trie[A], key: String, step: Int): Trie[A] = {
    if (step == key.length) {
      node.copy(value = None)
    } else {
      val index = indexInChilden(key, step)
      node.children(index) match {
        case None => node
        case Some(nextItem) =>
          val newNode = delete(nextItem, key, step + 1)
          val newIndexNode = if (newNode.value.isEmpty && newNode.children.forall(_.isEmpty)) {
            None
          } else {
            Some(newNode)
          }
          val newChildren = node.children.updated(index, newIndexNode)
          node.copy(children = newChildren)
      }
    }
  }

  @tailrec
  def search[A](node: Trie[A], key: String, step: Int): Option[A] = if (key.length == step) {
    node.value
  } else {
    val index = indexInChilden(key, step)
    node.children(index) match {
      case None => None
      case Some(nextItem) => search(nextItem, key, step + 1)
    }
  }
}