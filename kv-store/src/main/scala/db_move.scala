package sandbox

import scala.reflect.runtime.universe._

object KVStore {
  abstract class Key(name: String) {
    type Value
  }

  def newKey[A : TypeTag](name: String) =
    new Key(name) {
      val typ = typeOf[A]
      type Value = A
    }
}

class KVStore {
  import KVStore.Key
  import collection.mutable.Map

  val store = Map.empty[Key, Any]
  def get(key: Key): Option[key.Value] = store.get(key).asInstanceOf[Option[key.Value]]
  def set(key: Key)(value: key.Value): Unit = store.update(key, value)

  def setNew[A : TypeTag](keyName: String)(value: A) = {
    val key = KVStore.newKey[A](keyName)
    set(key)(value)
    key
  }
}

object Main {

  def main(args: Array[String]): Unit = {
    val dataStore = new KVStore

    val foo = KVStore.newKey[Int]("foo")
    val bar = KVStore.newKey[String]("bar")
    
    dataStore.set(foo)(123)
    dataStore.set(bar)("hello")
    dataStore.get(foo) // result: Option[Int] = Some(123)

    val baz = dataStore.setNew("baz")(true)

    dataStore.get(baz) // result: Option[Boolean] = Some(true)
    dataStore.set(baz)(false)

    dataStore.set(baz)(8) // Fails compilation
    dataStore.set(bar)(true) // Fails compilation
    dataStore.set(foo)("test") // Fails compilation
  }
}
