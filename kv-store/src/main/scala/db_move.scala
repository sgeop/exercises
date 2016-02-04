package sandbox

import scala.reflect.runtime.universe._

case class State[S, +A](run: S => (A, S)) {
  def unit[S, A](a: A): State[S, A] =
      State(s => (a, s))

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object KVStore {
  abstract class Key(name: String) {
    type Value
  }

  def newKey[A : TypeTag](name: String) =
    new Key(name) {
      type Value = A
    }
  
  type KVAction[+A] = State[KVStore, A]

  def get(key: Key): KVAction[Option[key.Value]] = 
    State({ s: KVStore => s.get(key) })

  def set(key: Key)(value: key.Value): KVAction[Unit] = 
    State({ s: KVStore => s.set(key)(value) })

  def modify(key: Key)(f: Option[key.Value] => key.Value): KVAction[Unit] =
    get(key).map(f).map(v => set(key)(v))
}


import KVStore.Key
case class KVStore(store: Map[KVStore.Key, Any] = Map.empty[KVStore.Key, Any] ) {
  
  type KVRes[A] = (A, KVStore)
  
  def get(key: Key): KVRes[Option[key.Value]] =
    (store.get(key).asInstanceOf[Option[key.Value]], this)
  
  def set(key: Key)(value: key.Value): KVRes[Unit] = 
    ((), KVStore(store + (key -> value)))
}

object Main {

  def main(args: Array[String]): Unit = {
    import KVStore.{get, set, newKey}
    
    val foo = KVStore.newKey[Int]("foo")
    val bar = KVStore.newKey[String]("bar")
    
    val proc = for {
      _ <- set(foo)(123)
      _ <- set(bar)("Hello")
      f <- get(foo)
      b <- get(bar)
    } yield (f.getOrElse(0) * 2).toString + " " + b.getOrElse("") + "!"

    val s = KVStore()
    println(proc.run(s)._1)
  }
}
