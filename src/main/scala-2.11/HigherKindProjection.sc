trait Functor[M[_]] {



  def map [U, V](m:M[U])(f: U=>V) :M[V]
}

type Tensor [T] = {
  type CoVector[X] = Function1[X, T] //Covariant
  type Vector[X] = Function1[T, X] //Contravariant
}

trait VectorFunctor[T] extends Functor[(Tensor[T])#Vector] { self =>
  override def map [U,V](v: T => U)(f: U => V): Function1[T, V] = f.compose(v)
}




// class Func extends VectorFunctor