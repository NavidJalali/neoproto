package neoproto

import cats.*
import cats.syntax.all.*
import neoproto.internal.macros.ReaderMacros

trait Reader[-In, +Out]:
  self =>
  def read(in: In): Result[String, Out]

  def map[A](f: Out => A): Reader[In, A] =
    new Reader[In, A]:
      override def read(in: In): Result[String, A] =
        self.read(in).map(f)

  def flatMap[I <: In, O](f: Out => Reader[I, O]): Reader[I, O] =
    new Reader[I, O]:
      override def read(in: I): Result[String, O] =
        self.read(in).flatMap(f(_).read(in))

  def >>[A](that: Reader[Out, A]): Reader[In, A] =
    new Reader[In, A]:
      override def read(in: In): Result[String, A] =
        self.read(in).flatMap(that.read)

object Reader:
  inline def gen[In, Out]: Reader[In, Out] = ${ ReaderMacros.readerGen[In, Out] }

  def apply[In, Out](using reader: Reader[In, Out]): Reader[In, Out] = reader

  given refl[A]: Reader[A, A] = new Reader[A, A]:
    def read(in: A): Result[Nothing, A] = Result.succeed(in)

  given [In]: Monad[Result[In, *]] =
    new Monad[Result[In, *]]:
      def pure[A](a: A): Result[In, A] =
        Result.succeed(a)

      def flatMap[A, B](fa: Result[In, A])(f: A => Result[In, B]): Result[In, B] =
        fa.flatMap(f)

      def tailRecM[A, B](a: A)(f: A => Result[In, Either[A, B]]): Result[In, B] =
        f(a).flatMap {
          case Left(a)  => tailRecM(a)(f)
          case Right(b) => Result.succeed(b)
        }

  given liftTraversable[F[_]: Traverse, A, B](using Reader[A, B]): Reader[F[A], F[B]] =
    new Reader[F[A], F[B]]:
      override def read(in: F[A]): Result[String, F[B]] =
        in.map(Reader[A, B].read(_)).sequence

  transparent inline def valueClass[A]: Any =
    ${ ReaderMacros.readerForValueClassGen[A] }
