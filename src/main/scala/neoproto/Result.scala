package neoproto

import cats.data.NonEmptyList
import cats.*
import cats.syntax.all.*

sealed trait Result[+E, +A]:
  self =>
  def fold[B](onFailure: NonEmptyList[E] => B, onSuccess: A => B): B =
    self match
      case Result.Success(value)  => onSuccess(value)
      case Result.Failure(errors) => onFailure(errors)

  def map[B](f: A => B): Result[E, B] =
    self match
      case Result.Success(value)  => Result.Success(f(value))
      case Result.Failure(errors) => Result.Failure(errors)

  def flatMap[E1 >: E, B](f: A => Result[E1, B]): Result[E1, B] =
    self match
      case Result.Success(value)  => f(value)
      case Result.Failure(errors) => Result.Failure(errors)

  def mapError[E1](f: E => E1): Result[E1, A] =
    self match
      case Result.Success(value)  => Result.Success(value)
      case Result.Failure(errors) => Result.Failure(errors.map(f))

  def orElse[E1 >: E, A1 >: A](other: => Result[E1, A1]): Result[E1, A1] =
    self match
      case Result.Success(value)  => Result.Success(value)
      case Result.Failure(errors) => other

  def toEither: Either[NonEmptyList[E], A] =
    self match
      case Result.Success(value)  => Right(value)
      case Result.Failure(errors) => Left(errors)

  def toOption: Option[A] =
    self match
      case Result.Success(value)  => Some(value)
      case Result.Failure(errors) => None

object Result:
  def succeed[A](value: A): Result[Nothing, A] = Success(value)
  def fail[E](error: E): Result[E, Nothing]    = Failure.one(error)

  def fromEither[E, A](either: Either[E, A]): Result[E, A] =
    either match
      case Left(error)  => Failure.one(error)
      case Right(value) => Success(value)

  def fromOption[E, A](option: Option[A], errorOnNone: => E): Result[E, A] =
    option match
      case Some(value) => Success(value)
      case None        => Failure.one(errorOnNone)

  final case class Success[+A](value: A) extends Result[Nothing, A]

  final case class Failure[+E](errors: NonEmptyList[E]) extends Result[E, Nothing]:
    self =>
    def ++[E1 >: E](other: Failure[E1]): Failure[E1] =
      Failure(self.errors.concatNel(other.errors))

    def map[E1](f: E => E1): Failure[E1] =
      Failure(self.errors.map(f))

    def flatMap[E1](f: E => Failure[E1]): Failure[E1] =
      Failure(self.errors.flatMap(f(_).errors))

  object Failure:
    def one[E](error: E): Failure[E]            = Failure(NonEmptyList.one(error))
    def many[E](first: E, rest: E*): Failure[E] = Failure(NonEmptyList(first, rest.toList))

    given [E]: Semigroup[Failure[E]] with
      def combine(x: Failure[E], y: Failure[E]): Failure[E] = x ++ y

  given [E]: Monad[Result[E, *]] =
    new Monad[Result[E, *]]:
      def pure[A](a: A): Result[E, A] =
        Result.succeed(a)

      def flatMap[A, B](fa: Result[E, A])(f: A => Result[E, B]): Result[E, B] =
        fa.flatMap(f)

      def tailRecM[A, B](a: A)(f: A => Result[E, Either[A, B]]): Result[E, B] =
        f(a).flatMap {
          case Left(a)  => tailRecM(a)(f)
          case Right(b) => Result.succeed(b)
        }

  given [E, A](using Monoid[A]): Monoid[Result[E, A]] =
    new Monoid[Result[E, A]]:
      override def empty: Result[E, A]                                     = Result.Success(Monoid[A].empty)
      override def combine(x: Result[E, A], y: Result[E, A]): Result[E, A] =
        (x, y) match
          case (Result.Success(a), Result.Success(b)) => Result.Success(Monoid[A].combine(a, b))
          case (Result.Failure(a), Result.Failure(b)) => Result.Failure(a.concatNel(b))
          case (Result.Failure(a), _)                 => Result.Failure(a)
          case (_, Result.Failure(b))                 => Result.Failure(b)
