/*
 * Copyright 2009 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */

package net.liftweb.util

import _root_.java.util.concurrent.{ConcurrentHashMap => CHash}
import _root_.java.lang.ThreadLocal
import _root_.scala.reflect.Manifest

trait Injector {
  implicit def inject[T](implicit man: Manifest[T]): Box[T]
}

trait SimpleInjector extends Injector {
  private val diHash: CHash[Class[_], Function0[_]] = new CHash

  implicit def inject[T](implicit man: Manifest[T]): Box[T] = diHash.get(man.erasure) match {
    case null => Empty
    case f => Full(f.apply().asInstanceOf[T])
  }

  def registerInjection[T](f: () => T)(implicit man: Manifest[T]) {
    diHash.put(man.erasure, f)
  }
}

trait Maker[T] {
  implicit def make: Box[T]
}

case class BoxMaker[T](fb: Box[() => T]) extends Maker[T] {
  implicit def make: Box[T] = fb.map(_.apply())
}

object Maker {
  def apply[T](value: T): Maker[T] = new Maker[T]{implicit def make: Box[T] = Full(value)}
  def apply[T](func:() => T): Maker[T] = new Maker[T]{implicit def make: Box[T] = Full(func())}
  def apply[T](func: Box[() => T]): Maker[T] = new Maker[T]{implicit def make: Box[T] = func.map(_.apply())}
  def apply1[T](box: Box[T]): Maker[T] = new Maker[T]{implicit def make: Box[T] = box}
  def apply2[T](func: Box[() => Box[T]]): Maker[T] = new Maker[T]{implicit def make: Box[T] = func.flatMap(_.apply())}

  implicit def vToMake[T](v: T): Maker[T] = this.apply(v)
  implicit def vToMake[T](v: () => T): Maker[T] = this.apply(v)
  implicit def vToMakeB1[T](v: Box[T]): Maker[T] = this.apply1(v)
  implicit def vToMakeB2[T](v: Box[() => T]): Maker[T] = this.apply(v)
  implicit def vToMakeB3[T](v: Box[() => Box[T]]): Maker[T] = this.apply2(v)
}

trait StackableMaker[T] extends Maker[T] {
  private val _stack: ThreadLocal[List[PValueHolder[Maker[T]]]] = new ThreadLocal

  private def stack: List[PValueHolder[Maker[T]]] = _stack.get() match {
    case null => Nil
    case x => x
  }

  def doWith[F](value: T)(f: => F): F =
  doWith(PValueHolder(Maker(value)))(f)

  def doWith[F](vFunc: () => T)(f: => F): F =
  doWith(PValueHolder(Maker(vFunc)))(f)

  def doWith[F](addl: PValueHolder[Maker[T]])(f: => F): F = {
    val old = _stack.get()
    _stack.set(addl :: stack)
    try {
      f
    } finally {
      _stack.set(old)
    }
  }

  protected final def find(in: List[PValueHolder[Maker[T]]]): Box[T] = in match {
    case Nil => Empty
    case x :: rest =>
      x.is.make match {
        case Full(v) => Full(v)
        case _ => find(rest)
      }
  }

  implicit def make: Box[T] = find(stack)
}


case class MakerStack[T](subMakers: PValueHolder[Maker[T]]*) extends StackableMaker[T] {
  private val _sub: List[PValueHolder[Maker[T]]] = subMakers.toList
    
  override implicit def make: Box[T] = super.make or find(_sub)
}

