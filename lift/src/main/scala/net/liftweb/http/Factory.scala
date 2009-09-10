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

package net.liftweb.http

import util._
import _root_.scala.reflect.Manifest
import _root_.java.util.concurrent.{ConcurrentHashMap => CHash}

/**
* A base trait for
*/
trait Factory extends Injector {
  private var manMap: CHash[Class[_], Maker[_]] = new CHash

  implicit def inject[T](implicit man: Manifest[T]): Box[T] = 
  (Box !! manMap.get(man.erasure)).flatMap(_.make.asInstanceOf[Box[T]])

  abstract class FactoryMaker[T](_default: () => T)
  (implicit man: Manifest[T]) extends StackableMaker[T] {
    manMap.put(man.erasure, this)

    object default extends PSettableValueHolder[Maker[T]] {
      private var value: Maker[T] = _default
      def get = value
      def is = get
      def set(v: Maker[T]): Maker[T] = {
        value = v
        v
      }
    }

    object session extends SessionVar[Maker[T]](Empty)
    object request extends RequestVar[Maker[T]](Empty)
    private val _sub: List[PValueHolder[Maker[T]]] = List(request, session,
                                                          default)


    override implicit def make: Box[T] = super.make or find(_sub)
  }
}
