/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb.record.form

import net.liftweb.http.S._

class FormReturn[A] {

    val errors = List[ValidationError]()
    val successTo: Option[String] = None
    val successObj: Option[A] = None
    def onSuccess(onProcess: (A) => Unit): FormReturn[A] = {
        successObj match {
            case None => this;
            case Some(obj) => 
                onProcess(obj)
                this
        }
    }

    def mkLiftErrors { errors foreach ( _.mkLiftError ) }
    def isError = errors.size > 0
    def isSuccess = !isError

    def continue {
        mkLiftErrors
        successTo match {
            case None => ;
            case Some(toWhere) => redirectTo(toWhere)
        }
    }

}

object FormReturn {

    def fail[A](msg: String) = new FormReturn[A]{
        override val errors = ValidationError(null, null, msg) :: Nil
    }

    def fail[A](name: String, msg: String) = new FormReturn[A]{
        override val errors = ValidationError(name, null, msg) :: Nil
    }
    
    def fail[A](validations: List[ValidationError]) = new FormReturn[A]{
        override val errors = validations
    }

    def success[A](toWhere: String) = new FormReturn[A]{
        override val successTo = Some(toWhere)
    }

    def success[A](obj: A, toWhere: String) = new FormReturn[A]{
        override val successTo = Some(toWhere)
        override val successObj = Some(obj)
    }

}
