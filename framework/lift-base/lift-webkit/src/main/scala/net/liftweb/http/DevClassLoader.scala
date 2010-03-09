/*
 * Copyright 2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb.http

class DevClassLoader(parent: ClassLoader) extends ClassLoader(parent) {
  override def loadClass(name2: String): Class[_] = {
    if (name2.startsWith("java") || name2.startsWith("scala") ||
        name2.startsWith("net.liftweb")) {
      super.loadClass(name2)
    } else {
    val name = name2.replace(".", "/")+".class"
    println("Loading "+name)
    try {
    import java.io._

    val st = parent.getResourceAsStream(name)

      def readWholeStream(in: InputStream): Array[Byte] = {
    val bos = new ByteArrayOutputStream
    val ba = new Array[Byte](4096)

    def readOnce {
      val len = in.read(ba)
      if (len > 0) bos.write(ba, 0, len)
      if (len >= 0) readOnce
    }

    readOnce

    in.close

    bos.toByteArray
  }

    val b = readWholeStream(st)

    defineClass(name2, b, 0, b.length)
    } catch {
      case e => println("***>>> "+e.getMessage)
        super.loadClass(name2)
    }
    }
  }
}
