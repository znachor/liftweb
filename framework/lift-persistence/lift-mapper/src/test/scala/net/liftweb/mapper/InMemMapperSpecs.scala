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
 
package net.liftweb {
package mapper {

import _root_.org.specs._
import _root_.org.specs.runner.JUnit3
import _root_.org.specs.runner.ConsoleRunner
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import Helpers._
import _root_.net.liftweb.json._
import _root_.java.sql.{Connection, DriverManager}

import view._


class InMemMapperSpecsAsTest extends JUnit3(InMemMapperSpecs)
object InMemMapperSpecsRunner extends ConsoleRunner(InMemMapperSpecs)

object InMemMapperSpecs extends Specification {
  
  val provider = DBProviders.H2MemoryProvider
  
  def init = {
    provider.setupDB
    Schemifier.schemify(true, Log.neverF _, SampleItem)
  }

  "ItemsList" should {
    "buffer items to save" in {
      init
      val il = new ItemsList[SampleItem] {
        val metaMapper = SampleItem
      }
      il.add
      il.add
      il.add
      il.current.length must_== 0
      il.added.length must_== 3
      
      il.save
      SampleItem.count must_== 3
      il.current.length must_== 3
    }
    
    "correctly handle removing an unsaved item" in {
      init
      val il = new ItemsList[SampleItem] {
        val metaMapper = SampleItem
      }
      il.add
      il.add
      il.add
      il.save
      
      il.add
      il.add
      il.add
      il.remove(il.added(1))
      il.remove(il.added(0))
      il.save
      SampleItem.count must_== 4
      il.added.length must_== 0
      il.removed.length must_== 0    
    }
  }
  
  "Cached obj of foreign key" should {
    "be cleared when necessary" in {
      init
      
      val item = SampleItem.create
      item.save
      val hasItem = HasSampleItem.create item item
      hasItem.item.obj.dmap(false)(_ eq item) must_== true
      hasItem.item(item.id.is)
      hasItem.item.obj.dmap(false)(_ eq item) must_== true
      hasItem.item(0)
      hasItem.item(item.id.is)
      hasItem.item.obj.dmap(false)(_ eq item) must_== false
    }
  }
  
}

class SampleItem extends LongKeyedMapper[SampleItem] with IdPK {
  def getSingleton = SampleItem
  object field extends MappedInt(this)
}

object SampleItem extends SampleItem with LongKeyedMetaMapper[SampleItem] {
  var counter = 0
  override def create = {
    val x: SampleItem = super.create
    x.field(counter)
    counter += 1
    x
  }
}

class HasSampleItem extends LongKeyedMapper[HasSampleItem] with IdPK {
  def getSingleton = HasSampleItem
  object item extends MappedLongForeignKey(this, SampleItem)
}
object HasSampleItem extends HasSampleItem with LongKeyedMetaMapper[HasSampleItem]



}
}
