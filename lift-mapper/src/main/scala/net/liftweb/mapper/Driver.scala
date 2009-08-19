package net.liftweb.mapper

/*
 * Copyright 2006-2008 WorldWide Conferencing, LLC
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

import _root_.java.sql.{Connection,PreparedStatement,ResultSet,Statement}
import _root_.net.liftweb.util._


abstract class DriverType(val name : String) {
  def binaryColumnType: String
  def clobColumnType: String
  def booleanColumnType: String
  def dateTimeColumnType: String
  def integerColumnType: String
  def integerIndexColumnType: String
  def enumColumnType: String
  def longForeignKeyColumnType: String
  def longIndexColumnType: String
  def enumListColumnType: String
  def longColumnType: String
  def doubleColumnType: String

  def supportsForeignKeys_? : Boolean = false
  def createTablePostpend: String = ""

  /**
   * Whether this database supports LIMIT clause in SELECTs.
   */
  def brokenLimit_? : Boolean = false

  /**
   * Whether the primary key has been defined by the index column.
   */
  def pkDefinedByIndexColumn_? : Boolean = false

  /**
   * Maximum value of the LIMIT clause in SELECT.
   */
  def maxSelectLimit : String = _root_.java.lang.Long.MAX_VALUE.toString

  /**
    * Performs an insert and optionally returns the ResultSet of the generated keys that were inserted. If no keys are
    * specified, return the number of rows updated.
    *
    * @param conn A connection that the method can optionally use if it needs to execute ancillary statements
    * @param query The prepared query string to use for the insert
    * @param setter A function that will set the parameters on the prepared statement
    * @param pkName Zero or more generated column names that need to be returned
    */
  def performInsert (conn : Connection, query : String, setter : PreparedStatement => Unit, pkNames : List[String]) : Either[ResultSet,Int] = pkNames match {
      case Nil => Right({val stmt = conn.prepareStatement(query); setter(stmt); stmt.executeUpdate})
      case pk => Left(performInsertWithPK(conn, query, setter, pk))
  }

  /*
   * Subclasses should override this method if they don't have proper getGeneratedKey support (JDBC3)
   */
  protected def performInsertWithPK (conn : Connection, query : String, setter : PreparedStatement => Unit, pkNames : List[String]) : ResultSet = {
      val stmt = conn.prepareStatement(query, Statement.RETURN_GENERATED_KEYS)
      setter(stmt)
      stmt.executeUpdate
      stmt.getGeneratedKeys
  }

  /**
   * Name of the default db schema. If not set, then the schema is assumed to
   * equal the db user name.
   */
  def defaultSchemaName : Box[String] = Empty

  type TypeMapFunc = PartialFunction[Int,Int]
  /**
   * Allow the driver to do specific remapping of column types for cases
   * where not all types are supported. Classes that want to do custom type
   * mapping for columns should override the customColumnTypeMap method.
   */
  def columnTypeMap : TypeMapFunc = 
    customColumnTypeMap orElse {
      case x => x
    }

  /**
   * Allows the Vendor-specific Driver to do custom type mapping for a particular
   * column type.
   */
  protected def customColumnTypeMap : TypeMapFunc = new TypeMapFunc {
    def apply (in : Int) = -1
    def isDefinedAt (in : Int) = false
  }

  /**
   * This method can be overriden by DriverType impls to allow for custom setup
   * of Primary Key Columns (creating sequeneces or special indices, for example).
   * The List of commands will be executed in order.
   */
  def primaryKeySetup(tableName : String, columnName : String) : List[String] = {
      List("ALTER TABLE "+tableName+" ADD CONSTRAINT "+tableName+"_PK PRIMARY KEY("+columnName+")")
  }
}

object DerbyDriver extends DriverType("Apache Derby") {
  def binaryColumnType = "LONG VARCHAR FOR BIT DATA"
  def booleanColumnType = "SMALLINT"
  def clobColumnType = "LONG VARCHAR"
  def dateTimeColumnType = "TIMESTAMP"
  def integerColumnType = "INTEGER"
  def integerIndexColumnType = "INTEGER NOT NULL GENERATED ALWAYS AS IDENITY"
  def enumColumnType = "BIGINT"
  def longForeignKeyColumnType = "BIGINT"
  def longIndexColumnType = "BIGINT NOT NULL GENERATED ALWAYS AS IDENTITY"
  def enumListColumnType = "BIGINT"
  def longColumnType = "BIGINT"
  def doubleColumnType = "DOUBLE"

  override def brokenLimit_? : Boolean = true
}

object MySqlDriver extends DriverType("MySQL") {
  def binaryColumnType = "MEDIUMBLOB"
  def clobColumnType = "LONGTEXT"
  def booleanColumnType = "BOOLEAN"
  def dateTimeColumnType = "DATETIME"
  def integerColumnType = "INTEGER"
  def integerIndexColumnType = "INTEGER NOT NULL AUTO_INCREMENT UNIQUE"
  def enumColumnType = "BIGINT"
  def longForeignKeyColumnType = "BIGINT UNSIGNED"
  def longIndexColumnType = "BIGINT UNSIGNED NOT NULL AUTO_INCREMENT UNIQUE KEY"
  def enumListColumnType = "BIGINT"
  def longColumnType = "BIGINT"
  def doubleColumnType = "DOUBLE"

  override def createTablePostpend: String = " ENGINE = InnoDB "
}

object H2Driver extends DriverType("H2") {
  def binaryColumnType = "BINARY"
  def clobColumnType = "LONGVARCHAR"
  def booleanColumnType = "BOOLEAN"
  def dateTimeColumnType = "TIMESTAMP"
  def integerColumnType = "INTEGER"
  def integerIndexColumnType = "INTEGER NOT NULL AUTO_INCREMENT"
  def enumColumnType = "BIGINT"
  def longForeignKeyColumnType = "BIGINT"
  def longIndexColumnType = "BIGINT NOT NULL AUTO_INCREMENT"
  def enumListColumnType = "BIGINT"
  def longColumnType = "BIGINT"
  def doubleColumnType = "DOUBLE"

  /**
   * Whether the primary key has been defined by the index column.
   * H2 creates primary key for a table, when AUTO_INCREMENT type
   * is used.
   */
  override def pkDefinedByIndexColumn_? : Boolean = true

  override def maxSelectLimit = "0";
}

object PostgreSqlDriver extends DriverType("PostgreSQL") {
  def binaryColumnType = "BYTEA"
  def clobColumnType = "TEXT"
  def booleanColumnType = "BOOLEAN"
  def dateTimeColumnType = "TIMESTAMP"
  def integerColumnType = "INTEGER"
  def integerIndexColumnType = "SERIAL"
  def enumColumnType = "BIGINT"
  def longForeignKeyColumnType = "BIGINT"
  def longIndexColumnType = "BIGSERIAL"
  def enumListColumnType = "BIGINT"
  def longColumnType = "BIGINT"
  def doubleColumnType = "DOUBLE PRECISION"

  /* PostgreSQL doesn't support generated keys via the JDBC driver. Instead, we use the RETURNING clause on the insert.
   * From: http://www.postgresql.org/docs/8.2/static/sql-insert.html
   */
  override def performInsertWithPK (conn : Connection, query : String, setter : PreparedStatement => Unit, pkNames : List[String]) : ResultSet = {
      val stmt = conn.prepareStatement(query + " RETURNING " + pkNames.mkString(","))
      setter(stmt)
      stmt.executeQuery
  }

  override def maxSelectLimit = "ALL"

  /**
   * "$user" schema is searched before "public", but it does not exist by default,
   * so "public" is our default choice.
   */
  override def defaultSchemaName : Box[String] = Full("public")
}

object SqlServerDriver extends DriverType("Microsoft SQL Server") {
  def binaryColumnType = "VARBINARY(MAX)"
  def booleanColumnType = "BIT"
  def clobColumnType = "VARCHAR(MAX)"
  def dateTimeColumnType = "TIMESTAMP"
  def integerColumnType = "INT"
  def integerIndexColumnType = "INT IDENTITY NOT NULL"
  def enumColumnType = "BIGINT"
  def longForeignKeyColumnType = "BIGINT"
  def longIndexColumnType = "BIGINT IDENTITY NOT NULL"
  def enumListColumnType = "BIGINT"
  def longColumnType = "BIGINT"
  def doubleColumnType = "FLOAT"

  override def defaultSchemaName : Box[String] = Full("dbo")
}

object OracleDriver extends DriverType("Oracle") {
  def binaryColumnType = "LONG RAW"
  def booleanColumnType = "NUMBER"
  def clobColumnType = "CLOB"
  def dateTimeColumnType = "TIMESTAMP"
  def integerColumnType = "NUMBER"
  def integerIndexColumnType = "NUMBER NOT NULL"
  def enumColumnType = "NUMBER"
  def longForeignKeyColumnType = "NUMBER"
  def longIndexColumnType = "NUMBER NOT NULL"
  def enumListColumnType = "NUMBER"
  def longColumnType = "NUMBER"
  def doubleColumnType = "NUMBER"

  import _root_.java.sql.Types
  override def customColumnTypeMap = {
    case Types.BOOLEAN => Types.INTEGER
  }

  override def primaryKeySetup(tableName : String, columnName : String) : List[String] = {
    /*
     * This trigger and sequence setup is taken from http://www.databaseanswers.org/sql_scripts/ora_sequence.htm
     */
    super.primaryKeySetup(tableName, columnName) :::
    List("CREATE SEQUENCE " + tableName + "_sequence START WITH 1 INCREMENT BY 1",
         "CREATE OR REPLACE TRIGGER " + tableName + "_trigger BEFORE INSERT ON " + tableName + " " +
         "FOR EACH ROW " +
         "WHEN (new." + columnName + " is null) " +
         "BEGIN " +
         "SELECT " + tableName + "_sequence.nextval INTO :new." + columnName + " FROM DUAL; " +
         "END;")
  }

  // Oracle supports returning generated keys only if we specify the names of the column(s) to return.
  override def performInsertWithPK (conn : Connection, query : String, setter : PreparedStatement => Unit, pkNames : List[String]) : ResultSet = {
      val stmt = conn.prepareStatement(query, pkNames.toArray)
      setter(stmt)
      stmt.executeUpdate
      stmt.getGeneratedKeys
  }
}

object MaxDbDriver extends DriverType("MaxDB") {
  def binaryColumnType = "BLOB"
  def booleanColumnType = "BOOLEAN"
  def clobColumnType = "CLOB"
  def dateTimeColumnType = "TIMESTAMP"
  def integerColumnType = "INTEGER"
  def integerIndexColumnType = "FIXED(10) DEFAULT SERIAL"
  def enumColumnType = "FIXED(38)"
  def longForeignKeyColumnType = "FIXED(38)"
  def longIndexColumnType = "FIXED(38) DEFAULT SERIAL"
  def enumListColumnType = "FIXED(38)"
  def longColumnType = "FIXED(38)"
  def doubleColumnType = "FLOAT(38)"
}
