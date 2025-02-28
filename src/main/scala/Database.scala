case class Database(tables: List[Table]) {
  override def toString: String = tables.mkString("\n")

  def create(tableName: String): Database = {
    if (tables.exists(_.name == tableName)) {
      return this
    }
    val newTable = Table(tableName, List())
    Database(tables :+ newTable)
  }

  def drop(tableName: String): Database = {
    Database(tables.filterNot(_.name == tableName))
  }

  def selectTables(tableNames: List[String]): Option[Database] = {
    val selectedTables = tables.filter(t => tableNames.contains(t.name))
    if (selectedTables.length == tableNames.length) {
      Some(Database(selectedTables))
    } else {
      None
    }
  }

  def join(table1: String, c1: String, table2: String, c2: String): Option[Table] = {
    val t1 = tables.find(_.name == table1)
    val t2 = tables.find(_.name == table2)

    (t1, t2) match {
      case (Some(t1), Some(t2)) =>
        val templateRowAll = (t1.tableData.head.keys ++ t2.tableData.head.keys).filterNot(key => key == c2 && c1 != c2)
        val templateRow = templateRowAll.toList.distinct.map(key => key -> "").toMap
        val map1 = t1.tableData.groupBy(_(c1))
        val map2 = t2.tableData.groupBy(_(c2))

        val bothInTables = t1.tableData.flatMap { row1 =>
          val key = row1(c1)
          val rows2 = map2.getOrElse(key, List())

          if (rows2.nonEmpty) {
            rows2.map(row2 => combineRows(templateRow, row1, row2, c2, c1))
          } else {
            List()
          }
        }
        val onlyInTable1 = t1.tableData.filterNot(row1 => map2.contains(row1(c1)))
        val joinedDataWithTable1 = bothInTables ++ onlyInTable1.map(row1 => combineRows(templateRow, row1, Map.empty, c2, c1))

        val onlyInTable2 = t2.tableData.filterNot(row2 => map1.contains(row2(c2)))
        val joinedDataWithTable2 = joinedDataWithTable1 ++ onlyInTable2.map(row2 => combineRows(templateRow, Map.empty, row2, c2, c1))

        Some(Table(table1, joinedDataWithTable2))

      case _ => None
    }
  }

  private def combineRows(templateRow: Row, r1: Row, r2: Row, joinColumn: String, originalColumn: String): Row = {
    val updatedR2 = if (r2.contains(joinColumn)) r2.map {
      case (k, v) => if (k == joinColumn) originalColumn -> v else k -> v
    } else r2
    val updatedR1 = if (r1.isEmpty && updatedR2.nonEmpty) updatedR2 else r1
    templateRow.keys.map { key =>
      val value = (updatedR1.get(key), updatedR2.get(key)) match {
        case (Some(v1), Some(v2)) if key == originalColumn => v2
        case (Some(v1), Some(v2)) if key != originalColumn && v1 == v2 => v1
        case (Some(v1), Some(v2)) if key != originalColumn => v1 + ";" + v2
        case (Some(v1), _) => v1
        case (_, Some(v2)) => v2
        case (None, None) => ""
      }
      key -> value
    }.toMap
  }

  def head(db: Database): Option[Table] = db.tables.headOption

  // Implement indexing here

  def apply(index: Int): Table = tables(index)
}
