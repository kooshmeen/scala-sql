
type Row = Map[String, String]
type Tabular = List[Row]

case class Table (tableName: String, tableData: Tabular) {

  override def toString: String = {
    if (tableData.isEmpty) {
      return ""
    }
    val header = tableData.head.keys.mkString(",")
    val rows = tableData.map(_.values.mkString(",")).mkString("\n")
    header + "\n" + rows
  }

  def insert(row: Row): Table = {
    if (!tableData.contains(row)) {
      Table(tableName, tableData :+ row)
    } else {
      this
    }
  }

  def delete(row: Row): Table = {
    Table(tableName, tableData.filterNot(_ == row))
  }

  def sort(column: String): Table = {
    Table(tableName, tableData.sortBy(_.getOrElse(column, "")))
  }

  def update(f: FilterCond, updates: Map[String, String]): Table = {
    val updatedData = tableData.map { row =>
      f.eval(row) match {
        case Some(true) => row ++ updates
        case _ => row
      }
    }
    Table(tableName, updatedData)
  }

  def filter(f: FilterCond): Table = {
    val filteredData = tableData.map { row =>
      f.eval(row) match {
        case Some(true) => row
        case _ => Map.empty
      }
    }.filter(_.nonEmpty)
    Table(tableName, filteredData)
  }

  def select(columns: List[String]): Table = {
    val selectedData = tableData.map { row =>
      columns.flatMap(col => row.get(col).map(col -> _)).toMap
    }
    Table(tableName, selectedData)
  }

  def header: List[String] = tableData.head.keys.toList
  def data: Tabular = tableData
  def name: String = tableName
}

object Table {
  def apply(name: String, s: String): Table = {
    val lines = s.split("\n").toList
    val header = lines.head.split(",")
    val data = lines.tail.map { line =>
      val values = line.split(",")
      header.zip(values).toMap
    }
    Table(name, data)
  }
}

extension (table: Table) {
  def rowIdx(i: Int): Row = {
    table.tableData(i)
  }
}
