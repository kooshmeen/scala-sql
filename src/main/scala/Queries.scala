object Queries {

  def killJackSparrow(t: Table): Option[Table] = queryT(queryT(Some(t), "FILTER", Field("name", _ == "Jack"))
    .flatMap(ft => (Some(t), "DELETE", ft.tableData.head)))

  def insertLinesThenSort(db: Database): Option[Table] = queryDB(Some(db), "CREATE", "Inserted Fellas")
    .flatMap(db => queryDB(Some(db), "SELECT", List("Inserted Fellas")))
    .flatMap(db => db.tables.headOption)
    .flatMap(table => List(
      Map("name" -> "Ana", "age" -> "93", "CNP" -> "455550555"),
      Map("name" -> "Diana", "age" -> "33", "CNP" -> "255532142"),
      Map("name" -> "Tatiana", "age" -> "55", "CNP" -> "655532132"),
      Map("name" -> "Rosmaria", "age" -> "12", "CNP" -> "855532172")
    ).foldLeft(Some(table): Option[Table])((tableOpt, person) => tableOpt.flatMap(table => queryT(Some(table), "INSERT", List(person)))))
    .flatMap(table => queryT(Some(table), "SORT", "age"))

  def youngAdultHobbiesJ(db: Database): Option[Table] = queryDB(Some(db), "JOIN", "People", "name", "Hobbies", "name")
    .flatMap(db => db.tables.headOption)
    .flatMap(table => queryT(Some(table), "FILTER", Field("age", _ < "25")))
    .flatMap(table => queryT(Some(table), "FILTER", Field("name", _.startsWith("J"))))
    .flatMap(table => queryT(Some(table), "FILTER", Field("hobby", _.nonEmpty)))
    .flatMap(table => queryT(Some(table), "EXTRACT", List("name", "hobby")))
}
