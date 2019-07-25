import org.scalatest.FunSuite


class ListShould extends FunSuite {
  test("Can remove the first element of list then return it") {
    val expected = List2[String]("world", "bonjour", "salut")

    val actual = List2.tail2(List2[String]("hello", "world", "bonjour", "salut"))

    assert(expected == actual, "Two lists should be equivalent")
  }

  test("Remove first element from an empty list should yield empty list") {
    val list = List2[String]()
    assert(list == List2.tail2(list), "Two lists should be equivalent")
  }

  test("Can change the head of list") {
    val source = List2[String]("hello", "world", "bonjour", "salut")

    val expected = List2[String]("newHead", "world", "bonjour", "salut")
    val actual = List2.setHead("newHead", source);

    assert(expected == actual, "Head should be changed to `newHead`")
  }


  test("Can set head on empty list") {
    val source = List2[String]();
    val newList = List2.setHead("KING", source)

    assert(List2[String]("KING") == newList)
  }


  test("Can remove first N elements from list") {
    val source = List2[String]("1", "2", "3", "4")

    val expected = List2[String]("4")

    assert(expected == List2.drop2(source, 3), "Should drop the first 3 elements")
  }

  test("Can remove all elements from list") {
    val source = List2[String]("1", "2", "3", "4")
    val expected = List2[String]()

    assert(expected == List2.drop(source, 4), "Should drop all elements")
  }

  test("Not throw when remove more elements than list capacity") {
    val source = List2[String]("1", "2", "3", "4")
    val expected = List2[String]()

    assert(expected == List2.drop(source, 5), "Should drop all elements")

  }

  test("Not remove any elements") {
    val source = List2[String]("1", "2", "3", "4")

    assert(source == List2.drop(source, 0), "Should not remove any element")
  }


  test("Not remove any elements when negative number of elements is asked") {
    val source = List2[String]("1", "2", "3", "4")

    assert(source == List2.drop(source, 0))
  }

  test("Drop as long as condition") {
    val source = List2[Int](100, 12, 41, 42, 1, 5, 4, 16, 88)
    val expected = List2[Int](1, 5, 4, 16, 88)

    val filtered = List2.dropWhile(source, (x: Int) => x > 10)
    assert(expected == filtered)
  }


}

