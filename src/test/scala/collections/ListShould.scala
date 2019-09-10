package collections

import org.scalatest.FunSuite



  class ListShould extends FunSuite {
    test("iterate"){
      val expected = List[String]("world", "bonjour", "salut")
      val actual = List.iterate(expected)
      assertResult(expected)(actual)
    }

    test("foldRight"){
      val list = List[Int](1,2,3)
      val result = List.foldRight[Int, Int](list, 42)((a, b) => a + b)

      assertResult(48)(result)
    }


    test("Can remove the first element of list then return it") {
      val expected = List[String]("world", "bonjour", "salut")

      val actual = List.tail2(List[String]("hello", "world", "bonjour", "salut"))

      assert(expected == actual, "Two lists should be equivalent")
    }

    test("Remove first element from an empty list should yield empty list") {
      val list = List[String]()
      assert(list == List.tail2(list), "Two lists should be equivalent")
    }

    test("Can change the head of list") {
      val source = List[String]("hello", "world", "bonjour", "salut")

      val expected = List[String]("newHead", "world", "bonjour", "salut")
      val actual = List.setHead("newHead", source);

      assert(expected == actual, "Head should be changed to `newHead`")
    }


    test("Can set head on empty list") {
      val source = List[String]();
      val newList = List.setHead("KING", source)

      assert(List[String]("KING") == newList)
    }


    test("Can remove first N elements from list") {
      val source = List[String]("1", "2", "3", "4")

      val expected = List[String]("4")

      assert(expected == List.drop2(source, 3), "Should drop the first 3 elements")
    }

    test("Can remove all elements from list") {
      val source = List[String]("1", "2", "3", "4")
      val expected = List[String]()

      assert(expected == List.drop(source, 4), "Should drop all elements")
    }

    test("Not throw when remove more elements than list capacity") {
      val source = List[String]("1", "2", "3", "4")
      val expected = List[String]()

      assert(expected == List.drop(source, 5), "Should drop all elements")

    }

    test("Not remove any elements") {
      val source = List[String]("1", "2", "3", "4")

      assert(source == List.drop(source, 0), "Should not remove any element")
    }


    test("Not remove any elements when negative number of elements is asked") {
      val source = List[String]("1", "2", "3", "4")

      assert(source == List.drop(source, 0))
    }

    test("Drop as long as condition") {
      val source = List[Int](100, 12, 41, 42, 1, 5, 4, 16, 88)
      val expected = List[Int](1, 5, 4, 16, 88)

      val filtered = List.dropWhile(source)(x => x > 10)
      assert(expected == filtered)
    }

    test("Get a new list built with all elements but the last of an existing list") {
      val source = List[Int](1, 2, 3, 4, 5)
      val expected = List[Int](1, 2, 3, 4)

      val rebuilt = List.init(source)
      assert(expected == rebuilt)
    }

    test("Can append a list to another") {
      val source = List[Int](1, 2, 3, 4, 5)
      val appended = List.append(source, List[Int](6, 7))

      assert(appended == List[Int](1, 2, 3, 4, 5, 6, 7))
    }
  }