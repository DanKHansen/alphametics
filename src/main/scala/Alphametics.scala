object Alphametics:
   def solve(equation: String): Option[Map[Char, Long]] =
      val (left, right) = parseEquation(equation)
      val uniqueLetters = (left.flatMap(_.toSet) ++ right.toSet).toSet.toList

      // Generate all permutations of digits 0-9 for the unique letters
      val digits = (0L to 9L).toList
      val permutations = digits.permutations

      // Find the first valid mapping
      permutations.collectFirst {
         case perm if isValidMapping(perm, uniqueLetters, left, right) =>
            uniqueLetters.zip(perm).toMap
      }

   private def parseEquation(equation: String): (Array[String], String) =
      val parts = equation.split("==").map(_.trim)
      val left = parts(0).split("\\+").map(_.trim)
      (left, parts(1))

   private def isValidMapping(perm: Seq[Long], letters: List[Char], left: Array[String], right: String): Boolean =
      val mapping = letters.zip(perm).toMap
      val leftSum = left.map(word => word.map(mapping).mkString("").toLong).sum
      val rightValue = right.map(mapping).mkString("").toLong

      // Check for leading zeros
      val hasLeadingZero = (word: String) => mapping(word.head) == 0L
      !left.exists(hasLeadingZero) && !hasLeadingZero(right) && leftSum == rightValue
