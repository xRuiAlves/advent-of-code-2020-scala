package day21

import utils.FileReader

import scala.collection.mutable

object Day21Part1 {
    type Food = (mutable.HashSet[String], mutable.HashSet[String])

    final val FOOD_REGEX = "^(.+)\\s\\(contains\\s(.+)\\)$".r

    def main(args: Array[String]): Unit = {
        val lines = FileReader.readFile("Advent-Of-Code-2020/day21/input.txt").toArray
        val foods: Array[Food] = lines.map(line => FOOD_REGEX.findFirstMatchIn(line) match {
            case Some(m) => (
                m.group(1).split(" ").to(mutable.HashSet),
                m.group(2).split(", ").to(mutable.HashSet)
            )
            case None => throw new Exception(s"Invalid input food: $line")
        })

        val possible_allergens = new mutable.HashMap[String, mutable.HashSet[String]]()
        val allergens = new mutable.HashMap[String, String]()

        foods.foreach { case (food_ingredients, food_allergens) => {
            food_allergens.foreach(allergen => possible_allergens.get(allergen) match {
                case Some(ing_list) => possible_allergens(allergen) = ing_list.intersect(food_ingredients)
                case None => possible_allergens(allergen) = food_ingredients.clone
            })
        }}

        while (possible_allergens.nonEmpty) {
            val curr_allergen = possible_allergens.toArray.sortWith(_._2.size < _._2.size).head

            if (curr_allergen._2.size != 1) {
                throw new Exception("Matching not possible")
            }

            val ing = curr_allergen._2.last
            allergens(curr_allergen._1) = ing

            possible_allergens.foreach(_._2.remove(ing))
            possible_allergens.remove(curr_allergen._1)
        }

        val ingredients_with_allergens = allergens.values.to(mutable.HashSet)
        val ingredients_without_allergens = foods.flatMap(_._1.diff(ingredients_with_allergens))

        println(ingredients_without_allergens.length)
    }
}
