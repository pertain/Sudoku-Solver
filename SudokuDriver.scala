/* SudokuDriver.scala
 *
 * Written by: William Ersing
 *
 *
 * Driver for a sudoku solver program 
 */

import io.Source._

object Driver {
	def main(args: Array[String]){
		println("Enter your Selection for board size\n")
		println("For 9x9 puzzles, enter '9'")
		println("For 16x16 puzzles, enter '16'")
		println("For 25x25 puzzles, enter '25'")
		print("Size: ")
		val gameSize = Console.readInt
		val puzzle = new Sudoku(gameSize)
		println("\nEnter puzzle file name, without the extension")
		println("(for example: puz9-2)")
		print("Filename: ")
		val puzName = Console.readLine
		puzzle.solveIt(puzName)
	}
}
