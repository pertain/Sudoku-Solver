/* Sudoku.scala
 *
 * Written by: William Ersing
 *
 * A functional sudoku solver algorithm.
 * Takes in puzzle text files and outputs a solution.
 * Values and coordinates are stored in tuples as such (x, y, value).
 * The idea was that it would be more efficient than using a 2d array,
 * but I am not convinced, because it runs out of memory.
 * 
 * NOTES:
 *		This program does compile, but it produces an OutOfMemoryError
 *		that I cannot find a solution for.
 *		The exact error: java.lang.OutOfMemoryError: GC overhead limit exceeded
 */

import io.Source

class Sudoku(boardSize: Int){
	type Cell = (Int,Int,Int)
	val charNumMap = ((-1, '.') :: ((1 to 35 toList) zip (('1' to '9' toList) ::: ('A' to 'Z' toList)))).toMap
	println(charNumMap)

	// Reads in a puzzle text file
	def readPuzzle(name: String) = {
		val lines = Source.fromFile(name + ".txt").getLines.toList
		lines.map(line => line.toList.map(symbol => symbol.asDigit))
	}

	// Parses puzzle input into a sequence of cell tuples
	def parseFile(in: List[List[Int]]) = in.zipWithIndex.map(i => i._1.zipWithIndex.map(j => (i._2, j._2, j._1))).flatMap(k => k)

	// Outputs a list of cells that do not contain initial values (unlocked)
	def unlockedCells(cells: List[Cell]) = cells.filter(i => i._3 == -1)

	// Outputs a list of cells that contain initial values (locked)
	def lockedCells(cells: List[Cell]) = cells.filterNot(i => i._3 == -1)

	// Outputs a list of cells in the same column as cell c
	def columnCells(c: Cell, cells: List[Cell]) = cells.filter(i => i._2 == c._2)

	// Outputs a list of cells in the same row as cell c
	def rowCells(c: Cell, cells: List[Cell]) = cells.filter(i => i._1 == c._1)

	// Outpusts a list of cells in the same box as cell c
	def boxCells(c: Cell, cells: List[Cell]) = {
		val boxNum = whichBox(c)
		val cellsInBox = boxNum match {
			case 1 => cells.filter { i => (i._1 < 3 && i._2 < 3) }
			case 2 => cells.filter { i => (i._1 < 3 && (i._2 > 2 && i._2 < 6)) }
			case 3 => cells.filter { i => (i._1 < 3 && i._2 > 5) }
			case 4 => cells.filter { i => ((i._1 > 2 && i._1 < 6) && i._2 < 3) }
			case 5 => cells.filter { i => ((i._1 > 2 && i._1 < 6) && (i._2 > 2 && i._2 < 6)) }
			case 6 => cells.filter { i => ((i._1 > 2 && i._1 < 6) && (i._2 > 5)) }
			case 7 => cells.filter { i => (i._1 > 5 && i._2 < 3) }
			case 8 => cells.filter { i => (i._1 > 5 && (i._2 > 2 && i._2 < 6)) }
			case 9 => cells.filter { i => (i._1 > 5 && i._2 > 5) }
		}
		cellsInBox
	}

	// Outputs the box number that the cell c resides in
	def whichBox(c: Cell) = c match {
		case (x,y,_) if(x < 3 && y < 3) => 1
		case (x,y,_) if(x < 3 && (y > 2 && y < 6)) => 2
		case (x,y,_) if(x < 3 && (y > 5 && y < 9)) => 3
		case (x,y,_) if((x > 2 && x < 6) && y < 3) => 4
		case (x,y,_) if((x > 2 && x < 6) && (y > 2 && y < 6)) => 5
		case (x,y,_) if((x > 2 && x < 6) && (y > 5 && y < 9)) => 6
		case (x,y,_) if(x > 5 && y < 3) => 7
		case (x,y,_) if(x > 5 && (y > 2 && y < 6)) => 8
		case _ => 9
	}

	// Checks row, column and box to determine if cell c value is valid
	def isValid(c: Cell, cells: List[Cell]) = {
		(columnCells(c, cells) ::: rowCells(c, cells) ::: boxCells(c, cells)).forall(i => i._3 != c._3)
	}


	/* This function would (ideally) eliminate many possible symbols for each cell, thus reducing the number of recursive solve calls

	def possibleValues(c: Cell, cells: List[Cell]) = {
		//val lockedCellsList = (columnCells(c, cells) :: (rowCells(c, cells) :: (boxCells(c, cells)
		(cells :\ List[Int]()) {(x, acc) => x._3 :: acc }
	}
	*/


    /* This display method was built for an earlier version of this program that used a 2d array rather than a list of tuples

	// Displays the game board
	def display(): Unit = {
		val boxSize = math.sqrt(boardSize)
		println()
		print("\u250F")							// top left corner of outer frame
		val framePadding: Int = ((boxSize - 1) * 2).toInt
		val frameIntercepts: Int = (boxSize * 2 + 2).toInt
		for(topFrame <- 0 to 2 * boardSize + framePadding){
			if((topFrame + 1) % frameIntercepts == 0 && topFrame != 0) print("\u252F")
			else print("\u2501")				// top side of outer frame
		}
		print("\u2513\n")						// top right of outer frame
		board.view.zipWithIndex.foreach { case (cells, i) =>
			if(i % boxSize == 0 && i != 0){
				print("\u2520")
				for(innerRowFrames <- 0 to 2 * boardSize + framePadding){
					if((innerRowFrames + 1) % frameIntercepts == 0 && innerRowFrames != 0) print("\u253C")
					else print("\u2500")		// inner row frames
				}
				print("\u2528\n")
			}
			print("\u2503")						// left side of outer frame
			cells.view.zipWithIndex.foreach { case (value, j) =>
				if(j % boxSize == 0 && j != 0) print(" \u2502")
				print(" " + charNumMap(value))
			}
			print(" \u2503\n")					// right side of outer frame
		}

		print("\u2517")							// bottom left corner of outer frame
		for(bottomFrame <- 0 to 2 * boardSize + framePadding){
			if((bottomFrame + 1) % frameIntercepts == 0 && bottomFrame != 0) print("\u2537")
			else print("\u2501")				// bottom side of outer frame
		}
		print("\u251B\n\n")						// bottom right corner of outer frame
	}
	*/


	// Recursive solve function
	def solve(notLocked: List[Cell], locked: List[Cell]): List[List[Cell]] = {
		def populateCells(unlocked: List[Cell]): List[List[Cell]] = {
			if(unlocked == Nil) List(List())
			else {
				for {
					populated <- populateCells(unlocked.init)
					symbol <- 1 to boardSize
					//symbol <- possibleValues 
					cell = (unlocked.last._1, unlocked.last._2, symbol)
					if(isValid(cell, populated ::: notLocked))
				} yield cell :: populated
			}
		}
		populateCells(notLocked)
	}


	// Sets up the sudoku board, and calls solve function
	def solveIt(name: String) = {
		val input = readPuzzle(name)
		val cells = parseFile(input)
		val cellsToSolve = unlockedCells(cells)
		val cellsToSkip = lockedCells(cells)
		solve(cellsToSolve, cellsToSkip)
	}
}
