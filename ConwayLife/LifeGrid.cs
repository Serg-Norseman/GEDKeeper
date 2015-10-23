/*
 *  ULife
 *  Author: Ian Lane (email: lanei@ideal.net.au)
 *  Copyright (C) 1998 Ian Lane
 *
 *  Synopsis: A Delphi control which implements the old computer simulation
 *  of Life. Useful for about boxes, screen savers or even as the
 *  core of a "Life" application.
 *
 *  Distribution: This control is free for public use and components may be
 *  freely descended from it as long as credit is given to the author.
 * 
 *  Converted to C#: 20/07/2011, Serg V. Zhdanovskih
 */

using System;

namespace ConwayLife
{
    public sealed class LifeGrid
    {
        private bool[] fGrid;
        private int fGridHeight;
        private int fGridWidth;

        public bool this[int X, int Y]
        {
            get { return this.fGrid[this.CellCoordToGridOffset(X, Y)]; }
            set { this.fGrid[this.CellCoordToGridOffset(X, Y)] = value; }
        }

        public int GridHeight
        {
            get { return this.fGridHeight; }
        }

        public int GridWidth
        {
            get { return this.fGridWidth; }
        }

        public int LiveCellCount
        {
            get {
        		int result = 0;
        		for (int i = 0; i < this.fGridWidth * this.fGridHeight; i++) result += bool2int(this.fGrid[i]);
        		return result;
        	}
        }

		
        public LifeGrid()
        {
        }

        public LifeGrid(int GridWidth, int GridHeight)
        {
            this.fGridHeight = GridHeight;
            this.fGridWidth = GridWidth;
            this.AllocGrid();
        }

        public void Destroy()
        {
            this.FreeGrid();
        }

        public void Assign(LifeGrid source)
        {
            this.SetGridSize(source.fGridWidth, source.fGridHeight);
            Array.Copy(source.fGrid, this.fGrid, fGridWidth * fGridHeight);
        }

        public void Clear()
        {
            Array.Clear(this.fGrid, 0, this.fGridWidth * this.fGridHeight);
        }
		
        public bool DoesCellLive(int X, int Y)
        {
            if (X >= fGridWidth) throw new IndexOutOfRangeException("X parameter out of range");
            if (Y >= fGridHeight) throw new IndexOutOfRangeException("Y parameter out of range");

            bool result;
            switch (NumberOfNeighbours(X, Y)) {
                case 2:
                    result = this[X, Y];
                    break;
                case 3:
                    result = true;
                    break;
                default:
                    result = false;
                    break;
            }
            return result;
        }

        public override bool Equals(object obj)
        {
        	LifeGrid source = obj as LifeGrid;
        	if (source == null) return false;
        	
        	if (this == source) return true;
        	
            if (source.fGridWidth != this.fGridWidth || source.fGridHeight != this.fGridHeight) {
                return false;
            } else {
        		bool[] sourceGrid = source.fGrid;
        		for (int i = 0; i < this.fGridHeight * this.fGridWidth; i++) {
        			if (this.fGrid[i] != sourceGrid[i]) {
        				return false;
        			}
        		}

                return true;
            }
        }

        public int NumberOfNeighbours(int X, int Y)
        {
            if (X >= GridWidth) throw new IndexOutOfRangeException("X parameter out of range");
            if (Y >= GridHeight) throw new IndexOutOfRangeException("Y parameter out of range");

            int xMinus1 = (X + GridWidth - 1) % GridWidth; /* Equivalent to (X - 1) with a "wrap" */
            int xPlus1 = (X + 1) % GridWidth;
            int yMinus1 = (Y + GridHeight - 1) % GridHeight; /* Equivalent to (Y - 1) with a "wrap" */
            int yPlus1 = (Y + 1) % GridHeight;

            /* Count the number of live neighbours that this cell has */
            int result = 0;

            if (this[xMinus1, yMinus1]) result++;
            if (this[X, yMinus1]) result++;
            if (this[xPlus1, yMinus1]) result++;
            if (this[xMinus1, Y]) result++;
            if (this[xPlus1, Y]) result++;
            if (this[xMinus1, yPlus1]) result++;
            if (this[X, yPlus1]) result++;
            if (this[xPlus1, yPlus1]) result++;
			
            return result;
        }

        public void SetGridSize(int NewGridWidth, int NewGridHeight)
        {
            this.FreeGrid();
            this.fGridWidth = NewGridWidth;
            this.fGridHeight = NewGridHeight;
            this.AllocGrid();
        }

        private void AllocGrid()
        {
            this.fGrid = new bool[this.fGridWidth * this.fGridHeight];
        }

        private void FreeGrid()
        {
            this.fGrid = null;
        }
		
        private int CellCoordToGridOffset(int X, int Y)
        {
            if (X >= fGridWidth) throw new IndexOutOfRangeException("X parameter out of range");
            if (Y >= fGridHeight) throw new IndexOutOfRangeException("Y parameter out of range");

            return Y * fGridWidth + X;
        }

        private static int bool2int(bool B)
        {
            return ((B) ? 1 : 0);
        }
    }
}