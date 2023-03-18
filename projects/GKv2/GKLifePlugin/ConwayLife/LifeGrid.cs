/*
 *  ULife, the old computer simulation of Life.
 *  Copyright (C) 1998 by Ian Lane (email: lanei@ideal.net.au)
 *
 *  Distribution: This control is free for public use and components may be
 *  freely descended from it as long as credit is given to the author.
 * 
 *  Converted to C#: 20/07/2011, Sergey V. Zhdanovskih.
 */

using System;

namespace GKLifePlugin.ConwayLife
{
    public sealed class LifeGrid : IDisposable, IEquatable<LifeGrid>
    {
        public const int MinGridHeight = 5;
        public const int MaxGridHeight = 1000;
        public const int MinGridWidth = 5;
        public const int MaxGridWidth = 1000;

        public const int DefaultGridHeight = 200;
        public const int DefaultGridWidth = 200;


        private short[] fGrid;
        private int fGridHeight;
        private int fGridWidth;

        public short this[int X, int Y]
        {
            get { return fGrid[CellCoordToGridOffset(X, Y)]; }
            set { fGrid[CellCoordToGridOffset(X, Y)] = value; }
        }

        public int GridHeight
        {
            get { return fGridHeight; }
        }

        public int GridWidth
        {
            get { return fGridWidth; }
        }

        public int LiveCellCount
        {
            get {
                int result = 0;
                for (int i = 0; i < fGridWidth * fGridHeight; i++) result += bool2int(fGrid[i]);
                return result;
            }
        }

        
        public LifeGrid()
        {
        }

        public LifeGrid(int gridWidth, int gridHeight)
        {
            fGridHeight = gridHeight;
            fGridWidth = gridWidth;
            AllocGrid();
        }

        public void Dispose()
        {
            FreeGrid();
        }

        public void Assign(LifeGrid source)
        {
            SetGridSize(source.fGridWidth, source.fGridHeight);
            Array.Copy(source.fGrid, fGrid, fGridWidth * fGridHeight);
        }

        public void Clear()
        {
            Array.Clear(fGrid, 0, fGridWidth * fGridHeight);
        }
        
        /// <summary>
        /// Conway's default rule.
        /// </summary>
        /// <param name="X"></param>
        /// <param name="Y"></param>
        /// <returns></returns>
        public bool DoesCellLive(int X, int Y)
        {
            if (X >= fGridWidth) throw new IndexOutOfRangeException("X parameter out of range");
            if (Y >= fGridHeight) throw new IndexOutOfRangeException("Y parameter out of range");

            bool result;
            switch (NumberOfNeighbours(X, Y)) {
                case 2:
                    result = this[X, Y] > 0;
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

        public bool Equals(LifeGrid other)
        {
            if (other == null) return false;
            if (this == other) return true;

            if (other.fGridWidth != fGridWidth || other.fGridHeight != fGridHeight) {
                return false;
            } else {
                short[] sourceGrid = other.fGrid;
                for (int i = 0; i < fGridHeight * fGridWidth; i++) {
                    if (fGrid[i] != sourceGrid[i]) {
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

            if (this[xMinus1, yMinus1] > 0) result++;
            if (this[X, yMinus1] > 0) result++;
            if (this[xPlus1, yMinus1] > 0) result++;
            if (this[xMinus1, Y] > 0) result++;
            if (this[xPlus1, Y] > 0) result++;
            if (this[xMinus1, yPlus1] > 0) result++;
            if (this[X, yPlus1] > 0) result++;
            if (this[xPlus1, yPlus1] > 0) result++;
            
            return result;
        }

        public void SetGridSize(int newGridWidth, int newGridHeight)
        {
            FreeGrid();
            fGridWidth = newGridWidth;
            fGridHeight = newGridHeight;
            AllocGrid();
        }

        private void AllocGrid()
        {
            fGrid = new short[fGridWidth * fGridHeight];
        }

        private void FreeGrid()
        {
            fGrid = null;
        }
        
        private int CellCoordToGridOffset(int X, int Y)
        {
            if (X >= fGridWidth) throw new IndexOutOfRangeException("X parameter out of range");
            if (Y >= fGridHeight) throw new IndexOutOfRangeException("Y parameter out of range");

            return Y * fGridWidth + X;
        }

        private static int bool2int(short B)
        {
            return ((B > 0) ? 1 : 0);
        }
    }
}
