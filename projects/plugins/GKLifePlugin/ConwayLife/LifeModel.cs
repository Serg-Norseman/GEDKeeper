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
using BSLib;

namespace GKLifePlugin.ConwayLife
{
    public interface ILifeViewer
    {
        void Invalidate();
    }


    public delegate void NotifyEvent(object sender);


    public class LifeModel : IDisposable
    {
        private readonly ILifeViewer fViewer;


        private bool fAcceptMouseClicks;
        private int fGeneration;
        private NotifyEvent fOnChange;
        private bool fShowGridLines;
        private int fHeight, fWidth;

        private readonly LifeGrid fGrid;
        private readonly LifeHistory fHistory;


        public byte this[int X, int Y]
        {
            get {
                return fGrid[X, Y];
            }
            set {
                if (fGrid[X, Y] != value) {
                    SetCell(X, Y, value);
                    ResetGeneration();
                    fHistory.Clear();
                }
            }
        }

        public int Generation
        {
            get { return fGeneration; }
        }

        public int LiveCellCount
        {
            get { return fGrid.LiveCellCount; }
        }

        public bool AcceptMouseClicks
        {
            get { return fAcceptMouseClicks; }
            set {
                if (value != fAcceptMouseClicks) {
                    fAcceptMouseClicks = value;
                    Change();
                }
            }
        }

        public int GridHeight
        {
            get { return fGrid.GridHeight; }
            set { SetGridSize(GridWidth, value); }
        }

        public int GridWidth
        {
            get { return fGrid.GridWidth; }
            set { SetGridSize(value, GridHeight); }
        }

        public bool ShowGridLines
        {
            get { return fShowGridLines; }
            set {
                if (value != fShowGridLines) {
                    fShowGridLines = value;
                    fViewer.Invalidate();
                }
            }
        }

        public NotifyEvent OnChange
        {
            get { return fOnChange; }
            set { fOnChange = value; }
        }


        public LifeModel(ILifeViewer viewer)
        {
            fViewer = viewer;
            fGrid = new LifeGrid(LifeGrid.DefaultGridWidth, LifeGrid.DefaultGridHeight);
            fHistory = new LifeHistory(LifeHistory.DefaultNumberOfHistoryLevels);

            fAcceptMouseClicks = false;
            fShowGridLines = false;
        }

        public void Dispose()
        {
            fGrid.Dispose();
            fHistory.Dispose();
        }

        public void SetSize(int width, int height)
        {
            fHeight = height;
            fWidth = width;
        }

        public ExtPoint CellAtPos(int X, int Y)
        {
            int clientWidth = fWidth;
            int clientHeight = fHeight;

            if (X < 0 || X >= clientWidth)
                throw new IndexOutOfRangeException("X coordinate is outside the control's bounds");
            if (Y < 0 || Y >= clientHeight)
                throw new IndexOutOfRangeException("Y coordinate is outside the control's bounds");

            int cellWidth = clientWidth / GridWidth;
            int offsetX = (clientWidth % GridWidth) / 2;

            int cellHeight = clientHeight / GridHeight;
            int offsetY = (clientHeight % GridHeight) / 2;

            return new ExtPoint((X - offsetX) / cellWidth, (Y - offsetY) / cellHeight);
        }

        public void SetCellAtPos(int x, int y)
        {
            if (fAcceptMouseClicks) {
                var pt = CellAtPos(x, y);
                byte val = this[pt.X, pt.Y];
                this[pt.X, pt.Y] = (byte)((val > 0) ? 0 : 1);
            }
        }

        public static int CellEdge(int coordinate, int fieldSize, int divisions)
        {
            int cellSize = fieldSize / divisions;
            int remainder = (fieldSize % divisions) / 2;

            int result = (coordinate * cellSize) + remainder;
            return result;
        }

        private static ExtRect CreateRect(int left, int top, int right, int bottom)
        {
            var result = new ExtRect(left, top, right - left + 1, bottom - top + 1);
            return result;
        }

        public ExtRect CellCoords(int X, int Y)
        {
            int ClientWidth = fWidth;
            int ClientHeight = fHeight;

            if (X >= GridWidth) throw new IndexOutOfRangeException("X parameter out of range");
            if (Y >= GridHeight) throw new IndexOutOfRangeException("Y parameter out of range");

            var result = CreateRect(
                CellEdge(X, ClientWidth, GridWidth),
                CellEdge(Y, ClientHeight, GridHeight),
                CellEdge(X + 1, ClientWidth, GridWidth),
                CellEdge(Y + 1, ClientHeight, GridHeight));
            return result;
        }

        protected void Change()
        {
            fViewer.Invalidate();

            fOnChange?.Invoke(this);
        }

        protected void SetCell(int X, int Y, byte value)
        {
            if (this[X, Y] != value) {
                fGrid[X, Y] = value;
                //InvalidateCell(X, Y);
            }
        }

        public void ClearCells()
        {
            fGrid.Clear();
            fHistory.Clear();
            Change();
        }

        public void RandomCells()
        {
            Random rnd = new Random();

            for (int x = 0; x < GridWidth; x++) {
                for (int y = 0; y < GridHeight; y++) {
                    this[x, y] = (byte)((rnd.NextDouble() < 0.4) ? 1 : 0);
                }
            }

            fViewer.Invalidate();
        }

        public int NextGeneration()
        {
            LifeGrid MostRecentGrid = fHistory.Add(fGrid);

            for (int y = 0; y < GridHeight; y++) {
                for (int x = 0; x < GridWidth; x++) {
                    byte live = MostRecentGrid.DoesCellLive(x, y);
                    SetCell(x, y, live);
                }
            }

            fGeneration++;
            Change();

            int result = fHistory.Contains(fGrid) + 1;
            return result;
        }

        public void SetGridSize(int newGridWidth, int newGridHeight)
        {
            if (newGridWidth != GridWidth || newGridHeight != GridHeight) {
                fHistory.Clear();
                fGrid.SetGridSize(newGridWidth, newGridHeight);

                Change();
            }
        }

        public void ResetGeneration()
        {
            fGeneration = 0;
            Change();
        }
    }
}
