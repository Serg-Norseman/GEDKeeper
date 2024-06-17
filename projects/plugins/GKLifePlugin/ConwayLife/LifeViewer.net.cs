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
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKLifePlugin.ConwayLife
{
    public delegate void NotifyEvent(object sender);

    public class LifeViewer : Drawable
    {
        public static readonly Color DefaultCellColor = Colors.Green;
        public static readonly Color DefaultBackgroundColor = Colors.Silver;
        public static readonly Color DefaultGridLineColor = Colors.Black;


        private bool fAcceptMouseClicks;
        private int fGeneration;
        private NotifyEvent fOnChange;
        private bool fShowGridLines;

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
                    Invalidate();
                }
            }
        }

        public NotifyEvent OnChange
        {
            get { return fOnChange; }
            set { fOnChange = value; }
        }

        public LifeViewer()
        {
            //DoubleBuffered = true;

            fGrid = new LifeGrid(LifeGrid.DefaultGridWidth, LifeGrid.DefaultGridHeight);
            fHistory = new LifeHistory(LifeHistory.DefaultNumberOfHistoryLevels);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fGrid.Dispose();
                fHistory.Dispose();
            }
            base.Dispose(disposing);
        }

        protected Point CellAtPos(int X, int Y)
        {
            int clientWidth = Width;
            int clientHeight = Height;

            if (X < 0 || X >= clientWidth)
                throw new IndexOutOfRangeException("X coordinate is outside the control's bounds");
            if (Y < 0 || Y >= clientHeight)
                throw new IndexOutOfRangeException("Y coordinate is outside the control's bounds");

            int cellWidth = clientWidth / GridWidth;
            int offsetX = (clientWidth % GridWidth) / 2;

            int cellHeight = clientHeight / GridHeight;
            int offsetY = (clientHeight % GridHeight) / 2;

            return new Point((X - offsetX) / cellWidth, (Y - offsetY) / cellHeight);
        }

        private int CellEdge(int coordinate, int fieldSize, int divisions)
        {
            int cellSize = fieldSize / divisions;
            int remainder = (fieldSize % divisions) / 2;

            int result = (coordinate * cellSize) + remainder;
            return result;
        }

        private static Rectangle CreateRect(int left, int top, int right, int bottom)
        {
            Rectangle result = new Rectangle(left, top, right - left + 1, bottom - top + 1);
            return result;
        }

        protected Rectangle CellCoords(int X, int Y)
        {
            int ClientWidth = Width;
            int ClientHeight = Height;

            if (X >= GridWidth) throw new IndexOutOfRangeException("X parameter out of range");
            if (Y >= GridHeight) throw new IndexOutOfRangeException("Y parameter out of range");

            Rectangle result = CreateRect(
                CellEdge(X, ClientWidth, GridWidth),
                CellEdge(Y, ClientHeight, GridHeight),
                CellEdge(X + 1, ClientWidth, GridWidth),
                CellEdge(Y + 1, ClientHeight, GridHeight));
            return result;
        }

        protected void Change()
        {
            Invalidate();

            if (fOnChange != null) fOnChange(this);
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            if (AcceptMouseClicks && (e.Buttons == MouseButtons.Primary)) {
                Point pt = CellAtPos((int)e.Location.X, (int)e.Location.Y);
                byte val = this[pt.X, pt.Y];
                this[pt.X, pt.Y] = (byte)((val > 0) ? 0 : 1);
            }

            base.OnMouseMove(e);
        }

        private void DrawGridLines(Graphics gfx, Pen pen)
        {
            int clientWidth = Width;
            int clientHeight = Height;
            int coord, i;

            for (i = 1; i < GridWidth; i++) {
                coord = CellEdge(i, clientWidth, GridWidth);
                gfx.DrawLine(pen, coord, 0, coord, clientHeight);
            }

            for (i = 1; i < GridHeight; i++) {
                coord = CellEdge(i, clientHeight, GridHeight);
                gfx.DrawLine(pen, 0, coord, clientWidth, coord);
            }
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            Graphics gfx = e.Graphics;

            if (fShowGridLines) {
                using (Pen pen = new Pen(DefaultGridLineColor)) {
                    DrawGridLines(gfx, pen);
                }
            }

            Color cellColor = DefaultCellColor;
            Color bordColor = UIHelper.Lighter(cellColor, 0.5f);

            // Draw all the live cells
            using (Brush brush = new SolidBrush(cellColor)) {
                using (Pen pen = new Pen(bordColor)) {
                    for (int y = 0; y < GridHeight; y++) {
                        for (int x = 0; x < GridWidth; x++) {
                            if (this[x, y] > 0) {
                                Rectangle r = CellCoords(x, y);
                                r.Inflate(-1, -1);
                                gfx.FillEllipse(brush, r);
                                gfx.DrawEllipse(pen, r);
                            }
                        }
                    }
                }
            }

            base.OnPaint(e);
        }

        protected override void OnSizeChanged(EventArgs e)
        {
            Invalidate();
            base.OnSizeChanged(e);
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

            Invalidate();
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
