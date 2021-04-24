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
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Windows.Forms;
using GKUI.Components;

namespace GKLifePlugin.ConwayLife
{
    public class LifeViewer : UserControl
    {
        private bool fAcceptMouseClicks;
        private int fGeneration;
        private Color fGridLineColor;
        private DashStyle fGridLineStyle;
        private NotifyEvent fOnChange;
        private DoesCellLiveEvent fOnDoesCellLive;
        private bool fShowGridLines;

        private readonly LifeGrid fGrid;
        private readonly LifeHistory fHistory;
        private readonly LifeOptions fOptions;
        private readonly LifeRules fRules;


        public short this[int X, int Y]
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
        
        public LifeHistory History
        {
            get { return fHistory; }
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
        
        public Color GridLineColor
        {
            get { return fGridLineColor; }
            set {
                if (value != fGridLineColor) {
                    fGridLineColor = value;
                    Invalidate();
                }
            }
        }

        public DashStyle GridLineStyle
        {
            get { return fGridLineStyle; }
            set {
                if (value != fGridLineStyle) {
                    fGridLineStyle = value;
                    Invalidate();
                }
            }
        }
        
        public int GridWidth
        {
            get { return fGrid.GridWidth; }
            set { SetGridSize(value, GridHeight); }
        }
        
        public int MaxNumberOfHistoryLevels
        {
            get { return fHistory.MaxLevels; }
            set {
                if (value < 1)
                    throw new IndexOutOfRangeException("MaxNumberOfHistoryLevels must be greater than 0");
                if (value > LifeConsts.MaxNumberOfHistoryLevels)
                    throw new IndexOutOfRangeException(string.Format("MaxNumberOfHistoryLevels must be greater than {0}", LifeConsts.MaxNumberOfHistoryLevels));

                fHistory.MaxLevels = value;
            }
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

        public LifeOptions Options
        {
            get { return fOptions; }
        }
        
        public NotifyEvent OnChange
        {
            get { return fOnChange; }
            set { fOnChange = value; }
        }

        public DoesCellLiveEvent OnDoesCellLive
        {
            get { return fOnDoesCellLive; }
            set { fOnDoesCellLive = value; }
        }
        
        public LifeRules Rules
        {
            get { return fRules; }
        }
        
        public LifeViewer()
        {
            DoubleBuffered = true;
            
            fOptions = new LifeOptions();
            fRules = new LifeRules();
            fGrid = new LifeGrid(LifeConsts.DefaultGridWidth, LifeConsts.DefaultGridHeight);
            fHistory = new LifeHistory(LifeConsts.DefaultNumberOfHistoryLevels);
            fGridLineColor = LifeConsts.DefaultGridLineColor;
            fGridLineStyle = LifeConsts.DefaultGridLineStyle;
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
            int ClientWidth = Width;
            int ClientHeight = Height;
            
            if ((X < 0) || (X >= ClientWidth))
                throw new IndexOutOfRangeException("X coordinate is outside the control's bounds");
            if ((Y < 0) || (Y >= ClientHeight))
                throw new IndexOutOfRangeException("Y coordinate is outside the control's bounds");

            Point result = new Point();
            
            int cellWidth = ClientWidth / GridWidth;
            int offsetX = (ClientWidth % GridWidth) / 2;

            if (X <= offsetX * (cellWidth + 1)) {
                result.X = X / (cellWidth + 1);
            } else {
                result.X = offsetX + (X - offsetX * (cellWidth + 1)) / cellWidth;
            }

            int cellHeight = ClientHeight / GridHeight;
            int offsetY = (ClientHeight % GridHeight) / 2;

            if (Y <= offsetY * (cellHeight + 1)) {
                result.Y = Y / (cellHeight + 1);
            } else {
                result.Y = offsetY + (Y - offsetY * (cellHeight + 1)) / cellHeight;
            }
            
            return result;
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

        protected bool DoesCellLive(int X, int Y, LifeGrid grid)
        {
            bool result = grid.DoesCellLive(X, Y);
            if (fOnDoesCellLive != null) fOnDoesCellLive(this, X, Y, grid, ref result);
            return result;
        }

        protected void InvalidateCell(int X, int Y)
        {
            if (X >= GridWidth)
                throw new IndexOutOfRangeException("X parameter out of range");
            if (Y >= GridHeight)
                throw new IndexOutOfRangeException("Y parameter out of range");

            Rectangle rect = CellCoords(X, Y);
            Invalidate(new Region(rect));
        }

        protected override void OnMouseUp(MouseEventArgs e)
        {
            if (AcceptMouseClicks && (e.Button == MouseButtons.Left)) {
                Point pt = CellAtPos(e.X, e.Y);
                short val = this[pt.X, pt.Y];
                this[pt.X, pt.Y] = (short)((val > 0) ? 0 : 1);
            }

            base.OnMouseUp(e);
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
                using (Pen pen = new Pen(Color.Black)) {
                    DrawGridLines(gfx, pen);
                }
            }

            Color cellColor = fOptions.LivingCellColor;
            Color bordColor = UIHelper.Lighter(cellColor, 0.5f);

            // Draw all the live cells
            using (Brush brush = new SolidBrush(cellColor))
            {
                using (Pen pen = new Pen(bordColor))
                {
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

        protected override void OnResize(EventArgs e)
        {
            Invalidate();
            base.OnResize(e);
        }

        protected void SetCell(int X, int Y, short value)
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
                    bool live = DoesCellLive(x, y, MostRecentGrid);
                    SetCell(x, y, (short)((live) ? 1 : 0));
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
