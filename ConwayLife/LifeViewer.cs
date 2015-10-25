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
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Windows.Forms;

namespace ConwayLife
{
    public class LifeViewer : UserControl
    {
        private bool fAcceptMouseClicks;
        private int fGeneration;
        private LifeGrid fGrid;
        private Color fGridLineColor;
        private DashStyle fGridLineStyle;
        private LifeHistory fHistory;
        private NotifyEvent fOnChange;
        private DoesCellLiveEvent fOnDoesCellLive;
        private LifeOptions fOptions;
        private bool fShowGridLines;
        private LifeRules fRules;


        public short this[int X, int Y]
        {
            get {
                return this.fGrid[X, Y];
            }
            set {
                if (this.fGrid[X, Y] != value) {
                    this.SetCell(X, Y, value);
                    this.ResetGeneration();
                    this.fHistory.Clear();
                }
            }
        }

        public int Generation
        {
            get { return this.fGeneration; }
        }
		
        public LifeHistory History
        {
            get { return this.fHistory; }
        }
		
        public int LiveCellCount
        {
            get { return this.fGrid.LiveCellCount; }
        }

        public bool AcceptMouseClicks
        {
            get { return this.fAcceptMouseClicks; }
            set {
                if (value != this.fAcceptMouseClicks) {
                    this.fAcceptMouseClicks = value;
                    this.Change();
                }
            }
        }

        public int GridHeight
        {
            get { return this.fGrid.GridHeight; }
            set { this.SetGridSize(GridWidth, value); }
        }
		
        public Color GridLineColor
        {
            get { return this.fGridLineColor; }
            set {
                if (value != this.fGridLineColor) {
                    this.fGridLineColor = value;
                    this.Invalidate();
                }
            }
        }
		
        public DashStyle GridLineStyle
        {
            get { return this.fGridLineStyle; }
            set {
                if (value != this.fGridLineStyle) {
                    this.fGridLineStyle = value;
                    this.Invalidate();
                }
            }
        }
		
        public int GridWidth
        {
            get { return this.fGrid.GridWidth; }
            set { this.SetGridSize(value, GridHeight); }
        }
		
        public int MaxNumberOfHistoryLevels
        {
            get { return this.fHistory.MaxLevels; }
            set {
                if (value < 1)
                    throw new IndexOutOfRangeException("MaxNumberOfHistoryLevels must be greater than 0");
                if (value > LifeConsts.MaxNumberOfHistoryLevels)
                    throw new IndexOutOfRangeException(string.Format("MaxNumberOfHistoryLevels must be greater than {0}", LifeConsts.MaxNumberOfHistoryLevels));

                this.fHistory.MaxLevels = value;
            }
        }

        public bool ShowGridLines
        {
            get { return this.fShowGridLines; }
            set {
                if (value != this.fShowGridLines) {
                    this.fShowGridLines = value;
                    this.Invalidate();
                }
            }
        }

        public LifeOptions Options
        {
        	get { return this.fOptions; }
        }
        
        public NotifyEvent OnChange
        {
            get { return this.fOnChange; }
            set { this.fOnChange = value; }
        }

        public DoesCellLiveEvent OnDoesCellLive
        {
            get { return this.fOnDoesCellLive; }
            set { this.fOnDoesCellLive = value; }
        }
		
        public LifeRules Rules
        {
            get { return this.fRules; }
        }
		
        public LifeViewer()
        {
            this.DoubleBuffered = true;
			
            this.fOptions = new LifeOptions();
            this.fRules = new LifeRules();
            this.fGrid = new LifeGrid(LifeConsts.DefaultGridWidth, LifeConsts.DefaultGridHeight);
            this.fHistory = new LifeHistory(LifeConsts.DefaultNumberOfHistoryLevels);
            this.fGridLineColor = LifeConsts.DefaultGridLineColor;
            this.fGridLineStyle = LifeConsts.DefaultGridLineStyle;
        }

        protected override void Dispose(bool disposing)
        {
        	if (disposing) {
        		this.fGrid.Dispose();
        		this.fHistory.Dispose();
        	}
        	base.Dispose(disposing);
        }

        protected Point CellAtPos(int X, int Y)
        {
            int ClientWidth = this.Width;
            int ClientHeight = this.Height;
			
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
            int ClientWidth = this.Width;
            int ClientHeight = this.Height;
			
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
            this.Invalidate();

            if (this.fOnChange != null) this.fOnChange(this);
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

            //ExtRect Rect = CellCoords(X, Y);
            //this.InvalidateRect(Handle, @Rect, True);
        }

        protected override void OnMouseUp(MouseEventArgs e)
        {
            if (this.AcceptMouseClicks && (e.Button == MouseButtons.Left)) {
                Point pt = this.CellAtPos(e.X, e.Y);
                short val = this[pt.X, pt.Y];
                this[pt.X, pt.Y] = (short)((val > 0) ? 0 : 1);
            }
        }

        /*protected void MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
		{
		}*/

        private void DrawGridLines(Graphics gfx, Pen pen)
        {
            /*Brush.Color = Color;
			Pen.Color = GridLineColor;
			Pen.Mode = pmMask;
			Pen.Style = GridLineStyle;*/

            int clientWidth = this.Width;
            int clientHeight = this.Height;
			
            int coord, i;

            for (i = 1; i < GridWidth; i++)
            {
                coord = CellEdge(i, clientWidth, GridWidth);
                gfx.DrawLine(pen, coord, 0, coord, clientHeight);
            }

            for (i = 1; i < GridHeight; i++)
            {
                coord = CellEdge(i, clientHeight, GridHeight);
                gfx.DrawLine(pen, 0, coord, clientWidth, coord);
            }
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            Graphics gfx = e.Graphics;
			
            if (this.fShowGridLines) {
                using (Pen pen = new Pen(Color.Black)) {
                    this.DrawGridLines(gfx, pen);
                }
            }

            Color cellColor = this.fOptions.LivingCellColor;
            Color bordColor = ColorUtils.lighter(cellColor, 0.5f);

            // Draw all the live cells
            using (Brush brush = new SolidBrush(cellColor))
            {
            	using (Pen pen = new Pen(bordColor))
            	{
            		for (int y = 0; y < this.GridHeight; y++) {
            			for (int x = 0; x < this.GridWidth; x++) {
            				if (this[x, y] > 0) {
            					Rectangle r = this.CellCoords(x, y);
            					r.Inflate(-1, -1);
            					gfx.FillEllipse(brush, r);
            					gfx.DrawEllipse(pen, r);
            				}
            			}
            		}
            	}
            }
        }

		protected override void OnResize(EventArgs e)
		{
			this.Invalidate();
			base.OnResize(e);
		}

		protected void SetCell(int X, int Y, short value)
        {
            if (this[X, Y] != value) {
                this.fGrid[X, Y] = value;
                this.InvalidateCell(X, Y);
            }
        }

        private void cmpLifeDoesCellLive(int X, int Y, LifeGrid grid, ref bool result)
        {
            if (grid[X, Y] > 0) {
                result = this.fRules.GetLiveCells(grid.NumberOfNeighbours(X, Y));
            } else {
                result = this.fRules.GetDeadCells(grid.NumberOfNeighbours(X, Y));
            }
        }

        public void ClearCells()
        {
            this.fGrid.Clear();
            this.fHistory.Clear();
            this.Change();
        }

        public void RandomCells()
        {
            Random rnd = new Random();
			
            for (int x = 0; x < this.GridWidth; x++) {
                for (int y = 0; y < this.GridHeight; y++) {
            		this[x, y] = (byte)((rnd.NextDouble() < 0.4) ? 1 : 0);
                }
            }

            this.Invalidate();
        }

        public int NextGeneration()
        {
            LifeGrid MostRecentGrid = this.fHistory.Add(this.fGrid);

            for (int y = 0; y < GridHeight; y++) {
            	for (int x = 0; x < GridWidth; x++) {
            		bool live = this.DoesCellLive(x, y, MostRecentGrid);
            		short val = this.fGrid[x, y];
            		this.SetCell(x, y, (short)((live) ? 1 : 0));
            	}
            }

            this.fGeneration++;
            this.Change();

            int result = this.fHistory.Contains(fGrid) + 1;
            return result;
        }

        public void SetGridSize(int newGridWidth, int newGridHeight)
        {
            if (newGridWidth != GridWidth || newGridHeight != GridHeight) {
                this.fHistory.Clear();
                this.fGrid.SetGridSize(newGridWidth, newGridHeight);

                this.Change();
            }
        }

        public void ResetGeneration()
        {
            this.fGeneration = 0;
            this.Change();
        }
    }
}