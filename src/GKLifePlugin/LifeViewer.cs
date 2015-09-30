using System;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Windows.Forms;

using GKCommon;

namespace GKLifePlugin
{
    public class LifeViewer : UserControl
    {
        private bool fAcceptMouseClicks;
        private Color fCellColor;
        private int fGeneration;
        private LifeGrid fGrid;
        private Color fGridLineColor;
        private DashStyle fGridLineStyle;
        private LifeHistory fHistory;
        private NotifyEvent fOnChange;
        private DoesCellLiveEvent fOnDoesCellLive;
        private bool fShowGridLines;
        private LifeRules fRules;


        public bool this[int X, int Y]
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
		
        public Color CellColor
        {
            get { return this.fCellColor; }
            set {
                if (value != this.fCellColor) {
                    this.fCellColor = value;
                    this.Invalidate();
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
                if (value > LifeConsts.AbsoluteMaxNumberOfHistoryLevels)
                    throw new IndexOutOfRangeException(string.Format("MaxNumberOfHistoryLevels must be greater than {0}", LifeConsts.AbsoluteMaxNumberOfHistoryLevels));

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

        protected ExtRect CellCoords(int X, int Y)
        {
            int ClientWidth = this.Width;
            int ClientHeight = this.Height;
			
            if (X >= GridWidth) throw new IndexOutOfRangeException("X parameter out of range");
            if (Y >= GridHeight) throw new IndexOutOfRangeException("Y parameter out of range");

            ExtRect Result;
            Result.Left = CellEdge(X, ClientWidth, GridWidth);
            Result.Top = CellEdge(Y, ClientHeight, GridHeight);
            Result.Right = CellEdge(X + 1, ClientWidth, GridWidth);
            Result.Bottom = CellEdge(Y + 1, ClientHeight, GridHeight);

            return Result;
        }

        protected void Change()
        {
            this.Invalidate();

            if (this.fOnChange != null) this.fOnChange(this);
        }

        protected bool DoesCellLive(int X, int Y, LifeGrid grid)
        {
            bool Result = grid.DoesCellLive(X, Y);
            if (fOnDoesCellLive != null) fOnDoesCellLive(this, X, Y, grid, ref Result);
            return Result;
        }

        protected void InvalidateCell(int X, int Y)
        {
            if (X >= GridWidth)
                throw new IndexOutOfRangeException("X parameter out of range");
            if (Y >= GridHeight)
                throw new IndexOutOfRangeException("Y parameter out of range");

            ExtRect Rect = CellCoords(X, Y);
            //this.InvalidateRect(Handle, @Rect, True);
        }

        protected override void OnMouseUp(MouseEventArgs e)
        {
            if (this.AcceptMouseClicks && (e.Button == MouseButtons.Left)) {
                Point pt = this.CellAtPos(e.X, e.Y);
                this[pt.X, pt.Y] = !this[pt.X, pt.Y];
            }
        }

        /*protected void MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
		{
		}*/

        private void DrawGridLines(Graphics gfx, Pen pen)
        {
            int Coordinate, i;

            /*Brush.Color = Color;
			Pen.Color = GridLineColor;
			Pen.Mode = pmMask;
			Pen.Style = GridLineStyle;*/

            int ClientWidth = this.Width;
            int ClientHeight = this.Height;
			
            for (i = 1; i <= GridWidth - 1; i++)
            {
                Coordinate = CellEdge(i, ClientWidth, GridWidth);
                gfx.DrawLine(pen, Coordinate, 0, Coordinate, ClientHeight);
            }

            for (i = 1; i <= GridHeight - 1; i++)
            {
                Coordinate = CellEdge(i, ClientHeight, GridHeight);
                gfx.DrawLine(pen, 0, Coordinate, ClientWidth, Coordinate);
            }
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            Graphics gfx = e.Graphics;
			
            Brush brush = new SolidBrush(this.fCellColor);

            if (this.fShowGridLines) {
                using (Pen pen = new Pen(Color.Black)) {
                    this.DrawGridLines(gfx, pen);
                }
            }

            // Draw all the live cells
            using (Pen pen = new Pen(Color.Green)) {
                for (int y = 0; y < this.GridHeight; y++) {
                    for (int x = 0; x < this.GridWidth; x++) {
                        if (this[x, y]) {
                            ExtRect r = this.CellCoords(x, y);
                            r.OffsetEx(1, 1);
                            Rectangle rt = r.ToRectangle();
                            gfx.FillEllipse(brush, rt);
                            gfx.DrawEllipse(pen, rt /*r.Left + 2, r.Top + 1, r.Right + 1, r.Bottom*/);
                        }
                    }
                }
            }

            // At design-time, draw a dashed line around the component
            /*if (csDesigning in ComponentState) {
        	Brush.Color = Color;
        	Brush.Style = bsCross;
        	FrameRect(Rect(0, 0, Width, Height))
        	}*/
        }

        protected void SetCell(int X, int Y, bool value)
        {
            if (this[X, Y] != value) {
                this.fGrid[X, Y] = value;
                this.InvalidateCell(X, Y);
            }
        }
		
        public LifeViewer()
        {
            this.DoubleBuffered = true;
			
            this.fRules = new LifeRules();
            this.fGrid = new LifeGrid(LifeConsts.DefaultGridWidth, LifeConsts.DefaultGridHeight);
            this.fHistory = new LifeHistory(LifeConsts.DefaultMaxNumberOfHistoryLevels);
            this.fCellColor = LifeConsts.DefaultCellColor;
            this.fGridLineColor = LifeConsts.DefaultGridLineColor;
            this.fGridLineStyle = LifeConsts.DefaultGridLineStyle;
        }

        private void cmpLifeDoesCellLive(int X, int Y, LifeGrid grid, ref bool result)
        {
            if (grid[X, Y]) {
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
                    this[x, y] = (rnd.NextDouble() < 0.5) ? true : false;
                }
            }

            this.Invalidate();
        }

        public int NextGeneration()
        {
            LifeGrid MostRecentGrid = this.fHistory.Add(this.fGrid);

            for (int y = 0; y < GridHeight; y++)
                for (int x = 0; x < GridWidth; x++)
                    this.SetCell(x, y, this.DoesCellLive(x, y, MostRecentGrid));

            this.fGeneration++;
            this.Change();

            int result = this.fHistory.Contains(fGrid) + 1;
            return result;
        }

        public void SetGridSize(int NewGridWidth, int NewGridHeight)
        {
            if (NewGridWidth != GridWidth || NewGridHeight != GridHeight) {
                this.fHistory.Clear();
                this.fGrid.SetGridSize(NewGridWidth, NewGridHeight);

                this.Change();
            }
        }

        public void Destroy()
        {
            this.fGrid.Destroy();
            this.fHistory.Destroy();
        }

        public void ResetGeneration()
        {
            this.fGeneration = 0;
            this.Change();
        }
    }
}