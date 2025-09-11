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
using System.Windows.Forms;
using GKUI.Components;

namespace GKLifePlugin.ConwayLife
{
    public class LifeViewer : UserControl, ILifeViewer
    {
        public static readonly Color DefaultCellColor = Color.Green;
        public static readonly Color DefaultBackgroundColor = Color.Silver;
        public static readonly Color DefaultGridLineColor = Color.Black;

        private readonly LifeModel fModel;

        public LifeModel Model { get { return fModel; } }


        public LifeViewer()
        {
            DoubleBuffered = true;
            fModel = new LifeModel(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fModel.Dispose();
            }
            base.Dispose(disposing);
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Left)
                fModel.SetCellAtPos(e.X, e.Y);

            base.OnMouseMove(e);
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            Graphics gfx = e.Graphics;

            if (fModel.ShowGridLines) {
                using (Pen pen = new Pen(DefaultGridLineColor)) {
                    int clientWidth = Width;
                    int clientHeight = Height;

                    for (int i = 1; i < fModel.GridWidth; i++) {
                        int coord = LifeModel.CellEdge(i, clientWidth, fModel.GridWidth);
                        gfx.DrawLine(pen, coord, 0, coord, clientHeight);
                    }

                    for (int i = 1; i < fModel.GridHeight; i++) {
                        int coord = LifeModel.CellEdge(i, clientHeight, fModel.GridHeight);
                        gfx.DrawLine(pen, 0, coord, clientWidth, coord);
                    }
                }
            }

            Color cellColor = DefaultCellColor;
            Color bordColor = UIHelper.Lighter(cellColor, 0.5f);

            using (Brush brush = new SolidBrush(cellColor))
            using (Pen pen = new Pen(bordColor)) {
                for (int y = 0; y < fModel.GridHeight; y++) {
                    for (int x = 0; x < fModel.GridWidth; x++) {
                        if (fModel[x, y] > 0) {
                            var r = UIHelper.Rt2Rt(fModel.CellCoords(x, y));
                            r.Inflate(-1, -1);
                            gfx.FillEllipse(brush, r);
                            gfx.DrawEllipse(pen, r);
                        }
                    }
                }
            }

            base.OnPaint(e);
        }

        protected override void OnResize(EventArgs e)
        {
            fModel.SetSize(Width, Height);
            Invalidate();
            base.OnResize(e);
        }
    }
}
