/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;
using GKCommon.TreeMap;

namespace GKDataQualityPlugin
{
    public sealed class MapFile : MapItem
    {
        public long Changes;
        public Color Color;

        public MapFile(string name, double size)
            : base(name, size)
        {
        }
    }

    /// <summary>
    /// TreeMap Viewer's control.
    /// </summary>
    public sealed class DQViewer : UserControl
    {
        private class DQModel : TreemapModel
        {
            public DQModel(int width, int height)
                : base(width, height)
            {
            }

            public override MapItem newItem(string name, double size)
            {
                return new MapFile(name, size);
            }
        }

        private readonly TreemapModel fModel;

        public TreemapModel Model
        {
            get { return fModel; }
        }

        public DQViewer()
        {
            DoubleBuffered = true;

            int w = 1200;
            int h = 800;
            fModel = new DQModel(w, h);
            fModel.CalcLayout(new MapRect(0, 0, w, h));
        }

        public void UpdateView()
        {
            fModel.CalcLayout(new MapRect(0, 0, Width, Height));
            Invalidate();
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            MapItem item = fModel.FindByCoord(e.X, e.Y);
            if (item != null) {
                (Parent as Form).Text = item.Name;
            }
        }

        protected override void OnResize(EventArgs e)
        {
            base.OnResize(e);
            UpdateView();
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            base.OnPaint(e);
            DrawItems(e.Graphics, fModel.GetItems(), Color.Black);
        }

        private void DrawItems(Graphics gfx, List<MapItem> items, Color color)
        {
            foreach (MapItem item in items) {
                MapRect rect = item.Bounds;

                if (item.IsLeaf()) {
                    gfx.FillRectangle(new SolidBrush(((MapFile)item).Color), (int)rect.X, (int)rect.Y, (int)rect.W, (int)rect.H);
                    gfx.DrawRectangle(new Pen(Color.Black), (int)rect.X, (int)rect.Y, (int)rect.W, (int)rect.H);
                } else {
                    gfx.DrawRectangle(new Pen(color), (int)rect.X, (int)rect.Y, (int)rect.W, (int)rect.H);
                    DrawItems(gfx, item.Items, color);
                }
            }
        }
    }
}
