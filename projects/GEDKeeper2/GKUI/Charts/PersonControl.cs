/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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

using System.Drawing;
using System.Drawing.Drawing2D;
using GKCommon;

namespace GKUI.Charts
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class PersonControl : ITreeControl
    {
        private readonly TreeChartBox fChart;
        private readonly Pen fCtlPen;
        private readonly Brush fCtlBrush;

        private Rectangle fDestRect;
        private TreeChartPerson fPerson;
        private bool fVisible;
        
        public bool Visible
        {
            get { return this.fVisible; }
            set {
                this.fVisible = value;
                this.fChart.Invalidate();
            }
        }
        
        public PersonControl(TreeChartBox chart)
        {
            this.fChart = chart;
            this.fCtlPen = new Pen(Color.Black, 2.0f);
            this.fCtlBrush = new SolidBrush(Color.FromArgb(128, 128, 128, 128));
        }

        public void Dispose()
        {
            this.fCtlPen.Dispose();
            this.fCtlBrush.Dispose();
        }

        public void SetPerson(TreeChartPerson person)
        {
            if (person == null) return;
            this.fPerson = person;
            
            ExtRect rt = this.fPerson.Rect.GetOffset(this.fChart.fSPX, this.fChart.fSPY);
            Rectangle rect = rt.ToRectangle();
            
            rect.X = rect.Right;
            rect.Width = 40;
            
            this.fDestRect = rect;
        }
        
        public void Update()
        {
        }

        public void Draw(Graphics gfx)
        {
            if (gfx == null) return;

            /*ExtRect rt = this.fPerson.Rect;
            rt = rt.GetOffset(this.fChart.fSPX, this.fChart.fSPY);
            Rectangle rect = rt.ToRectangle();
            
            //rect.Top = rect.Top;
            //rect.Bottom = rect.Bottom;
            rect.X = rect.Right;
            rect.Width = 40;*/
            
            gfx.InterpolationMode = InterpolationMode.HighQualityBicubic;
            gfx.SmoothingMode = SmoothingMode.HighQuality;
            gfx.PixelOffsetMode = PixelOffsetMode.HighQuality;
            gfx.CompositingQuality = CompositingQuality.HighQuality;
            
            gfx.FillRectangle(this.fCtlBrush, this.fDestRect);
            gfx.DrawRectangle(this.fCtlPen, this.fDestRect);
        }

        public bool Contains(int x, int y)
        {
            return fDestRect.Contains(x, y);
        }

        public void MouseDown(int x, int y)
        {
        }

        public void MouseMove(int x, int y, ThumbMoved thumbMoved)
        {
        }

        public void MouseUp(int x, int y)
        {
        }
    }
}
