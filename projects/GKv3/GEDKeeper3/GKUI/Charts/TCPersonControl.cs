/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using BSLib;
using Eto.Drawing;
using GKCore.Charts;
using GKUI.Components;

namespace GKUI.Charts
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class TCPersonControl : ITreeControl
    {
        private readonly Pen fCtlPen;
        private readonly Brush fCtlBrush;

        private TreeChartPerson fPerson;

        #region Public properties

        public override string Tip
        {
            get { return string.Empty; }
        }

        public override int Width
        {
            get { return 0; }
        }

        public override int Height
        {
            get { return 0; }
        }

        #endregion

        public TCPersonControl(TreeChartBox chart) : base(chart)
        {
            fCtlPen = new Pen(Colors.Black, 2.0f);
            fCtlBrush = new SolidBrush(Color.FromArgb(128, 128, 128, 128));
        }

        protected override void Dispose(bool disposing)
        {
            fCtlPen.Dispose();
            fCtlBrush.Dispose();
        }

        public void SetPerson(TreeChartPerson person)
        {
            if (person == null) return;
            fPerson = person;

            ExtPoint offsets = fChart.GetOffsets();
            ExtRect rt = fPerson.Rect.GetOffset(offsets.X, offsets.Y);
            Rectangle rect = UIHelper.Rt2Rt(rt);

            rect.X = rect.Right;
            rect.Width = 40;

            fDestRect = rect;
        }

        public override void UpdateState()
        {
        }

        public override void UpdateView()
        {
        }

        public override void Draw(Graphics gfx)
        {
            if (gfx == null) return;

            /*ExtRect rt = fPerson.Rect;
            rt = rt.GetOffset(fChart.fSPX, fChart.fSPY);
            Rectangle rect = rt.ToRectangle();
            
            //rect.Top = rect.Top;
            //rect.Bottom = rect.Bottom;
            rect.X = rect.Right;
            rect.Width = 40;*/
            
            gfx.AntiAlias = true;
            gfx.ImageInterpolation = ImageInterpolation.High;
            gfx.PixelOffsetMode = PixelOffsetMode.Half;
            
            gfx.FillRectangle(fCtlBrush, fDestRect);
            gfx.DrawRectangle(fCtlPen, fDestRect);
        }

        public override void MouseDown(int x, int y)
        {
        }

        public override void MouseMove(int x, int y)
        {
        }

        public override void MouseUp(int x, int y)
        {
        }
    }
}
