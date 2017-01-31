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

using System;
using System.Drawing.Drawing2D;
using GKCommon.Controls;
using GKCore;

namespace GKUI.Charts
{
    public abstract class CustomChart : GKScrollableControl
    {
        protected const float PI = 3.1415926535897931f;

        private static readonly object EventNavRefresh;


        private readonly NavigationStack fNavman;


        public event EventHandler NavRefresh
        {
            add { base.Events.AddHandler(CustomChart.EventNavRefresh, value); }
            remove { base.Events.RemoveHandler(CustomChart.EventNavRefresh, value); }
        }


        static CustomChart()
        {
            CustomChart.EventNavRefresh = new object();
        }

        protected CustomChart() : base()
        {
            fNavman = new NavigationStack();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (fNavman != null) fNavman.Dispose();
            }
            base.Dispose(disposing);
        }

        private void DoNavRefresh()
        {
            var eventHandler = (EventHandler)base.Events[CustomChart.EventNavRefresh];
            if (eventHandler == null) return;

            eventHandler(this, null);
        }


        protected abstract void SetNavObject(object obj);


        public bool NavAdd(object obj)
        {
            if (obj != null && !fNavman.Busy) {
                fNavman.Current = obj;
                return true;
            }
            return false;
        }

        public bool NavCanBackward()
        {
            return fNavman.CanBackward();
        }

        public bool NavCanForward()
        {
            return fNavman.CanForward();
        }

        public void NavNext()
        {
            if (!fNavman.CanForward()) return;

            fNavman.BeginNav();
            try
            {
                SetNavObject(fNavman.Next());
                DoNavRefresh();
            }
            finally
            {
                fNavman.EndNav();
            }
        }

        public void NavPrev()
        {
            if (!fNavman.CanBackward()) return;

            fNavman.BeginNav();
            try
            {
                SetNavObject(fNavman.Back());
                DoNavRefresh();
            }
            finally
            {
                fNavman.EndNav();
            }
        }

        internal static void CreateCircleSegment(GraphicsPath path,
                                                 int inRad, int extRad, float wedgeAngle,
                                                 float ang1, float ang2)
        {
            CreateCircleSegment(path, 0, 0, inRad, extRad, wedgeAngle, ang1, ang2);
        }

        internal static void CreateCircleSegment(GraphicsPath path, int ctX, int ctY,
                                                 int inRad, int extRad, float wedgeAngle,
                                                 float ang1, float ang2)
        {
            float angval1 = ang1 * PI / 180.0f;
            int px1 = ctX + (int)(inRad * Math.Cos(angval1));
            int py1 = ctY + (int)(inRad * Math.Sin(angval1));
            int px2 = ctX + (int)(extRad * Math.Cos(angval1));
            int py2 = ctY + (int)(extRad * Math.Sin(angval1));

            float angval2 = ang2 * PI / 180.0f;
            int nx1 = ctX + (int)(inRad * Math.Cos(angval2));
            int ny1 = ctY + (int)(inRad * Math.Sin(angval2));
            int nx2 = ctX + (int)(extRad * Math.Cos(angval2));
            int ny2 = ctY + (int)(extRad * Math.Sin(angval2));

            int ir2 = inRad * 2;
            int er2 = extRad * 2;

            path.StartFigure();
            path.AddLine(px2, py2, px1, py1);
            if (ir2 != 0) path.AddArc(ctX - inRad, ctY - inRad, ir2, ir2, ang1, wedgeAngle);
            path.AddLine(nx1, ny1, nx2, ny2);
            path.AddArc(ctX - extRad, ctY - extRad, er2, er2, ang2, -wedgeAngle);
            path.CloseFigure();
        }
    }
}
