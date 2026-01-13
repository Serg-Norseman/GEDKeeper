/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;
using GKCore.Design.Graphics;

namespace GKCore.Charts
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class TCPersonControl : ITreeControl
    {
        private IPen fCtlPen;
        private IBrush fCtlBrush;

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

        public TCPersonControl(ITreeChart chart) : base(chart)
        {
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fCtlPen.Dispose();
                fCtlBrush.Dispose();
            }
            base.Dispose(disposing);
        }

        public void SetPerson(TreeChartPerson person)
        {
            if (person == null) return;
            fPerson = person;

            ExtPoint offsets = fChart.GetOffsets();
            ExtRect rt = fPerson.Rect.GetOffset(offsets.X, offsets.Y);
            rt = ExtRect.CreateBounds(rt.Right, rt.Top, 40, rt.Height);
            fDestRect = rt;
        }

        public override void UpdateState()
        {
        }

        public override void UpdateView()
        {
        }

        public override void Draw(ChartRenderer gfx)
        {
            if (gfx == null) return;

            if (fCtlPen == null) fCtlPen = gfx.CreatePen(ChartRenderer.GetColor(GKColors.Black), 2.0f);
            if (fCtlBrush == null) fCtlBrush = gfx.CreateBrush(ChartRenderer.GetColor(GKColors.Gray));

            //gfx.FillRectangle(fCtlBrush, fDestRect);
            //gfx.DrawRectangle(fCtlPen, fDestRect);
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
