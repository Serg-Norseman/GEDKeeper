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
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Types;

namespace GKUI.Charts
{
    public delegate void ARootChangedEventHandler(object sender, GEDCOMIndividualRecord person);

    public enum CircleChartType { Ancestors, Descendants }

    /// <summary>
    /// 
    /// </summary>
    public abstract class CircleChart : CustomChart
    {
        protected class CircleSegment : BaseObject
        {
            public int Gen;
            public GEDCOMIndividualRecord IRec;
            public GraphicsPath Path;
            public int Rad;
            public float StartAngle;
            public float WedgeAngle;

            public CircleSegment(int generation)
            {
                this.Gen = generation;
                this.IRec = null;
                this.Path = new GraphicsPath();
            }

            protected override void Dispose(bool disposing)
            {
                if (disposing)
                {
                    if (this.Path != null) this.Path.Dispose();
                }
                base.Dispose(disposing);
            }
        }

        protected static readonly object EventRootChanged;

        protected const float PI = 3.1415926535897931f;
        protected const int CENTER_RAD = 90;
        protected const int DEFAULT_GEN_WIDTH = 60;

        protected readonly SolidBrush[] fCircleBrushes;
        protected readonly SolidBrush[] fDarkBrushes;
        protected readonly AncestorsCircleOptions fOptions;
        protected readonly List<CircleSegment> fSegments;

        private readonly IContainer fComponents;
        private readonly ToolTip fToolTip;

        protected int fCenterX;
        protected int fCenterY;
        protected int fGenWidth;
        private string fHint;
        protected int fIndividualsCount;
        protected int fMaxGenerations;
        protected int fOffsetX = 0;
        protected int fOffsetY = 0;
        protected GEDCOMIndividualRecord fRootPerson;
        protected CircleSegment fSelected;
        protected ShieldState fShieldState;

        private bool ArcText = false;


        public int GenWidth
        {
            get {
                return this.fGenWidth;
            }
            set {
                if (value < 20 || value > 100) return;

                this.fGenWidth = value;
                this.Changed();
            }
        }

        public int IndividualsCount
        {
            get { return this.fIndividualsCount; }
        }

        public AncestorsCircleOptions Options
        {
            get { return this.fOptions; }
        }

        public event ARootChangedEventHandler RootChanged
        {
            add { base.Events.AddHandler(CircleChart.EventRootChanged, value); }
            remove { base.Events.RemoveHandler(CircleChart.EventRootChanged, value); }
        }

        public GEDCOMIndividualRecord RootPerson
        {
            get {
                return this.fRootPerson;
            }
            set {
                this.fRootPerson = value;

                this.NavAdd(value);
                this.Changed();

                this.DoRootChanged(value);
            }
        }


        static CircleChart()
        {
            CircleChart.EventRootChanged = new object();
        }

        public CircleChart(IBaseWindow baseWin)
        {
            this.DoubleBuffered = true;

            this.fComponents = new System.ComponentModel.Container();

            this.fToolTip = new ToolTip(fComponents);
            this.fToolTip.AutoPopDelay = 5000;
            this.fToolTip.InitialDelay = 250;
            this.fToolTip.ReshowDelay = 50;
            this.fToolTip.ShowAlways = true;

            this.fCircleBrushes = new SolidBrush[AncestorsCircleOptions.MAX_BRUSHES];
            this.fDarkBrushes = new SolidBrush[AncestorsCircleOptions.MAX_BRUSHES];
            this.fGenWidth = DEFAULT_GEN_WIDTH;
            this.fMaxGenerations = 8;
            this.fOptions = new AncestorsCircleOptions();
            this.fSegments = new List<CircleSegment>();
            this.fSelected = null;
            this.fShieldState = baseWin.ShieldState;

            this.BackColor = this.fOptions.BrushColor[9];
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fComponents != null) fComponents.Dispose();
            }
            base.Dispose(disposing);
        }

        protected override void SetNavObject(object obj)
        {
            this.RootPerson = obj as GEDCOMIndividualRecord;
        }

        #region Abstract methods

        protected abstract void BuildPathTree();

        protected abstract void InternalDraw(Graphics gfx);

        #endregion

        private void CreateBrushes()
        {
            for (int i = 0; i < this.fOptions.BrushColor.Length; i++)
            {
                Color col = this.fOptions.BrushColor[i];

                this.fCircleBrushes[i] = new SolidBrush(col);
                this.fDarkBrushes[i] = new SolidBrush(GfxHelper.Darker(col, 0.2f));
            }
        }

        public void Changed()
        {
            this.CreateBrushes();
            this.BuildPathTree();
            this.Invalidate();
        }

        protected void DoRootChanged(GEDCOMIndividualRecord person)
        {
            ARootChangedEventHandler eventHandler = (ARootChangedEventHandler)base.Events[AncestorsCircle.EventRootChanged];
            if (eventHandler == null) return;

            eventHandler(this, person);
        }

        protected CircleSegment FindSegmentByRec(GEDCOMIndividualRecord iRec)
        {
            CircleSegment result = null;

            int num = this.fSegments.Count;
            for (int i = 0; i < num; i++) {
                CircleSegment segment = this.fSegments[i];

                if (segment.IRec == iRec) {
                    result = segment;
                    break;
                }
            }

            return result;
        }

        protected CircleSegment FindSegment(int mX, int mY)
        {
            CircleSegment result = null;

            int num = this.fSegments.Count;
            for (int i = 0; i < num; i++) {
                CircleSegment segment = this.fSegments[i];

                if (segment.Path.IsVisible(mX, mY)) {
                    result = segment;
                    break;
                }
            }

            return result;
        }

        protected void DrawPersonName(Graphics gfx, CircleSegment segment)
        {
            int gen = segment.Gen;
            GEDCOMIndividualRecord iRec = segment.IRec;

            string surn, givn;
            if (iRec == null) {
                if (gen == 0) {
                    givn = "Choose";
                    surn = "subject";
                } else {
                    return;
                }
            } else {
                string dummy;
                GKUtils.GetNameParts(iRec, out surn, out givn, out dummy);
            }

            int rad = segment.Rad - 20;
            float angle = segment.StartAngle + 90.0f + segment.WedgeAngle / 2;
            float wedgeAngle = segment.WedgeAngle;

            bool isNarrow = IsNarrowSegment(gfx, givn, rad, wedgeAngle, this.Font);

            if (gen == 0) {

                gfx.ResetTransform();
                gfx.TranslateTransform(this.fCenterX, this.fCenterY);

                SizeF sizeF = gfx.MeasureString(surn, this.Font);
                gfx.DrawString(surn, this.Font, this.fCircleBrushes[8], -sizeF.Width / 2f, -sizeF.Height / 2f - sizeF.Height / 2f);
                sizeF = gfx.MeasureString(givn, this.Font);
                gfx.DrawString(givn, this.Font, this.fCircleBrushes[8], -sizeF.Width / 2f, 0f);

            } else {

                if (isNarrow) {

                    float dx = (float)Math.Sin(PI * angle / 180.0) * rad;
                    float dy = (float)Math.Cos(PI * angle / 180.0) * rad;
                    gfx.ResetTransform();
                    gfx.TranslateTransform(this.fCenterX + dx, this.fCenterY - dy);
                    gfx.RotateTransform(angle - 90.0f);

                    SizeF size = gfx.MeasureString(givn, this.Font);
                    gfx.DrawString(givn, this.Font, this.fCircleBrushes[8], -size.Width / 2f, -size.Height / 2f);

                } else {

                    if (wedgeAngle < 20) {

                        float dx = (float)Math.Sin(PI * angle / 180.0) * rad;
                        float dy = (float)Math.Cos(PI * angle / 180.0) * rad;
                        gfx.ResetTransform();
                        gfx.TranslateTransform(this.fCenterX + dx, this.fCenterY - dy);
                        gfx.RotateTransform(angle);

                        SizeF size = gfx.MeasureString(givn, this.Font);
                        gfx.DrawString(givn, this.Font, this.fCircleBrushes[8], -size.Width / 2f, -size.Height / 2f);

                    } else if (wedgeAngle < 180) {

                        if (ArcText) {
                            if (gen == 2) {
                                SizeF sizeF = gfx.MeasureString(surn, this.Font);
                                DrawArcText(gfx, surn, this.fCenterX, this.fCenterY, rad + sizeF.Height / 2f,
                                            segment.StartAngle, segment.WedgeAngle, true, true, Font, this.fCircleBrushes[8]);

                                sizeF = gfx.MeasureString(givn, this.Font);
                                DrawArcText(gfx, givn, this.fCenterX, this.fCenterY, rad - sizeF.Height / 2f,
                                            segment.StartAngle, segment.WedgeAngle, true, true, Font, this.fCircleBrushes[8]);
                            } else {
                                DrawArcText(gfx, givn, this.fCenterX, this.fCenterY, rad,
                                            segment.StartAngle, segment.WedgeAngle, true, true, Font, this.fCircleBrushes[8]);
                            }
                        } else {
                            float dx = (float)Math.Sin(PI * angle / 180.0) * rad;
                            float dy = (float)Math.Cos(PI * angle / 180.0) * rad;
                            gfx.ResetTransform();
                            gfx.TranslateTransform(this.fCenterX + dx, this.fCenterY - dy);
                            gfx.RotateTransform(angle);
                            SizeF sizeF2 = gfx.MeasureString(surn, this.Font);
                            gfx.DrawString(surn, this.Font, this.fCircleBrushes[8], -sizeF2.Width / 2f, -sizeF2.Height / 2f);

                            sizeF2 = gfx.MeasureString(givn, this.Font);
                            dx = (float)Math.Sin(PI * angle / 180.0) * (rad - sizeF2.Height);
                            dy = (float)Math.Cos(PI * angle / 180.0) * (rad - sizeF2.Height);
                            gfx.ResetTransform();
                            gfx.TranslateTransform(this.fCenterX + dx, this.fCenterY - dy);
                            gfx.RotateTransform(angle);
                            gfx.DrawString(givn, this.Font, this.fCircleBrushes[8], -sizeF2.Width / 2f, -sizeF2.Height / 2f);
                        }

                    } else if (wedgeAngle < 200) {

                        if (ArcText) {
                            SizeF sizeF = gfx.MeasureString(surn, this.Font);
                            DrawArcText(gfx, surn, this.fCenterX, this.fCenterY, rad + sizeF.Height / 2f,
                                        segment.StartAngle, segment.WedgeAngle, true, true, Font, this.fCircleBrushes[8]);

                            sizeF = gfx.MeasureString(givn, this.Font);
                            DrawArcText(gfx, givn, this.fCenterX, this.fCenterY, rad - sizeF.Height / 2f,
                                        segment.StartAngle, segment.WedgeAngle, true, true, Font, this.fCircleBrushes[8]);
                        } else {
                            float dx = (float)Math.Sin(PI * angle / 180.0) * rad;
                            float dy = (float)Math.Cos(PI * angle / 180.0) * rad;
                            gfx.ResetTransform();
                            gfx.TranslateTransform(this.fCenterX + dx, this.fCenterY - dy);
                            gfx.RotateTransform(angle);

                            SizeF sizeF = gfx.MeasureString(surn, this.Font);
                            gfx.DrawString(surn, this.Font, this.fCircleBrushes[8], -sizeF.Width / 2f, -sizeF.Height / 2f);
                            sizeF = gfx.MeasureString(givn, this.Font);
                            gfx.DrawString(givn, this.Font, this.fCircleBrushes[8], -sizeF.Width / 2f, -sizeF.Height / 2f + sizeF.Height);
                        }

                    }
                }
            }
        }

        private static bool IsNarrowSegment(Graphics gfx, string text, float radius, float wedgeAngle, Font font)
        {
            var size = gfx.MeasureString(text, font);
            radius = radius + size.Height / 2.0f;

            float wedgeL = radius * (float)GfxHelper.DegreesToRadians(wedgeAngle);

            return (wedgeL / size.Width <= 0.9f);
        }

        private static void DrawArcText(Graphics gfx, string text, float centerX, float centerY, float radius,
                                        float startAngle, float wedgeAngle,
                                        bool inside, bool clockwise, Font font, Brush brush)
        {
            var size = gfx.MeasureString(text, font);
            radius = radius + size.Height / 2.0f;

            float textAngle = (float)GfxHelper.RadiansToDegrees((size.Width * 1.75f) / radius);
            float deltaAngle = (wedgeAngle - textAngle) / 2.0f;

            startAngle -= 90.0f + deltaAngle;

            for (var i = 0; i < text.Length; ++i)
            {
                gfx.ResetTransform();

                float offset = (textAngle * ((i + 0.0f) / text.Length));
                offset = clockwise ? -offset : offset;
                float angle = startAngle + offset;

                double radAngle = angle * (Math.PI / 180.0d);
                float x = (float)(centerX + Math.Sin(radAngle) * radius);
                float y = (float)(centerY + Math.Cos(radAngle) * radius);
                gfx.TranslateTransform(x, y);

                double charRotation = inside ? -angle + 180 : -angle;
                gfx.RotateTransform((float)(charRotation));

                string chr = new String(text[i], 1);
                gfx.DrawString(chr, font, brush, 0, 0);
            }
        }

        #region Protected inherited methods

        protected override bool IsInputKey(Keys keyData)
        {
            switch (keyData) {
                case Keys.Add:
                case Keys.Subtract:
                case Keys.Left:
                case Keys.Right:
                case Keys.Back:
                    return true;
            }

            return base.IsInputKey(keyData);
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            this.InternalDraw(e.Graphics);
        }

        protected override void OnResize(EventArgs e)
        {
            base.OnResize(e);
            this.Changed();
        }

        protected override void OnMouseUp(MouseEventArgs e)
        {
            base.OnMouseMove(e);

            CircleSegment selected = this.FindSegment(e.X, e.Y);

            if (selected != null && selected.IRec != null) {
                this.RootPerson = selected.IRec;
            }
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            base.OnMouseMove(e);

            CircleSegment selected = this.FindSegment(e.X, e.Y);

            string hint = "";
            if (!Equals(this.fSelected, selected)) {
                this.fSelected = selected;

                if (selected != null && selected.IRec != null) {
                    string name = selected.IRec.GetNameString(true, false);
                    hint = /*selected.Gen.ToString() + ", " + */name;
                }

                this.Invalidate();
            }

            if (!Equals(this.fHint, hint)) {
                this.fHint = hint;

                if (!string.IsNullOrEmpty(hint)) {
                    fToolTip.Show(hint, this, e.X, e.Y, 3000);
                }
            }
        }

        #endregion
    }
}
