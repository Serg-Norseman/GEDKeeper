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
                Gen = generation;
                IRec = null;
                Path = new GraphicsPath();
            }

            protected override void Dispose(bool disposing)
            {
                if (disposing)
                {
                    if (Path != null) Path.Dispose();
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
        /* TODO(brigadir15@gmail.com): Member `fMaxGenerations` should be a
         * const field, I believe. Member `CircleSegment::Gen` is used as an
         * index when accessing the array `fCircleBrushes`, for example. The
         * latter contains **predefined** brush list, with first eight elements
         * used as brushes for eight possible generations (`CircleChart` ctor
         * assigns `fMaxGenerations` with `8`; see code below).
         * Thus, if one will want to extent number of available visible
         * generation, he/she will have to change a huge amount of the code.
         * Therefore, to avoid someone's tendency to change initial values of
         * member `fMaxGenerations`, the member should be a const field. */
        protected int fMaxGenerations;
        protected int fOffsetX = 0;
        protected int fOffsetY = 0;
        protected Pen fPen;
        protected GEDCOMIndividualRecord fRootPerson;
        protected CircleSegment fSelected;
        protected ShieldState fShieldState;

        private bool ArcText = false;


        public int GenWidth
        {
            get {
                return fGenWidth;
            }
            set {
                if (value < 20 || value > 100) return;

                fGenWidth = value;
                Changed();
            }
        }

        public int IndividualsCount
        {
            get { return fIndividualsCount; }
        }

        public AncestorsCircleOptions Options
        {
            get { return fOptions; }
        }

        public event ARootChangedEventHandler RootChanged
        {
            add { base.Events.AddHandler(CircleChart.EventRootChanged, value); }
            remove { base.Events.RemoveHandler(CircleChart.EventRootChanged, value); }
        }

        public GEDCOMIndividualRecord RootPerson
        {
            get {
                return fRootPerson;
            }
            set {
                fRootPerson = value;

                NavAdd(value);
                Changed();

                DoRootChanged(value);
            }
        }


        static CircleChart()
        {
            CircleChart.EventRootChanged = new object();
        }

        protected CircleChart(IBaseWindow baseWin)
        {
            DoubleBuffered = true;

            fComponents = new Container();
            fToolTip = new ToolTip(fComponents);
            fToolTip.AutoPopDelay = 5000;
            fToolTip.InitialDelay = 250;
            fToolTip.ReshowDelay = 50;
            fToolTip.ShowAlways = true;

            fCircleBrushes = new SolidBrush[AncestorsCircleOptions.MAX_BRUSHES];
            fDarkBrushes = new SolidBrush[AncestorsCircleOptions.MAX_BRUSHES];
            fGenWidth = DEFAULT_GEN_WIDTH;
            fMaxGenerations = 8;
            fOptions = new AncestorsCircleOptions();
            fSegments = new List<CircleSegment>();
            fSelected = null;
            fShieldState = baseWin.ShieldState;

            BackColor = fOptions.BrushColor[9];
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fComponents != null) fComponents.Dispose();
                DisposeBrushes();
            }
            base.Dispose(disposing);
        }

        protected override void SetNavObject(object obj)
        {
            RootPerson = obj as GEDCOMIndividualRecord;
        }

        #region Abstract methods

        protected abstract void BuildPathTree();

        protected abstract void InternalDraw(Graphics gfx);

        #endregion

        private void CreateBrushes()
        {
            for (int i = 0; i < fOptions.BrushColor.Length; i++)
            {
                Color col = fOptions.BrushColor[i];

                fCircleBrushes[i] = new SolidBrush(col);
                fDarkBrushes[i] = new SolidBrush(SysUtils.Darker(col, 0.2f));
            }

            fPen = new Pen(fOptions.BrushColor[10]);
        }

        private void DisposeBrushes()
        {
            for (int i = 0; i < fOptions.BrushColor.Length; i++)
            {
                if (fCircleBrushes[i] != null) fCircleBrushes[i].Dispose();
                if (fDarkBrushes[i] != null) fDarkBrushes[i].Dispose();
            }

            if (fPen != null) fPen.Dispose();
        }

        public void Changed()
        {
            CreateBrushes();
            BuildPathTree();
            Invalidate();
        }

        protected void DoRootChanged(GEDCOMIndividualRecord person)
        {
            var eventHandler = (ARootChangedEventHandler)base.Events[CircleChart.EventRootChanged];
            if (eventHandler == null) return;

            eventHandler(this, person);
        }

        protected CircleSegment FindSegmentByRec(GEDCOMIndividualRecord iRec)
        {
            CircleSegment result = null;

            int num = fSegments.Count;
            for (int i = 0; i < num; i++) {
                CircleSegment segment = fSegments[i];

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

            int num = fSegments.Count;
            for (int i = 0; i < num; i++) {
                CircleSegment segment = fSegments[i];

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

            bool isNarrow = IsNarrowSegment(gfx, givn, rad, wedgeAngle, Font);

            if (gen == 0) {

                gfx.ResetTransform();
                gfx.TranslateTransform(fCenterX, fCenterY);

                SizeF sizeF = gfx.MeasureString(surn, Font);
                gfx.DrawString(surn, Font, fCircleBrushes[8], -sizeF.Width / 2f, -sizeF.Height / 2f - sizeF.Height / 2f);
                sizeF = gfx.MeasureString(givn, Font);
                gfx.DrawString(givn, Font, fCircleBrushes[8], -sizeF.Width / 2f, 0f);

            } else {

                if (isNarrow) {

                    float dx = (float)Math.Sin(PI * angle / 180.0) * rad;
                    float dy = (float)Math.Cos(PI * angle / 180.0) * rad;
                    gfx.ResetTransform();
                    gfx.TranslateTransform(fCenterX + dx, fCenterY - dy);
                    gfx.RotateTransform(angle - 90.0f);

                    SizeF size = gfx.MeasureString(givn, Font);
                    gfx.DrawString(givn, Font, fCircleBrushes[8], -size.Width / 2f, -size.Height / 2f);

                } else {

                    if (wedgeAngle < 20) {

                        float dx = (float)Math.Sin(PI * angle / 180.0) * rad;
                        float dy = (float)Math.Cos(PI * angle / 180.0) * rad;
                        gfx.ResetTransform();
                        gfx.TranslateTransform(fCenterX + dx, fCenterY - dy);
                        gfx.RotateTransform(angle);

                        SizeF size = gfx.MeasureString(givn, Font);
                        gfx.DrawString(givn, Font, fCircleBrushes[8], -size.Width / 2f, -size.Height / 2f);

                    } else if (wedgeAngle < 180) {

                        if (ArcText) {
                            if (gen == 2) {
                                SizeF sizeF = gfx.MeasureString(surn, Font);
                                DrawArcText(gfx, surn, fCenterX, fCenterY, rad + sizeF.Height / 2f,
                                            segment.StartAngle, segment.WedgeAngle, true, true, Font, fCircleBrushes[8]);

                                sizeF = gfx.MeasureString(givn, Font);
                                DrawArcText(gfx, givn, fCenterX, fCenterY, rad - sizeF.Height / 2f,
                                            segment.StartAngle, segment.WedgeAngle, true, true, Font, fCircleBrushes[8]);
                            } else {
                                DrawArcText(gfx, givn, fCenterX, fCenterY, rad,
                                            segment.StartAngle, segment.WedgeAngle, true, true, Font, fCircleBrushes[8]);
                            }
                        } else {
                            float dx = (float)Math.Sin(PI * angle / 180.0) * rad;
                            float dy = (float)Math.Cos(PI * angle / 180.0) * rad;
                            gfx.ResetTransform();
                            gfx.TranslateTransform(fCenterX + dx, fCenterY - dy);
                            gfx.RotateTransform(angle);
                            SizeF sizeF2 = gfx.MeasureString(surn, Font);
                            gfx.DrawString(surn, Font, fCircleBrushes[8], -sizeF2.Width / 2f, -sizeF2.Height / 2f);

                            sizeF2 = gfx.MeasureString(givn, Font);
                            dx = (float)Math.Sin(PI * angle / 180.0) * (rad - sizeF2.Height);
                            dy = (float)Math.Cos(PI * angle / 180.0) * (rad - sizeF2.Height);
                            gfx.ResetTransform();
                            gfx.TranslateTransform(fCenterX + dx, fCenterY - dy);
                            gfx.RotateTransform(angle);
                            gfx.DrawString(givn, Font, fCircleBrushes[8], -sizeF2.Width / 2f, -sizeF2.Height / 2f);
                        }

                    } else if (wedgeAngle < 200) {

                        if (ArcText) {
                            SizeF sizeF = gfx.MeasureString(surn, Font);
                            DrawArcText(gfx, surn, fCenterX, fCenterY, rad + sizeF.Height / 2f,
                                        segment.StartAngle, segment.WedgeAngle, true, true, Font, fCircleBrushes[8]);

                            sizeF = gfx.MeasureString(givn, Font);
                            DrawArcText(gfx, givn, fCenterX, fCenterY, rad - sizeF.Height / 2f,
                                        segment.StartAngle, segment.WedgeAngle, true, true, Font, fCircleBrushes[8]);
                        } else {
                            float dx = (float)Math.Sin(PI * angle / 180.0) * rad;
                            float dy = (float)Math.Cos(PI * angle / 180.0) * rad;
                            gfx.ResetTransform();
                            gfx.TranslateTransform(fCenterX + dx, fCenterY - dy);
                            gfx.RotateTransform(angle);

                            SizeF sizeF = gfx.MeasureString(surn, Font);
                            gfx.DrawString(surn, Font, fCircleBrushes[8], -sizeF.Width / 2f, -sizeF.Height / 2f);
                            sizeF = gfx.MeasureString(givn, Font);
                            gfx.DrawString(givn, Font, fCircleBrushes[8], -sizeF.Width / 2f, -sizeF.Height / 2f + sizeF.Height);
                        }

                    }
                }
            }
        }

        private static bool IsNarrowSegment(Graphics gfx, string text, float radius, float wedgeAngle, Font font)
        {
            var size = gfx.MeasureString(text, font);
            radius = radius + size.Height / 2.0f;

            float wedgeL = radius * (float)SysUtils.DegreesToRadians(wedgeAngle);

            return (wedgeL / size.Width <= 0.9f);
        }

        private static void DrawArcText(Graphics gfx, string text, float centerX, float centerY, float radius,
                                        float startAngle, float wedgeAngle,
                                        bool inside, bool clockwise, Font font, Brush brush)
        {
            var size = gfx.MeasureString(text, font);
            radius = radius + size.Height / 2.0f;

            float textAngle = (float)SysUtils.RadiansToDegrees((size.Width * 1.75f) / radius);
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
            InternalDraw(e.Graphics);

            base.OnPaint(e);
        }

        protected override void OnResize(EventArgs e)
        {
            base.OnResize(e);

            Changed();
        }

        protected override void OnMouseUp(MouseEventArgs e)
        {
            base.OnMouseMove(e);

            CircleSegment selected = FindSegment(e.X, e.Y);

            if (selected != null && selected.IRec != null) {
                RootPerson = selected.IRec;
            }
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            base.OnMouseMove(e);

            CircleSegment selected = FindSegment(e.X, e.Y);

            string hint = "";
            if (!Equals(fSelected, selected)) {
                fSelected = selected;

                if (selected != null && selected.IRec != null) {
                    string name = GKUtils.GetNameString(selected.IRec, true, false);
                    hint = /*selected.Gen.ToString() + ", " + */name;
                }

                Invalidate();
            }

            if (!Equals(fHint, hint)) {
                fHint = hint;

                if (!string.IsNullOrEmpty(hint)) {
                    fToolTip.Show(hint, this, e.X, e.Y, 3000);
                }
            }
        }

        #endregion
    }
}
