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

// #define FUN_ANIM

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
        public const int CENTRAL_INDEX = 9;

        protected class CircleSegment : BaseObject
        {
            public int Gen;
            public GEDCOMIndividualRecord IRec;
            public GraphicsPath Path;
            public float Rad;
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

        private enum RenderTarget
        {
            rtScreen,
            rtNonScreenCanvas
        }

        private enum MouseCaptured
        {
            mcNone,
            mcDrag
        }

        protected static readonly object EventRootChanged;

        protected const float CENTER_RAD = 90;
        protected const float DEFAULT_GEN_WIDTH = 60;

        protected readonly SolidBrush[] fCircleBrushes;
        protected readonly SolidBrush[] fDarkBrushes;
        protected readonly AncestorsCircleOptions fOptions;
        protected readonly List<CircleSegment> fSegments;

        private readonly IContainer fComponents;
        private readonly ToolTip fToolTip;

        /* This chart's GDI+ paths boundary (in the following order: left, top,
         * right and bottom). */
        private float[] fBounds;
        protected float fGenWidth;
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
        protected float fOffsetX = 0;
        protected float fOffsetY = 0;
        protected Pen fPen;
        protected GEDCOMIndividualRecord fRootPerson;
        protected CircleSegment fSelected;
        protected ShieldState fShieldState;
        /* Zoom factors */
        private float fZoomX = 1.0f;
        private float fZoomY = 1.0f;
        private float fZoomLowLimit = 0.0125f;
        private float fZoomHighLimit = 1000.0f;
        /* Mouse capturing. */
        private MouseCaptured fMouseCaptured;
        private int fMouseCaptureX;
        private int fMouseCaptureY;
        /* Animation timer. */
        #if FUN_ANIM
        private Timer fAppearingAnimationTimer;
        private UInt64 fAppearingAnimationTime = 0;
        private const UInt64 fAppearingAnimationTimeLimit = 17;
        #endif


        public float GenWidth
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
            add { Events.AddHandler(EventRootChanged, value); }
            remove { Events.RemoveHandler(EventRootChanged, value); }
        }

        public GEDCOMIndividualRecord RootPerson
        {
            get {
                return fRootPerson;
            }
            set {
                if (fRootPerson == value) return;
                fRootPerson = value;

                NavAdd(value);
                Changed();

                DoRootChanged(value);

                #if FUN_ANIM
                InitTimer();
                #endif
            }
        }


        static CircleChart()
        {
            EventRootChanged = new object();
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
            fBounds = new float[4];
            fMouseCaptured = MouseCaptured.mcNone;

            #if FUN_ANIM
            InitTimer();
            #endif

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

            /* Update scrolling area. */
            fBounds[0] = 0.0f;
            fBounds[1] = 0.0f;
            fBounds[2] = 0.0f;
            fBounds[3] = 0.0f;
            foreach (var segment in fSegments) {
                RectangleF bound = segment.Path.GetBounds();
                fBounds[0] = Math.Min(fBounds[0], bound.Left);
                fBounds[1] = Math.Min(fBounds[1], bound.Top);
                fBounds[2] = Math.Max(fBounds[2], bound.Right);
                fBounds[3] = Math.Max(fBounds[3], bound.Bottom);
            }

            Size boundary = GetPathsBoundaryI();
            AdjustViewPort(boundary, false);
        }

        protected void DoRootChanged(GEDCOMIndividualRecord person)
        {
            var eventHandler = (ARootChangedEventHandler)Events[EventRootChanged];
            if (eventHandler == null) return;

            eventHandler(this, person);
        }

        /// <summary>
        /// Gets boundary of the area that includes all paths of this chart.
        /// This member does not recalculates boundaries -- it uses calculation
        /// made by member `Changed`.
        /// Result includes width of a path's borders.
        /// </summary>
        /// <returns>The paths boundary.</returns>
        private SizeF GetPathsBoundaryF()
        {
            /* Add double width of the pen -- adjust both sides. */
            return new SizeF((fBounds[2] - fBounds[0] + (fPen.Width * 2)) * fZoomX,
                             (fBounds[3] - fBounds[1] + (fPen.Width * 2)) * fZoomY);
        }
        private Size GetPathsBoundaryI()
        {
            /* Add double width of the pen -- adjust both sides. */
            return new Size((int)((fBounds[2] - fBounds[0] + (fPen.Width * 2)) * fZoomX),
                            (int)((fBounds[3] - fBounds[1] + (fPen.Width * 2)) * fZoomY));
        }

        /// <summary>
        /// Returns a point that must be the center of this chart after it will
        /// be rendered on a target.
        /// </summary>
        /// <param name="target">Rendering target for which the client requires
        /// the center point.</param>
        /// <returns>Center point of this chart.</returns>
        private PointF GetCenter(RenderTarget target)
        {
            if (target == RenderTarget.rtScreen) {

                // Returns the center point of this chart relative to the upper left
                // point of this window's client area.
                // According to discussion at PR #99 (GitHub), this member centers this
                // chart on an axis when there's no scrolling required along that axis.
                // And if scrolling is required, this member aligns this chart on its
                // left edge.
                PointF center = new PointF();
                SizeF boundary = GetPathsBoundaryF();
                if (ClientSize.Width > boundary.Width) {
                    center.X = Math.Min(ClientSize.Width - fBounds[2] * fZoomX, ClientSize.Width >> 1);
                }
                else {
                    center.X = AutoScrollPosition.X + fOffsetX - fBounds[0] * fZoomX;
                }
                if (ClientSize.Height > boundary.Height) {
                    center.Y = Math.Min(ClientSize.Height - fBounds[3] * fZoomY, ClientSize.Height >> 1);
                }
                else {
                    center.Y = AutoScrollPosition.Y + fOffsetY - fBounds[1] * fZoomY;
                }
                return center;

            } else {

                // Returns the center point of this chart relative to the upper left
                // corner/point of printing canvas.
                return new PointF(AutoScrollPosition.X + fOffsetX - fBounds[0] * fZoomX,
                                  AutoScrollPosition.Y + fOffsetY - fBounds[1] * fZoomY);

            }
        }

        protected CircleSegment FindSegmentByRec(GEDCOMIndividualRecord iRec)
        {
            CircleSegment result = null;

            int numberOfSegments = fSegments.Count;
            for (int i = 0; i < numberOfSegments; i++) {
                CircleSegment segment = fSegments[i];

                if (segment.IRec == iRec) {
                    result = segment;
                    break;
                }
            }

            return result;
        }

        protected CircleSegment FindSegment(float mX, float mY)
        {
            PointF center = GetCenter(RenderTarget.rtScreen);
            mX -= center.X;
            mY -= center.Y;
            CircleSegment result = null;

            int numberOfSegments = fSegments.Count;
            for (int i = 0; i < numberOfSegments; i++) {
                CircleSegment segment = fSegments[i];
                /* Unfortunatelly, member `GraphicsPath.IsVisible(REAL, REAL,
                 * const Graphics*)` doesn't work for me. */
                if (segment.Path.IsVisible(mX / fZoomX, mY / fZoomY)) {
                    result = segment;
                    break;
                }
            }

            return result;
        }

        /// <summary>
        /// Renders this chart on the specified target and the context
        /// associated with the target.
        /// </summary>
        /// <param name="context">GDI+ rendering context.</param>
        /// <param name="target">Rendering target.</param>
        private void Render(Graphics context, RenderTarget target)
        {
            PointF center = GetCenter(target);
            if (RenderTarget.rtScreen == target) {
                if ((1.25f < fZoomX) || (1.25f < fZoomY)) {
                    context.TextRenderingHint = System.Drawing.Text.TextRenderingHint.AntiAlias;
                }
                #if !FUN_ANIM
                context.Transform = new Matrix(fZoomX, 0, 0, fZoomY, center.X, center.Y);
                #else
                float angle = (float)(3.5f * Math.Sin(fAppearingAnimationTime) *
                                      Math.Exp(-1.0 * fAppearingAnimationTime / fAppearingAnimationTimeLimit));
                float rotation = (float)(angle * Math.PI / 180.0f);
                float cosine = (float)(Math.Cos(rotation));
                float sine = (float)(Math.Sin(rotation));
                Matrix m = new Matrix(cosine, sine, -sine, cosine, center.X, center.Y);
                float zoomX = fZoomX + ((0 != fAppearingAnimationTime) ?
                                        (float)(Math.Exp(-1.0 * (fAppearingAnimationTime + 50.0f) / fAppearingAnimationTimeLimit)) : 0);
                float zoomY = fZoomY - ((0 != fAppearingAnimationTime) ?
                                        (float)(Math.Exp(-1.0 * (fAppearingAnimationTime + 50.0f) / fAppearingAnimationTimeLimit)) : 0);
                m.Scale(zoomX, zoomY);
                context.Transform = m;
                #endif
            } else {
                context.Transform = new Matrix(1, 0, 0, 1, center.X, center.Y);
            }
            context.SmoothingMode = SmoothingMode.AntiAlias;
            InternalDraw(context);
            context.ResetTransform();
        }

        /// <summary>
        /// Renders a specified <paramref name="segment"/>'s person name within
        /// the segment.
        /// </summary>
        /// <param name="gfx">GDI+ context to render on. This member may change
        /// this context's transformation. If it does, it also reverts the
        /// transformation back. Thus, from the point of view of the client code
        /// this member doesn't change the context's transformation.</param>
        /// <param name="segment">Source segment to be drawn on `gfx`.</param>
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

            float rad = segment.Rad - 20;
            float angle = segment.StartAngle + 90.0f + segment.WedgeAngle / 2;
            float wedgeAngle = segment.WedgeAngle;

            bool isNarrow = IsNarrowSegment(gfx, givn, rad, wedgeAngle, Font);
            Matrix previousTransformation = gfx.Transform;
            if (gen == 0) {

                SizeF size = gfx.MeasureString(surn, Font);
                gfx.DrawString(surn, Font, fCircleBrushes[8], -size.Width / 2f, -size.Height / 2f - size.Height / 2f);
                size = gfx.MeasureString(givn, Font);
                gfx.DrawString(givn, Font, fCircleBrushes[8], -size.Width / 2f, 0f);

            } else {

                if (isNarrow) {

                    float dx = (float)Math.Sin(Math.PI * angle / 180.0f) * rad;
                    float dy = (float)Math.Cos(Math.PI * angle / 180.0f) * rad;
                    float rotation = (float)((angle - 90.0f) * Math.PI / 180.0f);
                    float cosine = (float)(Math.Cos(rotation));
                    float sine = (float)(Math.Sin(rotation));
                    Matrix m = new Matrix(cosine, sine, -sine, cosine, dx, -dy);
                    m.Multiply(previousTransformation, MatrixOrder.Append);
                    gfx.Transform = m;

                    SizeF size = gfx.MeasureString(givn, Font);
                    gfx.DrawString(givn, Font, fCircleBrushes[8], -size.Width / 2f, -size.Height / 2f);

                } else {

                    if (wedgeAngle < 20) {

                        float dx = (float)Math.Sin(Math.PI * angle / 180.0f) * rad;
                        float dy = (float)Math.Cos(Math.PI * angle / 180.0f) * rad;
                        float rotation = (float)(angle * Math.PI / 180.0f);
                        float cosine = (float)(Math.Cos(rotation));
                        float sine = (float)(Math.Sin(rotation));
                        Matrix m = new Matrix(cosine, sine, -sine, cosine, dx, -dy);
                        m.Multiply(previousTransformation, MatrixOrder.Append);
                        gfx.Transform = m;

                        SizeF size = gfx.MeasureString(givn, Font);
                        gfx.DrawString(givn, Font, fCircleBrushes[8], -size.Width / 2f, -size.Height / 2f);

                    } else if (wedgeAngle < 180) {

                        if (fOptions.ArcText) {
                            if (gen == 2) {
                                SizeF size = gfx.MeasureString(surn, Font);
                                DrawArcText(gfx, surn, 0.0f, 0.0f, rad + size.Height / 2f,
                                            segment.StartAngle, segment.WedgeAngle, true, true, Font, fCircleBrushes[8]);

                                size = gfx.MeasureString(givn, Font);
                                DrawArcText(gfx, givn, 0.0f, 0.0f, rad - size.Height / 2f,
                                            segment.StartAngle, segment.WedgeAngle, true, true, Font, fCircleBrushes[8]);
                            } else {
                                DrawArcText(gfx, givn, 0.0f, 0.0f, rad,
                                            segment.StartAngle, segment.WedgeAngle, true, true, Font, fCircleBrushes[8]);
                            }
                        } else {
                            float dx = (float)Math.Sin(Math.PI * angle / 180.0f) * rad;
                            float dy = (float)Math.Cos(Math.PI * angle / 180.0f) * rad;
                            /* Change `gfx`'s transformation via direct matrix
                             * processing, not with its member functions because
                             * we are about to change the transformation several
                             * times (thus, we are avoiding transformation
                             * reseting on `gfx`). */
                            float rotation = (float)(angle * Math.PI / 180.0f);
                            float cosine = (float)(Math.Cos(rotation));
                            float sine = (float)(Math.Sin(rotation));
                            Matrix m = new Matrix(cosine, sine, -sine, cosine, dx, -dy);
                            m.Multiply(previousTransformation, MatrixOrder.Append);
                            gfx.Transform = m;
                            SizeF size = gfx.MeasureString(surn, Font);
                            gfx.DrawString(surn, Font, fCircleBrushes[8], -size.Width / 2f, -size.Height / 2f);

                            size = gfx.MeasureString(givn, Font);
                            dx = (float)Math.Sin(Math.PI * angle / 180.0f) * (rad - size.Height);
                            dy = (float)Math.Cos(Math.PI * angle / 180.0f) * (rad - size.Height);
                            m = new Matrix(cosine, sine, -sine, cosine, dx, -dy);
                            m.Multiply(previousTransformation, MatrixOrder.Append);
                            gfx.Transform = m;
                            gfx.DrawString(givn, Font, fCircleBrushes[8], -size.Width / 2f, -size.Height / 2f);
                        }

                    } else if (wedgeAngle < 361) {

                        if (fOptions.ArcText) {
                            SizeF size = gfx.MeasureString(surn, Font);
                            DrawArcText(gfx, surn, 0.0f, 0.0f, rad + size.Height / 2f,
                                        segment.StartAngle, segment.WedgeAngle, true, true, Font, fCircleBrushes[8]);

                            size = gfx.MeasureString(givn, Font);
                            DrawArcText(gfx, givn, 0.0f, 0.0f, rad - size.Height / 2f,
                                        segment.StartAngle, segment.WedgeAngle, true, true, Font, fCircleBrushes[8]);
                        } else {
                            float dx = (float)Math.Sin(Math.PI * angle / 180.0f) * rad;
                            float dy = (float)Math.Cos(Math.PI * angle / 180.0f) * rad;
                            float rotation = (float)(angle * Math.PI / 180.0f);
                            float cosine = (float)(Math.Cos(rotation));
                            float sine = (float)(Math.Sin(rotation));
                            Matrix m = new Matrix(cosine, sine, -sine, cosine, dx, -dy);
                            m.Multiply(previousTransformation, MatrixOrder.Append);
                            gfx.Transform = m;

                            SizeF size = gfx.MeasureString(surn, Font);
                            gfx.DrawString(surn, Font, fCircleBrushes[8], -size.Width / 2f, -size.Height / 2f);
                            size = gfx.MeasureString(givn, Font);
                            gfx.DrawString(givn, Font, fCircleBrushes[8], -size.Width / 2f, -size.Height / 2f + size.Height);
                        }

                    }
                }
            }
            gfx.Transform = previousTransformation;
        }

        private static bool IsNarrowSegment(Graphics gfx, string text, float radius, float wedgeAngle, Font font)
        {
            SizeF size = gfx.MeasureString(text, font);
            radius = radius + size.Height / 2.0f;

            float wedgeL = radius * (float)SysUtils.DegreesToRadians(wedgeAngle);

            return (wedgeL / size.Width <= 0.9f);
        }

        private static void DrawArcText(Graphics gfx, string text, float centerX, float centerY, float radius,
                                        float startAngle, float wedgeAngle,
                                        bool inside, bool clockwise, Font font, Brush brush)
        {
            SizeF size = gfx.MeasureString(text, font);
            radius = radius + size.Height / 2.0f;

            float textAngle = Math.Min((float)SysUtils.RadiansToDegrees((size.Width * 1.75f) / radius), wedgeAngle);
            float deltaAngle = (wedgeAngle - textAngle) / 2.0f;

            if (clockwise) {
                startAngle += deltaAngle;
            } else {
                startAngle += wedgeAngle - deltaAngle;
            }
            startAngle = -startAngle;

            Matrix previousTransformation = gfx.Transform;
            for (int i = 0; i < text.Length; ++i)
            {
                float offset = (textAngle * ((float)(i) / text.Length));
                float angle = clockwise ? startAngle - offset : startAngle + offset;

                double radAngle = angle * (Math.PI / 180.0d);
                float x = (float)(centerX + Math.Cos(radAngle) * radius);
                float y = (float)(centerY - Math.Sin(radAngle) * radius);
                float charRotation = 90 - (inside ? angle : angle + 180);
                charRotation *= (float)(Math.PI / 180.0f);
                float cosine = (float)(Math.Cos(charRotation));
                float sine = (float)(Math.Sin(charRotation));
                /* Translate and rotate. */
                Matrix m = new Matrix(cosine, sine, -sine, cosine, x, y);
                m.Multiply(previousTransformation, MatrixOrder.Append);
                gfx.Transform = m;

                string chr = new string(text[i], 1);
                gfx.DrawString(chr, font, brush, 0, 0);
            }
            gfx.Transform = previousTransformation;
        }

        #if FUN_ANIM
        private void InitTimer()
        {
            if ((null == fAppearingAnimationTimer) || !fAppearingAnimationTimer.Enabled) {
                fAppearingAnimationTime = 0;
                fAppearingAnimationTimer = new Timer();
                fAppearingAnimationTimer.Interval = 1;
                fAppearingAnimationTimer.Tick += AnimationTimerTick;
                fAppearingAnimationTimer.Start();
            }
        }

        private void AnimationTimerTick(object sender, EventArgs e)
        {
            ++fAppearingAnimationTime;
            if (fAppearingAnimationTimeLimit < fAppearingAnimationTime) {
                fAppearingAnimationTimer.Stop();
                fAppearingAnimationTime = 0;
            }
            Invalidate();
        }
        #endif

        #region Protected inherited methods

        protected override void OnPaint(PaintEventArgs e)
        {
            Render(e.Graphics, RenderTarget.rtScreen);
            base.OnPaint(e);
        }

        protected override void OnResize(EventArgs e)
        {
            base.OnResize(e);

            Changed();
        }

        protected override void OnKeyDown(KeyEventArgs e)
        {
            switch (e.KeyCode) {
                case Keys.Add:
                case Keys.Oemplus:
                    if (Keys.None == ModifierKeys) {
                        fZoomX = Math.Min(fZoomX + fZoomX * 0.05f, fZoomHighLimit);
                        fZoomY = Math.Min(fZoomY + fZoomY * 0.05f, fZoomHighLimit);
                        Size boundary = GetPathsBoundaryI();
                        AdjustViewPort(boundary, true);
                        Invalidate();
                    }
                    break;

                case Keys.Subtract:
                case Keys.OemMinus:
                    if (Keys.None == ModifierKeys) {
                        fZoomX = Math.Max(fZoomX - fZoomX * 0.05f, fZoomLowLimit);
                        fZoomY = Math.Max(fZoomY - fZoomY * 0.05f, fZoomLowLimit);
                        Size boundary = GetPathsBoundaryI();
                        AdjustViewPort(boundary, true);
                        Invalidate();
                    }
                    break;

                case Keys.D0:
                    if (e.Control) {
                        fZoomX = 1.0f;
                        fZoomY = 1.0f;
                        Size boundary = GetPathsBoundaryI();
                        AdjustViewPort(boundary, true);
                        Invalidate();
                    }
                    break;

                default:
                    base.OnKeyDown(e);
                    break;
            }
        }

        protected override void OnMouseDown(MouseEventArgs e)
        {
            if ((e.Button == MouseButtons.Right) && (HorizontalScroll.Visible || VerticalScroll.Visible)) {
                fMouseCaptured = MouseCaptured.mcDrag;
                fMouseCaptureX = e.X;
                fMouseCaptureY = e.Y;
                Cursor = Cursors.SizeAll;
            } else {
                base.OnMouseDown(e);
            }
        }

        protected override void OnMouseUp(MouseEventArgs e)
        {
            if (fMouseCaptured == MouseCaptured.mcDrag) {
                fMouseCaptured = MouseCaptured.mcNone;
                Cursor = Cursors.Default;
            }
            else if (e.Button == MouseButtons.Left) {
                CircleSegment selected = FindSegment(e.X, e.Y);
                if (selected != null && selected.IRec != null) {
                    RootPerson = selected.IRec;
                }
            } else {
                base.OnMouseUp(e);
            }
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            switch (fMouseCaptured) {
                case MouseCaptured.mcNone:
                    {
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
                    break;

                case MouseCaptured.mcDrag:
                    {
                        AdjustScroll(-(e.X - fMouseCaptureX), -(e.Y - fMouseCaptureY));
                        fMouseCaptureX = e.X;
                        fMouseCaptureY = e.Y;
                    }
                    break;
            }

            base.OnMouseMove(e);
        }

        protected override void OnMouseWheel(MouseEventArgs e)
        {
            if (Keys.None != (Keys.Control & ModifierKeys)) {
                if (0 > e.Delta) {
                    fZoomX = Math.Max(fZoomX - fZoomX * 0.05f, fZoomLowLimit);
                    fZoomY = Math.Max(fZoomY - fZoomY * 0.05f, fZoomLowLimit);
                } else {
                    fZoomX = Math.Min(fZoomX + fZoomX * 0.05f, fZoomHighLimit);
                    fZoomY = Math.Min(fZoomY + fZoomY * 0.05f, fZoomHighLimit);
                }
                Size boundary = GetPathsBoundaryI();
                AdjustViewPort(boundary, true);
                Invalidate();
            }
            else {
                base.OnMouseWheel(e);
            }
        }

        #endregion

        /* TODO(zsv): Temporary implementation. Refactoring this hierarchy later. */
        public override Size GetImageSize()
        {
            return GetPathsBoundaryI();
        }

        public override void RenderStaticImage(Graphics gfx, bool printer)
        {
            Render(gfx, RenderTarget.rtNonScreenCanvas);
        }
    }
}
