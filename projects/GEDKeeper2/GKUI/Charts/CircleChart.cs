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

//#define FUN_ANIM

using System;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Charts;
using GKCore.Interfaces;
using GKCore.Options;

namespace GKUI.Charts
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class CircleChart : CustomChart
    {
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

        private static readonly object EventRootChanged;

        private readonly IContainer fComponents;
        private readonly CircleChartModel fModel;
        private readonly ChartRenderer fRenderer;
        private readonly ToolTip fToolTip;

        private CircleChartType fChartType;
        private string fHint;
        private float fOffsetX = 0;
        private float fOffsetY = 0;
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


        public CircleChartType ChartType
        {
            get { return fChartType; }
            set { fChartType = value; }
        }

        public CircleChartModel Model
        {
            get { return fModel; }
        }

        public AncestorsCircleOptions Options
        {
            get { return fModel.Options; }
        }

        public event ARootChangedEventHandler RootChanged
        {
            add { Events.AddHandler(EventRootChanged, value); }
            remove { Events.RemoveHandler(EventRootChanged, value); }
        }

        public GEDCOMIndividualRecord RootPerson
        {
            get {
                return fModel.RootPerson;
            }
            set {
                if (fModel.RootPerson == value) return;
                fModel.RootPerson = value;

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

        public CircleChart(IBaseWindow baseWin)
        {
            DoubleBuffered = true;

            fRenderer = new TreeChartGfxRenderer();
            fModel = new CircleChartModel();
            fModel.SetRenderer(fRenderer);
            fModel.Options = new AncestorsCircleOptions();
            fModel.Base = baseWin;
            fModel.Font = this.Font;

            fComponents = new Container();
            fToolTip = new ToolTip(fComponents);
            fToolTip.AutoPopDelay = 5000;
            fToolTip.InitialDelay = 250;
            fToolTip.ReshowDelay = 50;
            fToolTip.ShowAlways = true;

            fMouseCaptured = MouseCaptured.mcNone;

            #if FUN_ANIM
            InitTimer();
            #endif

            BackColor = fModel.Options.BrushColor[9];
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fComponents != null) fComponents.Dispose();
                fModel.Dispose();
            }
            base.Dispose(disposing);
        }

        public void Changed()
        {
            fModel.CreateBrushes();

            switch (fChartType) {
                case CircleChartType.Ancestors:
                    fModel.BuildAncTree();
                    break;

                case CircleChartType.Descendants:
                    fModel.BuildDescTree();
                    break;
            }

            fModel.AdjustBounds();

            Size boundary = GetImageSize();
            AdjustViewPort(boundary, false);
        }

        private void DoRootChanged(GEDCOMIndividualRecord person)
        {
            var eventHandler = (ARootChangedEventHandler)Events[EventRootChanged];
            if (eventHandler != null)
                eventHandler(this, person);
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
            var bounds = fModel.Bounds;

            if (target == RenderTarget.rtScreen) {

                // Returns the center point of this chart relative to the upper left
                // point of this window's client area.
                // According to discussion at PR #99 (GitHub), this member centers this
                // chart on an axis when there's no scrolling required along that axis.
                // And if scrolling is required, this member aligns this chart on its
                // left edge.
                PointF center = new PointF();

                SizeF boundary = new SizeF(fModel.ImageWidth * fZoomX, fModel.ImageHeight * fZoomY);

                if (ClientSize.Width > boundary.Width) {
                    center.X = Math.Min(ClientSize.Width - bounds.Right * fZoomX, ClientSize.Width >> 1);
                }
                else {
                    center.X = AutoScrollPosition.X + fOffsetX - bounds.Left * fZoomX;
                }
                if (ClientSize.Height > boundary.Height) {
                    center.Y = Math.Min(ClientSize.Height - bounds.Bottom * fZoomY, ClientSize.Height >> 1);
                }
                else {
                    center.Y = AutoScrollPosition.Y + fOffsetY - bounds.Top * fZoomY;
                }
                return center;

            } else {

                // Returns the center point of this chart relative to the upper left
                // corner/point of printing canvas.
                return new PointF(AutoScrollPosition.X + fOffsetX - bounds.Left * fZoomX,
                                  AutoScrollPosition.Y + fOffsetY - bounds.Top * fZoomY);

            }
        }

        private CircleSegment FindSegment(float mX, float mY)
        {
            PointF center = GetCenter(RenderTarget.rtScreen);
            mX -= center.X;
            mY -= center.Y;
            CircleSegment result = null;

            int numberOfSegments = fModel.Segments.Count;
            for (int i = 0; i < numberOfSegments; i++) {
                CircleSegment segment = fModel.Segments[i];
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
            if (target == RenderTarget.rtScreen) {
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

            switch (fChartType) {
                case CircleChartType.Ancestors:
                    fModel.DrawAncestors(context);
                    break;

                case CircleChartType.Descendants:
                    fModel.DrawDescendants(context);
                    break;
            }

            context.ResetTransform();
        }

        #if FUN_ANIM
        private void InitTimer()
        {
            if ((fAppearingAnimationTimer == null) || !fAppearingAnimationTimer.Enabled) {
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

        protected override void SetNavObject(object obj)
        {
            RootPerson = obj as GEDCOMIndividualRecord;
        }

        protected override void OnDoubleClick(EventArgs e)
        {
            if (fChartType == CircleChartType.Ancestors) {
                fModel.GroupsMode = !fModel.GroupsMode;
            }

            base.OnDoubleClick(e);
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            Render(e.Graphics, RenderTarget.rtScreen);

            base.OnPaint(e);
        }

        protected override void OnResize(EventArgs e)
        {
            Changed();

            base.OnResize(e);
        }

        protected override void OnKeyDown(KeyEventArgs e)
        {
            switch (e.KeyCode) {
                case Keys.Add:
                case Keys.Oemplus:
                    if (Keys.None == ModifierKeys) {
                        fZoomX = Math.Min(fZoomX + fZoomX * 0.05f, fZoomHighLimit);
                        fZoomY = Math.Min(fZoomY + fZoomY * 0.05f, fZoomHighLimit);
                        Size boundary = GetImageSize();
                        AdjustViewPort(boundary, true);
                        Invalidate();
                    }
                    break;

                case Keys.Subtract:
                case Keys.OemMinus:
                    if (Keys.None == ModifierKeys) {
                        fZoomX = Math.Max(fZoomX - fZoomX * 0.05f, fZoomLowLimit);
                        fZoomY = Math.Max(fZoomY - fZoomY * 0.05f, fZoomLowLimit);
                        Size boundary = GetImageSize();
                        AdjustViewPort(boundary, true);
                        Invalidate();
                    }
                    break;

                case Keys.D0:
                    if (e.Control) {
                        fZoomX = 1.0f;
                        fZoomY = 1.0f;
                        Size boundary = GetImageSize();
                        AdjustViewPort(boundary, true);
                        Invalidate();
                    }
                    break;

                case Keys.Left:
                    if (fChartType == CircleChartType.Ancestors && fModel.RootPerson != null) {
                        GEDCOMIndividualRecord father, mother;
                        fModel.RootPerson.GetParents(out father, out mother);

                        if (father != null) {
                            RootPerson = father;
                        }
                    }
                    break;

                case Keys.Right:
                    if (fChartType == CircleChartType.Ancestors && fModel.RootPerson != null) {
                        GEDCOMIndividualRecord father, mother;
                        fModel.RootPerson.GetParents(out father, out mother);

                        if (mother != null) {
                            RootPerson = mother;
                        }
                    }
                    break;
            }

            base.OnKeyDown(e);
        }

        protected override void OnMouseDown(MouseEventArgs e)
        {
            if ((e.Button == MouseButtons.Right) && (HorizontalScroll.Visible || VerticalScroll.Visible)) {
                fMouseCaptured = MouseCaptured.mcDrag;
                fMouseCaptureX = e.X;
                fMouseCaptureY = e.Y;
                Cursor = Cursors.SizeAll;
            }

            base.OnMouseDown(e);
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
            }

            base.OnMouseUp(e);
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            switch (fMouseCaptured) {
                case MouseCaptured.mcNone:
                    {
                        CircleSegment selected = FindSegment(e.X, e.Y);

                        string hint = "";
                        if (!Equals(fModel.Selected, selected)) {
                            fModel.Selected = selected;

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
                Size boundary = GetImageSize();
                AdjustViewPort(boundary, true);
                Invalidate();
            }

            base.OnMouseWheel(e);
        }

        #endregion

        /// <summary>
        /// Gets boundary of the area that includes all paths of this chart.
        /// This member does not recalculates boundaries -- it uses calculation
        /// made by member `Changed`.
        /// Result includes width of a path's borders.
        /// </summary>
        /// <returns>The paths boundary.</returns>
        public override Size GetImageSize()
        {
            return new Size((int)(fModel.ImageWidth * fZoomX), (int)(fModel.ImageHeight * fZoomY));
        }

        public override void RenderStaticImage(Graphics gfx, bool printer)
        {
            Render(gfx, RenderTarget.rtNonScreenCanvas);
        }
    }
}
