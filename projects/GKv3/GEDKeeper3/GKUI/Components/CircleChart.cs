/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using BSLib;
using Eto.Drawing;
using Eto.Forms;
using GDModel;
using GKCore;
using GKCore.Charts;
using GKCore.Interfaces;
using GKCore.Options;
using GKUI.Platform;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class CircleChart : CustomChart, ICircleChart
    {
        private enum MouseCaptured
        {
            mcNone,
            mcDrag
        }

        private const float ZOOM_LOW_LIMIT = 0.0125f;
        private const float ZOOM_HIGH_LIMIT = 1000.0f;

        private readonly CircleChartModel fModel;

        private CircleChartType fChartType;
        private string fHint;
        //private float fOffsetX;
        //private float fOffsetY;
        /* Zoom factors */
        private float fZoom = 1.0f;
        /* Mouse capturing. */
        private MouseCaptured fMouseCaptured;
        private int fMouseCaptureX;
        private int fMouseCaptureY;


        public IBaseWindow Base
        {
            get { return fModel.Base; }
            set { fModel.Base = value; }
        }

        public CircleChartType ChartType
        {
            get { return fChartType; }
            set { fChartType = value; }
        }

        public CircleChartModel Model
        {
            get { return fModel; }
        }

        public CircleChartOptions Options
        {
            get { return fModel.Options; }
        }

        public int VisibleGenerations
        {
            get { return fModel.VisibleGenerations; }
            set {
                fModel.VisibleGenerations = value;
                Changed();
            }
        }

        public new float Zoom
        {
            get {
                return fZoom;
            }
            set {
                fZoom = value;

                ExtSize boundary = GetImageSize();
                SetImageSize(boundary, true);
                Invalidate();

                DoZoomChanged();
            }
        }

        public event ARootChangedEventHandler RootChanged;

        public event EventHandler ZoomChanged;

        public GDMIndividualRecord RootPerson
        {
            get {
                return fModel.RootPerson;
            }
            set {
                if (fModel.RootPerson != value) {
                    fModel.RootPerson = value;

                    NavAdd(value);
                    Changed();

                    DoRootChanged(value);
                }
            }
        }


        public CircleChart()
        {
            fRenderer = new EtoGfxRenderer();
            fModel = new CircleChartModel();
            fModel.SetRenderer(fRenderer);
            fModel.Options = new CircleChartOptions();
            fModel.Font = AppHost.GfxProvider.CreateFont(Font.FamilyName, Font.Size, false);

            fMouseCaptured = MouseCaptured.mcNone;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fModel.Dispose();
            }
            base.Dispose(disposing);
        }

        public override void SetRenderer(ChartRenderer renderer)
        {
            base.SetRenderer(renderer);
            fModel.SetRenderer(renderer);
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

            ExtSize boundary = GetImageSize();
            SetImageSize(boundary, false);

            InvalidateContent();
        }

        private void DoRootChanged(GDMIndividualRecord person)
        {
            var eventHandler = RootChanged;
            if (eventHandler != null)
                eventHandler(this, person);
        }

        private void DoZoomChanged()
        {
            var eventHandler = ZoomChanged;
            if (eventHandler != null)
                eventHandler(this, new EventArgs());
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

            //fOffsetX = 0;
            //fOffsetY = 0;

            if (target == RenderTarget.Screen) {

                // Returns the center point of this chart relative to the upper left
                // point of this window's client area.
                // According to discussion at PR #99 (GitHub), this member centers this
                // chart on an axis when there's no scrolling required along that axis.
                // And if scrolling is required, this member aligns this chart on its
                // left edge.
                PointF center = new PointF(base.ImageRect.Center);
                return center;

            } else {

                // Returns the center point of this chart relative to the upper left
                // corner/point of printing canvas.
                //return new PointF(fOffsetX - bounds.Left * fZoom, fOffsetY - bounds.Top * fZoom);
                return new PointF(-bounds.Left * fZoom, -bounds.Top * fZoom);

            }
        }

        private CircleSegment FindSegment(Point imPt)
        {
            ExtSize imSize = GetImageSize();
            float dX = (imPt.X - imSize.Width / 2.0f) / fZoom;
            float dY = (imPt.Y - imSize.Height / 2.0f) / fZoom;
            return fModel.FindSegment(dX, dY);
        }

        #region Protected inherited methods

        protected override void SetNavObject(object obj)
        {
            RootPerson = obj as GDMIndividualRecord;
        }

        protected override void OnMouseDoubleClick(MouseEventArgs e)
        {
            if (fChartType == CircleChartType.Ancestors) {
                fModel.GroupsMode = !fModel.GroupsMode;
            }

            e.Handled = true;
            base.OnMouseDoubleClick(e);
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            // blocking drawing while renderer switched to output to files
            if (fPaintLock) return;

            fRenderer.SetTarget(e.Graphics);
            RenderImage(RenderTarget.Screen);

            base.OnPaint(e);
        }

        protected override void OnSizeChanged(EventArgs e)
        {
            base.OnSizeChanged(e);
            Changed();
        }

        protected override void OnKeyDown(KeyEventArgs e)
        {
            switch (e.Key) {
                case Keys.Add:
                    if (Keys.None == e.Modifiers) {
                        Zoom = Math.Min(fZoom * 1.05f, ZOOM_HIGH_LIMIT);
                    }
                    e.Handled = true;
                    break;

                case Keys.Subtract:
                    if (Keys.None == e.Modifiers) {
                        Zoom = Math.Max(fZoom * 0.95f, ZOOM_LOW_LIMIT);
                    }
                    e.Handled = true;
                    break;

                case Keys.D0:
                    if (e.Control) {
                        Zoom = 1.0f;
                    }
                    e.Handled = true;
                    break;

                case Keys.Keypad8:
                case Keys.Up:
                    if (Keys.None == e.Modifiers) {
                        VisibleGenerations += 1;
                    }
                    e.Handled = true;
                    break;

                case Keys.Keypad2:
                case Keys.Down:
                    if (Keys.None == e.Modifiers) {
                        VisibleGenerations -= 1;
                    }
                    e.Handled = true;
                    break;

                case Keys.Keypad4:
                case Keys.Left:
                case Keys.Keypad6:
                case Keys.Right:
                    if (fChartType == CircleChartType.Ancestors && fModel.RootPerson != null) {
                        GDMIndividualRecord father, mother;
                        fModel.Base.Context.Tree.GetParents(fModel.RootPerson, out father, out mother);
                        var target = (e.Key == Keys.Left || e.Key == Keys.Keypad4) ? father : mother;
                        if (target != null) {
                            RootPerson = target;
                        }
                    }
                    e.Handled = true;
                    break;

                default:
                    base.OnKeyDown(e);
                    break;
            }
        }

        protected override void OnMouseDown(MouseEventArgs e)
        {
            if ((e.Buttons == MouseButtons.Alternate) && (HScroll || VScroll)) {
                Point pt = new Point(e.Location);
                fMouseCaptured = MouseCaptured.mcDrag;
                fMouseCaptureX = pt.X;
                fMouseCaptureY = pt.Y;
                Cursor = Cursors.Move;
            }

            e.Handled = true;
            base.OnMouseDown(e);
        }

        protected override void OnMouseUp(MouseEventArgs e)
        {
            Point irPt = GetImageRelativeLocation(e.Location, e.Buttons != MouseButtons.None);

            if (fMouseCaptured == MouseCaptured.mcDrag) {
                fMouseCaptured = MouseCaptured.mcNone;
                Cursor = Cursors.Default;
            } else if (e.Buttons == MouseButtons.Primary) {
                CircleSegment selected = FindSegment(irPt);
                if (selected != null && selected.IRec != null) {
                    RootPerson = selected.IRec;
                }
            }

            e.Handled = true;
            base.OnMouseUp(e);
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            Point irPt = GetImageRelativeLocation(e.Location, e.Buttons != MouseButtons.None);

            switch (fMouseCaptured) {
                case MouseCaptured.mcNone:
                    {
                        CircleSegment selected = FindSegment(irPt);

                        string hint = "";
                        if (!Equals(fModel.Selected, selected)) {
                            fModel.Selected = selected;

                            if (selected != null && selected.IRec != null) {
                                string name = GKUtils.GetNameString(selected.IRec, true, false);
                                hint = /*selected.Gen.ToString() + ", " + */name;
                            }

                            InvalidateContent();
                        }

                        if (!Equals(fHint, hint)) {
                            fHint = hint;

                            if (!string.IsNullOrEmpty(hint)) {
                                //fToolTip.Show(hint, this, mpt.X, mpt.Y, 3000);
                                ToolTip = hint;
                            }
                        }
                    }
                    break;

                case MouseCaptured.mcDrag:
                    {
                        Point pt = new Point(e.Location);
                        AdjustScroll(-(pt.X - fMouseCaptureX), -(pt.Y - fMouseCaptureY));
                        fMouseCaptureX = pt.X;
                        fMouseCaptureY = pt.Y;
                    }
                    break;
            }

            e.Handled = true;
            base.OnMouseMove(e);
        }

        protected override void OnMouseWheel(MouseEventArgs e)
        {
            if (Keys.None != (Keys.Control & e.Modifiers)) {
                if (e.Delta.Height < 0) {
                    Zoom = Math.Max(fZoom * 0.95f, ZOOM_LOW_LIMIT);
                } else {
                    Zoom = Math.Min(fZoom * 1.05f, ZOOM_HIGH_LIMIT);
                }
            }

            e.Handled = true;
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
        public override ExtSize GetImageSize()
        {
            return new ExtSize((int)(fModel.ImageWidth * fZoom), (int)(fModel.ImageHeight * fZoom));
        }

        /// <summary>
        /// Renders this chart on the specified target and the context
        /// associated with the target.
        /// </summary>
        /// <param name="target">Rendering target.</param>
        /// <param name="forciblyCentered"></param>
        public override void RenderImage(RenderTarget target, bool forciblyCentered = false)
        {
            PointF center = GetCenter(target);

            var backColor = fModel.Options.BrushColor[9];
            if (target == RenderTarget.Screen) {
                var rect = CanvasRectangle;
                fRenderer.DrawRectangle(null, backColor, 0, 0, rect.Width, rect.Height);
            } else if (target == RenderTarget.Printer) {
                fRenderer.DrawRectangle(null, UIHelper.ConvertColor(Colors.White), 0, 0, fModel.ImageWidth, fModel.ImageHeight);
            } else {
                fRenderer.DrawRectangle(null, backColor, 0, 0, fModel.ImageWidth, fModel.ImageHeight);
            }

            fRenderer.SaveTransform();
            fRenderer.TranslateTransform(center.X, center.Y);

            if (target == RenderTarget.Screen) {
                fRenderer.ScaleTransform(fZoom, fZoom);
            } else {
                fRenderer.ScaleTransform(1, 1);
            }

            switch (fChartType) {
                case CircleChartType.Ancestors:
                    fModel.DrawAncestors();
                    break;

                case CircleChartType.Descendants:
                    fModel.DrawDescendants();
                    break;
            }

            fRenderer.RestoreTransform();
        }
    }
}
