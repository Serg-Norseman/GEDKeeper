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

using System;
using Eto.Drawing;
using Eto.Forms;
using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Charts;
using GKCore.Interfaces;
using GKCore.Options;
using GKUI.Components;

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

        private readonly CircleChartModel fModel;
        private readonly ChartRenderer fRenderer;

        private CircleChartType fChartType;
        private string fHint;
        private float fOffsetX = 0;
        private float fOffsetY = 0;
        /* Zoom factors */
        private float fZoom = 1.0f;
        private float fZoomLowLimit = 0.0125f;
        private float fZoomHighLimit = 1000.0f;
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

        public AncestorsCircleOptions Options
        {
            get { return fModel.Options; }
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
            }
        }

        public event ARootChangedEventHandler RootChanged;

        public GEDCOMIndividualRecord RootPerson
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
            CenteredImage = true;

            fRenderer = new TreeChartGfxRenderer();
            fModel = new CircleChartModel();
            fModel.SetRenderer(fRenderer);
            fModel.Options = new AncestorsCircleOptions();
            fModel.Font = AppHost.GfxProvider.CreateFont(Font.FamilyName, Font.Size, false);

            fMouseCaptured = MouseCaptured.mcNone;

            BackgroundColor = UIHelper.ConvertColor(fModel.Options.BrushColor[9]);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
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

            ExtSize boundary = GetImageSize();
            SetImageSize(boundary);
        }

        private void DoRootChanged(GEDCOMIndividualRecord person)
        {
            var eventHandler = (ARootChangedEventHandler)RootChanged;
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
            if (target == RenderTarget.rtScreen) {

                // Returns the center point of this chart relative to the upper left
                // point of this window's client area.
                // According to discussion at PR #99 (GitHub), this member centers this
                // chart on an axis when there's no scrolling required along that axis.
                // And if scrolling is required, this member aligns this chart on its
                // left edge.
                PointF center = new PointF(base.ImageRect.Center);
                return center;

            } else {

                var bounds = fModel.Bounds;

                // Returns the center point of this chart relative to the upper left
                // corner/point of printing canvas.
                return new PointF(fOffsetX - bounds.Left * fZoom,
                                  fOffsetY - bounds.Top * fZoom);

            }
        }

        private CircleSegment FindSegment(PointF mpt)
        {
            Point imPt = GetImageRelativeLocation(mpt);
            ExtSize imSize = GetImageSize();
            float dX = (imPt.X - imSize.Width / 2) / fZoom;
            float dY = (imPt.Y - imSize.Height / 2) / fZoom;
            return fModel.FindSegment(dX, dY);
        }

        /// <summary>
        /// Renders this chart on the specified target and the context
        /// associated with the target.
        /// </summary>
        /// <param name="context">GDI+ rendering context.</param>
        /// <param name="target">Rendering target.</param>
        private void Render(Graphics context, RenderTarget target)
        {
            //context.SetClip(Viewport);

            PointF center = GetCenter(target);

            fModel.Renderer.SetTarget(context, true);
            fModel.Renderer.SaveTransform();
            fModel.Renderer.TranslateTransform(center.X, center.Y);

            if (target == RenderTarget.rtScreen) {
                fModel.Renderer.ScaleTransform(fZoom, fZoom);
            } else {
                fModel.Renderer.ScaleTransform(1, 1);
            }

            switch (fChartType) {
                case CircleChartType.Ancestors:
                    fModel.DrawAncestors();
                    break;

                case CircleChartType.Descendants:
                    fModel.DrawDescendants();
                    break;
            }

            fModel.Renderer.RestoreTransform(null);

            //context.ResetClip();
        }

        #region Protected inherited methods

        protected override void SetNavObject(object obj)
        {
            RootPerson = obj as GEDCOMIndividualRecord;
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
            Render(e.Graphics, RenderTarget.rtScreen);

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
                case Keys.Plus:
                    if (Keys.None == e.Modifiers) {
                        Zoom = Math.Min(fZoom * 1.05f, fZoomHighLimit);
                    }
                    break;

                case Keys.Minus:
                    if (Keys.None == e.Modifiers) {
                        Zoom = Math.Max(fZoom * 0.95f, fZoomLowLimit);
                    }
                    break;

                case Keys.D0:
                    if (e.Control) {
                        Zoom = 1.0f;
                    }
                    break;

                case Keys.Left:
                    if (fChartType == CircleChartType.Ancestors && fModel.RootPerson != null) {
                        GEDCOMFamilyRecord fam = fModel.RootPerson.GetParentsFamily();
                        var father = (fam == null) ? null : fam.GetHusband();
                        if (father != null) {
                            RootPerson = father;
                        }
                    }
                    break;

                case Keys.Right:
                    if (fChartType == CircleChartType.Ancestors && fModel.RootPerson != null) {
                        GEDCOMFamilyRecord fam = fModel.RootPerson.GetParentsFamily();
                        var mother = (fam == null) ? null : fam.GetWife();
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
            if (fMouseCaptured == MouseCaptured.mcDrag) {
                fMouseCaptured = MouseCaptured.mcNone;
                Cursor = Cursors.Default;
            }
            else if (e.Buttons == MouseButtons.Primary) {
                CircleSegment selected = FindSegment(e.Location);
                if (selected != null && selected.IRec != null) {
                    RootPerson = selected.IRec;
                }
            }

            e.Handled = true;
            base.OnMouseUp(e);
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            switch (fMouseCaptured) {
                case MouseCaptured.mcNone:
                    {
                        CircleSegment selected = FindSegment(e.Location);

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
                if (0 > e.Delta.Height) {
                    Zoom = Math.Max(fZoom * 0.95f, fZoomLowLimit);
                } else {
                    Zoom = Math.Min(fZoom * 1.05f, fZoomHighLimit);
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

        public override void RenderStaticImage(Graphics gfx, bool printer)
        {
            Render(gfx, RenderTarget.rtNonScreenCanvas);
        }
    }
}
