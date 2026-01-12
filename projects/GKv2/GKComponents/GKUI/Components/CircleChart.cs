/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using BSLib;
using GDModel;
using GKCore;
using GKCore.Charts;
using GKCore.Design;
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

        private readonly IContainer fComponents;
        private readonly CircleChartModel fModel;
        private readonly ToolTip fToolTip;

        private CircleChartType fChartType;
        private string fHint;
        private float fOffsetX;
        private float fOffsetY;
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

        public float Zoom
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
            fRenderer = new WFGfxRenderer();
            fModel = new CircleChartModel();
            fModel.SetRenderer(fRenderer);
            fModel.Options = new CircleChartOptions();
            fModel.Font = AppHost.GfxProvider.CreateFont(Font.Name, Font.Size, false);

            fComponents = new Container();
            fToolTip = new ToolTip(fComponents);
            fToolTip.AutoPopDelay = 5000;
            fToolTip.InitialDelay = 250;
            fToolTip.ReshowDelay = 50;
            fToolTip.ShowAlways = true;

            fMouseCaptured = MouseCaptured.mcNone;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fComponents != null) fComponents.Dispose();
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
        }

        private void DoRootChanged(GDMIndividualRecord person)
        {
            RootChanged?.Invoke(this, person);
        }

        private void DoZoomChanged()
        {
            ZoomChanged?.Invoke(this, new EventArgs());
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

            fOffsetX = AutoScrollPosition.X;
            fOffsetY = AutoScrollPosition.Y;

            if (target == RenderTarget.Screen) {

                // Returns the center point of this chart relative to the upper left
                // point of this window's client area.
                // According to discussion at PR #99 (GitHub), this member centers this
                // chart on an axis when there's no scrolling required along that axis.
                // And if scrolling is required, this member aligns this chart on its
                // left edge.
                PointF center = new PointF();

                SizeF boundary = new SizeF(fModel.ImageWidth * fZoom, fModel.ImageHeight * fZoom);

                if (ClientSize.Width > boundary.Width) {
                    center.X = Math.Min(ClientSize.Width - bounds.Right * fZoom, ClientSize.Width >> 1);
                } else {
                    center.X = fOffsetX - bounds.Left * fZoom;
                }

                if (ClientSize.Height > boundary.Height) {
                    center.Y = Math.Min(ClientSize.Height - bounds.Bottom * fZoom, ClientSize.Height >> 1);
                } else {
                    center.Y = fOffsetY - bounds.Top * fZoom;
                }

                return center;

            } else {

                // Returns the center point of this chart relative to the upper left
                // corner/point of printing canvas.
                //return new PointF(fOffsetX - bounds.Left * fZoom, fOffsetY - bounds.Top * fZoom);
                return new PointF(-bounds.Left * fZoom, -bounds.Top * fZoom);

            }
        }

        private CircleSegment FindSegment(float mX, float mY)
        {
            PointF center = GetCenter(RenderTarget.Screen);
            mX = (mX - center.X) / fZoom;
            mY = (mY - center.Y) / fZoom;
            return fModel.FindSegment(mX, mY);
        }

        #region Protected inherited methods

        protected override void SetNavObject(object obj)
        {
            RootPerson = obj as GDMIndividualRecord;
        }

        protected override void OnDoubleClick(EventArgs e)
        {
            if (fChartType == CircleChartType.Ancestors) {
                fModel.GroupsMode = !fModel.GroupsMode;
                Invalidate();
            }

            base.OnDoubleClick(e);
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            fRenderer.SetTarget(e.Graphics);
            RenderImage(RenderTarget.Screen);

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
                        Zoom = Math.Min(fZoom * 1.05f, ZOOM_HIGH_LIMIT);
                    }
                    break;

                case Keys.Subtract:
                case Keys.OemMinus:
                    if (Keys.None == ModifierKeys) {
                        Zoom = Math.Max(fZoom * 0.95f, ZOOM_LOW_LIMIT);
                    }
                    break;

                case Keys.D0:
                    if (e.Control) {
                        Zoom = 1.0f;
                    }
                    break;

                case Keys.Up:
                    VisibleGenerations += 1;
                    break;

                case Keys.Down:
                    VisibleGenerations -= 1;
                    break;

                case Keys.Left:
                case Keys.Right:
                    if (fChartType == CircleChartType.Ancestors && fModel.RootPerson != null) {
                        GDMIndividualRecord father, mother;
                        fModel.Base.Context.Tree.GetParents(fModel.RootPerson, out father, out mother);
                        var target = (e.KeyCode == Keys.Left) ? father : mother;
                        if (target != null) {
                            RootPerson = target;
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
            } else if (e.Button == MouseButtons.Left) {
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
                                string name = GKUtils.GetNameString(selected.IRec, false);
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
                if (e.Delta < 0) {
                    Zoom = Math.Max(fZoom * 0.95f, ZOOM_LOW_LIMIT);
                } else {
                    Zoom = Math.Min(fZoom * 1.05f, ZOOM_HIGH_LIMIT);
                }
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
            fRenderer.SetSmoothing(true);

            PointF center = GetCenter(target);

            var backColor = fModel.Options.BrushColor[9];
            if (target == RenderTarget.Screen) {
                fRenderer.DrawRectangle(null, backColor, 0, 0, Width, Height);
            } else if (target == RenderTarget.Printer) {
                fRenderer.DrawRectangle(null, UIHelper.ConvertColor(Color.White), 0, 0, fModel.ImageWidth, fModel.ImageHeight);
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
