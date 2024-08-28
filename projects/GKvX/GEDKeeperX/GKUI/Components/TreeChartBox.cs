/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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

//#define DEBUG_IMAGE

using System;
using BSLib;
using GDModel;
using GKCore;
using GKCore.Charts;
using GKCore.Interfaces;
using GKCore.Options;
using GKUI.Platform;
using SkiaSharp.Views.Forms;
using Xamarin.Forms;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class TreeChartBox : CustomChart, ITreeChart
    {
        #region Private fields

        private readonly TreeChartModel fModel;
        private readonly TreeControlsList<ITreeControl> fTreeControls;
        private readonly TweenLibrary fTween;

        private ITreeControl fActiveControl;
        private ChartControlMode fMode = ChartControlMode.Default;
        private int fMouseX;
        private int fMouseY;
        private TreeChartOptions fOptions;
        private TreeChartPerson fSelected;
        private GDMIndividualRecord fSaveSelection;
        private ITimer fTimer;
        private bool fTraceKinships;
        private bool fTraceSelected;

        #endregion

        #region Public properties

        public event PersonModifyEventHandler PersonModify;

        public event RootChangedEventHandler RootChanged;

        public event EventHandler PersonProperties;

        public event EventHandler ZoomChanged;

        public event InfoRequestEventHandler InfoRequest;


        public IBaseWindow Base
        {
            get { return fModel.Base; }
            set { fModel.Base = value; }
        }

        public bool CertaintyIndex
        {
            get {
                return fModel.CertaintyIndex;
            }
            set {
                fModel.CertaintyIndex = value;
                Invalidate();
            }
        }

        public int DepthLimitAncestors
        {
            get { return fModel.DepthLimitAncestors; }
            set { fModel.DepthLimitAncestors = value; }
        }

        public int DepthLimitDescendants
        {
            get { return fModel.DepthLimitDescendants; }
            set { fModel.DepthLimitDescendants = value; }
        }

        public int IndividualsCount
        {
            get { return fModel.PreparedIndividuals.Count; }
        }

        public new int Height // FIXME: resolve
        {
            get { return (int)base.Height; }
            set {  }
        }

        public new int Width // FIXME: resolve
        {
            get { return (int)base.Width; }
            set { }
        }

        public TreeChartKind Kind
        {
            get { return fModel.Kind; }
            set { fModel.Kind = value; }
        }

        public TreeChartModel Model
        {
            get { return fModel; }
        }

        public TreeChartOptions Options
        {
            get {
                return fOptions;
            }
            set {
                fOptions = value;
                fModel.Options = value;
            }
        }

        public override float Scale
        {
            get { return fModel.Scale; }
        }

        public TreeChartPerson Selected
        {
            get { return fSelected; }
            set { SetSelected(value); }
        }

        public bool TraceSelected
        {
            get { return fTraceSelected; }
            set { fTraceSelected = value; }
        }

        public bool TraceKinships
        {
            get { return fTraceKinships; }
            set { fTraceKinships = value; }
        }

        public bool XRefVisible
        {
            get {
                return fModel.XRefVisible;
            }
            set {
                fModel.XRefVisible = value;
                Invalidate();
            }
        }

        #endregion

        #region Instance control

        public TreeChartBox()
        {
            BackgroundColor = Color.White;

            fModel = new TreeChartModel(this);
            fRenderer = null;
            fSelected = null;
            fTraceSelected = true;

            fTreeControls = new TreeControlsList<ITreeControl>();
            //fTreeControls.Add(new TCScaleControl(this));
            //fTreeControls.Add(new TCGenerationsControl(this, TreeChartKind.ckDescendants));
            //fPersonControl = new PersonControl(this);

            fTween = new TweenLibrary();
        }

        public TreeChartBox(ChartRenderer renderer) : this()
        {
            SetRenderer(renderer);
        }

        /*protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fTween.Dispose();
                fModel.Dispose();

                if (fTimer != null) fTimer.Dispose();
                if (fTreeControls != null) fTreeControls.Dispose();
            }
            base.Dispose(disposing);
        }*/

        public void Invalidate()
        {
            base.InvalidateContent();
        }

        public override void SetRenderer(ChartRenderer renderer)
        {
            base.SetRenderer(renderer);
            fModel.SetRenderer(renderer);
        }

        public override void SetScale(float value)
        {
            fModel.Scale = value;

            RecalcChart();

            if (fTraceSelected && fSelected != null) {
                CenterPerson(fSelected, false);
            }

            fTreeControls.UpdateState();

            DoZoomChanged();
        }

        public void GenChart(bool rootCenter)
        {
            GenChart(fModel.Root.Rec, fModel.Kind, rootCenter);
        }

        public void GenChart(GDMIndividualRecord iRec, TreeChartKind kind, bool rootCenter)
        {
            if (iRec == null) return;

            try {
                GenChartInt(iRec, kind, rootCenter);

                NavAdd(new TreeChartNavItem(iRec, kind));
            } catch (Exception ex) {
                Logger.WriteError("TreeChartBox.GenChart()", ex);
            }
        }

        private void GenChartInt(GDMIndividualRecord iRec, TreeChartKind kind, bool rootCenter)
        {
            if (iRec == null) return;

            try {
                fSelected = null;

                fModel.GenChart(iRec, kind);

                RecalcChart();

                if (rootCenter) CenterPerson(fModel.Root, false);

                DoRootChanged(fModel.Root);
            } catch (Exception ex) {
                Logger.WriteError("TreeChartBox.GenChartInt()", ex);
            }
        }

        public void RefreshTree()
        {
            try {
                if (fModel.Root == null) return;

                GDMIndividualRecord rootRec = fModel.Root.Rec;

                SaveSelection();
                GenChart(rootRec, fModel.Kind, false);
                RestoreSelection();
            } catch (Exception ex) {
                Logger.WriteError("TreeChartBox.RefreshTree()", ex);
            }
        }

        public void SaveSelection()
        {
            fSaveSelection = (fSelected == null) ? null : fSelected.Rec;
        }

        public void RestoreSelection()
        {
            SelectByRec(fSaveSelection);
        }

        public void RebuildKinships(bool noRedraw = false)
        {
            if (!fOptions.Kinship || fSelected == null) return;

            try {
                fModel.KinRoot = fSelected;
                RecalcChart(noRedraw);
            } catch (Exception ex) {
                Logger.WriteError("TreeChartBox.RebuildKinships()", ex);
            }
        }

        #endregion

        #region Drawing routines

        public ExtPoint GetOffsets()
        {
            return fModel.GetOffsets();
        }

        private void DrawBackground(RenderTarget target, BackgroundMode background)
        {
            /*switch (background) {
                case BackgroundMode.bmNone:
                    break;

                case BackgroundMode.bmImage:
                case BackgroundMode.bmFill:
                case BackgroundMode.bmAny:
                    int width, height;
                    if (target == RenderTarget.Screen) {
                        var rect = CanvasRectangle;
                        width = rect.Width;
                        height = rect.Height;
                    } else {
                        // when rendering goes to a file, the fill should be on the entire area
                        width = fModel.ImageWidth;
                        height = fModel.ImageHeight;
                    }
                    if (BackgroundImage != null) {
                        // when printing, sheet filling is not needed
                        if (target != RenderTarget.Printer) {
                            using (Brush textureBrush = new TextureBrush(BackgroundImage)) {
                                fRenderer.FillRectangle(new BrushHandler(textureBrush), 0, 0, width, height);
                            }
                        }
                    } else {
                        fRenderer.DrawRectangle(null, UIHelper.ConvertColor(BackgroundColor), 0, 0, width, height);
                    }
                    break;
            }*/
        }

        private void InternalDraw(RenderTarget target, ChartDrawMode drawMode, BackgroundMode background)
        {
            fModel.PrepareDraw(drawMode);
            DrawBackground(target, background);
            fModel.Draw(drawMode);
        }

        #endregion

        #region Sizes and adjustment routines

        public ExtRect GetClientRect()
        {
            return UIHelper.Rt2Rt(base.ClientRectangle);
        }

        public ExtPoint GetDrawOrigin()
        {
            var viewport = base.Viewport;
            return new ExtPoint(viewport.Left, viewport.Top);
        }

        public void RecalcChart(bool noRedraw = false)
        {
            //Graphics gfx = null;
            if (fRenderer is XFGfxRenderer) {
                //gfx = CreateGraphics();
                //fRenderer.SetTarget(gfx, false);
            }

            try {
                fModel.RecalcChart();
            } finally {
                /*if (fRenderer is XFGfxRenderer && gfx != null) {
                    gfx.Dispose();
                }*/
            }

            var imageSize = GetImageSize();
            SetImageSize(imageSize, noRedraw);
        }

        public void ToggleCollapse(TreeChartPerson person)
        {
            try {
                if (person != null) {
                    fModel.ToggleCollapse(person);

                    SaveSelection();
                    RecalcChart(false);
                    RestoreSelection();
                }
            } catch (Exception ex) {
                Logger.WriteError("TreeChartBox.ToggleCollapse()", ex);
            }
        }

        public void ToggleCollapse()
        {
            ToggleCollapse(fSelected);
        }

        #endregion

        #region Event processing

        private void DoPersonModify(PersonModifyEventArgs eArgs)
        {
            var eventHandler = PersonModify;
            if (eventHandler != null)
                eventHandler(this, eArgs);
        }

        private void DoRootChanged(TreeChartPerson person)
        {
            var eventHandler = RootChanged;
            if (eventHandler != null)
                eventHandler(this, person);
        }

        private void DoInfoRequest(TreeChartPerson person)
        {
            var eventHandler = InfoRequest;
            if (eventHandler != null)
                eventHandler(this, person);
        }

        private void DoPersonProperties(EventArgs eArgs)
        {
            var eventHandler = PersonProperties;
            if (eventHandler != null)
                eventHandler(this, eArgs);
        }

        private void DoZoomChanged()
        {
            var eventHandler = ZoomChanged;
            if (eventHandler != null)
                eventHandler(this, new EventArgs());
        }

        protected override void OnSizeAllocated(double width, double height)
        {
            base.OnSizeAllocated(width, height);

            SaveSelection();

            var imageSize = GetImageSize();
            SetImageSize(imageSize);
            if (fTreeControls != null) fTreeControls.UpdateView();

            RestoreSelection();
        }

        /*protected override void OnScroll(ScrollEventArgs e)
        {
            base.OnScroll(e);

            //SaveSelection();
            //var imageSize = GetImageSize();
            //SetImageSize(imageSize);
            fTreeControls.UpdateView();
            //RestoreSelection();
        }*/

        protected override void OnPaint(SKPaintSurfaceEventArgs e)
        {
            fRenderer.SetTarget(e.Surface);

            InternalDraw(RenderTarget.Screen, ChartDrawMode.dmInteractive, BackgroundMode.bmAny);

            // interactive controls
            fTreeControls.Draw(fRenderer);

            base.OnPaint(e);
        }

        protected override void OnMouseDoubleClick(EventArgs e)
        {
            TreeChartPerson p = fSelected;
            DoPersonModify(new PersonModifyEventArgs(p));

            //e.Handled = true;
            base.OnMouseDoubleClick(e);
        }

        internal override void OnZoom(ZoomEventArgs e)
        {
            SetScale(e.Scale);
        }

        private MouseAction GetMouseAction(MouseEventArgs e, MouseEvent mouseEvent, out TreeChartPerson person)
        {
            var result = MouseAction.None;
            person = null;

            Point irPt = GetImageRelativeLocation(e.Location, e.Buttons != SKMouseButton.Unknown);
            int aX = (int)irPt.X;
            int aY = (int)irPt.Y;

            int num = fModel.Persons.Count;
            for (int i = 0; i < num; i++) {
                TreeChartPerson p = fModel.Persons[i];
                if (!p.IsVisible) continue;
                ExtRect persRt = p.Rect;

                if (persRt.Contains(aX, aY)) {
                    person = p;

                    if (e.Buttons == SKMouseButton.Left && mouseEvent == MouseEvent.meDown) {
                        result = MouseAction.Select;
                        break;
                    } else if (e.Buttons == SKMouseButton.Right && mouseEvent == MouseEvent.meUp) {
                        result = MouseAction.Properties;
                        break;
                    } else if (mouseEvent == MouseEvent.meMove) {
                        result = MouseAction.Highlight;
                        break;
                    }
                }

                ExtRect expRt = fModel.GetExpanderRect(persRt);
                if (e.Buttons == SKMouseButton.Left && mouseEvent == MouseEvent.meUp && expRt.Contains(aX, aY)) {
                    person = p;
                    result = MouseAction.Expand;
                    break;
                }

                expRt = TreeChartModel.GetPersonExpandRect(persRt);
                if (e.Buttons == SKMouseButton.Left && mouseEvent == MouseEvent.meUp && expRt.Contains(aX, aY)) {
                    person = p;
                    result = MouseAction.PersonExpand;
                    break;
                }

                ExtRect infoRt = fModel.GetInfoRect(persRt);
                if (e.Buttons == SKMouseButton.Left && mouseEvent == MouseEvent.meUp && infoRt.Contains(aX, aY)) {
                    person = p;
                    result = MouseAction.Info;
                    break;
                }
            }

            if (result == MouseAction.None && person == null) {
                if (e.Buttons == SKMouseButton.Right && mouseEvent == MouseEvent.meDown) {
                    result = MouseAction.Drag;
                }
            }

            return result;
        }

        protected override void OnMouseDown(MouseEventArgs e)
        {
            Point evtPt = e.Location;
            fMouseX = (int)evtPt.X;
            fMouseY = (int)evtPt.Y;

            switch (fMode) {
                case ChartControlMode.Default:
                    TreeChartPerson mPers;
                    MouseAction mAct = GetMouseAction(e, MouseEvent.meDown, out mPers);

                    switch (mAct) {
                        case MouseAction.Select:
                            SelectBy(mPers, true);
                            break;

                        case MouseAction.Drag:
                            fMode = ChartControlMode.DragImage;
                            break;
                    }
                    break;

                case ChartControlMode.DragImage:
                    break;

                case ChartControlMode.ControlsVisible:
                    // unused
                    break;
            }

            e.Handled = true;
            base.OnMouseDown(e);
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            switch (fMode) {
                case ChartControlMode.Default:
                    TreeChartPerson mPers;
                    MouseAction mAct = GetMouseAction(e, MouseEvent.meMove, out mPers);

                    if (mAct == MouseAction.Highlight) {
                        SetHighlight(mPers);
                    } else {
                        SetHighlight(null);
                    }
                    break;

                case ChartControlMode.DragImage:
                    // unused
                    break;

                case ChartControlMode.ControlsVisible:
                    // unused
                    break;
            }

            InvalidateContent();

            e.Handled = true;
            base.OnMouseMove(e);
        }

        protected override void OnMouseUp(MouseEventArgs e)
        {
            Point evtPt = e.Location;
            var ctlPoint = GetControlRelativeLocation(e.Location, e.Buttons != SKMouseButton.Unknown);

            switch (fMode) {
                case ChartControlMode.Default:
                    TreeChartPerson mPers;
                    MouseAction mAct = GetMouseAction(e, MouseEvent.meUp, out mPers);

                    switch (mAct) {
                        case MouseAction.None:
                            break;

                        case MouseAction.Properties:
                            SelectBy(mPers, false);
                            if (fSelected == mPers && fSelected.Rec != null) {
                                DoPersonProperties(new MouseEventArgs(e.Buttons/*, Keys.None*/, ctlPoint));
                            }
                            break;

                        case MouseAction.Expand:
                            GenChart(mPers.Rec, TreeChartKind.ckBoth, true);
                            DoRootChanged(mPers);
                            break;

                        case MouseAction.PersonExpand:
                            ToggleCollapse(mPers);
                            break;

                        case MouseAction.Info:
                            DoInfoRequest(mPers);
                            break;
                    }
                    break;

                case ChartControlMode.DragImage:
                    fMode = ChartControlMode.Default;
                    break;

                case ChartControlMode.ControlsVisible:
                    // unused
                    break;
            }

            e.Handled = true;
            base.OnMouseUp(e);
        }

        #endregion

        #region Navigation

        protected override void SetNavObject(object obj)
        {
            var navItem = obj as TreeChartNavItem;
            if (navItem == null) return;

            GenChartInt(navItem.IndiRec, navItem.ChartKind, true);
        }

        private void SetSelected(TreeChartPerson value)
        {
            if (fSelected != null) fSelected.Selected = false;
            fSelected = value;
            if (fSelected != null) fSelected.Selected = true;

            Invalidate();
        }

        private void SetHighlight(TreeChartPerson person)
        {
            if (fModel.HighlightedPerson == person) return;

            fModel.HighlightedPerson = person;

            /*if (person == null) {
                //fPersonControl.Visible = false;
            } else {
                //fPersonControl.SetPerson(person);
            }*/

            Invalidate();
        }

        public void SelectBy(TreeChartPerson person, bool needCenter)
        {
            if (person == null) return;

            SetSelected(person);

            //if (fTraceKinships && fOptions.Kinship) RebuildKinships(true);

            if (needCenter && fTraceSelected) CenterPerson(person);
        }

        public void SelectByRec(GDMIndividualRecord iRec)
        {
            if (iRec == null) return;

            int num = fModel.Persons.Count;
            for (int i = 0; i < num; i++) {
                TreeChartPerson p = fModel.Persons[i];
                if (p.Rec == iRec) {
                    SetSelected(p);

                    if (fTraceSelected) CenterPerson(p);

                    return;
                }
            }

            SetSelected(null);
        }

        public void CenterPerson(TreeChartPerson person, bool animation = true)
        {
            if (person == null || fTween.Busy) return;

            var viewport = this.Viewport;
            int widthMax = fModel.ImageWidth - viewport.Width;
            int heightMax = fModel.ImageHeight - viewport.Height;

            int srcX = viewport.Left;
            int srcY = viewport.Top;
            int dstX = Algorithms.CheckBounds(((person.PtX) - (viewport.Width / 2)), 0, widthMax);
            int dstY = Algorithms.CheckBounds(((person.PtY + (person.Height / 2)) - (viewport.Height / 2)), 0, heightMax);

            if ((srcX != dstX) || (srcY != dstY)) {
                int timeInterval = animation ? 20 : 1;
                fTween.StartTween(UpdateScrollPosition, srcX, srcY, dstX, dstY, TweenAnimation.EaseInOutQuad, timeInterval);
            }
        }

        #endregion

        #region Print support

        public override ExtSize GetImageSize()
        {
            return (fModel != null) ? fModel.ImageSize : ExtSize.Empty;
        }

        public override void RenderImage(RenderTarget target, bool forciblyCentered = false)
        {
            BackgroundMode bgMode = (target == RenderTarget.Printer) ? BackgroundMode.bmNone : BackgroundMode.bmAny;
            ChartDrawMode drawMode = (!forciblyCentered) ? ChartDrawMode.dmStatic : ChartDrawMode.dmStaticCentered;
            InternalDraw(target, drawMode, bgMode);
        }

        #endregion
    }
}
