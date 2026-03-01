/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using BSLib;
using GDModel;
using GKCore;
using GKCore.Charts;
using GKCore.Design;
using GKCore.Options;
using Terminal.Gui;
using gkcMouseEvent = GKCore.Charts.MouseEvent;
using tgMouseEvent = Terminal.Gui.MouseEvent;
using tgMouseEventArgs = Terminal.Gui.View.MouseEventArgs;

namespace GKUI.Components
{
    public delegate void MouseEventHandler(object sender, tgMouseEventArgs e);

    /// <summary>
    /// 
    /// </summary>
    public class TreeChartBox : CustomChart, ITreeChart
    {
        #region Private fields

        private readonly TreeChartModel fModel;
        private readonly TweenLibrary fTween;

        private ChartControlMode fMode = ChartControlMode.Default;
        private int fMouseX;
        private int fMouseY;
        private TreeChartOptions fOptions;
        private TreeChartPerson fSelected;
        private GDMIndividualRecord fSaveSelection;
        private bool fTraceKinships;
        private bool fTraceSelected;

        #endregion

        #region Public properties

        public event PersonModifyEventHandler PersonModify;

        public event RootChangedEventHandler RootChanged;

        public event MouseEventHandler PersonProperties;

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
            fModel = new TreeChartModel(this);
            fRenderer = null;
            fSelected = null;
            fTraceSelected = true;
            fTween = new TweenLibrary();
        }

        public TreeChartBox(ChartRenderer renderer) : this()
        {
            SetRenderer(renderer);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fTween.Dispose();
                fModel.Dispose();
            }
            base.Dispose(disposing);
        }

        int ITreeChart.Height { get; set; }
        int ITreeChart.Width { get; set; }

        public void Invalidate()
        {
            InvalidateContent();
        }

        public override void SetLayout(IChartLayout layout)
        {
            base.SetLayout(layout);
            fModel.SetLayout(layout);
        }

        public override void SetRenderer(ChartRenderer renderer)
        {
            base.SetRenderer(renderer);
            fModel.SetRenderer(renderer);
        }

        public override void SetScale(float value)
        {
            // not supported
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

        public void ResetBackground()
        {
        }

        private void DrawBackground(RenderTarget target, BackgroundMode background)
        {
            /*int width, height;
            if (target == RenderTarget.Screen) {
                var rect = ClientRectangle;
                width = rect.Width;
                height = rect.Height;
            } else {
                // when rendering goes to a file, the fill should be on the entire area
                width = fModel.ImageWidth;
                height = fModel.ImageHeight;
            }

            switch (background) {
                case BackgroundMode.bmNone:
                    break;

                case BackgroundMode.bmImage:
                case BackgroundMode.bmFill:
                case BackgroundMode.bmAny:
                    fRenderer.DrawRectangle(null, UIHelper.ConvertColor(BackColor), 0, 0, width, height);
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
            // For a component without a large nested canvas,
            // the render origin is independent of the viewport.
            return new ExtPoint(0, 0);
        }

        public void RecalcChart(bool noRedraw = false)
        {
            fRenderer.SetTarget(this);

            try {
                fModel.RecalcChart();
            } finally {
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
            PersonModify?.Invoke(this, eArgs);
        }

        private void DoRootChanged(TreeChartPerson person)
        {
            RootChanged?.Invoke(this, person);
        }

        private void DoInfoRequest(TreeChartPerson person)
        {
            InfoRequest?.Invoke(this, person);
        }

        private void DoPersonProperties(MouseEventArgs eArgs)
        {
            PersonProperties?.Invoke(this, eArgs);
        }

        private void DoZoomChanged()
        {
            ZoomChanged?.Invoke(this, new EventArgs());
        }

        public override bool OnKeyDown(KeyEvent e)
        {
            bool result = false;

            switch (e.Key) {
                case Key.F4:
                    ToggleCollapse();
                    result = true;
                    break;

                /*case (Key)'+':
                case (Key)'-':
                    // not supported
                    break;*/

                default:
                    result = base.OnKeyDown(e);
                    break;
            }

            return result;
        }

        /*protected override void OnSizeChanged(EventArgs e)
        {
            SaveSelection();

            var imageSize = GetImageSize();
            SetImageSize(imageSize);
            fTreeControls.UpdateView();

            RestoreSelection();

            base.OnSizeChanged(e);
        }*/

        protected override void OnPaint(View content)
        {
            fRenderer.SetTarget(content);

            InternalDraw(RenderTarget.Screen, ChartDrawMode.dmInteractive, BackgroundMode.bmAny);
        }

        private MouseAction GetMouseAction(tgMouseEvent e, gkcMouseEvent mouseEvent, out TreeChartPerson person)
        {
            var result = MouseAction.None;
            person = null;

            var viewport = base.Viewport;
            ExtPoint offsets = fModel.GetOffsets();
            int aX = e.X + viewport.Left - offsets.X;
            int aY = e.Y + viewport.Top - offsets.Y;
            var mouseFlags = e.Flags;

            int num = fModel.Persons.Count;
            for (int i = 0; i < num; i++) {
                TreeChartPerson p = fModel.Persons[i];
                if (!p.IsVisible) continue;
                ExtRect persRt = p.Rect;

                if (persRt.Contains(aX, aY)) {
                    person = p;

                    if (mouseFlags == MouseFlags.Button1Pressed) {
                        result = MouseAction.Select;
                        break;
                    } else if (mouseFlags == MouseFlags.Button3Released) {
                        result = MouseAction.Properties;
                        break;
                    } else if (mouseEvent == gkcMouseEvent.meMove) {
                        result = MouseAction.Highlight;
                        break;
                    }
                }

                /*ExtRect expRt = fModel.GetExpanderRect(persRt);
                if ((mouseFlags == MouseFlags.Button1Released) && expRt.Contains(aX, aY)) {
                    person = p;
                    result = MouseAction.Expand;
                    break;
                }

                expRt = fModel.GetCollapseRect(p);
                if ((mouseFlags == MouseFlags.Button1Released) && expRt.Contains(aX, aY)) {
                    person = p;
                    result = MouseAction.CollapseBranch;
                    break;
                }

                ExtRect infoRt = fModel.GetInfoRect(persRt);
                if ((mouseFlags == MouseFlags.Button1Released) && infoRt.Contains(aX, aY)) {
                    person = p;
                    result = MouseAction.Info;
                    break;
                }*/
            }

            if (result == MouseAction.None && person == null && mouseFlags == MouseFlags.Button3Pressed) {
                result = MouseAction.Drag;
            }

            return result;
        }

        public override bool MouseEvent(tgMouseEvent me)
        {
            if (me.Flags.HasFlag(MouseFlags.ReportMousePosition)) {
                return OnMouseMove(me);
            } else if (me.Flags.HasFlag(MouseFlags.Button1Pressed) || me.Flags.HasFlag(MouseFlags.Button3Pressed)) {
                return OnMouseDown(me);
            } else if (me.Flags.HasFlag(MouseFlags.Button1Released) || me.Flags.HasFlag(MouseFlags.Button3Released)) {
                return OnMouseUp(me);
            } else if (me.Flags.HasFlag(MouseFlags.Button1DoubleClicked)) {
                TreeChartPerson p = fSelected;
                DoPersonModify(new PersonModifyEventArgs(p));
                return true;
            }

            return base.MouseEvent(me);
        }

        protected bool OnMouseDown(tgMouseEvent e)
        {
            fMouseX = e.X;
            fMouseY = e.Y;

            switch (fMode) {
                case ChartControlMode.Default:
                    MouseAction mAct = GetMouseAction(e, gkcMouseEvent.meDown, out TreeChartPerson mPers);

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
                    // not supported
                    break;
            }

            return true;
        }

        protected bool OnMouseMove(tgMouseEvent e)
        {
            switch (fMode) {
                case ChartControlMode.Default:
                    /*MouseAction mAct = GetMouseAction(e, gkcMouseEvent.meMove, out TreeChartPerson mPers);
                    if (mAct == MouseAction.Highlight) {
                        SetHighlight(mPers);
                    } else {
                        SetHighlight(null);
                    }*/
                    break;

                case ChartControlMode.DragImage:
                    AdjustScroll((e.X - fMouseX), (e.Y - fMouseY));
                    fMouseX = e.X;
                    fMouseY = e.Y;
                    break;
            }

            return true;
        }

        protected bool OnMouseUp(tgMouseEvent e)
        {
            switch (fMode) {
                case ChartControlMode.Default:
                    MouseAction mAct = GetMouseAction(e, gkcMouseEvent.meUp, out TreeChartPerson mPers);

                    switch (mAct) {
                        case MouseAction.None:
                            break;

                        case MouseAction.Properties:
                            SelectBy(mPers, false);
                            if (fSelected == mPers && fSelected.Rec != null) {
                                DoPersonProperties(new MouseEventArgs(e));
                            }
                            break;

                        case MouseAction.Expand:
                            GenChart(mPers.Rec, TreeChartKind.ckBoth, true);
                            DoRootChanged(mPers);
                            break;

                        case MouseAction.CollapseBranch:
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
                    // not supported
                    break;
            }

            return true;
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
