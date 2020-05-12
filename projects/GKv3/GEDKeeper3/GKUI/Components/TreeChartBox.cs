﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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

//define DEBUG_IMAGE

using System;
using BSLib;
using Eto.Drawing;
using Eto.Forms;
using GDModel;
using GKCore;
using GKCore.Charts;
using GKCore.Interfaces;
using GKCore.Options;
using GKUI.Providers;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class TreeChartBox : CustomChart, ITreeChartBox
    {
        #region Private fields

        private readonly TreeChartModel fModel;
        private readonly TreeControlsList<ITreeControl> fTreeControls;
        private readonly TweenLibrary fTween;

        private ITreeControl fActiveControl;
        private long fHighlightedStart;
        private ChartControlMode fMode = ChartControlMode.ccmDefault;
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

        public int DepthLimit
        {
            get { return fModel.DepthLimit; }
            set { fModel.DepthLimit = value; }
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

        public float Scale
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

        #endregion

        #region Instance control

        public TreeChartBox() : base()
        {
            BackgroundColor = Colors.White;

            fModel = new TreeChartModel();
            fRenderer = null;
            fSelected = null;
            fTraceSelected = true;

            fTreeControls = new TreeControlsList<ITreeControl>();
            fTreeControls.Add(new TCScaleControl(this));
            fTreeControls.Add(new TCGenerationsControl(this));
            //fPersonControl = new PersonControl(this);

            InitTimer();
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

                if (fTimer != null) fTimer.Dispose();
                if (fTreeControls != null) fTreeControls.Dispose();
            }
            base.Dispose(disposing);
        }

        public override void SetRenderer(ChartRenderer renderer)
        {
            base.SetRenderer(renderer);
            fModel.SetRenderer(renderer);
        }

        private void InitTimer()
        {
            fTimer = AppHost.Instance.CreateTimer(10, TickTimer);
            fTimer.Start();
        }

        private void TickTimer(object sender, EventArgs e)
        {
            if (fModel.HighlightedPerson == null) return;

            DateTime st = DateTime.FromBinary(fHighlightedStart);
            DateTime cur = DateTime.Now;
            TimeSpan d = cur - st;

            if (d.TotalSeconds >= 1/* && !fPersonControl.Visible*/) {
                fModel.HighlightedPerson = null;
                //fPersonControl.Visible = true;
                Invalidate();
            }
        }

        public void SetScale(float value)
        {
            fModel.Scale = value;

            RecalcChart();

            if (fTraceSelected && fSelected != null) {
                CenterPerson(fSelected, false);
            }

            fTreeControls.UpdateState();

            DoZoomChanged();
        }

        public void GenChart(GDMIndividualRecord iRec, TreeChartKind kind, bool rootCenter)
        {
            if (iRec == null) return;

            try {
                fSelected = null;

                fModel.GenChart(iRec, kind, rootCenter);

                RecalcChart();

                if (rootCenter) CenterPerson(fModel.Root, false);

                NavAdd(iRec);
                DoRootChanged(fModel.Root);
            } catch (Exception ex) {
                Logger.LogWrite("TreeChartBox.GenChart(): " + ex.Message);
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
                Logger.LogWrite("TreeChartBox.RefreshTree(): " + ex.Message);
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
                Logger.LogWrite("TreeChartBox.RebuildKinships(): " + ex.Message);
            }
        }

        #endregion

        #region Drawing routines

        public ExtPoint GetOffsets()
        {
            return fModel.GetOffsets();
        }

        private void DrawBackground(BackgroundMode background)
        {
            if (background == BackgroundMode.bmNone) return;

            bool bgImage = false; /*((BackgroundImage != null) &&
                            (background == BackgroundMode.bmAny ||
                             background == BackgroundMode.bmImage));*/

            if (bgImage) {
                /*var imgRect = new Rectangle(0, 0, fImageWidth, fImageHeight);

                    using (Brush textureBrush = new TextureBrush(BackgroundImage, WrapMode.Tile)) {
                        gfx.FillRectangle(textureBrush, imgRect);
                    }*/
            } else {
                bool bgFill = (background == BackgroundMode.bmAny ||
                               background == BackgroundMode.bmImage);

                if (bgFill) {
                    fRenderer.DrawRectangle(null, UIHelper.ConvertColor(BackgroundColor), 0, 0, fModel.ImageWidth, fModel.ImageHeight);
                }
            }
        }

        private void InternalDraw(ChartDrawMode drawMode, BackgroundMode background)
        {
            // drawing relative offset of tree on graphics
            int spx = 0;
            int spy = 0;

            if (drawMode == ChartDrawMode.dmInteractive) {
                var imageViewport = base.ImageViewport;
                spx = imageViewport.Left;
                spy = imageViewport.Top;
                fModel.VisibleArea = UIHelper.Rt2Rt(base.Viewport);
            } else {
                if (drawMode == ChartDrawMode.dmStaticCentered) {
                    Size clientSize = CanvasRectangle.Size;

                    if (fModel.ImageWidth < clientSize.Width) {
                        spx += (clientSize.Width - fModel.ImageWidth) / 2;
                    }

                    if (fModel.ImageHeight < clientSize.Height) {
                        spy += (clientSize.Height - fModel.ImageHeight) / 2;
                    }
                }

                fModel.VisibleArea = ExtRect.CreateBounds(0, 0, fModel.ImageWidth, fModel.ImageHeight);
            }

            fModel.SetOffsets(spx, spy);

            DrawBackground(background);

#if DEBUG_IMAGE
            using (Pen pen = new Pen(Color.Red)) {
                fRenderer.DrawRectangle(pen, Color.Transparent, fSPX, fSPY, fImageWidth, fImageHeight);
            }
#endif

            bool hasDeep = (fSelected != null && fSelected != fModel.Root && fSelected.Rec != null);

            if (hasDeep && fOptions.DeepMode == DeepMode.Background) {
                DrawDeep(fOptions.DeepMode, spx, spy);
            }

            fRenderer.SetTranslucent(0.0f);
            fModel.Draw(drawMode);

            if (hasDeep && fOptions.DeepMode == DeepMode.Foreground) {
                DrawDeep(fOptions.DeepMode, spx, spy);
            }
        }

        private void DrawDeep(DeepMode mode, int spx, int spy)
        {
            try {
                using (var deepModel = new TreeChartModel()) {
                    deepModel.Assign(fModel);
                    deepModel.SetRenderer(fRenderer);
                    deepModel.DepthLimit = 2;
                    deepModel.GenChart(fSelected.Rec, TreeChartKind.ckBoth, true);
                    deepModel.RecalcChart(true);

                    var pers = deepModel.FindPersonByRec(fSelected.Rec);
                    int dmX = (spx + (fSelected.PtX - pers.PtX));
                    int dmY = (spy + (fSelected.PtY - pers.PtY));
                    deepModel.SetOffsets(dmX, dmY);
                    deepModel.VisibleArea = ExtRect.CreateBounds(0, 0, deepModel.ImageWidth, deepModel.ImageHeight);

                    switch (mode) {
                        case DeepMode.Background:
                            fRenderer.SetTranslucent(0.75f);
                            break;

                        case DeepMode.Foreground:
                            fRenderer.SetTranslucent(0.25f);
                            IPen xpen = fRenderer.CreatePen(ChartRenderer.GetColor(ChartRenderer.Black), 2.0f);
                            IColor bColor = ChartRenderer.GetColor(ChartRenderer.White);
                            fRenderer.DrawRoundedRectangle(xpen, bColor, dmX, dmY, deepModel.ImageWidth, deepModel.ImageHeight, 6);
                            fRenderer.SetTranslucent(0.00f);
                            break;
                    }

                    deepModel.Draw(ChartDrawMode.dmStatic);
                }
            } catch (Exception ex) {
                Logger.LogWrite("TreeChartBox.DrawDeep(): " + ex.Message);
            }
        }

        #endregion

        #region Sizes and adjustment routines

        /*private ExtRect GetImageViewport()
        {
            ExtRect viewport;

            var imageSize = GetImageSize();
            if (!imageSize.IsEmpty) {
                Rectangle scrollableViewport = this.Viewport;
                viewport = ExtRect.CreateBounds(
                    scrollableViewport.Left, scrollableViewport.Top,
                    scrollableViewport.Width, scrollableViewport.Height);
            } else {
                viewport = ExtRect.Empty;
            }

            return viewport;
        }*/

        public ExtRect GetClientRect()
        {
            return UIHelper.Rt2Rt(base.Viewport);
        }

        public void RecalcChart(bool noRedraw = false)
        {
            Graphics gfx = null;
            if (fRenderer is EtoGfxRenderer) {
                //gfx = CreateGraphics();
                //fRenderer.SetTarget(gfx, false);
            }

            try {
                fModel.RecalcChart(noRedraw);
            } finally {
                if (fRenderer is EtoGfxRenderer && gfx != null) {
                    gfx.Dispose();
                }
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
                Logger.LogWrite("TreeChartBox.ToggleCollapse(): " + ex.Message);
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
            var eventHandler = (PersonModifyEventHandler)PersonModify;
            if (eventHandler != null)
                eventHandler(this, eArgs);
        }

        private void DoRootChanged(TreeChartPerson person)
        {
            var eventHandler = (RootChangedEventHandler)RootChanged;
            if (eventHandler != null)
                eventHandler(this, person);
        }

        private void DoInfoRequest(TreeChartPerson person)
        {
            var eventHandler = (InfoRequestEventHandler)InfoRequest;
            if (eventHandler != null)
                eventHandler(this, person);
        }

        private void DoPersonProperties(MouseEventArgs eArgs)
        {
            var eventHandler = (/*Mouse*/ EventHandler)PersonProperties;
            if (eventHandler != null)
                eventHandler(this, eArgs);
        }

        private void DoZoomChanged()
        {
            var eventHandler = (EventHandler)ZoomChanged;
            if (eventHandler != null)
                eventHandler(this, new EventArgs());
        }

        protected override void OnKeyDown(KeyEventArgs e)
        {
            switch (e.Key) {
                case Keys.F4:
                    ToggleCollapse();
                    break;

                case Keys.Plus:
                    SetScale(fModel.Scale + 0.05f);
                    e.Handled = true;
                    break;

                case Keys.Minus:
                    SetScale(fModel.Scale - 0.05f);
                    e.Handled = true;
                    break;
            }

            base.OnKeyDown(e);
        }

        protected override void OnSizeChanged(EventArgs e)
        {
            base.OnSizeChanged(e);

            SaveSelection();

            var imageSize = GetImageSize();
            SetImageSize(imageSize);
            fTreeControls.UpdateView();

            RestoreSelection();
        }

        protected override void OnScroll(ScrollEventArgs e)
        {
            base.OnScroll(e);

            //SaveSelection();
            //var imageSize = GetImageSize();
            //SetImageSize(imageSize);
            fTreeControls.UpdateView();
            //RestoreSelection();
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            fRenderer.SetTarget(e.Graphics);

            InternalDraw(ChartDrawMode.dmInteractive, BackgroundMode.bmAny);

            // interactive controls
            fTreeControls.Draw(fRenderer);

            base.OnPaint(e);
        }

        protected override void OnMouseDoubleClick(MouseEventArgs e)
        {
            TreeChartPerson p = fSelected;
            DoPersonModify(new PersonModifyEventArgs(p));

            e.Handled = true;
            base.OnMouseDoubleClick(e);
        }

        protected override void OnMouseWheel(MouseEventArgs e)
        {
            if (e.Modifiers == Keys.Control) {
                float newScale = (e.Delta.Height > 0) ? fModel.Scale + 0.05f : fModel.Scale - 0.05f;
                SetScale(newScale);
            }

            e.Handled = true;
            base.OnMouseWheel(e);
        }

        private MouseAction GetMouseAction(MouseEventArgs e, MouseEvent mouseEvent, out TreeChartPerson person)
        {
            var result = MouseAction.maNone;
            person = null;

            Point mpt = GetImageRelativeLocation(e.Location);
            int aX = mpt.X;
            int aY = mpt.Y;

            int num = fModel.Persons.Count;
            for (int i = 0; i < num; i++) {
                TreeChartPerson p = fModel.Persons[i];
                ExtRect persRt = p.Rect;

                if (persRt.Contains(aX, aY)) {
                    person = p;

                    if (e.Buttons == MouseButtons.Primary && mouseEvent == MouseEvent.meDown) {
                        result = MouseAction.maSelect;
                        break;
                    } else if (e.Buttons == MouseButtons.Alternate && mouseEvent == MouseEvent.meUp) {
                        result = MouseAction.maProperties;
                        break;
                    } else if (mouseEvent == MouseEvent.meMove) {
                        result = MouseAction.maHighlight;
                        break;
                    }
                }

                ExtRect expRt = TreeChartModel.GetExpanderRect(persRt);
                if ((e.Buttons == MouseButtons.Primary && mouseEvent == MouseEvent.meUp) && expRt.Contains(aX, aY)) {
                    person = p;
                    result = MouseAction.maExpand;
                    break;
                }

                expRt = TreeChartModel.GetPersonExpandRect(persRt);
                if ((e.Buttons == MouseButtons.Primary && mouseEvent == MouseEvent.meUp) && expRt.Contains(aX, aY)) {
                    person = p;
                    result = MouseAction.maPersonExpand;
                    break;
                }

                ExtRect infoRt = TreeChartModel.GetInfoRect(persRt);
                if ((e.Buttons == MouseButtons.Primary && mouseEvent == MouseEvent.meUp) && infoRt.Contains(aX, aY)) {
                    person = p;
                    result = MouseAction.maInfo;
                    break;
                }
            }

            if (result == MouseAction.maNone && person == null) {
                if (e.Buttons == MouseButtons.Alternate && mouseEvent == MouseEvent.meDown) {
                    result = MouseAction.maDrag;
                }
            }

            return result;
        }

        protected override void OnMouseDown(MouseEventArgs e)
        {
            Point pt = new Point(e.Location);
            fMouseX = pt.X;
            fMouseY = pt.Y;

            Point scrPt = GetScrollRelativeLocation(e.Location);

            switch (fMode) {
                case ChartControlMode.ccmDefault:
                    TreeChartPerson mPers;
                    MouseAction mAct = GetMouseAction(e, MouseEvent.meDown, out mPers);

                    switch (mAct) {
                        case MouseAction.maSelect:
                            SelectBy(mPers, true);
                            break;

                        case MouseAction.maDrag:
                            Cursor = Cursors.Move; // SizeAll;
                            fMode = ChartControlMode.ccmDragImage;
                            break;
                    }
                    break;

                case ChartControlMode.ccmDragImage:
                    break;

                case ChartControlMode.ccmControlsVisible:
                    fTreeControls.MouseDown(scrPt.X, scrPt.Y);
                    break;
            }

            e.Handled = true;
            base.OnMouseDown(e);
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            //Point ctPt = new Point((int)e.Location.X, (int)e.Location.Y);
            Point scrPt = GetScrollRelativeLocation(e.Location);

            switch (fMode) {
                case ChartControlMode.ccmDefault:
                    TreeChartPerson mPers;
                    MouseAction mAct = GetMouseAction(e, MouseEvent.meMove, out mPers);

                    if (mAct == MouseAction.maHighlight) {
                        SetHighlight(mPers);
                    } else {
                        SetHighlight(null);

                        ITreeControl ctl = fTreeControls.Contains(scrPt.X, scrPt.Y);

                        if (ctl != null) {
                            fMode = ChartControlMode.ccmControlsVisible;
                            ctl.Visible = true;
                            ctl.MouseMove(scrPt.X, scrPt.Y);
                            fActiveControl = ctl;

                            //pt = new Point(pt.X + Left, pt.Y + Top);
                            //fToolTip.Show(ctl.Tip, this, pt, 1500);
                            ToolTip = ctl.Tip;
                        }
                    }
                    break;

                case ChartControlMode.ccmDragImage:
                    Point pt = new Point(e.Location);
                    AdjustScroll(-(pt.X - fMouseX), -(pt.Y - fMouseY));
                    fMouseX = pt.X;
                    fMouseY = pt.Y;
                    break;

                case ChartControlMode.ccmControlsVisible:
                    if (fActiveControl != null) {
                        if (!(fActiveControl.Contains(scrPt.X, scrPt.Y) || fActiveControl.MouseCaptured)) {
                            fMode = ChartControlMode.ccmDefault;
                            fActiveControl.Visible = false;
                            //fToolTip.Hide(this);
                            ToolTip = "";
                            fActiveControl = null;
                        } else {
                            fActiveControl.MouseMove(scrPt.X, scrPt.Y);
                        }
                    }
                    break;
            }

            e.Handled = true;
            base.OnMouseMove(e);
        }

        protected override void OnMouseUp(MouseEventArgs e)
        {
            Point scrPt = GetScrollRelativeLocation(e.Location);

            switch (fMode) {
                case ChartControlMode.ccmDefault:
                    TreeChartPerson mPers;
                    MouseAction mAct = GetMouseAction(e, MouseEvent.meUp, out mPers);

                    switch (mAct) {
                        case MouseAction.maNone:
                            break;

                        case MouseAction.maProperties:
                            SelectBy(mPers, false);
                            if (fSelected == mPers && fSelected.Rec != null) {
                                DoPersonProperties(new MouseEventArgs(e.Buttons, Keys.None, e.Location));
                            }
                            break;

                        case MouseAction.maExpand:
                            DoRootChanged(mPers);
                            GenChart(mPers.Rec, TreeChartKind.ckBoth, true);
                            break;

                        case MouseAction.maPersonExpand:
                            ToggleCollapse(mPers);
                            break;

                        case MouseAction.maInfo:
                            DoInfoRequest(mPers);
                            break;
                    }
                    break;

                case ChartControlMode.ccmDragImage:
                    Cursor = Cursors.Default;
                    fMode = ChartControlMode.ccmDefault;
                    break;

                case ChartControlMode.ccmControlsVisible:
                    fTreeControls.MouseUp(scrPt.X, scrPt.Y);
                    break;
            }

            e.Handled = true;
            base.OnMouseUp(e);
        }

        #endregion

        #region Navigation

        protected override void SetNavObject(object obj)
        {
            var iRec = obj as GDMIndividualRecord;
            GenChart(iRec, TreeChartKind.ckBoth, true);
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
            fHighlightedStart = DateTime.Now.ToBinary();

            /*if (person == null) {
                //fPersonControl.Visible = false;
            } else {
                //fPersonControl.SetPerson(person);
            }*/

            Invalidate();
        }

        private void SelectBy(TreeChartPerson person, bool needCenter)
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

        private void CenterPerson(TreeChartPerson person, bool animation = true)
        {
            if (person == null) return;

            Rectangle viewport = this.Viewport;
            int widthMax = fModel.ImageWidth - viewport.Width;
            int heightMax = fModel.ImageHeight - viewport.Height;

            int srcX = viewport.Left;
            int srcY = viewport.Top;
            int dstX = Math.Min(Math.Max(0, ((person.PtX) - (viewport.Width / 2))), widthMax);
            int dstY = Math.Min(Math.Max(0, ((person.PtY + (person.Height / 2)) - (viewport.Height / 2))), heightMax);

            if ((srcX != dstX) || (srcY != dstY)) {
                int timeInterval = animation ? 20 : 1;
                fTween.StartTween(UpdateScrollPosition, srcX, srcY, dstX, dstY, TweenAnimation.EaseInOutQuad, timeInterval);
            }
        }

        /*private void SetScroll(int x, int y)
        {
            UpdateScrollPosition(x, y);
        }*/

        #endregion

        #region Print support

        public override ExtSize GetImageSize()
        {
            return fModel.ImageSize;
        }

        public override void RenderImage(RenderTarget target, bool forciblyCentered = false)
        {
            BackgroundMode bgMode = (target == RenderTarget.Printer) ? BackgroundMode.bmImage : BackgroundMode.bmAny;
            ChartDrawMode drawMode = (!forciblyCentered) ? ChartDrawMode.dmStatic : ChartDrawMode.dmStaticCentered;
            InternalDraw(drawMode, bgMode);
        }

        #endregion
    }
}
