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

//define DEBUG_IMAGE

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

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
    public abstract class ITreeControl : BaseObject
    {
        protected readonly ITreeChartBox fChart;

        protected Rectangle fDestRect;
        protected bool fMouseCaptured;
        protected bool fVisible;

        public bool MouseCaptured
        {
            get { return fMouseCaptured; }
        }

        public bool Visible
        {
            get {
                return fVisible;
            }
            set {
                if (fVisible != value) {
                    fVisible = value;
                    fChart.Invalidate();
                }
            }
        }

        public abstract string Tip { get; }
        public abstract int Height { get; }
        public abstract int Width { get; }

        public abstract void UpdateState();
        public abstract void UpdateView();
        public abstract void Draw(Graphics gfx);
        public abstract void MouseDown(int x, int y);
        public abstract void MouseMove(int x, int y);
        public abstract void MouseUp(int x, int y);

        protected ITreeControl(ITreeChartBox chart)
        {
            fChart = chart;
        }

        public virtual bool Contains(int x, int y)
        {
            return fDestRect.Contains(x, y);
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public class TreeChartBox : CustomChart, ITreeChartBox
    {
        #region Subtypes

        private enum ChartControlMode
        {
            ccmDefault,
            ccmDragImage,
            ccmControlsVisible
        }

        private enum MouseAction
        {
            maNone,
            maSelect,
            maExpand,
            maDrag,
            maProperties,
            maHighlight
        }

        private enum MouseEvent
        {
            meDown,
            meMove,
            meUp
        }

        public sealed class TreeControlsList<T> : List<T>, IDisposable where T : ITreeControl
        {
            public void Draw(Graphics gfx)
            {
                if (gfx == null) return;

                for (int i = 0; i < Count; i++) {
                    T ctl = this[i];
                    if (ctl.Visible) ctl.Draw(gfx);
                }
            }

            public void UpdateState()
            {
                for (int i = 0; i < Count; i++) {
                    this[i].UpdateState();
                }
            }

            public void UpdateView()
            {
                for (int i = 0; i < Count; i++) {
                    this[i].UpdateView();
                }
            }

            public ITreeControl Contains(int x, int y)
            {
                for (int i = 0; i < Count; i++) {
                    ITreeControl ctl = this[i];
                    if (ctl.Contains(x, y)) return ctl;
                }

                return null;
            }

            public void MouseDown(int x, int y)
            {
                for (int i = 0; i < Count; i++) {
                    T ctl = this[i];
                    if (ctl.Visible) ctl.MouseDown(x, y);
                }
            }

            public void MouseMove(int x, int y, bool defaultChartMode)
            {
                for (int i = 0; i < Count; i++) {
                    T ctl = this[i];
                    if (ctl.Visible) ctl.MouseMove(x, y);
                }
            }

            public void MouseUp(int x, int y)
            {
                for (int i = 0; i < Count; i++) {
                    T ctl = this[i];
                    if (ctl.Visible) ctl.MouseUp(x, y);
                }
            }

            public void Dispose()
            {
                for (int i = 0; i < Count; i++) {
                    this[i].Dispose();
                }
                Clear();
            }
        }

        #endregion

        #region Private fields

        private readonly TreeChartModel fModel;
        private readonly ToolTip fToolTip;
        private readonly TreeControlsList<ITreeControl> fTreeControls;
        private readonly TweenLibrary fTween;

        private ITreeControl fActiveControl;
        private int fBorderWidth;
        private IContainer fComponents;
        private long fHighlightedStart;
        private ChartControlMode fMode = ChartControlMode.ccmDefault;
        private int fMouseX;
        private int fMouseY;
        private TreeChartOptions fOptions;
        private ChartRenderer fRenderer;
        private TreeChartPerson fSelected;
        private GEDCOMIndividualRecord fSaveSelection;
        private Timer fTimer;
        private bool fTraceKinships;
        private bool fTraceSelected;

        private static readonly object EventPersonModify;
        private static readonly object EventRootChanged;
        private static readonly object EventPersonProperties;

        #endregion

        #region Public properties

        public event PersonModifyEventHandler PersonModify
        {
            add { Events.AddHandler(EventPersonModify, value); }
            remove { Events.RemoveHandler(EventPersonModify, value); }
        }

        public event RootChangedEventHandler RootChanged
        {
            add { Events.AddHandler(EventRootChanged, value); }
            remove { Events.RemoveHandler(EventRootChanged, value); }
        }

        public event MouseEventHandler PersonProperties
        {
            add { Events.AddHandler(EventPersonProperties, value); }
            remove { Events.RemoveHandler(EventPersonProperties, value); }
        }

        public IBaseWindow Base
        {
            get { return fModel.Base; }
            set { fModel.Base = value; }
        }

        public int BorderWidth
        {
            get {
                return fBorderWidth;
            }
            set {
                if (fBorderWidth != value) {
                    fBorderWidth = value;
                    Invalidate();
                }
            }
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

        public new float Scale
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

        static TreeChartBox()
        {
            EventPersonModify = new object();
            EventRootChanged = new object();
            EventPersonProperties = new object();
        }

        public TreeChartBox()
        {
            SetStyle(ControlStyles.AllPaintingInWmPaint | ControlStyles.UserPaint | ControlStyles.OptimizedDoubleBuffer | ControlStyles.ResizeRedraw, true);
            UpdateStyles();

            BorderStyle = BorderStyle.Fixed3D;
            DoubleBuffered = true;
            TabStop = true;
            BackColor = Color.White;

            fModel = new TreeChartModel();
            fRenderer = null;
            fSelected = null;
            fToolTip = new ToolTip();
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
            if (disposing)
            {
                fTween.Dispose();
                fModel.Dispose();

                if (fTreeControls != null) fTreeControls.Dispose();
                if (fComponents != null) fComponents.Dispose();
            }
            base.Dispose(disposing);
        }

        public void SetRenderer(ChartRenderer renderer)
        {
            fRenderer = renderer;
            fModel.SetRenderer(renderer);
        }

        private void InitTimer()
        {
            fComponents = new Container();
            fTimer = new Timer(fComponents);
            fTimer.Interval = 1;
            fTimer.Tick += TickTimer;
            fTimer.Stop();
            fTimer.Enabled = false;
            fTimer.Enabled = true;
        }

        private void TickTimer(object sender, EventArgs e)
        {
            if (fModel.HighlightedPerson == null) return;

            DateTime st = DateTime.FromBinary(fHighlightedStart);
            DateTime cur = DateTime.Now;
            TimeSpan d = cur - st;

            if (d.TotalSeconds >= 1/* && !fPersonControl.Visible*/)
            {
                fModel.HighlightedPerson = null;
                //fPersonControl.Visible = true;
                Invalidate();
            }
        }

        public void SetScale(float value)
        {
            fModel.Scale = value;

            RecalcChart();

            if (fTraceSelected && fSelected != null)
            {
                CenterPerson(fSelected, false);
            }

            fTreeControls.UpdateState();
        }

        public void GenChart(GEDCOMIndividualRecord iRec, TreeChartKind kind, bool rootCenter)
        {
            if (iRec == null) return;

            try
            {
                fSelected = null;

                fModel.GenChart(iRec, kind, rootCenter);

                RecalcChart();

                if (rootCenter) CenterPerson(fModel.Root, false);

                NavAdd(iRec);
                DoRootChanged(fModel.Root);
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TreeChartBox.GenChart(): " + ex.Message);
            }
        }

        public void RefreshTree()
        {
            try {
                if (fModel.Root == null) return;

                GEDCOMIndividualRecord rootRec = fModel.Root.Rec;

                SaveSelection();
                GenChart(rootRec, fModel.Kind, false);
                RestoreSelection();
            }
            catch (Exception ex)
            {
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

            try
            {
                fModel.KinRoot = fSelected;
                RecalcChart(noRedraw);
            }
            catch (Exception ex)
            {
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

            bool bgImage = ((BackgroundImage != null) &&
                            (background == BackgroundMode.bmAny ||
                             background == BackgroundMode.bmImage));

            if (bgImage) {
                /*var imgRect = new Rectangle(0, 0, fImageWidth, fImageHeight);

                    using (Brush textureBrush = new TextureBrush(BackgroundImage, WrapMode.Tile)) {
                        gfx.FillRectangle(textureBrush, imgRect);
                    }*/
            } else {
                bool bgFill = (background == BackgroundMode.bmAny ||
                               background == BackgroundMode.bmImage);

                if (bgFill) {
                    fRenderer.DrawRectangle(null, UIHelper.ConvertColor(BackColor), 0, 0, fModel.ImageWidth, fModel.ImageHeight);
                }
            }
        }

        private void InternalDraw(ChartDrawMode drawMode, BackgroundMode background)
        {
            // drawing relative offset of tree on graphics
            int spx = 0;
            int spy = 0;

            if (drawMode == ChartDrawMode.dmInteractive) {
                /*Rectangle viewPort = GetImageViewPort();
                fSPX = -viewPort.Left;
                fSPY = -viewPort.Top;*/

                spx += fBorderWidth - -AutoScrollPosition.X;
                spy += fBorderWidth - -AutoScrollPosition.Y;

                Size sz = ClientSize;

                if (fModel.ImageWidth < sz.Width) {
                    spx += (sz.Width - fModel.ImageWidth) / 2;
                }

                if (fModel.ImageHeight < sz.Height) {
                    spy += (sz.Height - fModel.ImageHeight) / 2;
                }

                fModel.VisibleArea = GetSourceImageRegion();
            } else {
                if (drawMode == ChartDrawMode.dmStaticCentered) {
                    Size sz = ClientSize;

                    if (fModel.ImageWidth < sz.Width) {
                        spx += (sz.Width - fModel.ImageWidth) / 2;
                    }

                    if (fModel.ImageHeight < sz.Height) {
                        spy += (sz.Height - fModel.ImageHeight) / 2;
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

            fModel.Draw(fModel.Root, fModel.Kind, drawMode);
        }

        #endregion

        #region Sizes and adjustment routines

        public ExtRect GetClientRect()
        {
            Rectangle rt = this.ClientRectangle;
            return ExtRect.CreateBounds(rt.Left, rt.Top, rt.Width, rt.Height);
        }

        public void RecalcChart(bool noRedraw = false)
        {
            float fsz = (float)Math.Round(fOptions.DefFontSize * fModel.Scale);
            fModel.DrawFont = AppHost.GfxProvider.CreateFont(fOptions.DefFontName, fsz, false);

            Graphics gfx = null;
            if (fRenderer is TreeChartGfxRenderer) {
                gfx = CreateGraphics();
                fRenderer.SetTarget(gfx, false);
            }

            try {
                fModel.RecalcChart(noRedraw);
            } finally {
                if (fRenderer is TreeChartGfxRenderer && gfx != null) {
                    gfx.Dispose();
                }
            }

            var imageSize = GetImageSize();
            AdjustViewport(imageSize, noRedraw);
        }

        private Rectangle GetInsideViewPort(bool includePadding)
        {
            int left = 0;
            int top = 0;
            int width = ClientSize.Width;
            int height = ClientSize.Height;

            if (includePadding)
            {
                left += Padding.Left;
                top += Padding.Top;
                width -= Padding.Horizontal;
                height -= Padding.Vertical;
            }

            return new Rectangle(left, top, width, height);
        }

        private Rectangle GetImageViewPort()
        {
            Rectangle viewPort;

            var imageSize = GetImageSize();
            if (!imageSize.IsEmpty) {
                Rectangle innerRectangle = GetInsideViewPort(true);

                int x = !HScroll ? (innerRectangle.Width - (imageSize.Width + Padding.Horizontal)) / 2 : 0;
                int y = !VScroll ? (innerRectangle.Height - (imageSize.Height + Padding.Vertical)) / 2 : 0;

                int width = Math.Min(imageSize.Width - Math.Abs(AutoScrollPosition.X), innerRectangle.Width);
                int height = Math.Min(imageSize.Height - Math.Abs(AutoScrollPosition.Y), innerRectangle.Height);

                viewPort = new Rectangle(x + innerRectangle.Left, y + innerRectangle.Top, width, height);
            } else {
                viewPort = Rectangle.Empty;
            }

            return viewPort;
        }

        private ExtRect GetSourceImageRegion()
        {
            ExtRect region;

            var imageSize = GetImageSize();
            if (!imageSize.IsEmpty) {
                Rectangle viewPort = GetImageViewPort();
                region = ExtRect.CreateBounds(-AutoScrollPosition.X, -AutoScrollPosition.Y, viewPort.Width, viewPort.Height);
            } else {
                region = ExtRect.Empty;
            }

            return region;
        }

        #endregion

        #region Event processing

        private void DoPersonModify(PersonModifyEventArgs eArgs)
        {
            var eventHandler = (PersonModifyEventHandler)Events[EventPersonModify];
            if (eventHandler != null)
                eventHandler(this, eArgs);
        }

        private void DoRootChanged(TreeChartPerson person)
        {
            var eventHandler = (RootChangedEventHandler)Events[EventRootChanged];
            if (eventHandler != null)
                eventHandler(this, person);
        }

        private void DoPersonProperties(MouseEventArgs eArgs)
        {
            var eventHandler = (MouseEventHandler)Events[EventPersonProperties];
            if (eventHandler != null)
                eventHandler(this, eArgs);
        }

        protected override void OnKeyDown(KeyEventArgs e)
        {
            switch (e.KeyCode)
            {
                case Keys.Add:
                case Keys.Oemplus:
                    SetScale(fModel.Scale + 0.05f);
                    e.Handled = true;
                    break;

                case Keys.Subtract:
                case Keys.OemMinus:
                    SetScale(fModel.Scale - 0.05f);
                    e.Handled = true;
                    break;
            }

            base.OnKeyDown(e);
        }

        protected override void OnResize(EventArgs e)
        {
            SaveSelection();

            var imageSize = GetImageSize();
            AdjustViewport(imageSize);
            fTreeControls.UpdateView();

            RestoreSelection();

            base.OnResize(e);
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            Graphics gfx = e.Graphics;
            fRenderer.SetTarget(gfx, false);
            InternalDraw(ChartDrawMode.dmInteractive, BackgroundMode.bmAny);

            // interactive controls
            fTreeControls.Draw(gfx);

            base.OnPaint(e);
        }

        protected override void OnDoubleClick(EventArgs e)
        {
            TreeChartPerson p = fSelected;
            DoPersonModify(new PersonModifyEventArgs(p));

            base.OnDoubleClick(e);
        }

        protected override void OnMouseWheel(MouseEventArgs e)
        {
            if (ModifierKeys == Keys.Control) {
                float newScale = (e.Delta > 0) ? fModel.Scale + 0.05f : fModel.Scale - 0.05f;
                SetScale(newScale);
            } else {
                base.OnMouseWheel(e);
            }
        }

        private sealed class MouseActionRet
        {
            public readonly MouseAction Action;
            public readonly TreeChartPerson Person;

            public MouseActionRet(MouseAction action, TreeChartPerson person)
            {
                Action = action;
                Person = person;
            }
        }

        private MouseActionRet GetMouseAction(MouseEventArgs e, MouseEvent mouseEvent)
        {
            MouseAction action = MouseAction.maNone;
            TreeChartPerson person = null;

            ExtPoint offsets = fModel.GetOffsets();
            int aX = e.X - offsets.X;
            int aY = e.Y - offsets.Y;

            int num = fModel.Persons.Count;
            for (int i = 0; i < num; i++) {
                TreeChartPerson p = fModel.Persons[i];
                ExtRect persRt = p.Rect;

                if (persRt.Contains(aX, aY)) {
                    person = p;

                    if (e.Button == MouseButtons.Left && mouseEvent == MouseEvent.meDown)
                    {
                        action = MouseAction.maSelect;
                        break;
                    }
                    else if (e.Button == MouseButtons.Right && mouseEvent == MouseEvent.meUp)
                    {
                        action = MouseAction.maProperties;
                        break;
                    }
                    else if (mouseEvent == MouseEvent.meMove)
                    {
                        action = MouseAction.maHighlight;
                        break;
                    }
                }

                ExtRect expRt = TreeChartModel.GetExpanderRect(persRt);
                if ((e.Button == MouseButtons.Left && mouseEvent == MouseEvent.meUp) && expRt.Contains(aX, aY)) {
                    person = p;
                    action = MouseAction.maExpand;
                    break;
                }
            }

            if (action == MouseAction.maNone && person == null) {
                if (e.Button == MouseButtons.Right && mouseEvent == MouseEvent.meDown) {
                    action = MouseAction.maDrag;
                }
            }

            return new MouseActionRet(action, person);
        }

        protected override void OnMouseDown(MouseEventArgs e)
        {
            fMouseX = e.X;
            fMouseY = e.Y;

            switch (fMode) {
                case ChartControlMode.ccmDefault:
                    var mAct = GetMouseAction(e, MouseEvent.meDown);

                    switch (mAct.Action) {
                        case MouseAction.maSelect:
                            SelectBy(mAct.Person, true);
                            break;

                        case MouseAction.maDrag:
                            Cursor = Cursors.SizeAll;
                            fMode = ChartControlMode.ccmDragImage;
                            break;
                    }
                    break;

                case ChartControlMode.ccmDragImage:
                    break;

                case ChartControlMode.ccmControlsVisible:
                    fTreeControls.MouseDown(e.X, e.Y);
                    break;
            }

            base.OnMouseDown(e);
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            switch (fMode)
            {
                case ChartControlMode.ccmDefault:
                    var mAct = GetMouseAction(e, MouseEvent.meMove);

                    if (mAct.Action == MouseAction.maHighlight) {
                        SetHighlight(mAct.Person);
                    } else {
                        SetHighlight(null);

                        ITreeControl ctl = fTreeControls.Contains(e.X, e.Y);

                        if (ctl != null) {
                            fMode = ChartControlMode.ccmControlsVisible;
                            ctl.Visible = true;
                            ctl.MouseMove(e.X, e.Y);
                            fActiveControl = ctl;

                            var pt = new Point(e.X + Left, e.Y + Top);
                            fToolTip.Show(ctl.Tip, this, pt, 1500);
                        }
                    }
                    break;

                case ChartControlMode.ccmDragImage:
                    AdjustScroll(-(e.X - fMouseX), -(e.Y - fMouseY));
                    fMouseX = e.X;
                    fMouseY = e.Y;
                    break;

                case ChartControlMode.ccmControlsVisible:
                    if (fActiveControl != null) {
                        if (!(fActiveControl.Contains(e.X, e.Y) || fActiveControl.MouseCaptured)) {
                            fMode = ChartControlMode.ccmDefault;
                            fActiveControl.Visible = false;
                            fToolTip.Hide(this);
                            fActiveControl = null;
                        } else {
                            fActiveControl.MouseMove(e.X, e.Y);
                        }
                    }
                    break;
            }

            base.OnMouseMove(e);
        }

        protected override void OnMouseUp(MouseEventArgs e)
        {
            switch (fMode) {
                case ChartControlMode.ccmDefault:
                    var mAct = GetMouseAction(e, MouseEvent.meUp);

                    switch (mAct.Action) {
                        case MouseAction.maNone:
                            break;

                        case MouseAction.maProperties:
                            SelectBy(mAct.Person, false);
                            if (fSelected == mAct.Person && fSelected.Rec != null)
                            {
                                DoPersonProperties(new MouseEventArgs(e.Button, 1, e.X, e.Y, 0));
                            }
                            break;

                        case MouseAction.maExpand:
                            DoRootChanged(mAct.Person);
                            GenChart(mAct.Person.Rec, TreeChartKind.ckBoth, true);
                            break;
                    }
                    break;

                case ChartControlMode.ccmDragImage:
                    Cursor = Cursors.Default;
                    fMode = ChartControlMode.ccmDefault;
                    break;

                case ChartControlMode.ccmControlsVisible:
                    fTreeControls.MouseUp(e.X, e.Y);
                    break;
            }

            base.OnMouseUp(e);
        }

        #endregion

        #region Navigation

        protected override void SetNavObject(object obj)
        {
            var iRec = obj as GEDCOMIndividualRecord;
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

        public void SelectByRec(GEDCOMIndividualRecord iRec)
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

            int dstX = ((person.PtX) - (ClientSize.Width / 2));
            int dstY = ((person.PtY + (person.Height / 2)) - (ClientSize.Height / 2));

            if (dstX < 0) dstX = dstX + (0 - dstX);
            if (dstY < 0) dstY = dstY + (0 - dstY);

            int oldX = Math.Abs(AutoScrollPosition.X);
            int oldY = Math.Abs(AutoScrollPosition.Y);

            if ((oldX == dstX) && (oldY == dstY)) return;

            if (animation) {
                fTween.StartTween(SetScroll, oldX, oldY, dstX, dstY, TweenAnimation.EaseInOutQuad, 20);
            } else {
                //fTween.StopTween();
                //SetScroll(dstX, dstY);
                fTween.StartTween(SetScroll, oldX, oldY, dstX, dstY, TweenAnimation.EaseInOutQuad, 1);
            }
        }

        private void SetScroll(int x, int y)
        {
            TweenDelegate invoker = delegate(int newX, int newY) {
                UpdateScrollPosition(newX, newY);
            };

            if (InvokeRequired) {
                Invoke(invoker, x, y);
            } else {
                invoker(x, y);
            }
        }

        #endregion

        #region Print support

        public override ExtSize GetImageSize()
        {
            return new ExtSize(fModel.ImageWidth, fModel.ImageHeight);
        }

        public override void RenderStaticImage(Graphics gfx, bool printer)
        {
            BackgroundMode bgMode = (printer) ? BackgroundMode.bmImage : BackgroundMode.bmAny;

            fRenderer.SetTarget(gfx, false);
            RenderStatic(bgMode);
        }

        public void RenderStatic(BackgroundMode background, bool centered = false)
        {
            ChartDrawMode drawMode = (!centered) ? ChartDrawMode.dmStatic : ChartDrawMode.dmStaticCentered;
            InternalDraw(drawMode, background);
        }

        #endregion
    }
}
