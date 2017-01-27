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

//define DEBUG_IMAGE

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Imaging;
using System.Text.RegularExpressions;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Kinships;
using GKCore.Options;
using GKCore.Types;

namespace GKUI.Charts
{
    public delegate void ThumbMoved(int position);

    public delegate void PersonModifyEventHandler(object sender, PersonModifyEventArgs eArgs);

    public delegate void RootChangedEventHandler(object sender, TreeChartPerson person);

    /// <summary>
    /// 
    /// </summary>
    public class TreeChartBox : CustomChart
    {
        private const float HIGHLIGHTED_VAL = 0.1f;

        public const int DEF_MARGINS = 24;
        public const int DEF_SPOUSE_DISTANCE = 10;
        public const int DEF_BRANCH_DISTANCE = 40;
        public const int DEF_LEVEL_DISTANCE = 46;

        // Specifies the interior spacing of a node.
        public const int DEF_PERSON_NODE_PADDING = 10;

        #region Subtypes
        
        private enum ChartControlMode
        {
            ccmDefault,
            ccmDragImage,
            ccmControlsVisible
        }

        public enum ChartKind
        {
            ckAncestors,
            ckDescendants,
            ckBoth
        }

        public enum DrawMode
        {
            dmInteractive,
            dmStatic,
            dmStaticCentered
        }

        public enum MouseAction
        {
            maNone,
            maProperties,
            maExpand
        }

        public enum BackgroundMode
        {
            bmNone,
            bmImage,
            bmFill,
            bmAny
        }
        
        #endregion

        #region Private fields

        private readonly ChartFilter fFilter;
        private readonly PersonList fPersons;
        private readonly List<string> fPreparedFamilies;
        internal readonly List<string> fPreparedIndividuals;
        private readonly TreeChartRenderer fRenderer;
        private readonly ScaleControl fScaleControl;
        private readonly ToolTip fToolTip;
        
        private IBaseWindow fBase;
        private int fBranchDistance;
        private bool fCertaintyIndex;
        private int fDepthLimit;
        private Font fDrawFont;
        private KinshipsGraph fGraph;
        private ChartKind fKind;
        private TreeChartPerson fKinRoot;
        private int fLevelDistance;
        private int fMargins;
        private int fNodePadding;
        private TreeChartOptions fOptions;
        private bool fPathDebug;
        private TreeChartPerson fRoot;
        private float fScale;
        private TreeChartPerson fSelected;
        private GEDCOMIndividualRecord fSaveSelection;
        private ShieldState fShieldState;
        private Bitmap[] fSignsPic;
        private int fSpouseDistance;
        private bool fTraceKinships;
        private bool fTraceSelected;
        private GEDCOMTree fTree;
        
        private int fBorderWidth;
        private ExtRect fTreeBounds;

        protected int fImageHeight;
        protected int fImageWidth;
        private Size fImageSize;
        internal int fSPX; // drawing relative offset of tree on graphics
        internal int fSPY;
        protected Rectangle fVisibleArea;

        private Pen fLinePen;
        private Pen fDecorativeLinePen;
        private SolidBrush fSolidBlack;

        private int fMouseX;
        private int fMouseY;
        private ChartControlMode fMode = ChartControlMode.ccmDefault;
        
        private TreeChartPerson fHighlightedPerson;
        private long fHighlightedStart;
        //private PersonControl fPersonControl;

        private IContainer fComponents;
        private Timer fTimer;
        
        private Bitmap fExpPic;

        private static readonly object EventPersonModify;
        private static readonly object EventRootChanged;
        private static readonly object EventPersonProperties;
        
        #endregion

        #region Public properties
        
        public event PersonModifyEventHandler PersonModify
        {
            add { base.Events.AddHandler(TreeChartBox.EventPersonModify, value); }
            remove { base.Events.RemoveHandler(TreeChartBox.EventPersonModify, value); }
        }

        public event RootChangedEventHandler RootChanged
        {
            add { base.Events.AddHandler(TreeChartBox.EventRootChanged, value); }
            remove { base.Events.RemoveHandler(TreeChartBox.EventRootChanged, value); }
        }

        public event MouseEventHandler PersonProperties
        {
            add { base.Events.AddHandler(TreeChartBox.EventPersonProperties, value); }
            remove { base.Events.RemoveHandler(TreeChartBox.EventPersonProperties, value); }
        }

        public IBaseWindow Base
        {
            get { return fBase; }
            set {
                fBase = value;
                fTree = fBase.Tree;
                fGraph = new KinshipsGraph(fBase.Context);
            }
        }

        public int BorderWidth
        {
            get { return fBorderWidth; }
            set {
                if (fBorderWidth == value) return;

                fBorderWidth = value;
                Invalidate();
            }
        }

        public int BranchDistance
        {
            get { return fBranchDistance; }
            set { fBranchDistance = value; }
        }

        public bool CertaintyIndex
        {
            get { return fCertaintyIndex; }
            set {
                fCertaintyIndex = value;
                Invalidate();
            }
        }
        
        public int DepthLimit
        {
            get { return fDepthLimit; }
            set { fDepthLimit = value; }
        }

        public Font DrawFont
        {
            get { return fDrawFont; }
        }

        public ChartFilter Filter
        {
            get { return fFilter; }
        }

        public Size ImageSize
        {
            get { return fImageSize; }
        }

        public int IndividualsCount
        {
            get { return fPreparedIndividuals.Count; }
        }

        public ChartKind Kind
        {
            get { return fKind; }
            set { fKind = value; }
        }

        public int Margins
        {
            get { return fMargins; }
            set { fMargins = value; }
        }

        public int NodePadding
        {
            get { return fNodePadding; }
        }

        public TreeChartOptions Options
        {
            get { return fOptions; }
            set { fOptions = value; }
        }

        public bool PathDebug
        {
            get { return fPathDebug; }
            set { fPathDebug = value; }
        }

        public TreeChartPerson Root
        {
            get { return fRoot; }
        }

        public new float Scale
        {
            get { return fScale; }
        }

        public new ScaleControl ScaleControl
        {
            get { return fScaleControl; }
        }

        public TreeChartPerson Selected
        {
            get { return fSelected; }
            set { SetSelected(value); }
        }

        public ShieldState ShieldState
        {
            get { return fShieldState; }
            set { fShieldState = value; }
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
            TreeChartBox.EventPersonModify = new object();
            TreeChartBox.EventRootChanged = new object();
            TreeChartBox.EventPersonProperties = new object();
        }

        public TreeChartBox(TreeChartRenderer renderer) : base()
        {
            base.SetStyle(ControlStyles.AllPaintingInWmPaint | ControlStyles.UserPaint | ControlStyles.OptimizedDoubleBuffer | ControlStyles.ResizeRedraw, true);
            base.UpdateStyles();

            base.BorderStyle = BorderStyle.Fixed3D;
            base.DoubleBuffered = true;
            base.TabStop = true;
            base.BackColor = Color.White;

            InitSigns();

            fRenderer = renderer;
            fPersons = new PersonList(true);
            fFilter = new ChartFilter();
            fBranchDistance = DEF_BRANCH_DISTANCE;
            fLevelDistance = DEF_LEVEL_DISTANCE;
            fSpouseDistance = DEF_SPOUSE_DISTANCE;
            fMargins = DEF_MARGINS;
            fDepthLimit = -1;
            fSelected = null;
            fScale = 1.0f;
            fTraceSelected = true;
            fGraph = null;

            fPreparedFamilies = new List<string>();
            fPreparedIndividuals = new List<string>();

            fScaleControl = new ScaleControl(this);
            //fPersonControl = new PersonControl(this);
            fToolTip = new ToolTip();

            InitTimer();
            InitGraphics();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                DoneGraphics();

                fGraph.Dispose();
                fFilter.Dispose();
                fPersons.Dispose();
                DoneSigns();

                if (fDrawFont != null) fDrawFont.Dispose();
                if (fScaleControl != null) fScaleControl.Dispose();

                if (fComponents != null) fComponents.Dispose();
            }
            base.Dispose(disposing);
        }

        private static Bitmap PrepareImage(Bitmap source, bool makeTransp)
        {
            if (source == null) return null;

            try {
                var result = (Bitmap)source.Clone();

                if (makeTransp)
                {
                    #if __MonoCS__
                    result.MakeTransparent(); // FIXME: don't work
                    #else
                    result.MakeTransparent(source.GetPixel(0, 0));
                    #endif
                }

                return result;
            } catch (Exception ex) {
                Logger.LogWrite("TreeChartBox.PrepareImage(): " + ex.Message);
                return null;
            }
        }

        private void InitSigns()
        {
            try {
                fSignsPic = new Bitmap[9];

                fSignsPic[0] = PrepareImage(GKResources.iTGGeorgeCross, true);
                fSignsPic[1] = PrepareImage(GKResources.iTGSoldier, true);
                fSignsPic[2] = PrepareImage(GKResources.iTGSoldierFall, true);
                fSignsPic[3] = PrepareImage(GKResources.iTGVeteranRear, true);
                fSignsPic[4] = PrepareImage(GKResources.iTGBarbedWire, true);
                fSignsPic[5] = PrepareImage(GKResources.iTGIslamSym, false);
                fSignsPic[6] = PrepareImage(GKResources.iTGLatinCross, false);
                fSignsPic[7] = PrepareImage(GKResources.iTGOrthodoxCross, false);
                fSignsPic[8] = PrepareImage(GKResources.iTGOldRitualCross, false);

                fExpPic = PrepareImage(GKResources.iExpand, true);
            } catch (Exception ex) {
                Logger.LogWrite("TreeChartBox.InitSigns(): " + ex.Message);
            }
        }

        // FIXME
        private void DoneSigns()
        {
            // dummy
        }

        private void InitTimer()
        {
            fComponents = new Container();
            fTimer = new Timer(fComponents);
            fTimer.Interval = 1;
            fTimer.Tick += timer_Tick;
            fTimer.Stop();
            fTimer.Enabled = false;
            fTimer.Enabled = true;
        }

        private void timer_Tick(object sender, EventArgs e)
        {
            if (fHighlightedPerson == null) return;

            DateTime st = DateTime.FromBinary(fHighlightedStart);
            DateTime cur = DateTime.Now;
            TimeSpan d = cur - st;

            if (d.Seconds >= 1/* && !this.fPersonControl.Visible*/)
            {
                fHighlightedPerson = null;
                //this.fPersonControl.Visible = true;
                Invalidate();
            }
        }

        public void SetScale(float value)
        {
            if (value < 0.5 || value > 1.5) return;
            fScale = value;

            fScaleControl.ThumbPos = (int)Math.Round((value - 0.5f) *
                                                     fScaleControl.DCount);

            RecalcChart();

            if (fTraceSelected && fSelected != null)
            {
                CenterPerson(fSelected, false);
            }
        }

        public void GenChart(GEDCOMIndividualRecord iRec, ChartKind kind, bool rootCenter)
        {
            if (iRec == null) return;
            
            try
            {
                fKind = kind;
                fSelected = null;
                fPersons.Clear();
                fGraph.Clear();
                DoFilter(iRec);
                fRoot = null;
                fPreparedIndividuals.Clear();

                switch (fKind) {
                    case ChartKind.ckAncestors:
                        fPreparedFamilies.Clear();
                        fRoot = DoAncestorsStep(null, iRec, 1, false);
                        break;
                        
                    case ChartKind.ckDescendants:
                        fPreparedFamilies.Clear();
                        fRoot = DoDescendantsStep(null, iRec, 1);
                        break;

                    case ChartKind.ckBoth:
                        fPreparedFamilies.Clear();
                        fRoot = DoAncestorsStep(null, iRec, 1, false);
                        fPreparedFamilies.Clear();
                        DoDescendantsStep(null, iRec, 1);
                        break;
                }

                fKinRoot = fRoot;

                RecalcChart();

                if (rootCenter) CenterPerson(fRoot);
                
                NavAdd(iRec);
                DoRootChanged(fRoot);
            }
            catch (Exception ex)
            {
                fBase.Host.LogWrite("TreeChartBox.GenChart(): " + ex.Message);
            }
        }

        public void RefreshTree()
        {
            try {
                if (fRoot == null) return;

                GEDCOMIndividualRecord rootRec = fRoot.Rec;
                
                SaveSelection();
                GenChart(rootRec, fKind, false);
                RestoreSelection();
            }
            catch (Exception ex)
            {
                fBase.Host.LogWrite("TreeChartBox.RefreshTree(): " + ex.Message);
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
                fKinRoot = fSelected;
                RecalcChart(noRedraw);
            }
            catch (Exception ex)
            {
                fBase.Host.LogWrite("TreeChartBox.RebuildKinships(): " + ex.Message);
            }
        }

        #endregion

        #region Tree walking

        private bool hasMediaFail = false;

        private TreeChartPerson AddDescPerson(TreeChartPerson parent, GEDCOMIndividualRecord iRec, bool outsideKin, int generation)
        {
            try
            {
                TreeChartPerson result;

                if (fRoot != null && fRoot.Rec == iRec) {
                    result = fRoot;
                    result.Parent = parent;
                } else {
                    result = new TreeChartPerson(this);
                    result.BuildBy(iRec, ref hasMediaFail);
                    result.Generation = generation;
                    result.Parent = parent;
                    fPersons.Add(result);

                    if (fOptions.Kinship && iRec != null) {
                        result.Node = fGraph.AddIndividual(iRec);
                    }

                    if (!outsideKin && parent != null) {
                        parent.AddChild(result);
                    }
                }

                result.OutsideKin = outsideKin;
                result.SetFlag(PersonFlag.pfDescWalk);

                return result;
            }
            catch (Exception ex)
            {
                fBase.Host.LogWrite("TreeChartBox.AddDescPerson(): " + ex.Message);
                throw;
            }
        }

        private TreeChartPerson DoAncestorsStep(TreeChartPerson aChild, GEDCOMIndividualRecord aPerson, int generation, bool dupFlag)
        {
            try
            {
                TreeChartPerson result = null;

                if (aPerson != null)
                {
                    result = new TreeChartPerson(this);
                    result.BuildBy(aPerson, ref hasMediaFail);
                    result.Generation = generation;
                    result.SetFlag(PersonFlag.pfAncWalk);
                    fPersons.Add(result);

                    if (aChild != null)
                    {
                        result.AddChild(aChild);
                    }

                    if (fOptions.Kinship)
                    {
                        result.Node = fGraph.AddIndividual(aPerson);
                    }

                    if ((fDepthLimit <= -1 || generation != fDepthLimit) && aPerson.ChildToFamilyLinks.Count > 0 && !dupFlag)
                    {
                        GEDCOMFamilyRecord family = aPerson.ChildToFamilyLinks[0].Family;

                        bool isDup = (fPreparedFamilies.IndexOf(family.XRef) >= 0);
                        if (!isDup) fPreparedFamilies.Add(family.XRef);

                        if (GKUtils.IsRecordAccess(family.Restriction, fShieldState))
                        {
                            GEDCOMIndividualRecord iFather = family.GetHusband();
                            GEDCOMIndividualRecord iMother = family.GetWife();

                            bool divorced = (family.GetTagStringValue("_STAT") == "NOTMARR");

                            if (iFather != null && GKUtils.IsRecordAccess(iFather.Restriction, fShieldState))
                            {
                                result.Father = DoAncestorsStep(result, iFather, generation + 1, isDup);
                                if (result.Father != null)
                                {
                                    result.Father.Divorced = divorced;
                                    result.Father.IsDup = isDup;
                                    if (fOptions.Kinship)
                                    {
                                        fGraph.AddRelation(result.Node, result.Father.Node, RelationKind.rkParent, RelationKind.rkChild);
                                    }
                                }
                            } else {
                                result.Father = null;
                            }

                            if (iMother != null && GKUtils.IsRecordAccess(iMother.Restriction, fShieldState))
                            {
                                result.Mother = DoAncestorsStep(result, iMother, generation + 1, isDup);
                                if (result.Mother != null)
                                {
                                    result.Mother.Divorced = divorced;
                                    result.Mother.IsDup = isDup;
                                    if (fOptions.Kinship)
                                    {
                                        fGraph.AddRelation(result.Node, result.Mother.Node, RelationKind.rkParent, RelationKind.rkChild);
                                    }
                                }
                            } else {
                                result.Mother = null;
                            }

                            if (result.Father != null && result.Mother != null && fOptions.Kinship)
                            {
                                fGraph.AddRelation(result.Father.Node, result.Mother.Node, RelationKind.rkSpouse, RelationKind.rkSpouse);
                            }
                        }
                    } else {
                        if (aPerson.ChildToFamilyLinks.Count > 0) {
                            result.SetFlag(PersonFlag.pfHasInvAnc);
                        }
                    }
                    
                    if (aPerson.GetTotalChildsCount() > 1 || aPerson.SpouseToFamilyLinks.Count > 1) {
                        result.SetFlag(PersonFlag.pfHasInvDesc);
                    }
                }

                return result;
            }
            catch (Exception ex)
            {
                fBase.Host.LogWrite("TreeChartBox.DoAncestorsStep(): " + ex.Message);
                throw;
            }
        }

        private TreeChartPerson DoDescendantsStep(TreeChartPerson parent, GEDCOMIndividualRecord person, int level)
        {
            try
            {
                TreeChartPerson result = null;
                if (person != null && (!fOptions.ChildlessExclude || level <= 1 || person.SpouseToFamilyLinks.Count != 0 || !fBase.Context.IsChildless(person)))
                {
                    FilterGroupMode sourceMode = fFilter.SourceMode;

                    switch (sourceMode)
                    {
                        case FilterGroupMode.All:
                            break;

                        case FilterGroupMode.None:
                            if (person.SourceCitations.Count != 0) {
                                return null;
                            }
                            break;
                            
                        case FilterGroupMode.Any:
                            if (person.SourceCitations.Count == 0) {
                                return null;
                            }
                            break;

                        case FilterGroupMode.Selected:
                            GEDCOMSourceRecord filterSource;
                            if (fFilter.SourceRef == "") {
                                filterSource = null;
                            } else {
                                filterSource = fTree.XRefIndex_Find(fFilter.SourceRef) as GEDCOMSourceRecord;
                            }
                            if (person.IndexOfSource(filterSource) < 0) {
                                return null;
                            }
                            break;
                    }

                    ChartFilter.BranchCutType branchCut = fFilter.BranchCut;
                    if (branchCut != ChartFilter.BranchCutType.None)
                    {
                        if (!(bool)person.ExtData)
                        {
                            return null;
                        }
                    }

                    TreeChartPerson res = AddDescPerson(parent, person, false, level);
                    result = res;

                    int num = person.SpouseToFamilyLinks.Count;
                    for (int i = 0; i < num; i++)
                    {
                        GEDCOMFamilyRecord family = person.SpouseToFamilyLinks[i].Family;

                        // protection against invalid third-party files
                        if (family == null) {
                            fBase.Host.LogWrite("TreeChartBox.DoDescendantsStep(): null pointer to family");
                            continue;
                        }

                        bool isDup = (fPreparedFamilies.IndexOf(family.XRef) >= 0);
                        if (!isDup) fPreparedFamilies.Add(family.XRef);

                        if (GKUtils.IsRecordAccess(family.Restriction, fShieldState))
                        {
                            TreeChartPerson resParent = null;
                            GEDCOMSex sex = person.Sex;
                            TreeChartPerson ft = null;
                            TreeChartPerson mt = null;
                            PersonFlag descFlag = PersonFlag.pfDescByFather;

                            bool invalidSpouse = false;

                            switch (sex) {
                                case GEDCOMSex.svFemale:
                                    {
                                        GEDCOMIndividualRecord sp = family.GetHusband();
                                        resParent = AddDescPerson(null, sp, true, level);
                                        resParent.Sex = GEDCOMSex.svMale;
                                        ft = resParent;
                                        mt = res;
                                        descFlag = PersonFlag.pfDescByFather;
                                        break;
                                    }

                                case GEDCOMSex.svMale:
                                    {
                                        GEDCOMIndividualRecord sp = family.GetWife();
                                        resParent = AddDescPerson(null, sp, true, level);
                                        resParent.Sex = GEDCOMSex.svFemale;
                                        ft = res;
                                        mt = resParent;
                                        descFlag = PersonFlag.pfDescByMother;
                                        break;
                                    }

                                case GEDCOMSex.svNone:
                                case GEDCOMSex.svUndetermined:
                                    invalidSpouse = true;
                                    fBase.Host.LogWrite("TreeChartBox.DoDescendantsStep(): sex of spouse is undetermined");
                                    break;
                            }

                            if (resParent != null)
                            {
                                if (fOptions.Kinship)
                                {
                                    fGraph.AddRelation(res.Node, resParent.Node, RelationKind.rkSpouse, RelationKind.rkSpouse);
                                }

                                res.AddSpouse(resParent);
                                resParent.BaseSpouse = res;
                                resParent.BaseFamily = family;
                                
                                if (resParent.Rec != null) {
                                    if (resParent.Rec.ChildToFamilyLinks.Count > 0) {
                                        resParent.SetFlag(PersonFlag.pfHasInvAnc);
                                    }

                                    if (resParent.Rec.SpouseToFamilyLinks.Count > 1) {
                                        resParent.SetFlag(PersonFlag.pfHasInvDesc);
                                    }
                                }
                            }
                            else
                            {
                                resParent = res;
                            }

                            if (invalidSpouse) {
                                continue;
                            }

                            ft.IsDup = isDup;
                            mt.IsDup = isDup;

                            if ((fDepthLimit <= -1 || level != fDepthLimit) && (!isDup))
                            {
                                int num2 = family.Childrens.Count;
                                for (int j = 0; j < num2; j++)
                                {
                                    var childRec = family.Childrens[j].Value as GEDCOMIndividualRecord;

                                    // protection against invalid third-party files
                                    if (childRec == null) {
                                        fBase.Host.LogWrite("TreeChartBox.DoDescendantsStep(): null pointer to child");
                                        continue;
                                    }

                                    if (GKUtils.IsRecordAccess(childRec.Restriction, fShieldState))
                                    {
                                        TreeChartPerson child = DoDescendantsStep(resParent, childRec, level + 1);
                                        if (child != null)
                                        {
                                            child.Father = ft;
                                            child.Mother = mt;
                                            //int d = (int)desc_flag;
                                            child.SetFlag(descFlag);
                                            if (fOptions.Kinship)
                                            {
                                                fGraph.AddRelation(child.Node, ft.Node, RelationKind.rkParent, RelationKind.rkChild);
                                                fGraph.AddRelation(child.Node, mt.Node, RelationKind.rkParent, RelationKind.rkChild);
                                            }
                                        }
                                    }
                                }
                            } else {
                                if (family.Childrens.Count > 0) {
                                    ft.SetFlag(PersonFlag.pfHasInvDesc);
                                    mt.SetFlag(PersonFlag.pfHasInvDesc);
                                }
                            }
                        }
                    }
                }

                return result;
            }
            catch (Exception ex)
            {
                fBase.Host.LogWrite("TreeChartBox.DoDescendantsStep(): " + ex.Message);
                throw;
            }
        }

        #endregion

        #region Kinships

        private void FindRelationship(TreeChartPerson target)
        {
            if (target == null || target.Node == null || target.Rec == null) {
                target.Kinship = "";
            } else {
                target.Kinship = "[" + fGraph.GetRelationship(target.Rec) + "]";
                if (fPathDebug) {
                    target.PathDebug = fGraph.IndividualsPath;
                }
            }
        }

        #endregion

        #region Drawing routines

        private void InitGraphics()
        {
            fLinePen = new Pen(Color.Black, 1f);
            fDecorativeLinePen = new Pen(Color.Silver, 1f);
            fSolidBlack = new SolidBrush(Color.Black);
        }

        private void DoneGraphics()
        {
            fLinePen.Dispose();
            fDecorativeLinePen.Dispose();
            fSolidBlack.Dispose();
        }

        private static ExtRect GetExpanderRect(ExtRect personRect)
        {
            ExtRect expRt = ExtRect.Create(personRect.Left, personRect.Top - 18, personRect.Left + 16 - 1, personRect.Top - 2);
            return expRt;
        }

        private bool IsPersonVisible(ExtRect pnRect)
        {
            return fVisibleArea.IntersectsWith(pnRect.ToRectangle());
        }

        private static void CheckSwap(ref int val1, ref int val2)
        {
            if (val2 > val1) return;

            int tmp = val1;
            val1 = val2;
            val2 = tmp;
        }

        private bool IsLineVisible(int x1, int y1, int x2, int y2)
        {
            var rangeX = new Range<int>(fVisibleArea.Left, fVisibleArea.Right);
            var rangeY = new Range<int>(fVisibleArea.Top, fVisibleArea.Bottom);

            CheckSwap(ref x1, ref x2);
            CheckSwap(ref y1, ref y2);
            
            return rangeX.IsOverlapped(new Range<int>(x1, x2)) && rangeY.IsOverlapped(new Range<int>(y1, y2));
        }

        private void DrawLine(int x1, int y1, int x2, int y2)
        {
            if (!IsLineVisible(x1, y1, x2, y2)) return;

            int sX = fSPX + x1;
            int sX2 = fSPX + x2;
            int sY = fSPY + y1;
            int sY2 = fSPY + y2;
            fRenderer.DrawLine(fLinePen, sX, sY, sX2, sY2);

            if (fOptions.Decorative) {
                if (sX == sX2) {
                    fRenderer.DrawLine(fDecorativeLinePen, sX + 1, sY + 1, sX2 + 1, sY2 - 1);
                } else if (sY == sY2) {
                    fRenderer.DrawLine(fDecorativeLinePen, sX + 1, sY + 1, sX2 + 0, sY2 + 1);
                }
            }
        }

        private void DrawBorder(Pen xpen, ExtRect rt, bool dead, TreeChartPerson person)
        {
            Color bColor = person.GetFillColor(dead);
            if (fHighlightedPerson == person) {
                bColor = SysUtils.Lighter(bColor, HIGHLIGHTED_VAL);
            }

            if (person.Sex == GEDCOMSex.svFemale) {
                fRenderer.DrawRoundedRectangle(xpen, bColor, rt.Left, rt.Top, rt.GetWidth(), rt.GetHeight(), 6);
            } else {
                fRenderer.DrawRectangle(xpen, bColor, rt.Left, rt.Top, rt.GetWidth(), rt.GetHeight());
            }
        }

        private void DrawPerson(TreeChartPerson person, DrawMode drawMode)
        {
            try {
                ExtRect prt = person.Rect;
                if (drawMode == DrawMode.dmInteractive && !IsPersonVisible(prt))
                    return;

                prt.Offset(fSPX, fSPY);

                if (person.IsDead) {
                    ExtRect dt = prt.GetOffset(-2, -2);
                    DrawBorder(null, dt, true, person);
                }

                Pen xpen = null;
                try {
                    if (drawMode == DrawMode.dmInteractive && person.Selected) {
                        Color penColor = person.GetSelectedColor();
                        xpen = new Pen(penColor, 2.0f);
                    } else {
                        xpen = new Pen(Color.Black, 1.0f);
                    }

                    DrawBorder(xpen, prt, false, person);
                } finally {
                    if (xpen != null)
                        xpen.Dispose();
                }

                ExtRect brt = prt;
                if (person.Portrait != null) {
                    ExtRect portRt = person.PortraitArea.GetOffset(prt.Left, prt.Top);
                    fRenderer.DrawImage(person.Portrait, portRt.Left, portRt.Top,
                                        portRt.GetWidth(), portRt.GetHeight());

                    prt.Left += person.PortraitWidth;
                }

                int h = fRenderer.GetTextHeight(fDrawFont);
                int lines = person.Lines.Length;
                for (int k = 0; k < lines; k++) {
                    string line = person.Lines[k];

                    int stw = fRenderer.GetTextWidth(line, fDrawFont);
                    int rx = prt.Left + ((prt.Right - prt.Left + 1) - stw) / 2;
                    int ry = prt.Top + fNodePadding + (h * k);
                    fRenderer.DrawString(line, fDrawFont, fSolidBlack, rx, ry);
                }

                if (fOptions.SignsVisible && !person.Signs.IsEmpty()) {
                    int i = 0;
                    for (var cps = SpecialUserRef.urRI_StGeorgeCross; cps <= SpecialUserRef.urLast; cps++) {
                        if (!person.Signs.Contains(cps)) continue;

                        Bitmap pic = fSignsPic[(int)cps - 1];
                        fRenderer.DrawImage(pic, brt.Right, brt.Top - 21 + i * pic.Height);
                        i++;
                    }
                }

                // only interactive mode
                if (drawMode == DrawMode.dmInteractive) {
                    if (person.CanExpand) {
                        ExtRect expRt = GetExpanderRect(brt);
                        fRenderer.DrawImage(fExpPic, expRt.Left, expRt.Top);
                    }

                    // draw CI only for existing individuals
                    if (fCertaintyIndex && person.Rec != null) {
                        string cas = string.Format("{0:0.00}", person.CertaintyAssessment);
                        fRenderer.DrawString(cas, fDrawFont, fSolidBlack, brt.Left, brt.Bottom);
                    }
                }
            } catch (Exception ex) {
                fBase.Host.LogWrite("TreeChartBox.DrawPerson(): " + ex.Message);
            }
        }

        private void DrawAncestors(TreeChartPerson person, DrawMode drawMode)
        {
            Draw(person.Father, ChartKind.ckAncestors, drawMode);
            Draw(person.Mother, ChartKind.ckAncestors, drawMode);
            int crY = person.PtY - fLevelDistance / 2;

            if (person.Father != null) {
                DrawLine(person.Father.PtX, crY, person.PtX, crY);
                DrawLine(person.Father.PtX, person.Father.PtY + person.Father.Height, person.Father.PtX, crY);
            }

            if (person.Mother != null) {
                DrawLine(person.PtX, crY, person.Mother.PtX, crY);
                DrawLine(person.Mother.PtX, person.Mother.PtY + person.Mother.Height, person.Mother.PtX, crY);
            }

            if (person.Father != null || person.Mother != null) {
                DrawLine(person.PtX, crY, person.PtX, person.PtY);
            }
        }

        private void DrawDescendants(TreeChartPerson person, DrawMode drawMode)
        {
            int num = person.GetChildsCount();
            for (int i = 0; i < num; i++) {
                Draw(person.GetChild(i), ChartKind.ckDescendants, drawMode);
            }

            int spbOfs = (person.Height - 10) / (person.GetSpousesCount() + 1);
            int spbBeg = person.PtY + (person.Height - spbOfs * (person.GetSpousesCount() - 1)) / 2;

            switch (person.Sex) {
                case GEDCOMSex.svMale:
                    int num3 = person.GetSpousesCount();
                    for (int i = 0; i < num3; i++) {
                        int spbV = spbBeg + spbOfs * i;
                        DrawLine(person.Rect.Right + 1, spbV, person.GetSpouse(i).Rect.Left, spbV);
                    }
                    break;

                case GEDCOMSex.svFemale:
                    int num2 = person.GetSpousesCount();
                    for (int i = 0; i < num2; i++) {
                        int spbV = spbBeg + spbOfs * i;
                        DrawLine(person.GetSpouse(i).Rect.Right + 1, spbV, person.Rect.Left, spbV);
                    }
                    break;
            }

            int num4 = person.GetSpousesCount();
            for (int i = 0; i < num4; i++) {
                Draw(person.GetSpouse(i), ChartKind.ckDescendants, drawMode);
            }

            int crY = person.PtY + person.Height + fLevelDistance / 2;
            int cx = 0;
            if (person.BaseSpouse == null || (person.BaseSpouse != null && person.BaseSpouse.GetSpousesCount() > 1))
            {
                cx = person.PtX;
                spbBeg = person.PtY + person.Height - 1;
            }
            else
            {
                switch (person.Sex) {
                    case GEDCOMSex.svMale:
                        cx = (person.Rect.Right + person.BaseSpouse.Rect.Left) / 2;
                        break;

                    case GEDCOMSex.svFemale:
                        cx = (person.BaseSpouse.Rect.Right + person.Rect.Left) / 2;
                        break;
                }

                spbBeg -= spbOfs / 2;
            }

            if (person.GetChildsCount() != 0)
            {
                DrawLine(cx, spbBeg, cx, crY);
                if (person.GetChildsCount() == 1)
                {
                    TreeChartPerson child = person.GetChild(0);
                    DrawLine(child.PtX, crY, child.PtX, child.PtY);
                }
                else
                {
                    int bpx = person.GetChild(0).PtX;
                    int epx = person.GetChild(person.GetChildsCount() - 1).PtX;
                    DrawLine(bpx, crY, epx, crY);
                    int num5 = person.GetChildsCount();
                    for (int i = 0; i < num5; i++) {
                        TreeChartPerson child = person.GetChild(i);
                        DrawLine(child.PtX, crY, child.PtX, child.PtY);
                    }
                }
            }
        }

        private void DrawBackground(BackgroundMode background)
        {
            if (background != BackgroundMode.bmNone) {
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
                        fRenderer.DrawRectangle(null, BackColor, 0, 0, fImageWidth, fImageHeight);
                    }
                }
            }
        }

        private void InternalDraw(DrawMode drawMode, BackgroundMode background)
        {
            DrawBackground(background);

            fSPX = 0;
            fSPY = 0;

            if (drawMode == DrawMode.dmInteractive) {
                /*Rectangle viewPort = this.GetImageViewPort();
				this.fSPX = -viewPort.Left;
				this.fSPY = -viewPort.Top;*/

                fSPX += fBorderWidth - -AutoScrollPosition.X;
                fSPY += fBorderWidth - -AutoScrollPosition.Y;

                Size sz = ClientSize;

                if (fImageWidth < sz.Width) {
                    fSPX += (sz.Width - fImageWidth) / 2;
                }

                if (fImageHeight < sz.Height) {
                    fSPY += (sz.Height - fImageHeight) / 2;
                }

                fVisibleArea = GetSourceImageRegion();
            } else {
                if (drawMode == DrawMode.dmStaticCentered) {
                    Size sz = ClientSize;

                    if (fImageWidth < sz.Width) {
                        fSPX += (sz.Width - fImageWidth) / 2;
                    }

                    if (fImageHeight < sz.Height) {
                        fSPY += (sz.Height - fImageHeight) / 2;
                    }
                }

                fVisibleArea = new Rectangle(0, 0, fImageWidth, fImageHeight);
            }

            #if DEBUG_IMAGE
            using (Pen pen = new Pen(Color.Red)) {
                fRenderer.DrawRectangle(pen, Color.Transparent, fSPX, fSPY, fImageWidth, fImageHeight);
            }
            #endif

            Draw(fRoot, fKind, drawMode);
        }

        protected void Draw(TreeChartPerson person, ChartKind dirKind, DrawMode drawMode)
        {
            if (person == null) return;

            switch (fKind) {
                case ChartKind.ckAncestors:
                    DrawAncestors(person, drawMode);
                    break;

                case ChartKind.ckDescendants:
                    DrawDescendants(person, drawMode);
                    break;

                case ChartKind.ckBoth:
                    if (person == fRoot || dirKind == ChartKind.ckAncestors) DrawAncestors(person, drawMode);
                    if (person == fRoot || dirKind == ChartKind.ckDescendants) DrawDescendants(person, drawMode);
                    break;
            }

            DrawPerson(person, drawMode);
        }

        #endregion

        #region Sizes and adjustment routines

        public void Predef()
        {
            float sc = fScale;

            float fsz = (float)Math.Round(fOptions.DefFontSize * sc);
            fDrawFont = new Font(fOptions.DefFontName, fsz, FontStyle.Regular, GraphicsUnit.Point);

            fSpouseDistance = (int)Math.Round(DEF_SPOUSE_DISTANCE * sc);
            fBranchDistance = (int)Math.Round(DEF_BRANCH_DISTANCE * sc);
            fLevelDistance = (int)Math.Round(DEF_LEVEL_DISTANCE * sc);
            fMargins = (int)Math.Round(DEF_MARGINS * sc);
            fNodePadding = (int)(DEF_PERSON_NODE_PADDING * sc);
        }
        
        private int InitInfoSize()
        {
            int lines = 0;

            if (fOptions.FamilyVisible) {
                lines++;
            }

            if (!fOptions.DiffLines) {
                lines++;
            } else {
                lines++;
                lines++;
            }

            if (!fOptions.OnlyYears) {
                if (fOptions.BirthDateVisible) {
                    lines++;
                }
                if (fOptions.DeathDateVisible) {
                    lines++;
                }
            } else {
                lines++;
            }

            if (fOptions.Kinship) {
                lines++;
            }

            if (fPathDebug) {
                lines++;
            }

            return lines;
        }

        public void RecalcChart(bool noRedraw = false)
        {
            Predef();

            if (fOptions.Kinship && fKinRoot != null) {
                fGraph.SetTreeRoot(fKinRoot.Rec);
            }

            int lines = InitInfoSize();

            Graphics gfx = null;
            if (fRenderer is TreeChartGfxRenderer) {
                gfx = CreateGraphics();
                fRenderer.SetTarget(gfx);
            }
            try {
                int num = fPersons.Count;
                for (int i = 0; i < num; i++) {
                    TreeChartPerson p = fPersons[i];

                    if (fOptions.Kinship) {
                        FindRelationship(p);
                    }

                    p.CalcBounds(lines, fRenderer);
                }
            } finally {
                if (fRenderer is TreeChartGfxRenderer && gfx != null) {
                    gfx.Dispose();
                }
            }

            switch (fKind) {
                case ChartKind.ckAncestors:
                    RecalcAncestorsChart();
                    break;

                case ChartKind.ckDescendants:
                    RecalcDescendantsChart(true);
                    break;

                case ChartKind.ckBoth:
                    RecalcAncestorsChart();
                    RecalcDescendantsChart(false);
                    break;
            }

            // search bounds
            fTreeBounds = ExtRect.Create(int.MaxValue, int.MaxValue, 0, 0);
            int num2 = fPersons.Count;
            for (int i = 0; i < num2; i++) {
                TreeChartPerson p = fPersons[i];
                AdjustTreeBounds(p);
            }

            // adjust bounds
            int offsetX = 0 + fMargins - fTreeBounds.Left;
            int offsetY = 0 + fMargins - fTreeBounds.Top;
            fTreeBounds = ExtRect.Create(int.MaxValue, int.MaxValue, 0, 0);
            for (int i = 0; i < num2; i++) {
                TreeChartPerson p = fPersons[i];
                p.PtX += offsetX;
                p.PtY += offsetY;
                AdjustTreeBounds(p);
            }

            fImageHeight = fTreeBounds.GetHeight() + fMargins * 2;
            fImageWidth = fTreeBounds.GetWidth() + fMargins * 2;
            fImageSize = new Size(fImageWidth, fImageHeight);

            AdjustViewPort(fImageSize, noRedraw);
        }

        private void AdjustTreeBounds(TreeChartPerson person)
        {
            if (person == null) return;
            ExtRect prt = person.Rect;
            
            if (fTreeBounds.Left > prt.Left) fTreeBounds.Left = prt.Left;
            if (fTreeBounds.Top > prt.Top) fTreeBounds.Top = prt.Top;
            
            if (fTreeBounds.Right < prt.Right) fTreeBounds.Right = prt.Right;
            if (fTreeBounds.Bottom < prt.Bottom) fTreeBounds.Bottom = prt.Bottom;
        }
        
        private static void ShiftAnc(ref int[] edges, TreeChartPerson person, int offset)
        {
            TreeChartPerson pp = person;
            if (pp == null) return;

            do
            {
                pp.PtX += offset;
                edges[pp.Generation] = pp.Rect.Right;

                pp = (pp.GetChildsCount() < 1) ? null : pp.GetChild(0);
            }
            while (pp != null);
        }

        private void RecalcAnc(ExtList<TreeChartPerson> prev, ref int[] edges, TreeChartPerson person, int ptX, int ptY)
        {
            if (person == null) return;

            person.PtX = ptX;
            person.PtY = ptY;

            int gen = person.Generation;

            int offset = (edges[gen] > 0) ? fBranchDistance : fMargins;
            int bound = edges[gen] + offset;
            if (person.Rect.Left <= bound) {
                ShiftAnc(ref edges, person, bound - person.Rect.Left);
            }

            edges[gen] = person.Rect.Right;

            prev.Add(person);
            if (person.Rect.Top < 0)
            {
                offset = 0 - person.Rect.Top + fMargins;
                int num = prev.Count;
                for (int i = 0; i < num; i++) {
                    prev[i].PtY += offset;
                }
            }

            if (person.Father != null && person.Mother != null)
            {
                RecalcAnc(prev, ref edges, person.Father, person.PtX - (fSpouseDistance + person.Father.Width / 2), person.PtY - fLevelDistance - person.Height);
                RecalcAnc(prev, ref edges, person.Mother, person.PtX + (fSpouseDistance + person.Mother.Width / 2), person.PtY - fLevelDistance - person.Height);

                person.PtX = (person.Father.PtX + person.Mother.PtX) / 2;
                edges[gen] = person.Rect.Right;
            }
            else
            {
                TreeChartPerson anc = null;
                if (person.Father != null) {
                    anc = person.Father;
                } else if (person.Mother != null) {
                    anc = person.Mother;
                }
                
                if (anc != null) {
                    RecalcAnc(prev, ref edges, anc, person.PtX, person.PtY - fLevelDistance - person.Height);
                }
            }
        }

        private void RecalcAncestorsChart()
        {
            var edges = new int[256];
            Array.Clear(edges, 0, edges.Length);

            var prev = new ExtList<TreeChartPerson>();
            try
            {
                RecalcAnc(prev, ref edges, fRoot, fMargins, fMargins);
            }
            finally
            {
                prev.Dispose();
            }
        }

        private void ShiftDesc(TreeChartPerson person, int offset, bool isSingle)
        {
            if (person == null) return;

            if (person == fRoot) {
                isSingle = false;
            }

            person.PtX += offset;

            if (person.BaseSpouse != null && (person.BaseSpouse.Sex == GEDCOMSex.svFemale || person.BaseSpouse.GetSpousesCount() == 1))
            {
                ShiftDesc(person.BaseSpouse, offset, isSingle);
            } else {
                if (!isSingle) {
                    ShiftDesc(person.Father, offset, false);
                    ShiftDesc(person.Mother, offset, false);
                } else {
                    if (person.HasFlag(PersonFlag.pfDescByFather)) {
                        ShiftDesc(person.Father, offset, true);
                    } else {
                        if (person.HasFlag(PersonFlag.pfDescByMother)) {
                            ShiftDesc(person.Mother, offset, true);
                        }
                    }
                }
            }
        }

        private void RecalcDescChilds(ref int[] edges, TreeChartPerson person)
        {
            if (person.GetChildsCount() == 0) return;

            bool fixPair = person.BaseSpouse != null && person.BaseSpouse.GetSpousesCount() == 1;
            int centX = 0;

            if (fixPair) {
                GEDCOMSex sex = person.Sex;
                switch (sex) {
                    case GEDCOMSex.svMale:
                        centX = (person.Rect.Right + person.BaseSpouse.Rect.Left) / 2;
                        break;

                    case GEDCOMSex.svFemale:
                        centX = (person.BaseSpouse.Rect.Right + person.Rect.Left) / 2;
                        break;
                }
            } else {
                centX = person.PtX;
            }

            int curY = person.PtY + fLevelDistance + person.Height;
            int childsWidth = (person.GetChildsCount() - 1) * fBranchDistance;

            int num = person.GetChildsCount();
            for (int i = 0; i < num; i++) {
                childsWidth += person.GetChild(i).Width;
            }

            int curX = centX - childsWidth / 2;

            int num2 = person.GetChildsCount();
            for (int i = 0; i < num2; i++) {
                TreeChartPerson child = person.GetChild(i);
                RecalcDesc(ref edges, child, new Point(curX + child.Width / 2, curY), true);
                curX = child.Rect.Right + fBranchDistance;
            }

            curX = person.GetChild(0).PtX;
            if (person.GetChildsCount() > 1) {
                curX += (person.GetChild(person.GetChildsCount() - 1).PtX - curX) / 2;
            }

            if (fixPair) {
                switch (person.Sex) {
                    case GEDCOMSex.svMale:
                        ShiftDesc(person, curX - (fBranchDistance + person.Width) / 2 + 1 - person.PtX, true);
                        ShiftDesc(person.BaseSpouse, curX + (fBranchDistance + person.BaseSpouse.Width) / 2 - person.BaseSpouse.PtX, true);
                        break;

                    case GEDCOMSex.svFemale:
                        ShiftDesc(person, curX + (fBranchDistance + person.Width) / 2 - person.PtX, true);
                        ShiftDesc(person.BaseSpouse, curX - (fBranchDistance + person.BaseSpouse.Width) / 2 + 1 - person.BaseSpouse.PtX, true);
                        break;
                }
            } else {
                ShiftDesc(person, curX - person.PtX, true);
            }
        }

        private void RecalcDesc(ref int[] edges, TreeChartPerson person, Point aPt, bool predef)
        {
            if (person == null) return;
            
            int gen = person.Generation;
            if (predef) {
                person.PtX = aPt.X;
                person.PtY = aPt.Y;
            }

            int offset = (edges[gen] > 0) ? fBranchDistance : fMargins;
            int bound = edges[gen] + offset;
            if (person.Rect.Left <= bound) {
                ShiftDesc(person, bound - person.Rect.Left, true);
            }

            if (person.Sex == GEDCOMSex.svMale) {
                RecalcDescChilds(ref edges, person);
                edges[gen] = person.Rect.Right;
            }

            if (person.GetSpousesCount() > 0) {
                TreeChartPerson prev = person;
                int num = person.GetSpousesCount();
                for (int i = 0; i < num; i++) {
                    TreeChartPerson sp = person.GetSpouse(i);
                    var spPt = new Point();

                    switch (person.Sex) {
                        case GEDCOMSex.svMale:
                            spPt = new Point(prev.Rect.Right + (fBranchDistance + sp.Width / 2), person.PtY);
                            break;

                        case GEDCOMSex.svFemale:
                            spPt = new Point(prev.Rect.Left - (fBranchDistance + sp.Width / 2), person.PtY);
                            break;
                    }

                    RecalcDesc(ref edges, sp, spPt, true);

                    if (sp.Sex != GEDCOMSex.svMale) {
                        prev = sp;
                    }
                }
            }

            if (person.Sex == GEDCOMSex.svFemale) {
                RecalcDescChilds(ref edges, person);
                edges[gen] = person.Rect.Right;
            }
        }

        private void RecalcDescendantsChart(bool predef)
        {
            var edges = new int[256];
            Array.Clear(edges, 0, edges.Length);

            RecalcDesc(ref edges, fRoot, new Point(fMargins, fMargins), predef);
        }

        private void SetScroll(int x, int y)
        {
            AutoScrollPosition = new Point(x, y);
            Invalidate();
            OnScroll(new ScrollEventArgs(ScrollEventType.EndScroll, 0));
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

            if (!fImageSize.IsEmpty) {
                Rectangle innerRectangle = GetInsideViewPort(true);

                int x = !HScroll ? (innerRectangle.Width - (fImageSize.Width + Padding.Horizontal)) / 2 : 0;
                int y = !VScroll ? (innerRectangle.Height - (fImageSize.Height + Padding.Vertical)) / 2 : 0;

                int width = Math.Min(fImageSize.Width - Math.Abs(AutoScrollPosition.X), innerRectangle.Width);
                int height = Math.Min(fImageSize.Height - Math.Abs(AutoScrollPosition.Y), innerRectangle.Height);

                viewPort = new Rectangle(x + innerRectangle.Left, y + innerRectangle.Top, width, height);
            } else {
                viewPort = Rectangle.Empty;
            }

            return viewPort;
        }

        private Rectangle GetSourceImageRegion()
        {
            Rectangle region;

            if (!fImageSize.IsEmpty) {
                Rectangle viewPort = GetImageViewPort();
                region = new Rectangle(-AutoScrollPosition.X, -AutoScrollPosition.Y, viewPort.Width, viewPort.Height);
            } else {
                region = Rectangle.Empty;
            }

            return region;
        }

        #endregion

        #region Event processing
        
        private void DoPersonModify(PersonModifyEventArgs eArgs)
        {
            var eventHandler = (PersonModifyEventHandler)base.Events[TreeChartBox.EventPersonModify];
            if (eventHandler == null) return;

            eventHandler(this, eArgs);
        }
        
        private void DoRootChanged(TreeChartPerson person)
        {
            var eventHandler = (RootChangedEventHandler)base.Events[TreeChartBox.EventRootChanged];
            if (eventHandler == null) return;

            eventHandler(this, person);
        }
        
        private void DoPersonProperties(MouseEventArgs eArgs)
        {
            var eventHandler = (MouseEventHandler)base.Events[TreeChartBox.EventPersonProperties];
            if (eventHandler == null) return;

            eventHandler(this, eArgs);
        }

        #endregion

        #region Protected methods

        protected override bool IsInputKey(Keys keyData)
        {
            switch (keyData) {
                case Keys.Add:
                case Keys.Subtract:
                case Keys.Back:
                    return true;
            }

            return base.IsInputKey(keyData);
        }

        protected override void OnKeyDown(KeyEventArgs e)
        {
            base.OnKeyDown(e);

            e.Handled = false;
            switch (e.KeyCode) {
                case Keys.Add:
                    SetScale(fScale + 0.05f);
                    break;

                case Keys.Subtract:
                    SetScale(fScale - 0.05f);
                    break;

                case Keys.Back:
                    NavPrev();
                    return;

                default:
                    e.Handled = true;
                    break;
            }
        }

        protected override void OnResize(EventArgs e)
        {
            SaveSelection();
            
            AdjustViewPort(fImageSize);
            fScaleControl.Update();

            RestoreSelection();

            base.OnResize(e);
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            Graphics gfx = e.Graphics;
            fRenderer.SetTarget(gfx);
            InternalDraw(DrawMode.dmInteractive, BackgroundMode.bmAny);

            // interactive controls
            if (fScaleControl.Visible) fScaleControl.Draw(gfx);
            //if (fPersonControl.Visible) fPersonControl.Draw(gfx);
        }

        protected override void OnDoubleClick(EventArgs e)
        {
            TreeChartPerson p = fSelected;
            
            DoPersonModify(new PersonModifyEventArgs(p));
        }

        protected override void OnMouseWheel(MouseEventArgs e)
        {
            //base.OnMouseWheel(e);
            if (ModifierKeys == Keys.Control) {
                float newScale = (e.Delta > 0) ? fScale - 0.05f : fScale + 0.05f;

                SetScale(newScale);
            } else {
                int dx = 0, dy = 0;
                if (ModifierKeys == Keys.Shift) {
                    dx = -e.Delta;
                } else {
                    dy = -e.Delta;
                }
                AdjustScroll(dx, dy);
            }
        }

        private MouseAction GetMouseAction(MouseEventArgs e, bool isDown, out TreeChartPerson person)
        {
            var result = MouseAction.maNone;
            person = null;
            
            int aX = e.X - fSPX;
            int aY = e.Y - fSPY;
            int num = fPersons.Count;
            for (int i = 0; i < num; i++) {
                TreeChartPerson p = fPersons[i];
                ExtRect persRt = p.Rect;
                
                if ((e.Button == MouseButtons.Right) && persRt.Contains(aX, aY)) {
                    person = p;
                    result = MouseAction.maProperties;
                    break;
                }
                
                ExtRect expRt = GetExpanderRect(persRt);
                if ((e.Button == MouseButtons.Left) && expRt.Contains(aX, aY)) {
                    person = p;
                    result = MouseAction.maExpand;
                    break;
                }
            }

            return result;
        }
        
        protected override void OnMouseDown(MouseEventArgs e)
        {
            base.OnMouseDown(e);
            if (!Focused) Focus();

            fMouseX = e.X;
            fMouseY = e.Y;

            switch (fMode) {
                case ChartControlMode.ccmDefault:
                    SelectBy(e.X, e.Y, (e.Button == MouseButtons.Left));

                    if (fSelected == null && e.Button == MouseButtons.Right)
                    {
                        Cursor = Cursors.SizeAll;
                        fMode = ChartControlMode.ccmDragImage;
                    }
                    break;

                case ChartControlMode.ccmDragImage:
                    break;

                case ChartControlMode.ccmControlsVisible:
                    fScaleControl.MouseDown(e.X, e.Y);
                    break;
            }
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            switch (fMode)
            {
                case ChartControlMode.ccmDefault:
                    if (fScaleControl.Contains(e.X, e.Y)) {
                        fMode = ChartControlMode.ccmControlsVisible;
                        fScaleControl.Visible = true;
                        fScaleControl.MouseMove(e.X, e.Y, ThumbMoved);

                        var pt = new Point(e.X, e.Y);
                        pt.Offset(+Left, +Top);
                        fToolTip.Show(fScaleControl.Tip, this, pt, 1500);
                    } /*if (this.fPersonControl.Contains(e.X, e.Y)) {
						
					} */else {
                        TreeChartPerson p = FindPersonByCoords(e.X, e.Y);
                        if (fHighlightedPerson != p)
                        {
                            fHighlightedPerson = p;
                            fHighlightedStart = DateTime.Now.ToBinary();

                            if (p == null) {
                                //this.fPersonControl.Visible = false;
                            } else {
                                //this.fPersonControl.SetPerson(p);
                            }

                            Invalidate();
                        }
//
//						if (p != null && e.Button == MouseButtons.Left)
//						{
//							this.fTreeBox.DoDragDrop(p.Rec.XRef, DragDropEffects.Move);
//						}
                    }
                    break;

                case ChartControlMode.ccmDragImage:
                    AdjustScroll(-(e.X - fMouseX), -(e.Y - fMouseY));
                    fMouseX = e.X;
                    fMouseY = e.Y;
                    break;

                case ChartControlMode.ccmControlsVisible:
                    if (!(fScaleControl.Contains(e.X, e.Y) ||
                          fScaleControl.ThumbCaptured)) {
                        fMode = ChartControlMode.ccmDefault;
                        fScaleControl.Visible = false;
                        fToolTip.Hide(this);
                    } else {
                        fScaleControl.MouseMove(e.X, e.Y, ThumbMoved);
                    }
                    break;
            }
        }

        private void ThumbMoved(int position)
        {
            SetScale(0.5f + (((float)(position)) / fScaleControl.DCount));
        }

        protected override void OnMouseUp(MouseEventArgs e)
        {
            switch (fMode) {
                case ChartControlMode.ccmDefault:
                    TreeChartPerson mPers;
                    MouseAction mAct = GetMouseAction(e, false, out mPers);

                    switch (mAct) {
                        case MouseAction.maNone:
                            break;

                        case MouseAction.maProperties:
                            if (fSelected == mPers && fSelected.Rec != null)
                            {
                                DoPersonProperties(new MouseEventArgs(e.Button, 1, e.X, e.Y, 0));
                            }
                            break;

                        case MouseAction.maExpand:
                            DoRootChanged(mPers);
                            GenChart(mPers.Rec, ChartKind.ckBoth, true);
                            break;
                    }
                    break;

                case ChartControlMode.ccmDragImage:
                    Cursor = Cursors.Default;
                    fMode = ChartControlMode.ccmDefault;
                    break;

                case ChartControlMode.ccmControlsVisible:
                    fScaleControl.MouseUp(e.X, e.Y);
                    break;
            }
        }

        #endregion

        #region Navigation

        protected override void SetNavObject(object obj)
        {
            var iRec = obj as GEDCOMIndividualRecord;
            GenChart(iRec, ChartKind.ckBoth, true);
        }
        
        private void SetSelected(TreeChartPerson value)
        {
            if (fSelected != null) fSelected.Selected = false;
            fSelected = value;
            if (fSelected != null) fSelected.Selected = true;

            Invalidate();
        }

        private TreeChartPerson FindPersonByCoords(int aX, int aY)
        {
            TreeChartPerson result = null;
            
            aX -= fSPX;
            aY -= fSPY;
            int num = fPersons.Count;
            for (int i = 0; i < num; i++) {
                TreeChartPerson p = fPersons[i];
                if (p.Rect.Contains(aX, aY)) {
                    result = p;
                    break;
                }
            }

            return result;
        }

        private void SelectBy(int aX, int aY, bool needCenter)
        {
            TreeChartPerson p = FindPersonByCoords(aX, aY);
            SetSelected(p);

            //if (this.FTraceKinships && this.fOptions.Kinship) this.RebuildKinships(true);

            if (p != null && needCenter && fTraceSelected) CenterPerson(p);
        }

        public void SelectByRec(GEDCOMIndividualRecord iRec)
        {
            if (iRec == null) return;
            
            int num = fPersons.Count;
            for (int i = 0; i < num; i++) {
                TreeChartPerson p = fPersons[i];
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
                var tween = new TweenLibrary();
                tween.StartTween(SetScroll, oldX, oldY, dstX, dstY, TweenAnimation.EaseInOutQuad, 20);
            } else {
                SetScroll(dstX, dstY);
            }
        }

        #endregion

        #region Filtering and search

        public void DoFilter(GEDCOMIndividualRecord root)
        {
            if (root == null)
                throw new ArgumentNullException("root");

            if (fFilter.BranchCut != ChartFilter.BranchCutType.None) {
                GKUtils.InitExtCounts(fTree, 0);
                DoDescendantsFilter(root);
                root.ExtData = true;
            }
        }

        private bool DoDescendantsFilter(GEDCOMIndividualRecord person)
        {
            bool result = false;
            if (person != null)
            {
                ChartFilter.BranchCutType branchCut = fFilter.BranchCut;
                switch (branchCut) {
                    case ChartFilter.BranchCutType.Years:
                        int birthYear = GEDCOMUtils.GetRelativeYear(person, "BIRT");
                        result = (birthYear != 0 && birthYear >= fFilter.BranchYear);
                        break;

                    case ChartFilter.BranchCutType.Persons:
                        result = (fFilter.BranchPersons.IndexOf(person.XRef + ";") >= 0);
                        break;
                }

                int num = person.SpouseToFamilyLinks.Count;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMFamilyRecord family = person.SpouseToFamilyLinks[i].Family;

                    int num2 = family.Childrens.Count;
                    for (int j = 0; j < num2; j++)
                    {
                        GEDCOMIndividualRecord child = family.Childrens[j].Value as GEDCOMIndividualRecord;
                        bool resChild = DoDescendantsFilter(child);
                        result |= resChild;
                    }
                }
                person.ExtData = result;
            }
            return result;
        }

        public IList<ISearchResult> FindAll(String searchPattern)
        {
            var result = new List<ISearchResult>();
            
            Regex regex = GKUtils.InitMaskRegex(searchPattern);
            
            int num = fPersons.Count;
            for (int i = 0; i < num; i++) {
                TreeChartPerson person = fPersons[i];
                GEDCOMIndividualRecord iRec = person.Rec;
                if (iRec == null) continue;
                
                string fullname = GKUtils.GetNameString(iRec, true, false);
                if (GKUtils.MatchesRegex(fullname, regex)) {
                    //yield return new SearchResult(iRec);
                    result.Add(new SearchResult(iRec));
                }
            }
            
            return result;
        }
        
        #endregion

        #region Print support

        public void SaveSnapshot(string fileName)
        {
            string ext = SysUtils.GetFileExtension(fileName);

            if ((ext == ".bmp" || ext == ".jpg") && fImageWidth >= 65535)
            {
                GKUtils.ShowError(LangMan.LS(LSID.LSID_TooMuchWidth));
            }
            else
            {
                ImageFormat imFmt = ImageFormat.Png;
                if (ext == ".bmp") { imFmt = ImageFormat.Bmp; }
                else
                    if (ext == ".emf") { imFmt = ImageFormat.Emf; }
                else
                    if (ext == ".png") { imFmt = ImageFormat.Png; }
                else
                    if (ext == ".gif") { imFmt = ImageFormat.Gif; }
                else
                    if (ext == ".jpg") { imFmt = ImageFormat.Jpeg; }

                Image pic;
                if (Equals(imFmt, ImageFormat.Emf)) {
                    pic = new Metafile(fileName, CreateGraphics().GetHdc());
                } else {
                    pic = new Bitmap(fImageWidth, fImageHeight, PixelFormat.Format24bppRgb);
                }

                try
                {
                    using (Graphics gfx = Graphics.FromImage(pic)) {
                        fRenderer.SetTarget(gfx);
                        RenderStatic(BackgroundMode.bmAny);
                    }

                    pic.Save(fileName, imFmt);
                }
                finally
                {
                    pic.Dispose();
                }
            }
        }

        public bool IsLandscape()
        {
            return (fImageHeight < fImageWidth);
        }

        public Image GetPrintableImage()
        {
            var frameRect = new Rectangle(0, 0, fImageWidth, fImageHeight);
            Image image = new Metafile(CreateGraphics().GetHdc(), frameRect, MetafileFrameUnit.Pixel, EmfType.EmfOnly);

            using (Graphics gfx = Graphics.FromImage(image)) {
                fRenderer.SetTarget(gfx);
                RenderStatic(BackgroundMode.bmImage);
            }

            return image;
        }

        public void RenderStatic(BackgroundMode background, bool centered = false)
        {
            DrawMode drawMode = (!centered) ? DrawMode.dmStatic : DrawMode.dmStaticCentered;
            InternalDraw(drawMode, background);
        }

        #endregion
    }
}
