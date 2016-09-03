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
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;
using System.Text.RegularExpressions;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.SmartGraph;
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
        public const int DEF_MARGINS = 24;
        public const int DEF_SPOUSE_DISTANCE = 10;
        public const int DEF_BRANCH_DISTANCE = 40;
        public const int DEF_LEVEL_DISTANCE = 46;

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
            dmScreen,
            dmFile
        }

        public enum MouseAction
        {
            maNone,
            maProperties,
            maExpand
        }
        
        #endregion

        #region Private fields

        private readonly ChartFilter fFilter;
        private readonly Graph fGraph;
        private readonly PersonList fPersons;
        private readonly List<string> fPreparedFamilies;
        internal readonly List<string> fPreparedIndividuals;
        private readonly ScaleControl fScaleControl;
        private readonly ToolTip fToolTip;
        
        private IBaseWindow fBase;
        private int fBranchDistance;
        private bool fCertaintyIndex;
        private int fDepthLimit;
        private Font fDrawFont;
        private ChartKind fKind;
        private TreeChartPerson fKinRoot;
        private int fLevelDistance;
        private int fMargins;
        private TreeChartOptions fOptions;
        private bool fPathDebug;
        private TreeChartPerson fRoot;
        private float fScale;
        private TreeChartPerson fSelected;
        private GEDCOMIndividualRecord fSaveSelection;
        private ShieldState fShieldState;
        //private string[] FSignsData;
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
            get { return this.fBase; }
            set {
                this.fBase = value;
                this.fTree = this.fBase.Tree;
            }
        }

        public int BorderWidth
        {
            get { return this.fBorderWidth; }
            set { this.SetBorderWidth(value); }
        }

        public int BranchDistance
        {
            get { return this.fBranchDistance; }
            set { this.fBranchDistance = value; }
        }

        public bool CertaintyIndex
        {
            get { return this.fCertaintyIndex; }
            set {
                this.fCertaintyIndex = value;
                this.Invalidate();
            }
        }
        
        public int DepthLimit
        {
            get { return this.fDepthLimit; }
            set { this.fDepthLimit = value; }
        }

        public Font DrawFont
        {
            get { return this.fDrawFont; }
        }

        public ChartFilter Filter
        {
            get { return this.fFilter; }
        }

        public int IndividualsCount
        {
            get { return this.fPreparedIndividuals.Count; }
        }

        public ChartKind Kind
        {
            get { return fKind; }
            set { fKind = value; }
        }

        public int Margins
        {
            get { return this.fMargins; }
            set { this.fMargins = value; }
        }

        public TreeChartOptions Options
        {
            get { return this.fOptions; }
            set { this.fOptions = value; }
        }

        public bool PathDebug
        {
            get { return this.fPathDebug; }
            set { this.fPathDebug = value; }
        }

        public TreeChartPerson Root
        {
            get { return this.fRoot; }
        }

        public new float Scale
        {
            get { return this.fScale; }
            set {
                this.fScale = value;

                fScaleControl.ThumbPos = (int)Math.Round((value -  0.4f) / 0.1f);

                this.Predef();
                this.RecalcChart();

                if (this.fTraceSelected && this.Selected != null) {
                    this.CenterPerson(this.Selected, false);
                }
            }
        }

        public new ScaleControl ScaleControl
        {
            get { return this.fScaleControl; }
        }

        public TreeChartPerson Selected
        {
            get { return this.fSelected; }
            set { this.SetSelected(value); }
        }

        public ShieldState ShieldState
        {
            get { return this.fShieldState; }
            set { this.fShieldState = value; }
        }

        public bool TraceSelected
        {
            get { return this.fTraceSelected; }
            set { this.fTraceSelected = value; }
        }

        public bool TraceKinships
        {
            get { return this.fTraceKinships; }
            set { this.fTraceKinships = value; }
        }

        #endregion

        #region Instance control

        static TreeChartBox()
        {
            TreeChartBox.EventPersonModify = new object();
            TreeChartBox.EventRootChanged = new object();
            TreeChartBox.EventPersonProperties = new object();
        }

        public TreeChartBox() : base()
        {
            base.SetStyle(ControlStyles.AllPaintingInWmPaint | ControlStyles.UserPaint | ControlStyles.OptimizedDoubleBuffer | ControlStyles.ResizeRedraw, true);
            base.UpdateStyles();

            base.BorderStyle = BorderStyle.Fixed3D;
            base.DoubleBuffered = true;
            base.TabStop = true;
            base.BackColor = Color.White;

            this.InitSigns();

            this.fPersons = new PersonList(true);
            this.fFilter = new ChartFilter();
            this.fBranchDistance = DEF_BRANCH_DISTANCE;
            this.fLevelDistance = DEF_LEVEL_DISTANCE;
            this.fSpouseDistance = DEF_SPOUSE_DISTANCE;
            this.fMargins = DEF_MARGINS;
            this.fDepthLimit = -1;
            this.fSelected = null;
            this.fScale = 1.0f;
            this.fTraceSelected = true;
            this.fGraph = new Graph();

            this.fPreparedFamilies = new List<string>();
            this.fPreparedIndividuals = new List<string>();

            this.fScaleControl = new ScaleControl(this);
            //this.fPersonControl = new PersonControl(this);
            this.fToolTip = new ToolTip();

            this.InitTimer();
            this.InitGraphics();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                this.DoneGraphics();

                this.fGraph.Dispose();
                this.fFilter.Dispose();
                this.fPersons.Dispose();
                this.DoneSigns();

                if (this.fDrawFont != null) fDrawFont.Dispose();
                if (this.fScaleControl != null) this.fScaleControl.Dispose();

                if (this.fComponents != null) this.fComponents.Dispose();
            }
            base.Dispose(disposing);
        }

        private static Bitmap PrepareImage(Bitmap source, bool makeTransp)
        {
            if (source == null) return null;

            try {
                Bitmap result = (Bitmap)source.Clone();

                #if __MonoCS__
                result.MakeTransparent(); // don't work
                #else
                result.MakeTransparent(source.GetPixel(0, 0));
                #endif

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
            this.fComponents = new Container();
            this.fTimer = new Timer(this.fComponents);
            this.fTimer.Interval = 1;
            this.fTimer.Tick += this.timer_Tick;
            this.fTimer.Stop();
            this.fTimer.Enabled = false;
            this.fTimer.Enabled = true;
        }

        private void timer_Tick(object sender, EventArgs e)
        {
            if (this.fHighlightedPerson != null)
            {
                DateTime st = DateTime.FromBinary(this.fHighlightedStart);
                DateTime cur = DateTime.Now;
                TimeSpan d = cur - st;

                if (d.Seconds >= 1/* && !this.fPersonControl.Visible*/)
                {
                    this.fHighlightedPerson = null;
                    //this.fPersonControl.Visible = true;
                    this.Invalidate();
                }
            }
        }

        public void GenChart(GEDCOMIndividualRecord iRec, ChartKind kind, bool center)
        {
            if (iRec == null) return;
            
            try
            {
                this.Predef();

                this.fKind = kind;
                this.fSelected = null;
                this.fPersons.Clear();
                this.fGraph.Clear();
                this.DoFilter(iRec);
                this.fRoot = null;
                this.fPreparedIndividuals.Clear();

                switch (this.fKind) {
                    case ChartKind.ckAncestors:
                        this.fPreparedFamilies.Clear();
                        this.fRoot = this.DoAncestorsStep(null, iRec, 1, false);
                        break;
                        
                    case ChartKind.ckDescendants:
                        this.fPreparedFamilies.Clear();
                        this.fRoot = this.DoDescendantsStep(null, iRec, 1);
                        break;

                    case ChartKind.ckBoth:
                        this.fPreparedFamilies.Clear();
                        this.fRoot = this.DoAncestorsStep(null, iRec, 1, false);
                        this.fPreparedFamilies.Clear();
                        this.DoDescendantsStep(null, iRec, 1);
                        break;
                }

                this.fKinRoot = this.fRoot;

                this.RecalcChart();

                if (center) this.CenterPerson(this.fRoot);
                
                this.NavAdd(iRec);
                this.DoRootChanged(this.fRoot);
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("TreeChartBox.GenChart(): " + ex.Message);
            }
        }

        public void RefreshTree()
        {
            try {
                if (this.fRoot == null) return;

                GEDCOMIndividualRecord rootRec = this.fRoot.Rec;
                
                this.SaveSelection();
                this.GenChart(rootRec, this.fKind, false);
                this.RestoreSelection();
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("TreeChartBox.RefreshTree(): " + ex.Message);
            }
        }

        public void SaveSelection()
        {
            this.fSaveSelection = (this.fSelected == null) ? null : this.fSelected.Rec;
        }
        
        public void RestoreSelection()
        {
            this.SelectByRec(this.fSaveSelection);
        }

        public void RebuildKinships(bool noRedraw = false)
        {
            try
            {
                if (this.fOptions.Kinship) {
                    TreeChartPerson p = this.fSelected;
                    if (p != null) {
                        this.fKinRoot = p;
                        this.RecalcChart(noRedraw);
                    }
                }
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("TreeChartBox.RebuildKinships(): " + ex.Message);
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

                if (this.fRoot != null && this.fRoot.Rec == iRec) {
                    result = this.fRoot;
                    result.Parent = parent;
                } else {
                    result = new TreeChartPerson(this);
                    result.BuildBy(iRec, ref hasMediaFail);
                    result.Generation = generation;
                    result.Parent = parent;
                    this.fPersons.Add(result);

                    if (this.fOptions.Kinship && iRec != null) {
                        result.Node = this.fGraph.AddVertex(iRec.XRef, result);
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
                this.fBase.Host.LogWrite("TreeChartBox.AddDescPerson(): " + ex.Message);
                throw;
            }
        }

        private TreeChartPerson DoAncestorsStep(TreeChartPerson aChild, GEDCOMIndividualRecord aPerson, int aGeneration, bool dupFlag)
        {
            try
            {
                TreeChartPerson result = null;

                if (aPerson != null)
                {
                    result = new TreeChartPerson(this);
                    result.BuildBy(aPerson, ref hasMediaFail);
                    result.Generation = aGeneration;
                    result.SetFlag(PersonFlag.pfAncWalk);
                    this.fPersons.Add(result);

                    if (aChild != null)
                    {
                        result.AddChild(aChild);
                    }

                    if (this.fOptions.Kinship && aPerson != null)
                    {
                        result.Node = this.fGraph.AddVertex(aPerson.XRef, result);
                    }

                    if ((this.fDepthLimit <= -1 || aGeneration != this.fDepthLimit) && aPerson.ChildToFamilyLinks.Count > 0 && !dupFlag)
                    {
                        GEDCOMFamilyRecord family = aPerson.ChildToFamilyLinks[0].Family;

                        bool isDup = (this.fPreparedFamilies.IndexOf(family.XRef) >= 0);
                        if (!isDup) this.fPreparedFamilies.Add(family.XRef);

                        if (GKUtils.IsRecordAccess(family.Restriction, this.fShieldState))
                        {
                            GEDCOMIndividualRecord iFather = family.GetHusband();
                            GEDCOMIndividualRecord iMother = family.GetWife();

                            bool divorced = (family.GetTagStringValue("_STAT") == "NOTMARR");

                            if (iFather != null && GKUtils.IsRecordAccess(iFather.Restriction, this.fShieldState))
                            {
                                result.Father = this.DoAncestorsStep(result, iFather, aGeneration + 1, isDup);
                                if (result.Father != null)
                                {
                                    result.Father.Divorced = divorced;
                                    result.Father.IsDup = isDup;
                                    if (this.fOptions.Kinship)
                                    {
                                        this.fGraph.AddUndirectedEdge(result.Node, result.Father.Node, 1, (int)RelationKind.rkParent, (int)RelationKind.rkChild);
                                    }
                                }
                            } else {
                                result.Father = null;
                            }

                            if (iMother != null && GKUtils.IsRecordAccess(iMother.Restriction, this.fShieldState))
                            {
                                result.Mother = this.DoAncestorsStep(result, iMother, aGeneration + 1, isDup);
                                if (result.Mother != null)
                                {
                                    result.Mother.Divorced = divorced;
                                    result.Mother.IsDup = isDup;
                                    if (this.fOptions.Kinship)
                                    {
                                        this.fGraph.AddUndirectedEdge(result.Node, result.Mother.Node, 1, (int)RelationKind.rkParent, (int)RelationKind.rkChild);
                                    }
                                }
                            } else {
                                result.Mother = null;
                            }

                            if (result.Father != null && result.Mother != null && this.fOptions.Kinship)
                            {
                                this.fGraph.AddUndirectedEdge(result.Father.Node, result.Mother.Node, 1, (int)RelationKind.rkSpouse, (int)RelationKind.rkSpouse);
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
                this.fBase.Host.LogWrite("TreeChartBox.DoAncestorsStep(): " + ex.Message);
                throw;
            }
        }

        private TreeChartPerson DoDescendantsStep(TreeChartPerson parent, GEDCOMIndividualRecord person, int level)
        {
            try
            {
                TreeChartPerson result = null;
                if (person != null && (!this.fOptions.ChildlessExclude || level <= 1 || person.SpouseToFamilyLinks.Count != 0 || !this.fBase.Context.IsChildless(person)))
                {
                    FilterGroupMode sourceMode = this.fFilter.SourceMode;

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
                            if (this.fFilter.SourceRef == "") {
                                filterSource = null;
                            } else {
                                filterSource = this.fTree.XRefIndex_Find(this.fFilter.SourceRef) as GEDCOMSourceRecord;
                            }
                            if (person.IndexOfSource(filterSource) < 0) {
                                return null;
                            }
                            break;
                    }

                    ChartFilter.BranchCutType branchCut = this.fFilter.BranchCut;
                    if (branchCut != ChartFilter.BranchCutType.None)
                    {
                        if (!(bool)person.ExtData)
                        {
                            return null;
                        }
                    }

                    TreeChartPerson res = this.AddDescPerson(parent, person, false, level);
                    result = res;

                    int num = person.SpouseToFamilyLinks.Count;
                    for (int i = 0; i < num; i++)
                    {
                        GEDCOMFamilyRecord family = person.SpouseToFamilyLinks[i].Family;

                        bool isDup = (this.fPreparedFamilies.IndexOf(family.XRef) >= 0);
                        if (!isDup) this.fPreparedFamilies.Add(family.XRef);

                        if (GKUtils.IsRecordAccess(family.Restriction, this.fShieldState))
                        {
                            TreeChartPerson resParent = null;
                            GEDCOMSex sex = person.Sex;
                            TreeChartPerson ft = null;
                            TreeChartPerson mt = null;
                            PersonFlag descFlag = PersonFlag.pfDescByFather;

                            switch (sex) {
                                case GEDCOMSex.svFemale:
                                    {
                                        GEDCOMIndividualRecord sp = family.GetHusband();
                                        resParent = this.AddDescPerson(null, sp, true, level);
                                        resParent.Sex = GEDCOMSex.svMale;
                                        ft = resParent;
                                        mt = res;
                                        descFlag = PersonFlag.pfDescByFather;
                                        break;
                                    }

                                case GEDCOMSex.svMale:
                                    {
                                        GEDCOMIndividualRecord sp = family.GetWife();
                                        resParent = this.AddDescPerson(null, sp, true, level);
                                        resParent.Sex = GEDCOMSex.svFemale;
                                        ft = res;
                                        mt = resParent;
                                        descFlag = PersonFlag.pfDescByMother;
                                        break;
                                    }
                            }

                            if (resParent != null)
                            {
                                if (this.fOptions.Kinship)
                                {
                                    this.fGraph.AddUndirectedEdge(res.Node, resParent.Node, 1, (int)RelationKind.rkSpouse, (int)RelationKind.rkSpouse);
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

                            ft.IsDup = isDup;
                            mt.IsDup = isDup;

                            if ((this.fDepthLimit <= -1 || level != this.fDepthLimit) && (!isDup))
                            {
                                int num2 = family.Childrens.Count;
                                for (int j = 0; j < num2; j++)
                                {
                                    GEDCOMIndividualRecord childRec = (GEDCOMIndividualRecord)family.Childrens[j].Value;
                                    if (GKUtils.IsRecordAccess(childRec.Restriction, this.fShieldState))
                                    {
                                        TreeChartPerson child = this.DoDescendantsStep(resParent, childRec, level + 1);
                                        if (child != null)
                                        {
                                            child.Father = ft;
                                            child.Mother = mt;
                                            //int d = (int)desc_flag;
                                            child.SetFlag(descFlag);
                                            if (this.fOptions.Kinship)
                                            {
                                                this.fGraph.AddUndirectedEdge(child.Node, ft.Node, 1, (int)RelationKind.rkParent, (int)RelationKind.rkChild);
                                                this.fGraph.AddUndirectedEdge(child.Node, mt.Node, 1, (int)RelationKind.rkParent, (int)RelationKind.rkChild);
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
                this.fBase.Host.LogWrite("TreeChartBox.DoDescendantsStep(): " + ex.Message);
                throw;
            }
        }

        #endregion

        #region Kinship relations

        private void FindRelationship(TreeChartPerson target)
        {
            if (target == null) return;

            if (target.Node == null) {
                target.Kinship = "";
                return;
            }

            try
            {
                IEnumerable<IEdge> edgesPath = this.fGraph.GetPath(target.Node);

                string tmp = "";
                RelationKind prevRel = RelationKind.rkNone;
                RelationKind finRel = RelationKind.rkNone;
                int great = 0;

                foreach (Edge edge in edgesPath)
                {
                    TreeChartPerson xFrom = (TreeChartPerson)edge.Source.Value;
                    TreeChartPerson xTo = (TreeChartPerson)edge.Target.Value;
                    RelationKind curRel = FixLink(xFrom, xTo, (RelationKind)((int)edge.Value));

                    if (this.fPathDebug) {
                        if (tmp != "") tmp += ", ";
                        if (xFrom.Rec != null) tmp += (xFrom.Rec.XRef + ">" + GKData.RelationSigns[(int)curRel] + ">");
                        if (xTo.Rec != null) tmp += xTo.Rec.XRef;
                    }

                    if (prevRel != RelationKind.rkUndefined)
                    {
                        int g;
                        int lev;
                        finRel = KinshipsMan.FindKinship(prevRel, curRel, out g, out lev);
                        great += g;
                        prevRel = finRel;
                    }
                }

                if (this.fPathDebug) {
                    if (target.Rec != null) target.PathDebug = target.Rec.XRef + " ";

                    target.PathDebug = target.PathDebug + " [" + tmp + "]";
                }

                string result = "[" + FixRelation(target, finRel, great) + "]";
                target.Kinship = result;
            }
            finally
            {
            }
        }

        private static RelationKind FixLink(TreeChartPerson xFrom, TreeChartPerson xTo, RelationKind rel)
        {
            RelationKind xRel = rel;

            switch (rel)
            {
                case RelationKind.rkParent:
                    switch (xTo.Sex)
                    {
                        case GEDCOMSex.svMale:
                            xRel = RelationKind.rkFather;
                            break;
                        case GEDCOMSex.svFemale:
                            xRel = RelationKind.rkMother;
                            break;
                    }
                    break;

                case RelationKind.rkSpouse:
                    switch (xTo.Sex)
                    {
                        case GEDCOMSex.svMale:
                            xRel = RelationKind.rkHusband;
                            break;
                        case GEDCOMSex.svFemale:
                            xRel = RelationKind.rkWife;
                            break;
                    }
                    break;

                case RelationKind.rkChild:
                    switch (xTo.Sex)
                    {
                        case GEDCOMSex.svMale:
                            xRel = RelationKind.rkSon;
                            break;
                        case GEDCOMSex.svFemale:
                            xRel = RelationKind.rkDaughter;
                            break;
                    }
                    break;

                default:
                    xRel = rel;
                    break;
            }

            return xRel;
        }

        private static string FixRelation(TreeChartPerson target, RelationKind rel, int great)
        {
            string tmp = "";
            if (great != 0)
            {
                if (rel >= RelationKind.rkUncle && rel < RelationKind.rkNephew)
                {
                    tmp = GKData.Numerals[great] + GKData.NumKinship[(int)target.Sex] + " ";
                    if (rel == RelationKind.rkUncle)
                    {
                        rel = RelationKind.rkGrandfather;
                    }
                    if (rel == RelationKind.rkAunt)
                    {
                        rel = RelationKind.rkGrandmother;
                    }
                }
                else
                {
                    if (rel != RelationKind.rkUndefined)
                    {
                        tmp = GetGreat(great);
                    }
                }
            }
            else
            {
                tmp = "";
            }
            return tmp + LangMan.LS(GKData.RelationKinds[(int)rel]);
        }

        private static string GetGreat(int n)
        {
            string result = "";
            for (int i = 1; i <= n; i++)
            {
                result += "пра";
            }
            return result;
        }

        #endregion

        #region Drawing routines

        private bool IsPersonVisible(ExtRect pnRect)
        {
            return this.fVisibleArea.IntersectsWith(pnRect.ToRectangle());
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
            Range<int> rangeX = new Range<int>(this.fVisibleArea.Left, this.fVisibleArea.Right);
            Range<int> rangeY = new Range<int>(this.fVisibleArea.Top, this.fVisibleArea.Bottom);

            CheckSwap(ref x1, ref x2);
            CheckSwap(ref y1, ref y2);
            
            return rangeX.IsOverlapped(new Range<int>(x1, x2)) && rangeY.IsOverlapped(new Range<int>(y1, y2));
        }

        private void DrawLine(Graphics gfx, int x1, int y1, int x2, int y2)
        {
            if (!IsLineVisible(x1, y1, x2, y2)) return;
            
            int sX = this.fSPX + x1;
            int sX2 = this.fSPX + x2;
            int sY = this.fSPY + y1;
            int sY2 = this.fSPY + y2;
            gfx.DrawLine(fLinePen, sX, sY, sX2, sY2);

            if (this.fOptions.Decorative) {
                if (sX == sX2) {
                    gfx.DrawLine(fDecorativeLinePen, sX + 1, sY + 1, sX2 + 1, sY2 - 1);
                } else {
                    if (sY == sY2) {
                        gfx.DrawLine(fDecorativeLinePen, sX + 1, sY + 1, sX2 + 0, sY2 + 1);
                    }
                }
            }
        }

        private void InitGraphics()
        {
            fLinePen = new Pen(Color.Black, 1f);
            fDecorativeLinePen = new Pen(Color.Silver, 1f);
            fSolidBlack = new SolidBrush(Color.Black);
        }

        private void DoneGraphics()
        {
            this.fLinePen.Dispose();
            this.fDecorativeLinePen.Dispose();
            this.fSolidBlack.Dispose();
        }

        private void DrawText(Graphics gfx, ExtRect rt, string s, int h, int line)
        {
            int stw = gfx.MeasureString(s, this.DrawFont).ToSize().Width;
            int rx = rt.Left + ((rt.Right - rt.Left + 1) - stw) / 2;
            int ry = rt.Top + (10 + (h * line));
            gfx.DrawString(s, this.DrawFont, fSolidBlack, rx, ry);
        }
        
        private const float HIGHLIGHTED_VAL = 0.1f;
        
        private void DrawBorder(Graphics gfx, Pen xpen, ExtRect rt, bool dead, TreeChartPerson person)
        {
            Rectangle rect = rt.ToRectangle();
            Color bColor;
            bool highlighted = (this.fHighlightedPerson == person);
            
            switch (person.Sex) {
                case GEDCOMSex.svMale:
                    {
                        if (!dead) {
                            if (person.IsDup) {
                                bColor = Color.FromArgb(192, 192, 192);
                            } else {
                                bColor = person.Divorced ? this.Options.UnHusbandColor : this.Options.MaleColor;
                            }
                        } else {
                            bColor = Color.Black;
                        }
                        
                        if (highlighted) bColor = GfxHelper.Lighter(bColor, HIGHLIGHTED_VAL);
                        gfx.FillRectangle(new SolidBrush(bColor), rect.Left, rect.Top, rect.Width, rect.Height);
                        gfx.DrawRectangle(xpen, rect.Left, rect.Top, rect.Width, rect.Height);
                        break;
                    }

                case GEDCOMSex.svFemale:
                    {
                        if (!dead) {
                            if (person.IsDup) {
                                bColor = Color.FromArgb(192, 192, 192);
                            } else {
                                bColor = person.Divorced ? this.Options.UnWifeColor : this.Options.FemaleColor;
                            }
                        } else {
                            bColor = Color.Black;
                        }

                        if (highlighted) bColor = GfxHelper.Lighter(bColor, HIGHLIGHTED_VAL);
                        GraphicsPath path = GfxHelper.CreateRoundedRectangle(rect.Left, rect.Top, rect.Width, rect.Height, 6);
                        
                        /*gfx.TranslateTransform(3, 3);
					GKUtils.DrawPathWithFuzzyLine(path, gfx, Color.Black, 200, 20, 2);
					gfx.ResetTransform();*/

                        gfx.FillPath(new SolidBrush(bColor), path);
                        gfx.DrawPath(xpen, path);
                        break;
                    }

                default:
                    {
                        bColor = !dead ? this.Options.UnkSexColor : Color.Black;
                        gfx.FillRectangle(new SolidBrush(bColor), rect.Left, rect.Top, rect.Width, rect.Height);
                        gfx.DrawRectangle(xpen, rect.Left, rect.Top, rect.Width, rect.Height);
                        break;
                    }
            }
        }

        private static ExtRect GetExpanderRect(ExtRect personRect)
        {
            ExtRect expRt = ExtRect.Create(personRect.Left, personRect.Top - 18, personRect.Left + 16 - 1, personRect.Top - 2);
            return expRt;
        }

        private void DrawPerson(Graphics gfx, int spx, int spy, TreeChartPerson person, DrawMode drawMode)
        {
            try {
                ExtRect rt = person.Rect;
                if (drawMode == DrawMode.dmScreen && !this.IsPersonVisible(rt))
                    return;

                rt.Offset(spx, spy);
                int h = gfx.MeasureString("A", this.DrawFont).ToSize().Height;

                if (person.IsDead) {
                    ExtRect dt = rt.GetOffset(-2, -2);
                    this.DrawBorder(gfx, fLinePen, dt, true, person);
                }

                Pen xpen = null;
                try {
                    if (drawMode == DrawMode.dmScreen && person.Selected) {
                        const float penWidth = 2.0f;

                        Color penColor;
                        switch (person.Sex) {
                            case GEDCOMSex.svMale:
                                penColor = Color.Blue;
                                break;

                            case GEDCOMSex.svFemale:
                                penColor = Color.Red;
                                break;

                            default:
                                penColor = Color.Black;
                                break;
                        }
                        xpen = new Pen(penColor, penWidth);
                    } else {
                        xpen = new Pen(Color.Black, 1f);
                    }

                    this.DrawBorder(gfx, xpen, rt, false, person);
                } finally {
                    if (xpen != null)
                        xpen.Dispose();
                }
                
                if (drawMode == DrawMode.dmScreen && person.CanExpand) {
                    ExtRect expRt = GetExpanderRect(rt);
                    gfx.DrawImage(fExpPic, expRt.Left, expRt.Top);
                }

                // draw CI only for existing individuals
                if (this.fCertaintyIndex && person.Rec != null) {
                    string cas = string.Format("{0:0.00}", person.CertaintyAssessment);
                    gfx.DrawString(cas, this.DrawFont, fSolidBlack, rt.Left, rt.Bottom);
                }

                if (person.Portrait != null) {
                    ExtRect portRt = person.PortraitArea.GetOffset(rt.Left, rt.Top);
                    gfx.DrawImage(person.Portrait, portRt.ToRectangle());

                    rt.Left += person.PortraitWidth;
                }

                int lines = person.Lines.Length;
                for (int k = 0; k < lines; k++) {
                    this.DrawText(gfx, rt, person.Lines[k], h, k);
                }

                if (this.Options.SignsVisible && !person.Signs.IsEmpty()) {
                    int i = 0;
                    for (SpecialUserRef cps = SpecialUserRef.urRI_StGeorgeCross; cps <= SpecialUserRef.urLast; cps++) {
                        if (person.Signs.Contains(cps)) {
                            Bitmap pic = this.fSignsPic[(int)cps - 1];
                            gfx.DrawImage(pic, rt.Right, rt.Top - 21 + i * pic.Height);
                            i++;
                        }
                    }
                }
            } catch (Exception ex) {
                this.fBase.Host.LogWrite("TreeChartBox.DrawPerson(): " + ex.Message);
            }
        }

        private void DrawAncestors(Graphics gfx, TreeChartPerson person, DrawMode drawMode)
        {
            this.Draw(gfx, person.Father, ChartKind.ckAncestors, drawMode);
            this.Draw(gfx, person.Mother, ChartKind.ckAncestors, drawMode);
            int crY = person.PtY - this.fLevelDistance / 2;

            if (person.Father != null) {
                this.DrawLine(gfx, person.Father.PtX, crY, person.PtX, crY);
                this.DrawLine(gfx, person.Father.PtX, person.Father.PtY + person.Father.Height, person.Father.PtX, crY);
            }

            if (person.Mother != null) {
                this.DrawLine(gfx, person.PtX, crY, person.Mother.PtX, crY);
                this.DrawLine(gfx, person.Mother.PtX, person.Mother.PtY + person.Mother.Height, person.Mother.PtX, crY);
            }

            if (person.Father != null || person.Mother != null) {
                this.DrawLine(gfx, person.PtX, crY, person.PtX, person.PtY);
            }
        }

        private void DrawDescendants(Graphics gfx, TreeChartPerson person, DrawMode drawMode)
        {
            int num = person.GetChildsCount();
            for (int i = 0; i < num; i++) {
                this.Draw(gfx, person.GetChild(i), ChartKind.ckDescendants, drawMode);
            }

            int spbOfs = (person.Height - 10) / (person.GetSpousesCount() + 1);
            int spbBeg = person.PtY + (person.Height - spbOfs * (person.GetSpousesCount() - 1)) / 2;

            switch (person.Sex) {
                case GEDCOMSex.svMale:
                    int num3 = person.GetSpousesCount();
                    for (int i = 0; i < num3; i++) {
                        int spbV = spbBeg + spbOfs * i;
                        this.DrawLine(gfx, person.Rect.Right + 1, spbV, person.GetSpouse(i).Rect.Left, spbV);
                    }
                    break;

                case GEDCOMSex.svFemale:
                    int num2 = person.GetSpousesCount();
                    for (int i = 0; i < num2; i++) {
                        int spbV = spbBeg + spbOfs * i;
                        this.DrawLine(gfx, person.GetSpouse(i).Rect.Right + 1, spbV, person.Rect.Left, spbV);
                    }
                    break;
            }

            int num4 = person.GetSpousesCount();
            for (int i = 0; i < num4; i++) {
                this.Draw(gfx, person.GetSpouse(i), ChartKind.ckDescendants, drawMode);
            }

            int crY = person.PtY + person.Height + this.fLevelDistance / 2;
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
                this.DrawLine(gfx, cx, spbBeg, cx, crY);
                if (person.GetChildsCount() == 1)
                {
                    TreeChartPerson child = person.GetChild(0);
                    this.DrawLine(gfx, child.PtX, crY, child.PtX, child.PtY);
                }
                else
                {
                    int bpx = person.GetChild(0).PtX;
                    int epx = person.GetChild(person.GetChildsCount() - 1).PtX;
                    this.DrawLine(gfx, bpx, crY, epx, crY);
                    int num5 = person.GetChildsCount();
                    for (int i = 0; i < num5; i++) {
                        TreeChartPerson child = person.GetChild(i);
                        this.DrawLine(gfx, child.PtX, crY, child.PtX, child.PtY);
                    }
                }
            }
        }

        private void InternalDraw(Graphics gfx, DrawMode drawMode)
        {
            Rectangle imgRect = new Rectangle(0, 0, fImageWidth, fImageHeight);
            if (this.BackgroundImage == null) {
                using (Brush brush = new SolidBrush(this.BackColor)) {
                    gfx.FillRectangle(brush, imgRect);
                }
            } else {
                using (TextureBrush textureBrush = new TextureBrush(this.BackgroundImage, WrapMode.Tile)) {
                    gfx.FillRectangle(textureBrush, imgRect);
                }
            }

            this.fSPX = 0;
            this.fSPY = 0;

            if (drawMode == DrawMode.dmScreen) {
                /*Rectangle viewPort = this.GetImageViewPort();
			    
				this.fSPX = -viewPort.Left;
				this.fSPY = -viewPort.Top;*/

                this.fSPX += this.fBorderWidth - -this.AutoScrollPosition.X;
                this.fSPY += this.fBorderWidth - -this.AutoScrollPosition.Y;

                Size sz = this.ClientSize;

                if (this.fImageWidth < sz.Width) {
                    this.fSPX += (sz.Width - this.fImageWidth) / 2;
                }

                if (this.fImageHeight < sz.Height) {
                    this.fSPY += (sz.Height - this.fImageHeight) / 2;
                }

                this.fVisibleArea = this.GetSourceImageRegion();
            } else {
                this.fVisibleArea = new Rectangle(0, 0, this.fImageWidth, this.fImageHeight);
            }

            #if DEBUG_IMAGE
            Rectangle irt = new Rectangle(this.fSPX, this.fSPY, this.fImageWidth - 1, this.fImageHeight - 1);
            using (Pen pen = new Pen(Color.Red)) {
                gfx.DrawRectangle(pen, irt);
            }
            #endif

            this.Draw(gfx, this.fRoot, this.fKind, drawMode);

            if (fScaleControl.Visible) fScaleControl.Draw(gfx);
            //if (fPersonControl.Visible) fPersonControl.Draw(gfx);
        }

        protected void Draw(Graphics gfx, TreeChartPerson person, ChartKind dirKind, DrawMode drawMode)
        {
            if (person != null) {
                switch (this.fKind) {
                    case ChartKind.ckAncestors:
                        this.DrawAncestors(gfx, person, drawMode);
                        break;

                    case ChartKind.ckDescendants:
                        this.DrawDescendants(gfx, person, drawMode);
                        break;

                    case ChartKind.ckBoth:
                        if (person == this.fRoot || dirKind == ChartKind.ckAncestors) this.DrawAncestors(gfx, person, drawMode);
                        if (person == this.fRoot || dirKind == ChartKind.ckDescendants) this.DrawDescendants(gfx, person, drawMode);
                        break;
                }

                this.DrawPerson(gfx, this.fSPX, this.fSPY, person, drawMode);
            }
        }

        #endregion

        #region Sizes and adjustment routines

        private void Predef()
        {
            double sc = this.fScale;
            int fsz = (int)Math.Round(this.fOptions.DefFontSize * sc);

            string fontName = (fsz <= 7) ? "Small Fonts" : this.fOptions.DefFontName;

            this.fDrawFont = new Font(fontName, fsz, FontStyle.Regular, GraphicsUnit.Point);
            this.fSpouseDistance = (int)Math.Round(DEF_SPOUSE_DISTANCE * sc);
            this.fBranchDistance = (int)Math.Round(DEF_BRANCH_DISTANCE * sc);
            this.fLevelDistance = (int)Math.Round(DEF_LEVEL_DISTANCE * sc);
            this.fMargins = (int)Math.Round(DEF_MARGINS * sc);
        }
        
        private int InitInfoSize()
        {
            int lines = 0;

            if (this.fOptions.FamilyVisible) {
                lines++;
            }

            if (!this.fOptions.DiffLines) {
                lines++;
            } else {
                lines++;
                lines++;
            }

            if (!this.fOptions.OnlyYears) {
                if (this.fOptions.BirthDateVisible) {
                    lines++;
                }
                if (this.fOptions.DeathDateVisible) {
                    lines++;
                }
            } else {
                lines++;
            }

            if (this.fOptions.Kinship) {
                lines++;
            }

            if (this.fPathDebug) {
                lines++;
            }

            return lines;
        }
        
        private void RecalcChart(bool noRedraw = false)
        {
            if (this.fOptions.Kinship && this.fKinRoot != null) {
                this.fGraph.FindPathTree(this.fKinRoot.Node);
            }

            int lines = this.InitInfoSize();

            using (Graphics gfx = this.CreateGraphics()) {
                int num = this.fPersons.Count;
                for (int i = 0; i < num; i++) {
                    TreeChartPerson p = this.fPersons[i];

                    if (this.fOptions.Kinship) {
                        this.FindRelationship(p);
                    }

                    p.CalcBounds(lines, gfx);
                }
            }

            switch (this.fKind) {
                case ChartKind.ckAncestors:
                    this.RecalcAncestorsChart();
                    break;

                case ChartKind.ckDescendants:
                    this.RecalcDescendantsChart(true);
                    break;

                case ChartKind.ckBoth:
                    this.RecalcAncestorsChart();
                    this.RecalcDescendantsChart(false);
                    break;
            }

            // search bounds
            this.fTreeBounds = ExtRect.Create(int.MaxValue, int.MaxValue, 0, 0);
            int num2 = this.fPersons.Count;
            for (int i = 0; i < num2; i++) {
                TreeChartPerson p = this.fPersons[i];
                this.AdjustTreeBounds(p);
            }

            // adjust bounds
            int offsetX = 0 + this.fMargins - this.fTreeBounds.Left;
            int offsetY = 0 + this.fMargins - this.fTreeBounds.Top;
            this.fTreeBounds = ExtRect.Create(int.MaxValue, int.MaxValue, 0, 0);
            for (int i = 0; i < num2; i++) {
                TreeChartPerson p = this.fPersons[i];
                p.PtX += offsetX;
                p.PtY += offsetY;
                this.AdjustTreeBounds(p);
            }

            this.fImageHeight = this.fTreeBounds.GetHeight() + this.fMargins * 2;
            this.fImageWidth = this.fTreeBounds.GetWidth() + this.fMargins * 2;
            this.fImageSize = new Size(this.fImageWidth, this.fImageHeight);

            this.AdjustViewPort(this.fImageSize, noRedraw);
        }

        private void AdjustTreeBounds(TreeChartPerson person)
        {
            if (person == null) return;
            ExtRect prt = person.Rect;
            
            if (this.fTreeBounds.Left > prt.Left) this.fTreeBounds.Left = prt.Left;
            if (this.fTreeBounds.Top > prt.Top) this.fTreeBounds.Top = prt.Top;
            
            if (this.fTreeBounds.Right < prt.Right) this.fTreeBounds.Right = prt.Right;
            if (this.fTreeBounds.Bottom < prt.Bottom) this.fTreeBounds.Bottom = prt.Bottom;
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

            int offset = (edges[gen] > 0) ? this.fBranchDistance : this.fMargins;
            int bound = edges[gen] + offset;
            if (person.Rect.Left <= bound) {
                ShiftAnc(ref edges, person, bound - person.Rect.Left);
            }

            edges[gen] = person.Rect.Right;

            prev.Add(person);
            if (person.Rect.Top < 0)
            {
                offset = 0 - person.Rect.Top + this.fMargins;
                int num = prev.Count;
                for (int i = 0; i < num; i++) {
                    prev[i].PtY += offset;
                }
            }

            if (person.Father != null && person.Mother != null)
            {
                this.RecalcAnc(prev, ref edges, person.Father, person.PtX - (this.fSpouseDistance + person.Father.Width / 2), person.PtY - this.fLevelDistance - person.Height);
                this.RecalcAnc(prev, ref edges, person.Mother, person.PtX + (this.fSpouseDistance + person.Mother.Width / 2), person.PtY - this.fLevelDistance - person.Height);

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
                    this.RecalcAnc(prev, ref edges, anc, person.PtX, person.PtY - this.fLevelDistance - person.Height);
                }
            }
        }

        private void RecalcAncestorsChart()
        {
            int[] edges = new int[256];
            Array.Clear(edges, 0, edges.Length);

            ExtList<TreeChartPerson> prev = new ExtList<TreeChartPerson>();
            try
            {
                this.RecalcAnc(prev, ref edges, this.fRoot, this.fMargins, this.fMargins);
            }
            finally
            {
                prev.Dispose();
            }
        }

        private void ShiftDesc(TreeChartPerson person, int offset, bool isSingle)
        {
            if (person == null) return;

            if (person == this.fRoot) {
                isSingle = false;
            }

            person.PtX += offset;

            if (person.BaseSpouse != null && (person.BaseSpouse.Sex == GEDCOMSex.svFemale || person.BaseSpouse.GetSpousesCount() == 1))
            {
                this.ShiftDesc(person.BaseSpouse, offset, isSingle);
            } else {
                if (!isSingle) {
                    this.ShiftDesc(person.Father, offset, isSingle);
                    this.ShiftDesc(person.Mother, offset, isSingle);
                } else {
                    if (person.HasFlag(PersonFlag.pfDescByFather)) {
                        this.ShiftDesc(person.Father, offset, isSingle);
                    } else {
                        if (person.HasFlag(PersonFlag.pfDescByMother)) {
                            this.ShiftDesc(person.Mother, offset, isSingle);
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

            int curY = person.PtY + this.fLevelDistance + person.Height;
            int childsWidth = (person.GetChildsCount() - 1) * this.fBranchDistance;

            int num = person.GetChildsCount();
            for (int i = 0; i < num; i++) {
                childsWidth += person.GetChild(i).Width;
            }

            int curX = centX - childsWidth / 2;

            int num2 = person.GetChildsCount();
            for (int i = 0; i < num2; i++) {
                TreeChartPerson child = person.GetChild(i);
                this.RecalcDesc(ref edges, child, new Point(curX + child.Width / 2, curY), true);
                curX = child.Rect.Right + this.fBranchDistance;
            }

            curX = person.GetChild(0).PtX;
            if (person.GetChildsCount() > 1) {
                curX += (person.GetChild(person.GetChildsCount() - 1).PtX - curX) / 2;
            }

            if (fixPair) {
                switch (person.Sex) {
                    case GEDCOMSex.svMale:
                        this.ShiftDesc(person, curX - (this.BranchDistance + person.Width) / 2 + 1 - person.PtX, true);
                        this.ShiftDesc(person.BaseSpouse, curX + (this.BranchDistance + person.BaseSpouse.Width) / 2 - person.BaseSpouse.PtX, true);
                        break;

                    case GEDCOMSex.svFemale:
                        this.ShiftDesc(person, curX + (this.BranchDistance + person.Width) / 2 - person.PtX, true);
                        this.ShiftDesc(person.BaseSpouse, curX - (this.BranchDistance + person.BaseSpouse.Width) / 2 + 1 - person.BaseSpouse.PtX, true);
                        break;
                }
            } else {
                this.ShiftDesc(person, curX - person.PtX, true);
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

            int offset = (edges[gen] > 0) ? this.fBranchDistance : this.fMargins;
            int bound = edges[gen] + offset;
            if (person.Rect.Left <= bound) {
                this.ShiftDesc(person, bound - person.Rect.Left, true);
            }

            if (person.Sex == GEDCOMSex.svMale) {
                this.RecalcDescChilds(ref edges, person);
                edges[gen] = person.Rect.Right;
            }

            if (person.GetSpousesCount() > 0) {
                TreeChartPerson prev = person;
                int num = person.GetSpousesCount();
                for (int i = 0; i < num; i++) {
                    TreeChartPerson sp = person.GetSpouse(i);
                    Point spPt = new Point();

                    switch (person.Sex) {
                        case GEDCOMSex.svMale:
                            spPt = new Point(prev.Rect.Right + (this.fBranchDistance + sp.Width / 2), person.PtY);
                            break;

                        case GEDCOMSex.svFemale:
                            spPt = new Point(prev.Rect.Left - (this.fBranchDistance + sp.Width / 2), person.PtY);
                            break;
                    }

                    this.RecalcDesc(ref edges, sp, spPt, true);

                    if (sp.Sex != GEDCOMSex.svMale) {
                        prev = sp;
                    }
                }
            }

            if (person.Sex == GEDCOMSex.svFemale) {
                this.RecalcDescChilds(ref edges, person);
                edges[gen] = person.Rect.Right;
            }
        }

        private void RecalcDescendantsChart(bool predef)
        {
            int[] edges = new int[256];
            Array.Clear(edges, 0, edges.Length);

            this.RecalcDesc(ref edges, this.fRoot, new Point(this.fMargins, this.fMargins), predef);
        }

        private void SetBorderWidth(int value)
        {
            if (this.fBorderWidth != value) {
                this.fBorderWidth = value;
                this.Invalidate();
            }
        }

        private void SetScroll(int x, int y)
        {
            this.AutoScrollPosition = new Point(x, y);
            this.Invalidate();
            this.OnScroll(new ScrollEventArgs(ScrollEventType.EndScroll, 0));
        }

        private Rectangle GetInsideViewPort(bool includePadding)
        {
            int left = 0;
            int top = 0;
            int width = this.ClientSize.Width;
            int height = this.ClientSize.Height;

            if (includePadding)
            {
                left += this.Padding.Left;
                top += this.Padding.Top;
                width -= this.Padding.Horizontal;
                height -= this.Padding.Vertical;
            }

            return new Rectangle(left, top, width, height);
        }

        private Rectangle GetImageViewPort()
        {
            Rectangle viewPort;

            if (!this.fImageSize.IsEmpty) {
                Rectangle innerRectangle = this.GetInsideViewPort(true);

                int x = !this.HScroll ? (innerRectangle.Width - (this.fImageSize.Width + this.Padding.Horizontal)) / 2 : 0;
                int y = !this.VScroll ? (innerRectangle.Height - (this.fImageSize.Height + this.Padding.Vertical)) / 2 : 0;

                int width = Math.Min(this.fImageSize.Width - Math.Abs(this.AutoScrollPosition.X), innerRectangle.Width);
                int height = Math.Min(this.fImageSize.Height - Math.Abs(this.AutoScrollPosition.Y), innerRectangle.Height);

                viewPort = new Rectangle(x + innerRectangle.Left, y + innerRectangle.Top, width, height);
            } else {
                viewPort = Rectangle.Empty;
            }

            return viewPort;
        }

        private Rectangle GetSourceImageRegion()
        {
            Rectangle region;

            if (!this.fImageSize.IsEmpty) {
                Rectangle viewPort = this.GetImageViewPort();
                region = new Rectangle(-this.AutoScrollPosition.X, -this.AutoScrollPosition.Y, viewPort.Width, viewPort.Height);
            } else {
                region = Rectangle.Empty;
            }

            return region;
        }

        #endregion

        #region Event processing
        
        private void DoPersonModify(PersonModifyEventArgs eArgs)
        {
            PersonModifyEventHandler eventHandler = (PersonModifyEventHandler)base.Events[TreeChartBox.EventPersonModify];
            if (eventHandler == null) return;

            eventHandler(this, eArgs);
        }
        
        private void DoRootChanged(TreeChartPerson person)
        {
            RootChangedEventHandler eventHandler = (RootChangedEventHandler)base.Events[TreeChartBox.EventRootChanged];
            if (eventHandler == null) return;

            eventHandler(this, person);
        }
        
        private void DoPersonProperties(MouseEventArgs eArgs)
        {
            MouseEventHandler eventHandler = (MouseEventHandler)base.Events[TreeChartBox.EventPersonProperties];
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
                    this.Scale += 0.05f;
                    break;

                case Keys.Subtract:
                    this.Scale -= 0.05f;
                    break;

                case Keys.Back:
                    this.NavPrev();
                    return;

                default:
                    e.Handled = true;
                    break;
            }
        }

        protected override void OnResize(EventArgs e)
        {
            this.SaveSelection();
            
            this.AdjustViewPort(this.fImageSize);
            this.fScaleControl.Update();

            this.RestoreSelection();

            base.OnResize(e);
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            this.InternalDraw(e.Graphics, DrawMode.dmScreen);
        }

        protected override void OnDoubleClick(EventArgs e)
        {
            TreeChartPerson p = this.Selected;
            
            this.DoPersonModify(new PersonModifyEventArgs(p));
        }

        protected override void OnMouseWheel(MouseEventArgs e)
        {
            //base.OnMouseWheel(e);
            if (ModifierKeys == Keys.Control) {
                float newScale = this.Scale;

                if (e.Delta > 0) {
                    newScale -= 0.05f;
                } else {
                    newScale += 0.05f;
                }

                if (newScale < 0.5 || newScale > 1.5) return;
                this.Scale = newScale;
            } else {
                int dx = 0, dy = 0;
                if (ModifierKeys == Keys.Shift) {
                    dx = -e.Delta;
                } else {
                    dy = -e.Delta;
                }
                this.AdjustScroll(dx, dy);
            }
        }

        private MouseAction GetMouseAction(MouseEventArgs e, bool isDown, out TreeChartPerson person)
        {
            MouseAction result = MouseAction.maNone;
            person = null;
            
            int aX = e.X - this.fSPX;
            int aY = e.Y - this.fSPY;
            int num = this.fPersons.Count;
            for (int i = 0; i < num; i++) {
                TreeChartPerson p = this.fPersons[i];
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
            if (!this.Focused) base.Focus();

            this.fMouseX = e.X;
            this.fMouseY = e.Y;

            switch (this.fMode) {
                case ChartControlMode.ccmDefault:
                    this.SelectBy(e.X, e.Y, (e.Button == MouseButtons.Left));

                    if (this.fSelected == null && e.Button == MouseButtons.Right)
                    {
                        this.Cursor = Cursors.SizeAll;
                        this.fMode = ChartControlMode.ccmDragImage;
                    }
                    break;

                case ChartControlMode.ccmDragImage:
                    break;

                case ChartControlMode.ccmControlsVisible:
                    this.fScaleControl.MouseDown(e.X, e.Y);
                    break;
            }
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            switch (this.fMode)
            {
                case ChartControlMode.ccmDefault:
                    if (this.fScaleControl.Contains(e.X, e.Y)) {
                        this.fMode = ChartControlMode.ccmControlsVisible;
                        this.fScaleControl.Visible = true;
                        this.fScaleControl.MouseMove(e.X, e.Y, ThumbMoved);

                        Point pt = new Point(e.X, e.Y);
                        pt.Offset(+this.Left, +this.Top);
                        this.fToolTip.Show(this.fScaleControl.Tip, this, pt, 1500);
                    } /*if (this.fPersonControl.Contains(e.X, e.Y)) {
						
					} */else {
                        TreeChartPerson p = this.FindPersonByCoords(e.X, e.Y);
                        if (this.fHighlightedPerson != p)
                        {
                            this.fHighlightedPerson = p;
                            this.fHighlightedStart = DateTime.Now.ToBinary();

                            if (p == null) {
                                //this.fPersonControl.Visible = false;
                            } else {
                                //this.fPersonControl.SetPerson(p);
                            }

                            this.Invalidate();
                        }
//
//						if (p != null && e.Button == MouseButtons.Left)
//						{
//							this.fTreeBox.DoDragDrop(p.Rec.XRef, DragDropEffects.Move);
//						}
                    }
                    break;

                case ChartControlMode.ccmDragImage:
                    this.AdjustScroll(-(e.X - this.fMouseX), -(e.Y - this.fMouseY));
                    this.fMouseX = e.X;
                    this.fMouseY = e.Y;
                    break;

                case ChartControlMode.ccmControlsVisible:
                    if (!this.fScaleControl.Contains(e.X, e.Y)) {
                        this.fMode = ChartControlMode.ccmDefault;
                        this.fScaleControl.Visible = false;
                        this.fToolTip.Hide(this);
                    } else {
                        this.fScaleControl.MouseMove(e.X, e.Y, ThumbMoved);
                    }
                    break;
            }
        }

        private void ThumbMoved(int position)
        {
            this.Scale = 0.4f + (position * 0.1f);
        }

        protected override void OnMouseUp(MouseEventArgs e)
        {
            switch (this.fMode) {
                case ChartControlMode.ccmDefault:
                    TreeChartPerson mPers;
                    MouseAction mAct = this.GetMouseAction(e, false, out mPers);

                    switch (mAct) {
                        case MouseAction.maNone:
                            break;

                        case MouseAction.maProperties:
                            if (this.fSelected == mPers && this.fSelected.Rec != null)
                            {
                                this.DoPersonProperties(new MouseEventArgs(e.Button, 1, e.X, e.Y, 0));
                            }
                            break;

                        case MouseAction.maExpand:
                            this.DoRootChanged(mPers);
                            this.GenChart(mPers.Rec, ChartKind.ckBoth, true);
                            break;
                    }
                    break;

                case ChartControlMode.ccmDragImage:
                    this.Cursor = Cursors.Default;
                    this.fMode = ChartControlMode.ccmDefault;
                    break;

                case ChartControlMode.ccmControlsVisible:
                    this.fScaleControl.MouseUp(e.X, e.Y);
                    break;
            }
        }

        #endregion

        #region Navigation

        protected override void SetNavObject(object obj)
        {
            GEDCOMIndividualRecord iRec = obj as GEDCOMIndividualRecord;
            this.GenChart(iRec, ChartKind.ckBoth, true);
        }
        
        private void SetSelected(TreeChartPerson value)
        {
            if (this.fSelected != null) this.fSelected.Selected = false;
            this.fSelected = value;
            if (this.fSelected != null) this.fSelected.Selected = true;

            this.Invalidate();
        }

        private TreeChartPerson FindPersonByCoords(int aX, int aY)
        {
            TreeChartPerson result = null;
            
            aX -= this.fSPX;
            aY -= this.fSPY;
            int num = this.fPersons.Count;
            for (int i = 0; i < num; i++) {
                TreeChartPerson p = this.fPersons[i];
                if (p.Rect.Contains(aX, aY)) {
                    result = p;
                    break;
                }
            }

            return result;
        }

        private void SelectBy(int aX, int aY, bool needCenter)
        {
            TreeChartPerson p = this.FindPersonByCoords(aX, aY);
            this.SetSelected(p);

            //if (this.FTraceKinships && this.fOptions.Kinship) this.RebuildKinships(true);

            if (p != null && needCenter && this.fTraceSelected) CenterPerson(p);
        }

        public void SelectByRec(GEDCOMIndividualRecord iRec)
        {
            if (iRec == null) return;
            
            int num = this.fPersons.Count;
            for (int i = 0; i < num; i++) {
                TreeChartPerson p = this.fPersons[i];
                if (p.Rec == iRec) {
                    this.SetSelected(p);

                    if (this.fTraceSelected) CenterPerson(p);

                    return;
                }
            }

            this.SetSelected(null);
        }

        private void CenterPerson(TreeChartPerson person, bool animation = true)
        {
            if (person == null) return;

            int dstX = ((person.PtX) - (this.ClientSize.Width / 2));
            int dstY = ((person.PtY + (person.Height / 2)) - (this.ClientSize.Height / 2));

            if (dstX < 0) dstX = dstX + (0 - dstX);
            if (dstY < 0) dstY = dstY + (0 - dstY);

            int oldX = Math.Abs(this.AutoScrollPosition.X);
            int oldY = Math.Abs(this.AutoScrollPosition.Y);

            if ((oldX == dstX) && (oldY == dstY)) return;

            if (animation) {
                TweenLibrary tween = new TweenLibrary();
                tween.StartTween(this.SetScroll, oldX, oldY, dstX, dstY, TweenAnimation.EaseInOutQuad, 20);
            } else {
                this.SetScroll(dstX, dstY);
            }
        }

        #endregion

        #region Filtering and search

        public void DoFilter(GEDCOMIndividualRecord root)
        {
            if (root == null)
                throw new ArgumentNullException("root");

            if (this.fFilter.BranchCut != ChartFilter.BranchCutType.None) {
                GKUtils.InitExtCounts(this.fTree, 0);
                this.DoDescendantsFilter(root);
                root.ExtData = true;
            }
        }

        private bool DoDescendantsFilter(GEDCOMIndividualRecord person)
        {
            bool result = false;
            if (person != null)
            {
                ChartFilter.BranchCutType branchCut = this.fFilter.BranchCut;
                switch (branchCut) {
                    case ChartFilter.BranchCutType.Years:
                        int birthYear = GEDCOMUtils.GetRelativeYear(person, "BIRT");
                        result = (birthYear != 0 && birthYear >= this.fFilter.BranchYear);
                        break;

                    case ChartFilter.BranchCutType.Persons:
                        result = (this.fFilter.BranchPersons.IndexOf(person.XRef + ";") >= 0);
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
            List<ISearchResult> result = new List<ISearchResult>();
            
            Regex regex = GKUtils.InitMaskRegex(searchPattern);
            
            int num = this.fPersons.Count;
            for (int i = 0; i < num; i++) {
                TreeChartPerson person = this.fPersons[i];
                GEDCOMIndividualRecord iRec = person.Rec;
                if (iRec == null) continue;
                
                string fullname = iRec.GetNameString(true, false);
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
            string ext = FileHelper.GetFileExtension(fileName);

            if ((ext == ".bmp" || ext == ".jpg") && this.fImageWidth >= 65535)
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
                    pic = new Metafile(fileName, this.CreateGraphics().GetHdc());
                } else {
                    pic = new Bitmap(this.fImageWidth, this.fImageHeight, PixelFormat.Format24bppRgb);
                }
                
                try
                {
                    using (Graphics gfx = Graphics.FromImage(pic)) {
                        this.Predef();
                        this.InternalDraw(gfx, DrawMode.dmFile);
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
            return (this.fImageHeight < this.fImageWidth);
        }

        public Image GetPrintableImage()
        {
            Rectangle frameRect = new Rectangle(0, 0, this.fImageWidth, this.fImageHeight);
            Image image = new Metafile(this.CreateGraphics().GetHdc(), frameRect, MetafileFrameUnit.Pixel, EmfType.EmfOnly);

            using (Graphics gfx = Graphics.FromImage(image)) {
                this.Predef();
                this.InternalDraw(gfx, DrawMode.dmFile);
            }

            return image;
        }

        #endregion
    }
}
