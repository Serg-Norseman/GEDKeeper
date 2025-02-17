/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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

//#define DEBUG_RECALC
//#define DESK_METHOD

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Design.Graphics;
using GKCore.Interfaces;
using GKCore.Kinships;
using GKCore.Options;
using GKCore.Search;
using GKCore.Types;
using BSDColors = GKCore.Design.BSDConsts.Colors;

namespace GKCore.Charts
{
    public delegate void PersonModifyEventHandler(object sender, PersonModifyEventArgs eArgs);

    public delegate void RootChangedEventHandler(object sender, TreeChartPerson person);

    public delegate void InfoRequestEventHandler(object sender, TreeChartPerson person);

    public enum TreeChartKind
    {
        ckAncestors,
        ckDescendants,
        ckBoth
    }

    public struct LineHandle
    {
        public int X1, Y1;
        public int X2, Y2;

        public float Distance
        {
            get { return (float)Math.Sqrt(X1 * X2 + Y1 * Y2); }
        }
    }

    /// <summary>
    ///
    /// </summary>
    public class TreeChartModel : ChartModel
    {
        private Dictionary<LineHandle, float> fLines = new Dictionary<LineHandle, float>();

        private const int EDGES_SIZE = 255; // -127..+127, 0..254, 255, 1 unused

        public const int DEF_MARGINS = 24;
        public const int DEF_SPOUSE_DISTANCE = 20;
        public const int DEF_BRANCH_DISTANCE = 40;
        public const int DEF_LEVEL_DISTANCE = 46;
        public const float HIGHLIGHTED_VAL = 0.1f;

        // Specifies the interior spacing of a node.
        public const int DEF_PERSON_NODE_PADDING = 10;

        private readonly ChartFilter fFilter;
        private readonly List<TreeChartPerson> fPersons;
        private readonly HashSet<string> fPreparedFamilies;
        private readonly HashSet<string> fPreparedIndividuals;

        private IColor fClrBlack;

        private IBaseContext fContext;
        private IBaseWindow fBase;
        private IFont fBoldFont;
        private int fBranchDistance;
        private bool fCertaintyIndex;
        private int fDefCharWidth;
        private int fDepthLimitAncestors;
        private int fDepthLimitDescendants;
        private IFont fDrawFont;
#if !DESK_METHOD
        private int[] fEdges;
#else
        private TreeChartDesk fDesk;
#endif
        private IImage fBookmarkPic;
        private IImage fExpPic;
        private IImage fInfoPic;
        private IImage fPersExpPic;
        private GKVarCache<GDMIndividualRecord, bool> fFilterData;
        private KinshipsGraph fGraph;
        private bool fHasMediaFail;
        private TreeChartPerson fHighlightedPerson;
        private long fHighlightedStart;
        private TreeChartKind fKind;
        private TreeChartPerson fKinRoot;
        private int fLevelDistance;
        private IPen fLinePen;
        private IPen fLineDecorativePen;
        private IPen fLineSelectedPen;
        private IPen fLineDottedPen;
        private IPen fLineDottedDecorativePen;
        private IPen fLineDottedSelectedPen;
        private IPen fLineTMSYPen;
        private IPen fLineTMSNPen;
        private int fMargins;
        private int fNodePadding;
        private TreeChartOptions fOptions;
        private TreeChartPerson fRoot;
        private GDMIndividualRecord fRootRec;
        private float fScale;
        private float fPicScale;
        private IImage[] fSignsPic;
        private IBrush fSolidBlack;
        private IBrush fSolidF, fSolidM;
        private int fSpouseDistance;
        private ITimer fTimer;
        private GDMTree fTree;
        private ExtRect fTreeBounds;
        private ITreeChart fView;
        private ExtRect fVisibleArea;
        private bool fXRefVisible;


        public IBaseWindow Base
        {
            get { return fBase; }
            set {
                fBase = value;
                fContext = fBase.Context;
                fGraph = new KinshipsGraph(fContext);
                fTree = fContext.Tree;
            }
        }

        public IFont BoldFont
        {
            get { return fBoldFont; }
            set { fBoldFont = value; }
        }

        public int BranchDistance
        {
            get { return fBranchDistance; }
            set { fBranchDistance = value; }
        }

        public bool CertaintyIndex
        {
            get { return fCertaintyIndex; }
            set { fCertaintyIndex = value; }
        }

        public int DepthLimitAncestors
        {
            get { return fDepthLimitAncestors; }
            set { fDepthLimitAncestors = value; }
        }

        public int DepthLimitDescendants
        {
            get { return fDepthLimitDescendants; }
            set { fDepthLimitDescendants = value; }
        }

        public IFont DrawFont
        {
            get { return fDrawFont; }
            set { fDrawFont = value; }
        }

        public ChartFilter Filter
        {
            get { return fFilter; }
        }

        internal bool HasMediaFail
        {
            get { return fHasMediaFail; }
            set { fHasMediaFail = value; }
        }

        public TreeChartPerson HighlightedPerson
        {
            get { return fHighlightedPerson; }
            set {
                fHighlightedPerson = value;
                fHighlightedStart = DateTime.Now.ToBinary();
            }
        }

        public TreeChartKind Kind
        {
            get { return fKind; }
            set { fKind = value; }
        }

        public TreeChartPerson KinRoot
        {
            get { return fKinRoot; }
            set { fKinRoot = value; }
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

        public IList<TreeChartPerson> Persons
        {
            get { return fPersons; }
        }

        public HashSet<string> PreparedIndividuals
        {
            get { return fPreparedIndividuals; }
        }

        public TreeChartPerson Root
        {
            get { return fRoot; }
        }

        public float Scale
        {
            get {
                return fScale;
            }
            set {
                if (value < 0.5f) value = 0.5f;
                if (value > 1.5f) value = 1.5f;

                fScale = value;
            }
        }

        public ExtRect VisibleArea
        {
            get { return fVisibleArea; }
            set { fVisibleArea = value; }
        }

        public bool XRefVisible
        {
            get { return fXRefVisible; }
            set { fXRefVisible = value; }
        }


        public TreeChartModel(ITreeChart view)
        {
            fDepthLimitAncestors = -1;
            fDepthLimitDescendants = -1;
#if !DESK_METHOD
            fEdges = new int[EDGES_SIZE];
#else
            fDesk = new TreeChartDesk();
#endif
            fFilter = new ChartFilter();
            fFilterData = new GKVarCache<GDMIndividualRecord, bool>();
            fGraph = null;
            fPersons = new List<TreeChartPerson>();
            fPreparedFamilies = new HashSet<string>();
            fPreparedIndividuals = new HashSet<string>();
            fScale = 1.0f;
            fPicScale = 1.0f;

            fBranchDistance = DEF_BRANCH_DISTANCE;
            fLevelDistance = DEF_LEVEL_DISTANCE;
            fSpouseDistance = DEF_SPOUSE_DISTANCE;
            fMargins = DEF_MARGINS;

            fView = view;
            fTimer = AppHost.Instance.CreateTimer(100, TickTimer);
            fTimer.Enabled = true;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fGraph != null) fGraph.Dispose();
                fFilter.Dispose();

                DoneGraphics();
                DoneSigns();
                if (fBoldFont != null) fBoldFont.Dispose();
                if (fDrawFont != null) fDrawFont.Dispose();
#if DESK_METHOD
                fDesk.Clear();
#endif

                if (fTimer != null) fTimer.Dispose();
            }
            base.Dispose(disposing);
        }

        private void TickTimer(object sender, EventArgs e)
        {
            if (fHighlightedPerson != null) {
                DateTime st = DateTime.FromBinary(fHighlightedStart);
                DateTime cur = DateTime.Now;
                TimeSpan d = cur - st;

                if (d.TotalSeconds >= 1/* && !fPersonControl.Visible*/) {
                    fHighlightedPerson = null;
                    //fPersonControl.Visible = true;
                    if (fView != null) fView.Invalidate();
                }
            }

            if (fOptions.TrackMatchedSources && fLines.Count > 0) {
                foreach (var lineKey in fLines.Keys.ToList()) {
                    var dashOffset = fLines[lineKey];
                    if (dashOffset < lineKey.Distance) {
                        dashOffset += 1;
                    } else {
                        dashOffset = 0;
                    }
                    fLines[lineKey] = dashOffset;
                }
                if (fView != null) fView.Invalidate();
            }
        }

        private static IImage PrepareImage(ChartRenderer renderer, string name, bool makeTransp)
        {
            if (string.IsNullOrEmpty(name))
                return null;

            try {
                return renderer.LoadResourceImage("Resources." + name, makeTransp);
            } catch (Exception ex) {
                Logger.WriteError("TreeChartModel.PrepareImage()", ex);
                return null;
            }
        }

        private void InitSigns(ChartRenderer renderer)
        {
            if (fSignsPic != null)
                return;

            try {
                int uRefsNum = GKData.SpecialUserRefs.Length;
                fSignsPic = new IImage[uRefsNum];
                for (int i = 0; i < uRefsNum; i++) {
                    fSignsPic[i] = PrepareImage(renderer, GKData.SpecialUserRefs[i].ResName, false);
                }

                fExpPic = PrepareImage(renderer, "btn_expand.gif", true);
                fPersExpPic = PrepareImage(renderer, "btn_expand2.gif", true);
                fInfoPic = PrepareImage(renderer, "btn_info.gif", true);
                fBookmarkPic = PrepareImage(renderer, "tg_bookmark.png", false);
            } catch (Exception ex) {
                Logger.WriteError("TreeChartModel.InitSigns()", ex);
            }
        }

        private void DoneSigns()
        {
            // dummy
        }

        public void Assign(TreeChartModel sourceModel)
        {
            Base = sourceModel.fBase;

            fBoldFont = sourceModel.fBoldFont;
            fBranchDistance = sourceModel.fBranchDistance;
            fCertaintyIndex = sourceModel.fCertaintyIndex;
            fDepthLimitAncestors = sourceModel.fDepthLimitAncestors;
            fDepthLimitDescendants= sourceModel.fDepthLimitDescendants;
            fDrawFont = sourceModel.fDrawFont;
            fBookmarkPic = sourceModel.fBookmarkPic;
            fExpPic = sourceModel.fExpPic;
            fPersExpPic = sourceModel.fPersExpPic;
            fInfoPic = sourceModel.fInfoPic;
            //fKind = sourceModel.fKind;
            //fKinRoot = sourceModel.fKinRoot;
            fLevelDistance = sourceModel.fLevelDistance;
            fLinePen = sourceModel.fLinePen;
            fLineDecorativePen = sourceModel.fLineDecorativePen;
            fLineSelectedPen = sourceModel.fLineSelectedPen;
            fLineDottedPen = sourceModel.fLineDottedPen;
            fLineDottedDecorativePen = sourceModel.fLineDottedDecorativePen;
            fLineDottedSelectedPen = sourceModel.fLineDottedSelectedPen;
            fMargins = sourceModel.fMargins;
            fNodePadding = sourceModel.fNodePadding;
            fOptions = sourceModel.fOptions;
            //fRoot = sourceModel.fRoot;
            fScale = sourceModel.fScale;
            fSignsPic = sourceModel.fSignsPic;
            fSolidBlack = sourceModel.fSolidBlack;
            fSolidF = sourceModel.fSolidF;
            fSolidM = sourceModel.fSolidM;
            fSpouseDistance = sourceModel.fSpouseDistance;
            fTree = sourceModel.fTree;
        }

        private bool IsExtendedTree()
        {
            return fOptions.ExtendedTree && fKind == TreeChartKind.ckBoth;
        }

        /*private bool IsExtPerson(TreeChartPerson person)
        {
            return IsExtendedTree() && (person.Rec == fRootRec || person.HasFlag(PersonFlag.pfRootSpouse));
        }*/

        private bool IsExtRootSpouse(TreeChartPerson person)
        {
            return IsExtendedTree() && person != null && person.HasFlag(PersonFlag.pfRootSpouse);
        }

        public void GenChart(GDMIndividualRecord indiRec, TreeChartKind kind)
        {
            fLines.Clear();

            fPersons.Clear();
            fGraph.Clear();
            fPreparedIndividuals.Clear();
            fPreparedFamilies.Clear();
            fRoot = null;
#if DESK_METHOD
            fDesk.Reset();
#endif

            fKind = kind;
            DoFilter(indiRec);
            fRootRec = indiRec;
            fRoot = VisitPerson(null, indiRec, 0);
            fKinRoot = fRoot;
        }

        #region Tree walking

        private bool RequireAncestors(TreeChartPerson person)
        {
            return ((fKind == TreeChartKind.ckAncestors || fKind == TreeChartKind.ckBoth) && (person.Rec == fRootRec || person.HasFlag(PersonFlag.pfRootSpouse)));
        }

        private bool RequireDescendants(TreeChartPerson person)
        {
            return ((fKind == TreeChartKind.ckDescendants || fKind == TreeChartKind.ckBoth) && (person.Generation >= 0));
        }

        private TreeChartPerson CreatePerson(GDMIndividualRecord iRec, int generation, bool prevSearch = false)
        {
            // search root or previous added ancestors
            TreeChartPerson result = (!prevSearch) ? null : FindPersonByRec(iRec);

            if (result == null) {
                result = new TreeChartPerson(this);
                result.BuildBy(iRec);
                result.Generation = generation;
                fPersons.Add(result);

                if (fOptions.Kinship && iRec != null) {
                    result.Node = fGraph.AddIndividual(iRec);
                }
            }

            return result;
        }

        private void VisitAncestors(TreeChartPerson personNode, bool dupFlag)
        {
            if (personNode == null) return;

            try {
                personNode.SetFlag(PersonFlag.pfAncWalk);

                var indiRec = personNode.Rec;

                if ((fDepthLimitAncestors <= -1 || Math.Abs(personNode.Generation) != fDepthLimitAncestors) && indiRec.ChildToFamilyLinks.Count > 0 && !dupFlag) {
                    GDMChildToFamilyLink childLink = indiRec.ChildToFamilyLinks[0];

                    var adopted = (childLink.PedigreeLinkageType == GDMPedigreeLinkageType.plAdopted);
                    personNode.SetFlag(PersonFlag.pfAdopted, adopted);

                    GDMFamilyRecord family = fTree.GetPtrValue(childLink);

                    bool isDup = fPreparedFamilies.Contains(family.XRef);
                    if (!isDup) fPreparedFamilies.Add(family.XRef);

                    if (fContext.IsRecordAccess(family.Restriction)) {
                        GDMIndividualRecord iFather, iMother;
                        fTree.GetSpouses(family, out iFather, out iMother);

                        bool commonLaw = (family.Status == GDMMarriageStatus.MarrNotRegistered);

                        bool divorced = (family.Status == GDMMarriageStatus.MarrDivorced);

                        if (iFather != null && fContext.IsRecordAccess(iFather.Restriction)) {
                            personNode.Father = VisitParent(personNode, iFather, personNode.Generation - 1, isDup, divorced, adopted, commonLaw);
                        }

                        if (iMother != null && fContext.IsRecordAccess(iMother.Restriction)) {
                            personNode.Mother = VisitParent(personNode, iMother, personNode.Generation - 1, isDup, divorced, adopted, commonLaw);
                        }

                        personNode.SetParents();

                        if (personNode.Father != null && personNode.Mother != null && fOptions.Kinship) {
                            fGraph.AddRelation(personNode.Father.Node, personNode.Mother.Node, RelationKind.rkSpouse, RelationKind.rkSpouse, (!commonLaw ? RelationExt.None : RelationExt.CommonLaw));
                        }

                        if (fOptions.MarriagesDates) {
                            DateFormat dateFormat = (fOptions.OnlyYears) ? DateFormat.dfYYYY : DateFormat.dfDD_MM_YYYY;
                            var marDate = GKUtils.GetMarriageDateStr(family, dateFormat, GlobalOptions.Instance.ShowDatesSign);
                            if (!string.IsNullOrEmpty(marDate)) {
                                if (personNode.Father != null) {
                                    personNode.Father.MarriageDate = marDate;
                                } else if (personNode.Mother != null) {
                                    personNode.Mother.MarriageDate = marDate;
                                }
                            }
                        }
                    }
                } else {
                    if (indiRec.ChildToFamilyLinks.Count > 0) {
                        personNode.SetFlag(PersonFlag.pfHasInvAnc);
                    }
                }

                if (fTree.GetTotalChildrenCount(indiRec) > 1 || indiRec.SpouseToFamilyLinks.Count > 1) {
                    personNode.SetFlag(PersonFlag.pfHasInvDesc);
                }
            } catch (Exception ex) {
                Logger.WriteError("TreeChartModel.VisitAncestors()", ex);
                throw;
            }
        }

        private TreeChartPerson VisitParent(TreeChartPerson childNode, GDMIndividualRecord parentRec, int generation,
            bool dupFlag = false, bool divorced = false, bool adopted = false, bool commonLaw = false)
        {
            if (parentRec == null) return null;

            try {
                TreeChartPerson result = null;

                result = CreatePerson(parentRec, generation);

                result.SetFlag(PersonFlag.pfCommonLaw, commonLaw);
                result.SetFlag(PersonFlag.pfDivorced, divorced);
                result.IsDup = dupFlag;

#if DESK_METHOD
                fDesk.Add(result);
#endif

                if (childNode != null) {
                    result.AddChild(childNode);

                    if (fOptions.Kinship) {
                        fGraph.AddRelation(childNode.Node, result.Node, RelationKind.rkParent, RelationKind.rkChild, (!adopted ? RelationExt.None : RelationExt.Adoption));
                    }
                }

                VisitAncestors(result, dupFlag);

                return result;
            } catch (Exception ex) {
                Logger.WriteError("TreeChartModel.VisitParent()", ex);
                throw;
            }
        }

        private bool CheckDescendantFilter(GDMIndividualRecord person, int level)
        {
            bool result = true;

            switch (fFilter.SourceMode) {
                case FilterGroupMode.All:
                    break;

                case FilterGroupMode.None:
                    if (person.HasSourceCitations) {
                        result = false;
                    }
                    break;

                case FilterGroupMode.Any:
                    if (!person.HasSourceCitations) {
                        result = false;
                    }
                    break;

                case FilterGroupMode.Selected:
                    GDMSourceRecord filterSource;
                    filterSource = string.IsNullOrEmpty(fFilter.SourceRef) ? null : fTree.FindXRef<GDMSourceRecord>(fFilter.SourceRef);
                    if (person.IndexOfSource(filterSource) < 0) {
                        result = false;
                    }
                    break;
            }

            if ((fFilter.BranchCut != ChartFilter.BranchCutType.None) && (!fFilterData[person])) {
                result = false;
            }

            return result;
        }

        private TreeChartPerson AddDescPerson(TreeChartPerson parent, GDMIndividualRecord iRec, bool outsideKin, int generation)
        {
            try {
                TreeChartPerson result;

                if (fRoot != null && fRoot.Rec == iRec) {
                    result = fRoot;
                } else {
                    result = CreatePerson(iRec, generation);

                    if (!outsideKin && parent != null) {
                        parent.AddChild(result);
                    }
                }

                result.SetFlag(PersonFlag.pfOutsideKin, outsideKin);
                result.SetFlag(PersonFlag.pfDescWalk);

                return result;
            } catch (Exception ex) {
                Logger.WriteError("TreeChartModel.AddDescPerson()", ex);
                throw;
            }
        }

        private TreeChartPerson VisitPerson(TreeChartPerson parent, GDMIndividualRecord indiRec, int generation)
        {
            try {
                TreeChartPerson result = null;
                if (indiRec == null) return result;

                int spousesNum = indiRec.SpouseToFamilyLinks.Count;

                // if the person have more than one families - to hide unknown spouses it is impossible
                bool skipUnkSpouses = fOptions.HideUnknownSpouses && spousesNum < 2;

                bool skipChildless = fOptions.ChildlessExclude && fContext.IsChildless(indiRec);

                if (skipChildless && generation != 0 && spousesNum == 0)
                    return null;

                if (!CheckDescendantFilter(indiRec, generation))
                    return null;

                result = AddDescPerson(parent, indiRec, false, generation);

#if DESK_METHOD
                if (!fDesk.Exists(result))
                    fDesk.Add(result);
#endif

                // the man's spouses align to his right
                if (result.Sex == GDMSex.svMale && RequireAncestors(result)) {
                    VisitAncestors(result, false);
                }

                bool withoutSpouses = fOptions.HideDescSpouses;

                for (int i = 0; i < spousesNum; i++) {
                    GDMFamilyRecord family = fTree.GetPtrValue(indiRec.SpouseToFamilyLinks[i]);
                    if (family == null || !fContext.IsRecordAccess(family.Restriction)) continue;

                    bool isDup = fPreparedFamilies.Contains(family.XRef);
                    if (!isDup) fPreparedFamilies.Add(family.XRef);

                    bool commonLaw = (family.Status == GDMMarriageStatus.MarrNotRegistered);
                    result.SetFlag(PersonFlag.pfCommonLaw, commonLaw);

                    bool divorced = (family.Status == GDMMarriageStatus.MarrDivorced);
                    result.SetFlag(PersonFlag.pfDivorced, divorced);

                    TreeChartPerson resParent = null;
                    TreeChartPerson ft = null;
                    TreeChartPerson mt = null;
                    PersonFlag descFlag;
                    bool skipSpouse = false;

                    switch (indiRec.Sex) {
                        case GDMSex.svFemale: {
                                GDMIndividualRecord sp = fTree.GetPtrValue(family.Husband);
                                skipSpouse = withoutSpouses || (skipUnkSpouses && (sp == null));

                                if (!skipSpouse) {
                                    resParent = AddDescPerson(null, sp, true, generation);
                                    resParent.Sex = GDMSex.svMale;
                                    resParent.SetFlag(PersonFlag.pfSpouse);
                                    resParent.SetFlag(PersonFlag.pfCommonLaw, commonLaw);
                                    resParent.SetFlag(PersonFlag.pfDivorced, divorced);

#if DESK_METHOD
                                    fDesk.AddBefore(result, resParent);
#endif

                                    ft = resParent;
                                    ft.IsDup = isDup;

                                    mt = result;
                                    mt.IsDup = isDup;

                                    descFlag = PersonFlag.pfDescByFather;
                                } else {
                                    resParent = null;
                                    mt = result;
                                    mt.IsDup = isDup;
                                    descFlag = PersonFlag.pfDescByMother;
                                }
                                break;
                            }

                        case GDMSex.svMale: {
                                GDMIndividualRecord sp = fTree.GetPtrValue(family.Wife);
                                skipSpouse = withoutSpouses || (skipUnkSpouses && (sp == null));

                                if (!skipSpouse) {
                                    resParent = AddDescPerson(null, sp, true, generation);
                                    resParent.Sex = GDMSex.svFemale;
                                    resParent.SetFlag(PersonFlag.pfSpouse);
                                    resParent.SetFlag(PersonFlag.pfCommonLaw, commonLaw);
                                    resParent.SetFlag(PersonFlag.pfDivorced, divorced);

#if DESK_METHOD
                                    fDesk.Add(resParent);
#endif

                                    ft = result;
                                    ft.IsDup = isDup;

                                    mt = resParent;
                                    mt.IsDup = isDup;

                                    descFlag = PersonFlag.pfDescByMother;
                                } else {
                                    resParent = null;
                                    ft = result;
                                    ft.IsDup = isDup;
                                    descFlag = PersonFlag.pfDescByFather;
                                }
                                break;
                            }

                        default:
                            // sex of spouse is undetermined
                            continue;
                    }

                    if (resParent != null) {
                        if (fOptions.Kinship) {
                            fGraph.AddRelation(result.Node, resParent.Node, RelationKind.rkSpouse, RelationKind.rkSpouse, (!commonLaw ? RelationExt.None : RelationExt.CommonLaw));
                        }

                        result.AddSpouse(resParent);
                        resParent.BaseSpouse = result;
                        resParent.BaseFamily = family;

                        if (resParent.Rec != null) {
                            if (fOptions.MarriagesDates) {
                                //DateFormat dateFormat = (fOptions.OnlyYears) ? DateFormat.dfYYYY : DateFormat.dfDD_MM_YYYY;
                                DateFormat dateFormat = DateFormat.dfYYYY;
                                GlobalOptions glob = GlobalOptions.Instance;
                                var marDate = GKUtils.GetMarriageDateStr(family, dateFormat, glob.ShowDatesSign);
                                resParent.MarriageDate = marDate;
                            }

                            if (resParent.Rec.ChildToFamilyLinks.Count > 0) {
                                resParent.SetFlag(PersonFlag.pfHasInvAnc);
                            }

                            if (resParent.Rec.SpouseToFamilyLinks.Count > 1) {
                                resParent.SetFlag(PersonFlag.pfHasInvDesc);
                            }

                            // additionally display the ancestors of the spouses of the central person
                            if (IsExtendedTree() && indiRec == fRootRec) {
                                resParent.SetFlag(PersonFlag.pfRootSpouse);
                                VisitAncestors(resParent, false);
                            }
                        }
                    } else {
                        resParent = result;
                    }

                    if (RequireDescendants(result) && (fDepthLimitDescendants <= -1 || Math.Abs(generation) != fDepthLimitDescendants) && (!isDup)) {
                        int num2 = family.Children.Count;
                        for (int j = 0; j < num2; j++) {
                            var childRec = fTree.GetPtrValue(family.Children[j]);
                            if (childRec == null || !fContext.IsRecordAccess(childRec.Restriction)) continue;

                            TreeChartPerson child = VisitPerson(resParent, childRec, generation + 1);
                            if (child == null) continue;

                            child.Father = ft;
                            child.Mother = mt;
                            child.SetFlag(descFlag);
                            child.SetParents();

                            GDMChildToFamilyLink childLink = childRec.FindChildToFamilyLink(family);
                            var adopted = (childLink != null && (childLink.PedigreeLinkageType == GDMPedigreeLinkageType.plAdopted));
                            child.SetFlag(PersonFlag.pfAdopted, adopted);

                            if (fOptions.Kinship) {
                                if (ft != null) fGraph.AddRelation(child.Node, ft.Node, RelationKind.rkParent, RelationKind.rkChild, (!adopted ? RelationExt.None : RelationExt.Adoption));
                                if (mt != null) fGraph.AddRelation(child.Node, mt.Node, RelationKind.rkParent, RelationKind.rkChild, (!adopted ? RelationExt.None : RelationExt.Adoption));
                            }
                        }
                    } else {
                        if (family.Children.Count > 0) {
                            if (ft != null) ft.SetFlag(PersonFlag.pfHasInvDesc);
                            if (mt != null) mt.SetFlag(PersonFlag.pfHasInvDesc);
                        }
                    }
                }

                // the man's spouses align to his right
                if (result.Sex == GDMSex.svFemale && RequireAncestors(result)) {
                    VisitAncestors(result, false);
                }

                return result;
            } catch (Exception ex) {
                Logger.WriteError("TreeChartModel.VisitPerson()", ex);
                throw;
            }
        }

        #endregion

        #region Kinships

        private void FindRelationship(TreeChartPerson target)
        {
            if (target == null) return;

            if (target.Node == null || target.Rec == null) {
                target.Kinship = "";
            } else {
                string kinship = fGraph.GetRelationship(target.Rec, false, GlobalOptions.Instance.ShortKinshipForm);
                if (kinship == "?") {
                    kinship = "-";
                }
                target.Kinship = "[" + kinship + "]";
            }
        }

        #endregion

        #region Sizes and adjustment routines

        public void ToggleCollapse(TreeChartPerson person)
        {
            if (person != null) {
                person.IsCollapsed = !person.IsCollapsed;

                if (person.GetSpousesCount() > 0) {
                    int num = person.GetSpousesCount();
                    for (int i = 0; i < num; i++) {
                        TreeChartPerson sp = person.GetSpouse(i);
                        sp.IsCollapsed = person.IsCollapsed;
                    }
                }
            }
        }

        private void Predef()
        {
            fPicScale = (fScale < 1.0f) ? fScale : 1.0f;
            float fsz = (float)Math.Round(fOptions.DefFontSize * fScale, 2);
#if NETCORE && OS_MSWIN
            // Eto.Forms [WPF] <= 2.7.4
            // https://social.msdn.microsoft.com/Forums/en-US/98717e53-89f7-4d5f-823b-7184781a7b85/wpf-formattedtext-randomly-disappears-in-high-resolution-images
            fsz = Math.Max(fsz, 5.0f);
#endif
            string fontName = fOptions.DefFontName;
            fBoldFont = fRenderer.CreateFont(fontName, fsz, true);
            fDrawFont = fRenderer.CreateFont(fontName, fsz, false);
            if (fRenderer != null) {
                fDefCharWidth = fRenderer.GetTextWidth("A", fDrawFont);
            }

            fBranchDistance = (int)Math.Round(fOptions.BranchDistance * fScale);
            fLevelDistance = (int)Math.Round(fOptions.LevelDistance * fScale);
            fMargins = (int)Math.Round(fOptions.Margins * fScale);
            fNodePadding = (int)(fOptions.Padding * fScale);
            fSpouseDistance = (int)Math.Round(fOptions.SpouseDistance * fScale);
        }

        private void RecalcPersonBounds()
        {
            if (fOptions.Kinship && fKinRoot != null) {
                fGraph.SetTreeRoot(fKinRoot.Rec);
            }

            int lines = TreeChartPerson.InitInfoSize(fOptions);

            int maxWidth = 0;
            int num = fPersons.Count;
            for (int i = 0; i < num; i++) {
                TreeChartPerson p = fPersons[i];

                if (fOptions.Kinship) {
                    FindRelationship(p);
                }

                p.IsVisible = false;
                p.CalcBounds(lines, fRenderer);

                maxWidth = Math.Max(maxWidth, p.Width);
            }

            if (fOptions.SameCardsWidth) {
                for (int i = 0; i < num; i++) {
                    fPersons[i].Width = maxWidth;
                }
            }
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

        private void RecalcTreeBounds()
        {
            // search bounds
            fTreeBounds = ExtRect.Create(int.MaxValue, int.MaxValue, 0, 0);
            int num2 = fPersons.Count;
            for (int i = 0; i < num2; i++) {
                TreeChartPerson p = fPersons[i];
                if (p.IsVisible) {
                    AdjustTreeBounds(p);
                }
            }

            // adjust bounds
            int offsetX = 0 + fMargins - fTreeBounds.Left;
            int offsetY = 0 + fMargins - fTreeBounds.Top;
            fTreeBounds = ExtRect.Create(int.MaxValue, int.MaxValue, 0, 0);
            for (int i = 0; i < num2; i++) {
                TreeChartPerson p = fPersons[i];
                if (p.IsVisible) {
                    p.PtX += offsetX;
                    p.PtY += offsetY;
                    AdjustTreeBounds(p);
                }
            }

            fImageHeight = Math.Max(fTreeBounds.GetHeight(), 0) + fMargins * 2;
            fImageWidth = Math.Max(fTreeBounds.GetWidth(), 0) + fMargins * 2;
        }

        public void RecalcChart()
        {
            Predef();
            RecalcPersonBounds();
            //ClearEdges();

            switch (fKind) {
                case TreeChartKind.ckAncestors:
                    RecalcAncestorsChart();
                    break;

                case TreeChartKind.ckDescendants:
                    RecalcDescendantsChart(true);
                    break;

                case TreeChartKind.ckBoth:
                    RecalcAncestorsChart();
                    RecalcDescendantsChart(false);
                    break;
            }

            RecalcTreeBounds();
        }

        private void RecalcAncestorsChart()
        {
#if !DESK_METHOD
            ClearEdges();
#endif

            if (IsExtendedTree()) {
                // the man's spouses align to his right
                if (fRoot.Sex == GDMSex.svMale) {
                    RecalcAnc(fRoot, fMargins, fMargins);
                }

                int spousesCount = fRoot.GetSpousesCount();
                for (int i = 0; i < spousesCount; i++) {
                    TreeChartPerson sp = fRoot.GetSpouse(i);
                    RecalcAnc(sp, fMargins, fMargins);
                }

                // the woman's spouses align to her left
                if (fRoot.Sex == GDMSex.svFemale) {
                    RecalcAnc(fRoot, fMargins, fMargins);
                }
            } else {
                RecalcAnc(fRoot, fMargins, fMargins);
            }
        }

        private void RecalcDescendantsChart(bool predef)
        {
#if !DESK_METHOD
            ClearEdges();
#endif
            RecalcDesc(fRoot, fMargins, fMargins, predef);
        }

#if !DESK_METHOD
        private void ClearEdges()
        {
            Array.Clear(fEdges, 0, EDGES_SIZE);
        }
#endif

        private void SetEdge(int generation, int value)
        {
#if !DESK_METHOD
            if (generation < -127 || generation > 127)
                throw new ArgumentOutOfRangeException("generation");

            int index = generation + 127;
            fEdges[index] = value;
#endif
        }

        private void SetEdge(TreeChartPerson person)
        {
            SetEdge(person.Generation, person.Rect.Right);
        }

        /// <summary>
        /// If the option to minimize the diagram width is not selected,
        /// the function takes the right edge from the previous branch of the given generation,
        /// otherwise it searches for the rightmost edge from the current generation and all subsequent ones.
        /// </summary>
        private int GetEdge(TreeChartPerson person, bool ancestors)
        {
#if !DESK_METHOD
            int generation = person.Generation;

            if (generation < -127 || generation > 127)
                throw new ArgumentOutOfRangeException("generation");

            int index = generation + 127;
            int result = fEdges[index];

            if (!fOptions.MinimizingWidth) {
                if (ancestors) {
                    for (int i = index - 1; i >= 0; i--) {
                        result = Math.Max(result, fEdges[i]);
                    }
                } else {
                    for (int i = index + 1; i < EDGES_SIZE; i++) {
                        result = Math.Max(result, fEdges[i]);
                    }
                }
            }

            return result;
#else
            var left = fDesk.GetBefore(person);

            if (left == null) {
                return 0;
            } else {
                return left.Rect.Right;
            }
#endif
        }

        private int GetCurrentBranchLeftBound(TreeChartPerson person, bool ancestors)
        {
            int edge = GetEdge(person, ancestors);
            int offset = (edge > 0) ? fBranchDistance : fMargins;
            int bound = edge + offset;
            return bound;
        }

        /// <summary>
        /// The function returns the Y-coordinate of the next generation,
        /// depending on the type of movement (ancestors, descendants) and the chart inversion option.
        /// </summary>
        private int NextGenY(TreeChartPerson person, bool ancestors)
        {
            int sign = (ancestors) ? -1 : +1;
            int sign2 = (!fOptions.InvertedTree) ? +1 : -1;
            int offset = (fLevelDistance + person.Height) * sign * sign2;
            return person.PtY + offset;
        }

        /// <summary>
        /// A recursive shift of previously passed generations of descendants is needed in this case:
        /// if the previously passed generation did not intersect on the left, and on the current generation
        /// the left branch hangs over the previous ones, only the current generation will be broken to the right,
        /// and the underlying ones will remain under the overhanging left branch.
        /// </summary>
        private void ShiftAnc(TreeChartPerson person, int offset)
        {
            TreeChartPerson pp = person;
            if (pp == null) return;

            do {
                pp.PtX += offset;
                SetEdge(pp);

                pp = (pp.GetChildsCount() < 1) ? null : pp.GetChild(0);
            }
            while (pp != null && pp.Generation <= 0);
        }

        private void RecalcAnc(TreeChartPerson person, int ptX, int ptY)
        {
            if (person == null) return;

            WriteDebugInfo("RecalcAnc", person);

            person.PtX = ptX;
            person.PtY = ptY;

            int bound = GetCurrentBranchLeftBound(person, true);
            if (person.Rect.Left < bound) {
                ShiftAnc(person, bound - person.Rect.Left);
            }

            SetEdge(person);

            person.IsVisible = true;

            if (person.IsCollapsed) {
                return;
            }

            if (person.Father != null && person.Mother != null) {
                RecalcAnc(person.Father, person.PtX - (fSpouseDistance + person.Father.Width / 2), NextGenY(person, true));
                RecalcAnc(person.Mother, person.PtX + (fSpouseDistance + person.Mother.Width / 2), NextGenY(person, true));

                // alignment of child coordinates between parents
                person.PtX = (person.Father.PtX + person.Mother.PtX) / 2;

                SetEdge(person);
            } else {
                TreeChartPerson parent = null;
                if (person.Father != null) {
                    parent = person.Father;
                } else if (person.Mother != null) {
                    parent = person.Mother;
                }

                if (parent != null) {
                    RecalcAnc(parent, person.PtX, NextGenY(person, true));
                }
            }
        }

        private static void WriteDebugInfo(string method, TreeChartPerson person)
        {
#if DEBUG_RECALC
            var persName = ((person.Rec == null) ? "" : GKUtils.GetNameString(person.Rec, true, false));
            Logger.WriteInfo(string.Format("{0}({1})", method, persName));
#endif
        }

        /// <summary>
        /// Shift spouse and central person (if female) if needed to shift their descendants.
        /// </summary>
        /// <param name="person"></param>
        /// <param name="offset"></param>
        /// <returns>
        ///     if true, then the shift of the previous iteration can be performed
        /// </returns>
        private bool ShiftSpousesFrom(TreeChartPerson person, int offset)
        {
            // can not move the ancestors of the spouses to the left,
            // because there may be ancestors of the central person or another spouse;
            // it is impossible to find out exactly where the other branch is on the left,
            // because at this stage, we do not know which branch is being processed
            if (offset < 0)
                return false;

            WriteDebugInfo("ShiftSpousesFrom", person);

            TreeChartPerson basePerson, lastPers = null;
            int startIndex;

            if (person.BaseSpouse == null) {
                // root
                basePerson = person;
                startIndex = (person.Sex == GDMSex.svMale) ? 0 : basePerson.GetSpousesCount();
            } else {
                // root spouse
                basePerson = person.BaseSpouse;
                startIndex = basePerson.IndexOfSpouse(person);

                // only a woman can be on the right as a central person,
                // so if we move her spouse, we need to move her
                if (person.Sex == GDMSex.svMale) {
                    lastPers = basePerson;
                }

                // if the base spouse of the current person is a man
                // and there is only one marriage, we also shift his
                if (person.BaseSpouse.Sex == GDMSex.svMale && person.BaseSpouse.GetSpousesCount() == 1) {
                    lastPers = basePerson;
                }
            }

            for (int i = startIndex; i < basePerson.GetSpousesCount(); i++) {
                var sp = basePerson.GetSpouse(i);
                if (!ShiftPerson(sp, offset, false)) return false;
            }

            if (lastPers != null) {
                if (!ShiftPerson(lastPers, offset, false)) return false;
            }

            return true;
        }

        /// <summary>
        /// Recursive shift of all higher ancestors (straight line).
        /// </summary>
        /// <param name="person"></param>
        /// <param name="offset"></param>
        /// <param name="ext">
        ///     If true - use additional checks.
        /// </param>
        /// <param name="verify">
        ///     If true - check for violation of the boundary of the left branch,
        ///     taking into account the distance between the branches and the offset.
        /// </param>
        /// <returns>
        ///     If the result is false, then abort the process
        ///     (without shifting the current node and all previously passed).
        /// </returns>
        private bool ShiftPerson(TreeChartPerson person, int offset, bool ext, bool verify = false)
        {
            if (person == null || offset == 0) return true;

            WriteDebugInfo("ShiftDesc", person);

            // fix #189
            if (verify && (person.Rect.Left + offset < GetEdge(person, true) + fBranchDistance)) {
                return false;
            }

            if (ext && IsExtendedTree() && person.HasFlag(PersonFlag.pfRootSpouse)) {
                return ShiftSpousesFrom(person, offset);
            } else if (ext && person.BaseSpouse != null && (person.BaseSpouse.Sex == GDMSex.svFemale || person.BaseSpouse.GetSpousesCount() == 1)) {
                if (!ShiftPerson(person.BaseSpouse, offset, true, verify))
                    return false;
            } else {
                if (person.Generation <= 0) {
                    // shifts the parents of the central person
                    // following its shifts due to the growth of branches of descendants

                    if (!ShiftPerson(person.Father, offset, false, verify))
                        return false;

                    if (!ShiftPerson(person.Mother, offset, false, verify))
                        return false;
                } else {
                    // following the shifts of a person due to the growth of branches of descendants,
                    // shifts that of her parents, along which the branch came

                    TreeChartPerson parent = null;
                    if (person.HasFlag(PersonFlag.pfDescByFather)) {
                        parent = person.Father;
                    } else if (person.HasFlag(PersonFlag.pfDescByMother)) {
                        parent = person.Mother;
                    }
                    if (!ShiftPerson(parent, offset, true, verify))
                        return false;
                }
            }

            person.PtX += offset;
            return true;
        }

        private void RecalcDescChilds(TreeChartPerson person)
        {
            if (person.IsCollapsed) {
                return;
            }

            int childrenCount = person.GetChildsCount();
            if (childrenCount == 0) return;

            WriteDebugInfo("RecalcDescChilds", person);

            bool alignPair = person.BaseSpouse != null && person.BaseSpouse.GetSpousesCount() == 1;
            int centX = 0;

            if (alignPair) {
                switch (person.Sex) {
                    case GDMSex.svMale:
                        centX = (person.Rect.Right + person.BaseSpouse.Rect.Left) / 2;
                        break;

                    case GDMSex.svFemale:
                        centX = (person.BaseSpouse.Rect.Right + person.Rect.Left) / 2;
                        break;
                }
            } else {
                centX = person.PtX;
            }

            int childrenWidth = 0;
            for (int i = 0; i < childrenCount; i++) {
                childrenWidth += person.GetChild(i).Width;
            }
            if (childrenCount > 1) {
                childrenWidth += (childrenCount - 1) * fBranchDistance;
            }

            int curX = centX - childrenWidth / 2;
            int curY = NextGenY(person, false);

            for (int i = 0; i < childrenCount; i++) {
                TreeChartPerson child = person.GetChild(i);
                RecalcDesc(child, curX + child.Width / 2, curY, true);
                curX = child.Rect.Right + fBranchDistance;
            }

            // This code is designed to align parents in the center of the location of children (across width),
            // because in the process of drawing children, various kinds of displacement are formed,
            // and the initial arrangement of the parents can be very laterally,
            // after the formation of a complete tree of their descendants.
            // However, this may be a problem (reason of #189) in the case if a shift initiated from descendants,
            // must be performed to the left with an overlay on an already formed side branch.

            /*if (IsExtPerson(person)) return;

            WriteDebugInfo("RecalcDescChilds.AutoAlign", person);

            curX = person.GetChild(0).PtX;
            if (childrenCount > 1) {
                curX += (person.GetChild(childrenCount - 1).PtX - curX) / 2;
            }

            // FIXME: displacement #1
            if (alignPair) {
                int offset;
                switch (person.Sex) {
                    case GDMSex.svMale:
                        // fix #189
                        offset = curX - (fBranchDistance + person.Width) / 2 + 1 - person.PtX;
                        if (person.Rect.Left + offset < GetEdge(person, false)) {
                            return;
                        }

                        ShiftPerson(person, curX - (fBranchDistance + person.Width) / 2 + 1 - person.PtX, true);
                        ShiftPerson(person.BaseSpouse, curX + (fBranchDistance + person.BaseSpouse.Width) / 2 - person.BaseSpouse.PtX, true);
                        break;

                    case GDMSex.svFemale:
                        // fix #189
                        offset = curX - (fBranchDistance + person.BaseSpouse.Width) / 2 + 1 - person.BaseSpouse.PtX;
                        if (person.BaseSpouse.Rect.Left + offset < GetEdge(person.BaseSpouse, false)) {
                            return;
                        }

                        ShiftPerson(person, curX + (fBranchDistance + person.Width) / 2 - person.PtX, true);
                        ShiftPerson(person.BaseSpouse, curX - (fBranchDistance + person.BaseSpouse.Width) / 2 + 1 - person.BaseSpouse.PtX, true);
                        break;
                }
            } else {
                ShiftPerson(person, curX - person.PtX, true, true);
            }*/
        }

        private void RecalcDesc(TreeChartPerson person, int ptX, int ptY, bool predef)
        {
            if (person == null) return;

            WriteDebugInfo("RecalcDesc", person);

            person.IsVisible = true;

            if (predef && !IsExtRootSpouse(person)) {
                person.PtX = ptX;
                person.PtY = ptY;
            }

            int bound = GetCurrentBranchLeftBound(person, false);
            if (person.Rect.Left < bound) {
                ShiftPerson(person, bound - person.Rect.Left, true);
            }

            // the man's spouses align to his right
            if (person.Sex == GDMSex.svMale) {
                RecalcDescChilds(person);
                SetEdge(person);
            }

            int spousesCount = person.GetSpousesCount();
            if (spousesCount > 0) {
                TreeChartPerson prev = person;
                for (int i = 0; i < spousesCount; i++) {
                    TreeChartPerson sp = person.GetSpouse(i);

                    // position of spouses by default
                    // without regard to other factors and subsequent children
                    int spX = 0;
                    int spOffset = (fBranchDistance + sp.Width / 2);
                    switch (person.Sex) {
                        case GDMSex.svMale:
                            spX = prev.Rect.Right + spOffset;
                            break;

                        case GDMSex.svFemale:
                            spX = prev.Rect.Left - spOffset;
                            break;
                    }

                    RecalcDesc(sp, spX, person.PtY, true);

                    // spouses arranged from first to last from left to right
                    // therefore for several wifes of one man, the previous node is the previous wife
                    // however, for several husbands of one woman, the previous node is a woman
                    if (sp.Sex != GDMSex.svMale) {
                        prev = sp;
                    }
                }
            }

            // the woman's spouses align to her left
            if (person.Sex == GDMSex.svFemale) {
                RecalcDescChilds(person);
                SetEdge(person);
            }

            // FIXME: Temporary hack: if this person does not specify a particular sex,
            // then breaks the normal sequence of formation of coordinates.
            if (person.Sex == GDMSex.svUnknown || person.Sex == GDMSex.svIntersex) {
                SetEdge(person);
            }

            // Fix of long-distance displacement of male nodes in the presence of more than
            // one marriage and a large tree of descendants from the first wife.
            // Warning: hard breaks ancestors location in case of extended tree mode!
            // FIXME: displacement #2
            /*if (!IsExtendedTree() && (person.Sex == GDMSex.svMale && spousesCount >= 2)) {
                WriteDebugInfo("RecalcDesc.FixLDD", person);

                var firstWife = person.GetSpouse(0);
                if (firstWife.GetChildsCount() > 0) {
                    int d = firstWife.Rect.Left - person.Rect.Right;
                    if (d > fBranchDistance * 1.5f) {
                        //person.SetFlag(PersonFlag.pfSpecialMark);
                        int offset = (d - fBranchDistance);
                        ShiftPerson(person, offset, true);
                    }
                }
            }*/
        }

        #endregion

        #region Filtering and search

        public void DoFilter(GDMIndividualRecord root)
        {
            if (root == null)
                throw new ArgumentNullException("root");

            if (fFilter.BranchCut == ChartFilter.BranchCutType.None) return;

            fFilterData.Clear();
            DoDescendantsFilter(root);
            fFilterData[root] = true;
        }

        private bool DoDescendantsFilter(GDMIndividualRecord person)
        {
            bool result = false;
            if (person == null) return result;

            try {
                switch (fFilter.BranchCut) {
                    case ChartFilter.BranchCutType.Years:
                        int birthYear = person.GetChronologicalYear(GEDCOMTagName.BIRT);
                        result = (birthYear != 0 && birthYear >= fFilter.BranchYear);
                        break;

                    case ChartFilter.BranchCutType.Persons:
                        result = (fFilter.BranchPersons.IndexOf(person.XRef + ";") >= 0);
                        break;
                }

                int num = person.SpouseToFamilyLinks.Count;
                for (int i = 0; i < num; i++) {
                    GDMFamilyRecord family = fTree.GetPtrValue(person.SpouseToFamilyLinks[i]);

                    int num2 = family.Children.Count;
                    for (int j = 0; j < num2; j++) {
                        GDMIndividualRecord child = fTree.GetPtrValue(family.Children[j]);
                        bool resChild = DoDescendantsFilter(child);
                        result |= resChild;
                    }
                }

                fFilterData[person] = result;
            } catch (Exception ex) {
                Logger.WriteError("TreeChartModel.DoDescendantsFilter()", ex);
            }

            return result;
        }

        public IList<ISearchResult> FindAll(string searchPattern)
        {
            var result = new List<ISearchResult>();
            var allNames = GlobalOptions.Instance.SearchAndFilterByAllNames;
            Regex regex = GKUtils.InitMaskRegex(searchPattern);

            int num = fPersons.Count;
            for (int i = 0; i < num; i++) {
                GDMIndividualRecord iRec = fPersons[i].Rec;
                if (iRec != null && GKUtils.IsMatchesNames(iRec, regex, allNames)) {
                    result.Add(new SearchResult(iRec));
                }
            }

            return result;
        }

        #endregion

        #region Navigation

        public TreeChartPerson FindPersonByRec(GDMIndividualRecord iRec, bool onlyPrimary = false)
        {
            if (iRec != null) {
                int num = fPersons.Count;
                for (int i = 0; i < num; i++) {
                    TreeChartPerson p = fPersons[i];
                    if (p.Rec == iRec && (!onlyPrimary || !p.IsDup)) {
                        return p;
                    }
                }
            }

            return null;
        }

        public TreeChartPerson FindPersonByCoords(int aX, int aY)
        {
            TreeChartPerson result = null;

            aX -= fOffsetX;
            aY -= fOffsetY;
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

        #endregion

        #region Drawing routines

        public override void SetRenderer(ChartRenderer renderer)
        {
            base.SetRenderer(renderer);

            InitSigns(renderer);
            InitGraphics();
        }

        public void InitGraphics()
        {
            DoneGraphics();

            fClrBlack = ChartRenderer.GetColor(BSDColors.Black);

            // Anti-aliasing works differently in EtoForms and WinForms.
            IColor clrLine, clrDecor;
#if GK3
            clrLine = ChartRenderer.GetColor(BSDColors.DimGray);
            clrDecor = ChartRenderer.GetColor(BSDColors.DarkGray);
#else
            clrLine = ChartRenderer.GetColor(BSDColors.Black);
            clrDecor = ChartRenderer.GetColor(BSDColors.DimGray);
#endif

            fLinePen = fRenderer.CreatePen(clrLine, 1f);
            fLineDecorativePen = fRenderer.CreatePen(clrDecor, 1f);
            fLineSelectedPen = fRenderer.CreatePen(clrLine, 2.4f);
            fLineDottedPen = fRenderer.CreatePen(clrLine, 1f, new float[] { 4.0F, 2.0F });
            fLineDottedDecorativePen = fRenderer.CreatePen(clrDecor, 1f, new float[] {4.0F, 2.0F});
            fLineDottedSelectedPen = fRenderer.CreatePen(clrLine, 2.4f, new float[] { 4.0F, 2.0F });
            fSolidBlack = fRenderer.CreateBrush(fClrBlack);
            fSolidF = fRenderer.CreateBrush(ChartRenderer.GetColor(BSDColors.Red));
            fSolidM = fRenderer.CreateBrush(ChartRenderer.GetColor(BSDColors.Blue));

            var clrGreen = ChartRenderer.GetColor(BSDColors.ForestGreen);
            fLineTMSYPen = fRenderer.CreatePen(clrGreen, 2.4f, new float[] { 4.0F, 2.0F });
            var clrCrimson = ChartRenderer.GetColor(BSDColors.Crimson);
            fLineTMSNPen = fRenderer.CreatePen(clrCrimson, 2.4f, new float[] { 4.0F, 2.0F });
        }

        private void DoneGraphics()
        {
            if (fLinePen != null) fLinePen.Dispose();
            if (fLineDecorativePen != null) fLineDecorativePen.Dispose();
            if (fLineSelectedPen != null) fLineSelectedPen.Dispose();
            if (fLineDottedPen != null) fLineDottedPen.Dispose();
            if (fLineDottedDecorativePen != null) fLineDottedDecorativePen.Dispose();
            if (fLineDottedSelectedPen != null) fLineDottedSelectedPen.Dispose();
            if (fSolidBlack != null) fSolidBlack.Dispose();
            if (fSolidF != null) fSolidF.Dispose();
            if (fSolidM != null) fSolidM.Dispose();
        }

        public ExtRect GetExpanderRect(ExtRect personRect)
        {
            int psz = (int)(16 * fPicScale);
            ExtRect expRt = ExtRect.Create(personRect.Left, personRect.Top - psz - 2, personRect.Left + psz - 1, personRect.Top - 2);
            return expRt;
        }

        public ExtRect GetInfoRect(ExtRect personRect)
        {
            int psz = (int)(16 * fPicScale);
            ExtRect expRt = ExtRect.Create(personRect.Right - psz, personRect.Top - psz - 2, personRect.Right, personRect.Top - 2);
            return expRt;
        }

        public static ExtRect GetPersonExpandRect(ExtRect personRect)
        {
            int x = personRect.Left + (personRect.GetWidth() - 16) / 2;
            ExtRect expRt = ExtRect.Create(x, personRect.Top - 18, x + 16 - 1, personRect.Top - 2);
            return expRt;
        }

        private bool IsPersonVisible(ExtRect pnRect)
        {
            return fVisibleArea.IntersectsWith(pnRect);
        }

        private bool IsLineVisible(int x1, int y1, int x2, int y2)
        {
            if (fVisibleArea.GetWidth() <= 0 || fVisibleArea.GetHeight() <= 0) {
                return false;
            }

            var rangeX = new Range<int>(fVisibleArea.Left, fVisibleArea.Right);
            var rangeY = new Range<int>(fVisibleArea.Top, fVisibleArea.Bottom);

            return rangeX.IsOverlapped(new Range<int>(x1, x2)) && rangeY.IsOverlapped(new Range<int>(y1, y2));
        }

        private void DrawLine(int x1, int y1, int x2, int y2, bool isDotted, bool isTracked, bool isMatched)
        {
            if (x2 < x1) {
                int tmp = x1;
                x1 = x2;
                x2 = tmp;
            }

            if (y2 < y1) {
                int tmp = y1;
                y1 = y2;
                y2 = tmp;
            }

            if (!IsLineVisible(x1, y1, x2, y2)) return;

            IPen linePen, decorativeLinePen;

            if (isTracked && fOptions.TrackMatchedSources) {
                /*if (!isDotted) {
                    linePen = fLineSelectedPen;
                } else {
                    linePen = fLineDottedSelectedPen;
                }*/
                linePen = isMatched ? fLineTMSYPen : fLineTMSNPen;
                decorativeLinePen = null;

                var lineHandle = new LineHandle() { X1 = x1, Y1 = y1, X2 = x2, Y2 = y2 };
                float dashOffset;
                if (!fLines.TryGetValue(lineHandle, out dashOffset)) {
                    dashOffset = 0;
                    fLines.Add(lineHandle, dashOffset);
                }
                linePen.DashOffset = dashOffset;
            } else if (isTracked && fOptions.TrackSelectedLines) {
                if (!isDotted) {
                    linePen = fLineSelectedPen;
                } else {
                    linePen = fLineDottedSelectedPen;
                }
                decorativeLinePen = null;
            } else {
                if (!isDotted) {
                    linePen = fLinePen;
                    decorativeLinePen = fLineDecorativePen;
                } else {
                    linePen = fLineDottedPen;
                    decorativeLinePen = fLineDottedDecorativePen;
                }
            }

            int sX = fOffsetX + x1;
            int sX2 = fOffsetX + x2;
            int sY = fOffsetY + y1;
            int sY2 = fOffsetY + y2;
            fRenderer.DrawLine(linePen, sX, sY, sX2, sY2);

            if (fOptions.Decorative && decorativeLinePen != null) {
                if (sX == sX2) {
                    fRenderer.DrawLine(decorativeLinePen, sX + 1, sY + 1, sX2 + 1, sY2 - 1);
                } else if (sY == sY2) {
                    fRenderer.DrawLine(decorativeLinePen, sX + 1, sY + 1, sX2 + 0, sY2 + 1);
                }
            }
        }

        private const float RoundedRectRadius = 6.0f;

        private void DrawBorder(IPen xpen, ExtRect rt, bool dead, TreeChartPerson person)
        {
            IColor bColor = person.GetFillColor(dead);
            if (fHighlightedPerson == person) {
                bColor = bColor.Lighter(HIGHLIGHTED_VAL);
            }

            if (person.Sex == GDMSex.svFemale) {
                fRenderer.DrawRoundedRectangle(xpen, bColor, rt.Left, rt.Top, rt.GetWidth(), rt.GetHeight(), RoundedRectRadius);
            } else {
                fRenderer.DrawRectangle(xpen, bColor, rt.Left, rt.Top, rt.GetWidth(), rt.GetHeight());
            }
        }

        private void DrawCoverGlass(ExtRect rt, TreeChartPerson person)
        {
            float rad = (person.Sex == GDMSex.svFemale) ? RoundedRectRadius : 0.0f;
            fRenderer.DrawCoverGlass(rt.Left, rt.Top, rt.GetWidth(), rt.GetHeight(), rad);
        }

        private static string TruncString(string str, int threshold)
        {
            if (str.Length > threshold)
                return str.Substring(0, threshold) + "…";
            return str;
        }

        private void DrawPerson(TreeChartPerson person, ChartDrawMode drawMode, bool isSelected)
        {
            try {
                ExtRect prt = person.Rect;
                if (drawMode == ChartDrawMode.dmInteractive && !IsPersonVisible(prt))
                    return;

                prt.Offset(fOffsetX, fOffsetY);

                if (person.HasFlag(PersonFlag.pfIsDead) && fOptions.MourningEdges) {
                    ExtRect dt = prt.GetOffset(-2, -2);
                    DrawBorder(null, dt, true, person);
                }

                IPen xpen = null;
                try {
                    if (isSelected) {
                        IColor penColor = person.GetSelectedColor();
                        xpen = fRenderer.CreatePen(penColor, 2.0f);
                    } else {
                        xpen = fRenderer.CreatePen(fClrBlack, 1.0f);
                    }

                    DrawBorder(xpen, prt, false, person);
                } finally {
                    if (xpen != null)
                        xpen.Dispose();
                }

                ExtRect brt = prt;
                if (person.Portrait != null) {
                    ExtRect portRt = person.PortraitArea.GetOffset(prt.Left, prt.Top);
                    fRenderer.DrawImage(person.Portrait, portRt.Left, portRt.Top, portRt.GetWidth(), portRt.GetHeight(), person.Rec.XRef);

                    prt.Left += person.PortraitWidth;
                }

                int bh = fRenderer.GetTextHeight(fBoldFont);
                int th = fRenderer.GetTextHeight(fDrawFont);
                int ry = prt.Top + fNodePadding;
                int prtWidth = (prt.Right - prt.Left + 1);
                int maxLength = (prtWidth / fDefCharWidth);

                int lines = person.Lines.Length;
                for (int k = 0; k < lines; k++) {
                    string line = person.Lines[k];

                    int lh;
                    IFont font;
                    if (fOptions.BoldNames && k < person.NameLines) {
                        lh = bh;
                        font = fBoldFont;
                    } else {
                        lh = th;
                        font = fDrawFont;
                    }

                    int stw = fRenderer.GetTextWidth(line, font);
                    if (fOptions.URNotesVisible && line == person.Note && stw > prtWidth) {
                        line = TruncString(line, maxLength);
                        stw = fRenderer.GetTextWidth(line, font);
                    }

                    int rx = prt.Left + (prtWidth - stw) / 2;

                    fRenderer.DrawString(line, font, fSolidBlack, rx, ry, fOptions.TextEffect);

                    ry += lh;
                }

                DrawCoverGlass(brt, person);

                if (fOptions.SignsVisible && !person.Signs.IsEmpty()) {
                    int i = 0;
                    int dy = (int)(21 * fPicScale);
                    for (var cps = SpecialUserRef.urRI_StGeorgeCross; cps <= SpecialUserRef.urLast; cps++) {
                        if (!person.Signs.Contains(cps)) continue;

                        IImage pic = fSignsPic[(int)cps];
                        int pW = (int)(pic.Width * fPicScale);
                        int pH = (int)(pic.Height * fPicScale);
                        fRenderer.DrawImage(pic, brt.Right + 1, brt.Top - dy + i * pH, pW, pH, cps.ToString());
                        i++;
                    }
                }

                // only for existing individuals
                if (person.Rec != null) {
                    // draw XRef
                    if (fXRefVisible) {
                        DrawText(person.Rec.XRef, brt.Right, brt.Bottom, 3, false);
                    }

                    // draw CI
                    if (fCertaintyIndex) {
                        string cas = string.Format("{0:0.00}", person.CertaintyAssessment);
                        fRenderer.DrawString(cas, fDrawFont, fSolidBlack, brt.Left, brt.Bottom);
                    }
                }

                // only interactive mode
                if (drawMode == ChartDrawMode.dmInteractive) {
                    if (person.HasFlag(PersonFlag.pfCanExpand)) {
                        ExtRect expRt = GetExpanderRect(brt);
                        fRenderer.DrawImage(fExpPic, expRt.Left, expRt.Top, expRt.Width, expRt.Height, string.Empty);
                    }

                    if (person.IsCollapsed) {
                        ExtRect expRt = GetPersonExpandRect(brt);
                        fRenderer.DrawImage(fPersExpPic, expRt.Left, expRt.Top, string.Empty);
                    }

                    ExtRect infoRt = GetInfoRect(brt);

                    if (person.Selected) {
                        fRenderer.DrawImage(fInfoPic, infoRt.Left, infoRt.Top, infoRt.Width, infoRt.Height, string.Empty);
                    }

                    if (person.HasFlag(PersonFlag.pfBookmark)) {
                        int pW = (int)(fBookmarkPic.Width * fPicScale);
                        int pH = (int)(fBookmarkPic.Height * fPicScale);
                        fRenderer.DrawImage(fBookmarkPic, brt.Right - pW * 2, brt.Top - pH - 2, pW, pH, string.Empty);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("TreeChartModel.DrawPerson()", ex);
            }
        }

        #region Tracking

        private bool HasTrackedSources(TreeChartPerson person1, TreeChartPerson person2)
        {
            if (person1 == null || person2 == null) return false;

            return person1.IntersectSources(person2);
        }

        private bool HasTrackedChild(TreeChartPerson person, ChartDrawMode drawMode)
        {
            int childrenCount = person.GetChildsCount();
            for (int i = 0; i < childrenCount; i++) {
                TreeChartPerson child = person.GetChild(i);
                if (HasTrackedLines(child, drawMode)) {
                    return true;
                }
            }
            return false;
        }

        private bool HasTrackedLines(TreeChartPerson person, ChartDrawMode drawMode)
        {
            return (drawMode == ChartDrawMode.dmInteractive && person != null && person.Selected);
        }

        #endregion

        private bool IsDottedLines(TreeChartPerson person)
        {
            return (person != null && person.HasFlag(PersonFlag.pfAdopted) && fOptions.DottedLinesOfAdoptedChildren);
        }

        private void DrawParentAges(TreeChartPerson person, int pY, int crY)
        {
            if (!fOptions.ParentAges || (string.IsNullOrEmpty(person.FatherAge) && string.IsNullOrEmpty(person.MotherAge)))
                return;

            var tH = (int)fRenderer.GetTextSize("0", fDrawFont).Height;
            int aY = (!fOptions.InvertedTree) ? crY + (pY - crY - tH) / 2 : pY + (crY - pY - tH) / 2;

            DrawText(person.FatherAge, person.PtX - 4, aY, 3, fSolidM);
            DrawText(person.MotherAge, person.PtX + 4, aY, 2, fSolidF);
        }

        private void DrawAncestors(TreeChartPerson person, ChartDrawMode drawMode, bool isTracked)
        {
            if (person.IsCollapsed || (person.Father == null && person.Mother == null)) {
                return;
            }

            bool isTrackedAncestors = isTracked || HasTrackedLines(person.Father, drawMode) || HasTrackedLines(person.Mother, drawMode);
            bool isMatchedFather, isMatchedMother;
            if (isTrackedAncestors) {
                isMatchedFather = HasTrackedSources(person, person.Father);
                isMatchedMother = HasTrackedSources(person, person.Mother);
            } else {
                isMatchedFather = false;
                isMatchedMother = false;
            }

            Draw(person.Father, TreeChartKind.ckAncestors, drawMode);
            Draw(person.Mother, TreeChartKind.ckAncestors, drawMode);

            int crY, parY, pY;
            if (!fOptions.InvertedTree) {
                pY = person.PtY;
                crY = person.PtY - fLevelDistance / 2;
                parY = ((person.Father != null) ? person.Father.PtY + person.Father.Height : person.Mother.PtY + person.Mother.Height) + 1;
            } else {
                pY = person.PtY + person.Height;
                crY = person.PtY + person.Height + fLevelDistance / 2;
                parY = ((person.Father != null) ? person.Father.PtY : person.Mother.PtY) - 1;
            }

            bool dotted = IsDottedLines(person);

            DrawLine(person.PtX, crY, person.PtX, pY, dotted, isTrackedAncestors, isMatchedFather || isMatchedMother); // v
            DrawParentAges(person, pY, crY);

            string marrDate = null;

            if (person.Father != null) {
                DrawLine(person.Father.PtX, crY, person.PtX, crY, dotted, isTrackedAncestors, isMatchedFather); // h
                DrawLine(person.Father.PtX, parY, person.Father.PtX, crY, dotted, isTrackedAncestors, isMatchedFather); // v

                if (!string.IsNullOrEmpty(person.Father.MarriageDate) && marrDate == null) {
                    marrDate = person.Father.MarriageDate;
                }
            }

            if (person.Mother != null) {
                DrawLine(person.Mother.PtX, crY, person.PtX, crY, dotted, isTrackedAncestors, isMatchedMother); // h
                DrawLine(person.Mother.PtX, parY, person.Mother.PtX, crY, dotted, isTrackedAncestors, isMatchedMother); // v

                if (!string.IsNullOrEmpty(person.Mother.MarriageDate) && marrDate == null) {
                    marrDate = person.Mother.MarriageDate;
                }
            }

            if (!string.IsNullOrEmpty(marrDate)) {
                int q = (!fOptions.InvertedTree) ? 1 : 2;
                DrawText(marrDate, person.PtX, crY, q);
            }
        }

        private void DrawText(string text, float x, float y, int quad, bool offset = true)
        {
            DrawText(text, x, y, quad, fSolidBlack, offset);
        }

        private void DrawText(string text, float x, float y, int quad, IBrush brush, bool offset = true)
        {
            if (string.IsNullOrEmpty(text))
                return;

            // quadrant clockwise from 00 hours
            if (offset) {
                x += fOffsetX;
                y += fOffsetY;
            }
            ExtSizeF tsz = fRenderer.GetTextSize(text, fDrawFont);

            switch (quad) {
                case 1:
                    y -= tsz.Height;
                    break;
                case 2:
                    break;
                case 3:
                    x -= tsz.Width;
                    break;
                case 4:
                    x -= tsz.Width;
                    y -= tsz.Height;
                    break;
            }

            fRenderer.DrawString(text, fDrawFont, brush, x, y, fOptions.TextEffect);
        }

        private void DrawDescendants(TreeChartPerson person, ChartDrawMode drawMode, bool isTracked)
        {
            int spousesCount = person.GetSpousesCount();
            int childrenCount = person.GetChildsCount();

            // draw lines of spouses
            int spbOfs = (person.Height - 10) / (spousesCount + 1);
            int spbBeg = person.PtY + (person.Height - spbOfs * (spousesCount - 1)) / 2;
            switch (person.Sex) {
                case GDMSex.svMale:
                    for (int i = 0; i < spousesCount; i++) {
                        TreeChartPerson spouse = person.GetSpouse(i);
                        bool commonLaw = fOptions.DottedLinesOfCommonLawSpouses && spouse.HasFlag(PersonFlag.pfCommonLaw);
                        bool isTrackedSpouse = isTracked || HasTrackedLines(spouse, drawMode) || HasTrackedChild(spouse, drawMode);
                        bool isMatchedSpouse = isTrackedSpouse && HasTrackedSources(person, spouse);

                        int spbV = spbBeg + spbOfs * i;
                        DrawLine(spouse.Rect.Left, spbV, person.Rect.Right + 1, spbV, commonLaw, isTrackedSpouse, isMatchedSpouse); // h

                        if (!string.IsNullOrEmpty(spouse.MarriageDate)) {
                            int q = (!fOptions.InvertedTree) ? 4 : 3;
                            DrawText(spouse.MarriageDate, spouse.Rect.Left, spbV, q);
                        }
                    }
                    break;

                case GDMSex.svFemale:
                    for (int i = 0; i < spousesCount; i++) {
                        TreeChartPerson spouse = person.GetSpouse(i);
                        bool commonLaw = fOptions.DottedLinesOfCommonLawSpouses && spouse.HasFlag(PersonFlag.pfCommonLaw);
                        bool isTrackedSpouse = isTracked || HasTrackedLines(spouse, drawMode) || HasTrackedChild(spouse, drawMode);
                        bool isMatchedSpouse = isTrackedSpouse && HasTrackedSources(person, spouse);

                        int spbV = spbBeg + spbOfs * i;
                        DrawLine(spouse.Rect.Right + 1, spbV, person.Rect.Left, spbV, commonLaw, isTrackedSpouse, isMatchedSpouse); // h

                        if (!string.IsNullOrEmpty(spouse.MarriageDate)) {
                            int q = (!fOptions.InvertedTree) ? 1 : 2;
                            DrawText(spouse.MarriageDate, spouse.Rect.Right + 1, spbV, q);
                        }
                    }
                    break;
            }

            // draw spouses
            for (int i = 0; i < spousesCount; i++) {
                Draw(person.GetSpouse(i), TreeChartKind.ckDescendants, drawMode);
            }

            // draw lines of children
            if (!person.IsCollapsed && childrenCount != 0) {
                int crY;
                if (!fOptions.InvertedTree) {
                    crY = person.PtY + person.Height + fLevelDistance / 2;
                } else {
                    crY = person.PtY - fLevelDistance / 2;
                }

                bool vertLineBySpouse;
                int cx, spXB = 0, spXE = 0;
                if (person.BaseSpouse == null || (person.BaseSpouse.GetSpousesCount() > 1)) {
                    cx = person.PtX;
                    spbBeg = person.PtY + person.Height - 1;
                    vertLineBySpouse = true;
                } else {
                    switch (person.Sex) {
                        case GDMSex.svMale:
                            spXB = person.Rect.Right;
                            spXE = person.BaseSpouse.Rect.Left;
                            break;

                        case GDMSex.svFemale:
                            spXB = person.BaseSpouse.Rect.Right;
                            spXE = person.Rect.Left;
                            break;
                    }

                    cx = (spXB + spXE) / 2;
                    spbBeg -= spbOfs / 2;
                    vertLineBySpouse = false;
                }

                if (person.BaseSpouse != null && HasTrackedLines(person.BaseSpouse, drawMode)) {
                    isTracked = true;
                }

                TreeChartPerson child0 = person.GetChild(0);
                int chY = (!fOptions.InvertedTree) ? child0.PtY : child0.PtY + child0.Height;
                int chXB = child0.PtX;
                int chXE = (childrenCount > 1) ? person.GetChild(childrenCount - 1).PtX : child0.PtX;
                int chXM = (chXB + chXE) / 2;

                bool isTrackedChildren = false;
                bool isMatchedChildren = false;
                for (int i = 0; i < childrenCount; i++) {
                    TreeChartPerson child = person.GetChild(i);
                    bool dotted = IsDottedLines(child);

                    bool isTrackedChild = isTracked || HasTrackedLines(child, drawMode);
                    if (isTrackedChild) isTrackedChildren = true;
                    bool isMatchedChild = isTrackedChild && (HasTrackedSources(person, child) || (person.BaseSpouse != null && HasTrackedSources(person.BaseSpouse, child)));
                    if (isMatchedChild) isMatchedChildren = true;

                    if (childrenCount > 1) {
                        int jX;
                        if (child.PtX < cx /*chXM*/) {
                            jX = Math.Min(cx /*chXM*/, person.GetChild(i + 1).PtX);
                        } else {
                            jX = Math.Max(cx /*chXM*/, person.GetChild(i - 1).PtX);
                        }
                        DrawLine(child.PtX, crY, jX, crY, dotted, isTrackedChild, isMatchedChild); // h
                    }

                    DrawLine(child.PtX, chY, child.PtX, crY, dotted, isTrackedChild, isMatchedChild); // v
                    DrawParentAges(child, chY, crY);
                }

                // vertical line from the horizontal junction of spouses to the horizontal junction of children
                if (vertLineBySpouse || (cx >= chXB && cx <= chXE)) {
                    DrawLine(cx, crY, cx, spbBeg, false, isTrackedChildren, isMatchedChildren); // v
                } else {
                    if (chXM >= spXB && chXM <= spXE) {
                        DrawLine(chXM, crY, chXM, spbBeg, false, isTrackedChildren, isMatchedChildren); // v
                    } else {
                        DrawLine(cx, crY, cx, spbBeg, false, isTrackedChildren, isMatchedChildren); // v
                    }
                }

                // draw children
                for (int i = 0; i < childrenCount; i++) {
                    Draw(person.GetChild(i), TreeChartKind.ckDescendants, drawMode);
                }
            }
        }

        public void Draw(ChartDrawMode drawMode)
        {
            fRenderer.SetSmoothing(true);
            fRenderer.SetTranslucent(0.0f);

            InitGraphics();

            if (IsExtendedTree()) {
                // the man's spouses align to his right
                if (fRoot.Sex == GDMSex.svMale) {
                    Draw(fRoot, fKind, drawMode);
                }

                int spousesCount = fRoot.GetSpousesCount();
                for (int i = 0; i < spousesCount; i++) {
                    TreeChartPerson sp = fRoot.GetSpouse(i);
                    Draw(sp, TreeChartKind.ckAncestors, drawMode);
                }

                // the woman's spouses align to her left
                if (fRoot.Sex == GDMSex.svFemale) {
                    Draw(fRoot, fKind, drawMode);
                }
            } else {
                Draw(fRoot, fKind, drawMode);
            }

            if (fOptions.BorderStyle != GfxBorderStyle.None) {
                var rt = ExtRect.CreateBounds(fOffsetX, fOffsetY, fImageWidth, fImageHeight);
                BorderPainter.DrawBorder(fRenderer, rt, fOptions.BorderStyle);
            }
        }

        private void Draw(TreeChartPerson person, TreeChartKind dirKind, ChartDrawMode drawMode)
        {
            if (person == null) return;

            bool isTracked = HasTrackedLines(person, drawMode);

            switch (dirKind) {
                case TreeChartKind.ckAncestors:
                    DrawAncestors(person, drawMode, isTracked);
                    break;

                case TreeChartKind.ckDescendants:
                    DrawDescendants(person, drawMode, isTracked);
                    break;

                case TreeChartKind.ckBoth:
                    if (person == fRoot || dirKind == TreeChartKind.ckAncestors) DrawAncestors(person, drawMode, isTracked);
                    if (person == fRoot || dirKind == TreeChartKind.ckDescendants) DrawDescendants(person, drawMode, isTracked);
                    break;
            }

            DrawPerson(person, drawMode, isTracked);
        }

        public void PrepareDraw(ChartDrawMode drawMode)
        {
            // drawing relative offset of tree on graphics
            var viewport = fView.Viewport;

            if (drawMode == ChartDrawMode.dmInteractive) {
                fVisibleArea = viewport;
            } else {
                fVisibleArea = ExtRect.CreateBounds(0, 0, fImageWidth, fImageHeight);
            }

            int spx, spy;
            if (drawMode == ChartDrawMode.dmStatic) {
                spx = 0;
                spy = 0;
            } else {
                bool virtualCanvas = fView.VirtualCanvas;
                var imgViewport = fView.ImageViewport;

                if (fImageWidth < viewport.Width) {
                    spx = (viewport.Width - fImageWidth) / 2;
                } else {
                    if (virtualCanvas) {
                        spx = -viewport.Left;
                    } else {
                        spx = imgViewport.Left;
                    }
                }

                if (fImageHeight < viewport.Height) {
                    spy = (viewport.Height - fImageHeight) / 2;
                } else {
                    if (virtualCanvas) {
                        spy = -viewport.Top;
                    } else {
                        spy = imgViewport.Top;
                    }
                }
            }
            SetOffsets(spx, spy);
        }

        #endregion

        public static bool CheckTreeChartSize(GDMTree tree, GDMIndividualRecord iRec, TreeChartKind chartKind)
        {
            bool result = true;
            if (!GlobalOptions.Instance.CheckTreeSize) return result;

            var chartOptions = GlobalOptions.Instance.TreeChartOptions;

            if (chartKind == TreeChartKind.ckAncestors || chartKind == TreeChartKind.ckBoth) {
                int ancestorsLimit = (!chartOptions.SeparateDepth) ? chartOptions.DepthLimit : chartOptions.DepthLimitAncestors;

                int ancCount = GKUtils.GetAncestorsCount(tree, iRec, ancestorsLimit);
                if (ancCount > 2048) {
                    AppHost.StdDialogs.ShowMessage(string.Format(LangMan.LS(LSID.AncestorsNumberIsInvalid), ancCount.ToString()));
                    return false;
                }
            }

            if (chartKind >= TreeChartKind.ckDescendants && chartKind <= TreeChartKind.ckBoth) {
                int descendantsLimit = (!chartOptions.SeparateDepth) ? chartOptions.DepthLimit : chartOptions.DepthLimitDescendants;

                int descCount = GKUtils.GetDescendantsCount(tree, iRec, descendantsLimit);
                if (descCount > 2048) {
                    AppHost.StdDialogs.ShowMessage(string.Format(LangMan.LS(LSID.DescendantsNumberIsInvalid), descCount.ToString()));
                    result = false;
                }
            }

            return result;
        }

        #region Maps

        public List<GDMIndividualRecord> GetAncestors(TreeChartPerson person)
        {
            var result = new HashSet<GDMIndividualRecord>();
            GetAncestors(person, result);
            return result.ToList();
        }

        private void GetAncestors(TreeChartPerson person, HashSet<GDMIndividualRecord> ancestors)
        {
            if (person == null) return;
            if (person.Rec != null) ancestors.Add(person.Rec);

            if (person.Father != null) GetAncestors(person.Father, ancestors);
            if (person.Mother != null) GetAncestors(person.Mother, ancestors);
        }

        public List<GDMIndividualRecord> GetDescendants(TreeChartPerson person)
        {
            var result = new HashSet<GDMIndividualRecord>();
            GetDescendants(person, result);
            return result.ToList();
        }

        private void GetDescendants(TreeChartPerson person, HashSet<GDMIndividualRecord> descendants)
        {
            if (person == null) return;
            if (person.Rec != null) descendants.Add(person.Rec);

            int childrenCount = person.GetChildsCount();
            for (int i = 0; i < childrenCount; i++) {
                GetDescendants(person.GetChild(i), descendants);
            }

            int spousesCount = person.GetSpousesCount();
            for (int i = 0; i < spousesCount; i++) {
                GetDescendants(person.GetSpouse(i), descendants);
            }
        }

        #endregion
    }
}
