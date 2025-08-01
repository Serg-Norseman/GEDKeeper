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

//#define DESK_METHOD

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Kinships;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Options;
using GKCore.Search;
using GKCore.Types;
using GKCore.Utilities;
using GKUI.Themes;

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

    public sealed class LineHandle
    {
        public int X1, Y1;
        public int X2, Y2;
        public float Distance;
        public float DashOffset;

        public LineHandle(int x1, int y1, int x2, int y2)
        {
            X1 = x1;
            Y1 = y1;
            X2 = x2;
            Y2 = y2;
            Distance = (float)Math.Sqrt(X1 * X2 + Y1 * Y2);
            DashOffset = 0;
        }

        public static int GetHashCode(int x1, int y1, int x2, int y2)
        {
            int hash = x1;
            hash = (hash << 5) + hash;
            hash ^= y1;
            hash = (hash << 5) + hash;
            hash ^= x2;
            hash = (hash << 5) + hash;
            hash ^= y2;
            return hash;
        }
    }

    /// <summary>
    ///
    /// </summary>
    public class TreeChartModel : ChartModel
    {
        public const int DEF_MARGINS = 24;
        public const int DEF_SPOUSE_DISTANCE = 20;
        public const int DEF_BRANCH_DISTANCE = 40;
        public const int DEF_LEVEL_DISTANCE = 46;
        public const float HIGHLIGHTED_VAL = 0.1f;

        // Specifies the interior spacing of a node.
        public const int DEF_PERSON_NODE_PADDING = 10;

        private readonly ChartFilter fFilter;
        private readonly GKVarCache<GDMIndividualRecord, bool> fFilterData;
        private readonly Dictionary<int, LineHandle> fLines;
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
        private IImage fBookmarkPic;
        private IImage fExpPic;
        private IImage fInfoPic;
        private IImage fCollapsePic;
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

        internal int DefCharWidth
        {
            get { return fDefCharWidth; }
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

        internal int LevelDistance
        {
            get { return fLevelDistance; }
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

        internal int SpouseDistance
        {
            get { return fSpouseDistance; }
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
            fFilter = new ChartFilter();
            fFilterData = new GKVarCache<GDMIndividualRecord, bool>();
            fGraph = null;
            fLines = new Dictionary<int, LineHandle>();
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

            var layout = new DefTreeLayout();
            SetLayout(layout);
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
            if (fView == null) return;

            bool reqInv = false;

            if (fHighlightedPerson != null) {
                DateTime st = DateTime.FromBinary(fHighlightedStart);
                DateTime cur = DateTime.Now;
                TimeSpan d = cur - st;

                if (d.TotalSeconds >= 1/* && !fPersonControl.Visible*/) {
                    fHighlightedPerson = null;
                    //fPersonControl.Visible = true;
                    reqInv = true;
                }
            }

            if (fOptions.TrackMatchedSources && fLines.Count > 0) {
                foreach (var line in fLines.Values) {
                    var dashOffset = line.DashOffset;
                    if (dashOffset < line.Distance) {
                        dashOffset += 1;
                    } else {
                        dashOffset = 0;
                    }
                    line.DashOffset = dashOffset;
                }
                reqInv = true;
            }

            if (reqInv) fView.Invalidate();
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

        private static IImage GetUIImage(ChartRenderer renderer, string resName, ThemeElement themeElement)
        {
            IImage image;
            if (AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                image = PrepareImage(renderer, resName, true);
            } else {
                image = AppHost.ThemeManager.GetThemeImage(themeElement, true);
            }
            return image;
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

                fExpPic = GetUIImage(renderer, "btn_expand.gif", ThemeElement.Glyph_Expand);
                fCollapsePic = GetUIImage(renderer, "btn_expand2.gif", ThemeElement.Glyph_Expand2);
                fInfoPic = GetUIImage(renderer, "btn_info.gif", ThemeElement.Glyph_Info);
                fBookmarkPic = GetUIImage(renderer, "tg_bookmark.png", ThemeElement.Glyph_Bookmark);
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
            fCollapsePic = sourceModel.fCollapsePic;
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

        internal bool IsExtendedTree()
        {
            return fOptions.ExtendedTree && fKind == TreeChartKind.ckBoth;
        }

        /*private bool IsExtPerson(TreeChartPerson person)
        {
            return IsExtendedTree() && (person.Rec == fRootRec || person.HasFlag(PersonFlag.pfRootSpouse));
        }*/

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

        private string GetMarriageDate(GDMFamilyRecord family)
        {
            DateFormat dateFormat = fOptions.OnlyYears ? DateFormat.dfYYYY : DateFormat.dfDD_MM_YYYY;
            return GKUtils.GetMarriageDateStr(family, dateFormat, GlobalOptions.Instance.ShowDatesSign);
        }

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
                            fGraph.AddRelation(personNode.Father.Node, personNode.Mother.Node, KinshipType.ktSpouse, KinshipType.ktSpouse, (!commonLaw ? KinshipExt.None : KinshipExt.CommonLaw));
                        }

                        if (fOptions.MarriagesDates) {
                            var marDate = GetMarriageDate(family);
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
                        fGraph.AddRelation(childNode.Node, result.Node, KinshipType.ktParent, KinshipType.ktChild, (!adopted ? KinshipExt.None : KinshipExt.Adoption));
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
                            fGraph.AddRelation(result.Node, resParent.Node, KinshipType.ktSpouse, KinshipType.ktSpouse, (!commonLaw ? KinshipExt.None : KinshipExt.CommonLaw));
                        }

                        result.AddSpouse(resParent);
                        resParent.BaseSpouse = result;
                        resParent.BaseFamily = family;

                        if (fOptions.MarriagesDates) {
                            var marDate = GetMarriageDate(family);
                            resParent.MarriageDate = marDate;
                        }

                        if (resParent.Rec != null) {
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
                                if (ft != null) fGraph.AddRelation(child.Node, ft.Node, KinshipType.ktParent, KinshipType.ktChild, (!adopted ? KinshipExt.None : KinshipExt.Adoption));
                                if (mt != null) fGraph.AddRelation(child.Node, mt.Node, KinshipType.ktParent, KinshipType.ktChild, (!adopted ? KinshipExt.None : KinshipExt.Adoption));
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
                string kinship = fGraph.DetermineKinship(target.Rec, false, GlobalOptions.Instance.ShortKinshipForm);
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
#if NETCOREAPP && OS_MSWIN
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

                if (fOptions.MarriagesDates && !string.IsNullOrEmpty(p.MarriageDate)) {
                    p.MarriageDateWidth = fRenderer.GetTextWidth(p.MarriageDate, fDrawFont);
                }
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

            fLayout.RecalcChart();

            RecalcTreeBounds();
        }

        #endregion

        #region Filtering and search

        public void DoFilter(GDMIndividualRecord root)
        {
            if (root == null)
                throw new ArgumentNullException(nameof(root));

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
                        result = birthYear != 0 && birthYear >= fFilter.BranchYear;
                        break;

                    case ChartFilter.BranchCutType.Persons:
                        result = fFilter.BranchPersons.IndexOf(person.XRef + ";") >= 0;
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

        public override void SetLayout(IChartLayout layout)
        {
            base.SetLayout(layout);
        }

        public override void SetRenderer(ChartRenderer renderer)
        {
            base.SetRenderer(renderer);

            InitSigns(renderer);
            InitGraphics();
        }

        public void InitGraphics()
        {
            DoneGraphics();

            fClrBlack = ChartRenderer.GetColor(GKColors.Black);

            // Anti-aliasing works differently in EtoForms and WinForms.
            IColor clrLine, clrDecor;
#if GK3
            clrLine = ChartRenderer.GetColor(GKColors.DimGray);
            clrDecor = ChartRenderer.GetColor(GKColors.DarkGray);
#else
            clrLine = ChartRenderer.GetColor(GKColors.Black);
            clrDecor = ChartRenderer.GetColor(GKColors.DimGray);
#endif

            fLinePen = fRenderer.CreatePen(clrLine, 1f);
            fLineDecorativePen = fRenderer.CreatePen(clrDecor, 1f);
            fLineSelectedPen = fRenderer.CreatePen(clrLine, 2.4f);
            fLineDottedPen = fRenderer.CreatePen(clrLine, 1f, new float[] { 4.0F, 2.0F });
            fLineDottedDecorativePen = fRenderer.CreatePen(clrDecor, 1f, new float[] {4.0F, 2.0F});
            fLineDottedSelectedPen = fRenderer.CreatePen(clrLine, 2.4f, new float[] { 4.0F, 2.0F });
            fSolidBlack = fRenderer.CreateBrush(fClrBlack);
            fSolidF = fRenderer.CreateBrush(ChartRenderer.GetColor(GKColors.Red));
            fSolidM = fRenderer.CreateBrush(ChartRenderer.GetColor(GKColors.Blue));

            var clrGreen = ChartRenderer.GetColor(GKColors.ForestGreen);
            fLineTMSYPen = fRenderer.CreatePen(clrGreen, 2.4f, new float[] { 4.0F, 2.0F });
            var clrCrimson = ChartRenderer.GetColor(GKColors.Crimson);
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
            ExtRect expRt = new ExtRect(personRect.Left, personRect.Top - psz - 2, psz, psz);
            return expRt;
        }

        public ExtRect GetInfoRect(ExtRect personRect)
        {
            int psz = (int)(16 * fPicScale);
            ExtRect expRt = new ExtRect(personRect.Right - psz, personRect.Top - psz - 2, psz, psz);
            return expRt;
        }

        public ExtRect GetCollapseRect(TreeChartPerson pers)
        {
            int psz = (int)(16 * fPicScale);
            ExtRect collRt;
            int x = pers.PtX - (psz / 2);
            collRt.Left = x;
            collRt.Top = pers.PtY - psz - 2;
            collRt.Right = x + psz - 1;
            collRt.Bottom = pers.PtY - 2;
            return collRt;
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

            /*if (fVisibleArea.GetWidth() <= 0 || fVisibleArea.GetHeight() <= 0) {
                return false;
            }*/
            bool isLineVisible = SysUtils.HasRangeIntersection(fVisibleArea.Left, fVisibleArea.Right, x1, x2)
                && SysUtils.HasRangeIntersection(fVisibleArea.Top, fVisibleArea.Bottom, y1, y2);

            if (!isLineVisible) return;

            IPen linePen, decorativeLinePen;

            if (isTracked && fOptions.TrackMatchedSources) {
                /*if (!isDotted) {
                    linePen = fLineSelectedPen;
                } else {
                    linePen = fLineDottedSelectedPen;
                }*/
                linePen = isMatched ? fLineTMSYPen : fLineTMSNPen;
                decorativeLinePen = null;

                LineHandle line;
                int hash = LineHandle.GetHashCode(x1, y1, x2, y2);
                if (!fLines.TryGetValue(hash, out line)) {
                    line = new LineHandle(x1, y1, x2, y2);
                    fLines.Add(hash, line);
                }
                linePen.DashOffset = line.DashOffset;
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

        private const int RoundedRectRadius = 6;

        private void DrawBorder(IPen xpen, ExtRect rt, bool dead, TreeChartPerson person)
        {
            IColor bColor = person.GetFillColor(dead);
            if (fHighlightedPerson == person) {
                bColor = bColor.Lighter(HIGHLIGHTED_VAL);
            }

            int cornersRadius = (person.Sex == GDMSex.svFemale) ? RoundedRectRadius : 0;
            fRenderer.DrawRectangle(xpen, bColor, rt.Left, rt.Top, rt.GetWidth(), rt.GetHeight(), cornersRadius);
        }

        private void DrawCoverGlass(ExtRect rt, TreeChartPerson person)
        {
            float rad = (person.Sex == GDMSex.svFemale) ? RoundedRectRadius : 0.0f;
            fRenderer.DrawCoverGlass(rt.Left, rt.Top, rt.GetWidth(), rt.GetHeight(), rad);
        }

        private void DrawPerson(TreeChartPerson person, ChartDrawMode drawMode, bool isSelected)
        {
            try {
                ExtRect persRt = person.Rect;
                if (drawMode == ChartDrawMode.dmInteractive && !fVisibleArea.IntersectsWith(persRt))
                    return;

                persRt.Offset(fOffsetX, fOffsetY);

                if (person.HasFlag(PersonFlag.pfIsDead) && fOptions.MourningEdges) {
                    ExtRect dt = persRt.GetOffset(-2, -2);
                    DrawBorder(null, dt, true, person);
                }

                IPen xpen = isSelected ? fRenderer.CreatePen(person.GetSelectedColor(), 2.0f) : fRenderer.CreatePen(fClrBlack, 1.0f);
                using (xpen) {
                    DrawBorder(xpen, persRt, false, person);
                }

                ExtRect bordRt = persRt;
                if (person.Portrait != null) {
                    ExtRect portRt = person.PortraitArea.GetOffset(persRt.Left, persRt.Top);
                    fRenderer.DrawImage(person.Portrait, portRt.Left, portRt.Top, portRt.GetWidth(), portRt.GetHeight(), person.Rec.XRef);

                    persRt.Left += person.PortraitWidth;
                }

                int bh = (int)fBoldFont.Height;
                int th = (int)fDrawFont.Height;
                int ry = persRt.Top + fNodePadding;
                int prtWidth = (persRt.Right - persRt.Left + 1);

                for (int k = 0, num = person.Lines.Length; k < num; k++) {
                    int lh;
                    IFont font;
                    if (fOptions.BoldNames && k < person.NameLines) {
                        lh = bh;
                        font = fBoldFont;
                    } else {
                        lh = th;
                        font = fDrawFont;
                    }

                    int rx = persRt.Left + (prtWidth - person.Widths[k]) / 2;
                    fRenderer.DrawString(person.Lines[k], font, fSolidBlack, rx, ry, fOptions.TextEffect);
                    ry += lh;
                }

                DrawCoverGlass(bordRt, person);

                if (fOptions.SignsVisible && !person.Signs.IsEmpty()) {
                    int i = 0;
                    int dy = (int)(21 * fPicScale);
                    for (var cps = SpecialUserRef.urRI_StGeorgeCross; cps <= SpecialUserRef.urLast; cps++) {
                        if (!person.Signs.Contains(cps)) continue;

                        IImage pic = fSignsPic[(int)cps];
                        int pW = (int)(pic.Width * fPicScale);
                        int pH = (int)(pic.Height * fPicScale);
                        fRenderer.DrawImage(pic, bordRt.Right + 1, bordRt.Top - dy + i * pH, pW, pH, cps.ToString());
                        i++;
                    }
                }

                // only for existing individuals
                if (person.Rec != null) {
                    // draw XRef
                    if (fXRefVisible) {
                        DrawText(person.Rec.XRef, bordRt.Right, bordRt.Bottom, 3, false);
                    }

                    // draw CI
                    if (fCertaintyIndex) {
                        fRenderer.DrawString(person.CertaintyAssessment, fDrawFont, fSolidBlack, bordRt.Left, bordRt.Bottom);
                    }
                }

                // only interactive mode
                if (drawMode == ChartDrawMode.dmInteractive) {
                    if (person.HasFlag(PersonFlag.pfCanExpand)) {
                        ExtRect expRt = GetExpanderRect(bordRt);
                        fRenderer.DrawImage(fExpPic, expRt.Left, expRt.Top, expRt.Width, expRt.Height, string.Empty);
                    }

                    if (person.IsCollapsed) {
                        ExtRect expRt = GetCollapseRect(person);
                        expRt.Offset(fOffsetX, fOffsetY);
                        fRenderer.DrawImage(fCollapsePic, expRt.Left, expRt.Top, expRt.Width, expRt.Height, string.Empty);
                    }

                    if (person.Selected) {
                        ExtRect infoRt = GetInfoRect(bordRt);
                        fRenderer.DrawImage(fInfoPic, infoRt.Left, infoRt.Top, infoRt.Width, infoRt.Height, string.Empty);
                    }

                    if (person.HasFlag(PersonFlag.pfBookmark)) {
                        int pW = (int)(fBookmarkPic.Width * fPicScale);
                        int pH = (int)(fBookmarkPic.Height * fPicScale);
                        fRenderer.DrawImage(fBookmarkPic, bordRt.Right - pW * 2, bordRt.Top - pH - 2, pW, pH, string.Empty);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("TreeChartModel.DrawPerson()", ex);
            }
        }

        #region Tracking

        private static bool HasTrackedSources(TreeChartPerson person1, TreeChartPerson person2)
        {
            if (person1 == null || person2 == null) return false;

            return person1.IntersectSources(person2);
        }

        private static bool HasTrackedChild(TreeChartPerson person, ChartDrawMode drawMode)
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

        private static bool HasTrackedLines(TreeChartPerson person, ChartDrawMode drawMode)
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

            var tH = (int)fDrawFont.Height;
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
            int fX = -1, mX = -1;

            if (person.Father != null) {
                DrawLine(person.Father.PtX, crY, person.PtX, crY, dotted, isTrackedAncestors, isMatchedFather); // h
                DrawLine(person.Father.PtX, parY, person.Father.PtX, crY, dotted, isTrackedAncestors, isMatchedFather); // v

                fX = person.Father.PtX;
                if (!string.IsNullOrEmpty(person.Father.MarriageDate) && marrDate == null) {
                    marrDate = person.Father.MarriageDate;
                }
            }

            if (person.Mother != null) {
                DrawLine(person.Mother.PtX, crY, person.PtX, crY, dotted, isTrackedAncestors, isMatchedMother); // h
                DrawLine(person.Mother.PtX, parY, person.Mother.PtX, crY, dotted, isTrackedAncestors, isMatchedMother); // v

                mX = person.Mother.PtX;
                if (!string.IsNullOrEmpty(person.Mother.MarriageDate) && marrDate == null) {
                    marrDate = person.Mother.MarriageDate;
                }
            }

            if (!string.IsNullOrEmpty(marrDate)) {
                int q = (!fOptions.InvertedTree) ? 1 : 2;

                if (fX >= 0 && mX >= 0) {
                    DrawTextCt(marrDate, fX, mX, crY, q);
                } else {
                    DrawText(marrDate, person.PtX, crY, q);
                }
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

        private void DrawTextCt(string text, float x1, float x2, float y, int quad, bool offset = true)
        {
            if (string.IsNullOrEmpty(text))
                return;

            ExtSizeF tsz = fRenderer.GetTextSize(text, fDrawFont);
            float x = x1 + (x2 - x1 - tsz.Width) / 2;

            // quadrant clockwise from 00 hours
            if (offset) {
                x += fOffsetX;
                y += fOffsetY;
            }

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

            fRenderer.DrawString(text, fDrawFont, fSolidBlack, x, y, fOptions.TextEffect);
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

        private static void GetAncestors(TreeChartPerson person, HashSet<GDMIndividualRecord> ancestors)
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

        private static void GetDescendants(TreeChartPerson person, HashSet<GDMIndividualRecord> descendants)
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
