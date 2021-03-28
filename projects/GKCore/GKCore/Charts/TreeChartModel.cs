/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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
using System.Collections.Generic;
using System.Text.RegularExpressions;
using BSLib;
using BSLib.Design.Graphics;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Interfaces;
using GKCore.Kinships;
using GKCore.Options;
using GKCore.Search;
using GKCore.Types;

using BSDColors = BSLib.Design.BSDConsts.Colors;

namespace GKCore.Charts
{
    public delegate void PersonModifyEventHandler(object sender, PersonModifyEventArgs eArgs);

    public delegate void RootChangedEventHandler(object sender, TreeChartPerson person);

    public delegate void InfoRequestEventHandler(object sender, TreeChartPerson person);

    /// <summary>
    /// 
    /// </summary>
    public class PersonList : ExtList<TreeChartPerson>
    {
        public PersonList(bool ownsObjects) : base(ownsObjects)
        {
        }
    }

    public enum TreeChartKind
    {
        ckAncestors,
        ckDescendants,
        ckBoth
    }

    /// <summary>
    /// 
    /// </summary>
    public class TreeChartModel : ChartModel
    {
        public const int DEF_MARGINS = 24;
        public const int DEF_SPOUSE_DISTANCE = 10;
        public const int DEF_BRANCH_DISTANCE = 40;
        public const int DEF_LEVEL_DISTANCE = 46;
        public const float HIGHLIGHTED_VAL = 0.1f;

        // Specifies the interior spacing of a node.
        public const int DEF_PERSON_NODE_PADDING = 10;

        private readonly ChartFilter fFilter;
        private readonly PersonList fPersons;
        private readonly IList<string> fPreparedFamilies;
        private readonly IList<string> fPreparedIndividuals;

        private IBaseWindow fBase;
        private int fBranchDistance;
        private bool fCertaintyIndex;
        private IPen fDecorativeLinePen;
        private IPen fDottedLinePen;
        private IPen fDottedDecorativeLinePen;
        private int fDepthLimit;
        private IFont fDrawFont;
        private int[] fEdges;
        private IImage fExpPic;
        private IImage fInfoPic;
        private IImage fPersExpPic;
        private GKVarCache<GDMIndividualRecord, bool> fFilterData;
        private KinshipsGraph fGraph;
        private bool fHasMediaFail;
        private TreeChartPerson fHighlightedPerson;
        private TreeChartKind fKind;
        private TreeChartPerson fKinRoot;
        private int fLevelDistance;
        private IPen fLinePen;
        private int fMargins;
        private int fNodePadding;
        private TreeChartOptions fOptions;
        private bool fPathDebug;
        private TreeChartPerson fRoot;
        private float fScale;
        private IImage[] fSignsPic;
        private IBrush fSolidBlack;
        private int fSpouseDistance;
        private GDMTree fTree;
        private ExtRect fTreeBounds;
        private ExtRect fVisibleArea;


        public IBaseWindow Base
        {
            get { return fBase; }
            set {
                fBase = value;
                fGraph = new KinshipsGraph(fBase.Context);
                fTree = fBase.Context.Tree;
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
            set { fCertaintyIndex = value; }
        }

        public int DepthLimit
        {
            get { return fDepthLimit; }
            set { fDepthLimit = value; }
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
            set { fHighlightedPerson = value; }
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

        public bool PathDebug
        {
            get { return fPathDebug; }
            set { fPathDebug = value; }
        }

        public PersonList Persons
        {
            get { return fPersons; }
        }

        public IList<string> PreparedIndividuals
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


        public TreeChartModel()
        {
            fDepthLimit = -1;
            fEdges = new int[256];
            fFilter = new ChartFilter();
            fFilterData = new GKVarCache<GDMIndividualRecord, bool>();
            fGraph = null;
            fPersons = new PersonList(true);
            fPreparedFamilies = new List<string>();
            fPreparedIndividuals = new List<string>();
            fScale = 1.0f;

            fBranchDistance = DEF_BRANCH_DISTANCE;
            fLevelDistance = DEF_LEVEL_DISTANCE;
            fSpouseDistance = DEF_SPOUSE_DISTANCE;
            fMargins = DEF_MARGINS;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fGraph != null) fGraph.Dispose();
                fFilter.Dispose();
                fPersons.Dispose();

                DoneGraphics();
                DoneSigns();
                if (fDrawFont != null) fDrawFont.Dispose();
            }
            base.Dispose(disposing);
        }

        private static IImage PrepareImage(string name, bool makeTransp)
        {
            if (name == null) return null;

            try {
                var result = AppHost.GfxProvider.LoadResourceImage(name, makeTransp);
                return result;
            } catch (Exception ex) {
                Logger.WriteError("TreeChartModel.PrepareImage()", ex);
                return null;
            }
        }

        private void InitSigns()
        {
            try {
                var signsPic = new IImage[9];
                signsPic[0] = PrepareImage("tg_george_cross.gif", true);
                signsPic[1] = PrepareImage("tg_soldier.gif", true);
                signsPic[2] = PrepareImage("tg_soldier_fall.gif", true);
                signsPic[3] = PrepareImage("tg_veteran_rear.gif", true);
                signsPic[4] = PrepareImage("tg_barbed_wire.gif", true);
                signsPic[5] = PrepareImage("tg_islam_sym.gif", false);
                signsPic[6] = PrepareImage("tg_latin_cross.gif", false);
                signsPic[7] = PrepareImage("tg_orthodox_cross.gif", false);
                signsPic[8] = PrepareImage("tg_oldritual_cross.gif", false);
                fSignsPic = signsPic;

                fExpPic = PrepareImage("btn_expand.gif", true);
                fPersExpPic = PrepareImage("btn_expand2.gif", true);
                fInfoPic = PrepareImage("btn_info.gif", true);
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

            fBranchDistance = sourceModel.fBranchDistance;
            fCertaintyIndex = sourceModel.fCertaintyIndex;
            fDecorativeLinePen = sourceModel.fDecorativeLinePen;
            fDottedLinePen = sourceModel.fDottedLinePen;
            fDottedDecorativeLinePen = sourceModel.fDottedDecorativeLinePen;
            fDepthLimit = sourceModel.fDepthLimit;
            fDrawFont = sourceModel.fDrawFont;
            fExpPic = sourceModel.fExpPic;
            fPersExpPic = sourceModel.fPersExpPic;
            fInfoPic = sourceModel.fInfoPic;
            //fKind = sourceModel.fKind;
            //fKinRoot = sourceModel.fKinRoot;
            fLevelDistance = sourceModel.fLevelDistance;
            fLinePen = sourceModel.fLinePen;
            fMargins = sourceModel.fMargins;
            fNodePadding = sourceModel.fNodePadding;
            fOptions = sourceModel.fOptions;
            //fRoot = sourceModel.fRoot;
            fScale = sourceModel.fScale;
            fSignsPic = sourceModel.fSignsPic;
            fSolidBlack = sourceModel.fSolidBlack;
            fSpouseDistance = sourceModel.fSpouseDistance;
            fTree = sourceModel.fTree;
        }

        public void GenChart(GDMIndividualRecord iRec, TreeChartKind kind, bool rootCenter)
        {
            fKind = kind;
            fPersons.Clear();
            fGraph.Clear();
            DoFilter(iRec);
            fRoot = null;
            fPreparedIndividuals.Clear();

            switch (fKind) {
                case TreeChartKind.ckAncestors:
                    fPreparedFamilies.Clear();
                    fRoot = DoAncestorsStep(null, iRec, 1, false);
                    break;

                case TreeChartKind.ckDescendants:
                    fPreparedFamilies.Clear();
                    fRoot = DoDescendantsStep(null, iRec, 1);
                    break;

                case TreeChartKind.ckBoth:
                    fPreparedFamilies.Clear();
                    fRoot = DoAncestorsStep(null, iRec, 1, false);
                    fPreparedFamilies.Clear();
                    DoDescendantsStep(null, iRec, 1);
                    break;
            }

            fKinRoot = fRoot;
        }

        #region Tree walking

        private TreeChartPerson AddDescPerson(TreeChartPerson parent, GDMIndividualRecord iRec, bool outsideKin, int generation)
        {
            try
            {
                TreeChartPerson result;

                if (fRoot != null && fRoot.Rec == iRec) {
                    result = fRoot;
                    result.Parent = parent;
                } else {
                    result = CreatePerson(iRec, generation);
                    result.Parent = parent;

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
                Logger.WriteError("TreeChartModel.AddDescPerson()", ex);
                throw;
            }
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

        private TreeChartPerson DoAncestorsStep(TreeChartPerson aChild, GDMIndividualRecord aPerson, int generation, bool dupFlag)
        {
            try
            {
                TreeChartPerson result = null;

                if (aPerson != null)
                {
                    result = CreatePerson(aPerson, generation);
                    result.SetFlag(PersonFlag.pfAncWalk);

                    if (aChild != null) {
                        result.AddChild(aChild);
                    }

                    if ((fDepthLimit <= -1 || generation != fDepthLimit) && aPerson.ChildToFamilyLinks.Count > 0 && !dupFlag)
                    {
                        GDMChildToFamilyLink childLink = aPerson.ChildToFamilyLinks[0];
                        result.Adopted = (childLink.PedigreeLinkageType == GDMPedigreeLinkageType.plAdopted);
                        GDMFamilyRecord family = fTree.GetPtrValue(childLink);

                        bool isDup = (fPreparedFamilies.IndexOf(family.XRef) >= 0);
                        if (!isDup) fPreparedFamilies.Add(family.XRef);

                        if (fBase.Context.IsRecordAccess(family.Restriction))
                        {
                            GDMIndividualRecord iFather = fTree.GetPtrValue(family.Husband);
                            GDMIndividualRecord iMother = fTree.GetPtrValue(family.Wife);

                            bool divorced = (family.Status == GDMMarriageStatus.MarrDivorced);

                            if (iFather != null && fBase.Context.IsRecordAccess(iFather.Restriction))
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

                            if (iMother != null && fBase.Context.IsRecordAccess(iMother.Restriction))
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

                            if (fOptions.MarriagesDates) {
                                DateFormat dateFormat = DateFormat.dfYYYY;
                                var marDate = GKUtils.GetMarriageDateStr(family, dateFormat);
                                if (!string.IsNullOrEmpty(marDate)) {
                                    if (result.Father != null) {
                                        result.Father.MarriageDate = marDate;
                                    } else if (result.Mother != null) {
                                        result.Mother.MarriageDate = marDate;
                                    }
                                }
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
                Logger.WriteError("TreeChartModel.DoAncestorsStep()", ex);
                throw;
            }
        }

        private bool CheckDescendantFilter(GDMIndividualRecord person, int level)
        {
            bool result = true;

            switch (fFilter.SourceMode)
            {
                case FilterGroupMode.All:
                    break;

                case FilterGroupMode.None:
                    if (person.SourceCitations.Count != 0) {
                        result = false;
                    }
                    break;

                case FilterGroupMode.Any:
                    if (person.SourceCitations.Count == 0) {
                        result = false;
                    }
                    break;

                case FilterGroupMode.Selected:
                    GDMSourceRecord filterSource;
                    if (string.IsNullOrEmpty(fFilter.SourceRef)) {
                        filterSource = null;
                    } else {
                        filterSource = fTree.XRefIndex_Find(fFilter.SourceRef) as GDMSourceRecord;
                    }
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

        private TreeChartPerson DoDescendantsStep(TreeChartPerson parent, GDMIndividualRecord person, int level)
        {
            try
            {
                TreeChartPerson result = null;
                if (person == null) return result;

                int spousesNum = person.SpouseToFamilyLinks.Count;

                // if the person have more than one families - to hide unknown spouses it is impossible
                bool skipUnkSpouses = fOptions.HideUnknownSpouses && spousesNum < 2;

                bool skipChildless = fOptions.ChildlessExclude && fBase.Context.IsChildless(person);

                if (!skipChildless || level <= 1 || spousesNum != 0)
                {
                    if (!CheckDescendantFilter(person, level))
                        return null;

                    result = AddDescPerson(parent, person, false, level);

                    for (int i = 0; i < spousesNum; i++)
                    {
                        GDMFamilyRecord family = fTree.GetPtrValue(person.SpouseToFamilyLinks[i]);

                        // protection against invalid third-party files
                        if (family == null) {
                            Logger.WriteError("TreeChartModel.DoDescendantsStep(): null pointer to family");
                            continue;
                        }

                        bool isDup = (fPreparedFamilies.IndexOf(family.XRef) >= 0);
                        if (!isDup) fPreparedFamilies.Add(family.XRef);

                        if (!fBase.Context.IsRecordAccess(family.Restriction)) continue;

                        TreeChartPerson resParent = null;
                        TreeChartPerson ft = null;
                        TreeChartPerson mt = null;
                        PersonFlag descFlag = PersonFlag.pfDescByFather;
                        bool invalidSpouse = false;
                        bool skipUnk = false;

                        switch (person.Sex) {
                            case GDMSex.svFemale:
                                {
                                    GDMIndividualRecord sp = fTree.GetPtrValue(family.Husband);
                                    skipUnk = skipUnkSpouses && (sp == null);

                                    if (!skipUnk) {
                                        resParent = AddDescPerson(null, sp, true, level);
                                        resParent.Sex = GDMSex.svMale;
                                        resParent.SetFlag(PersonFlag.pfSpouse);

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

                            case GDMSex.svMale:
                                {
                                    GDMIndividualRecord sp = fTree.GetPtrValue(family.Wife);
                                    skipUnk = skipUnkSpouses && (sp == null);

                                    if (!skipUnk) {
                                        resParent = AddDescPerson(null, sp, true, level);
                                        resParent.Sex = GDMSex.svFemale;
                                        resParent.SetFlag(PersonFlag.pfSpouse);

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
                                invalidSpouse = true;
                                Logger.WriteError("TreeChartModel.DoDescendantsStep(): sex of spouse is undetermined");
                                break;
                        }

                        if (invalidSpouse) {
                            continue;
                        }

                        if (resParent != null) {
                            if (fOptions.Kinship) {
                                fGraph.AddRelation(result.Node, resParent.Node, RelationKind.rkSpouse, RelationKind.rkSpouse);
                            }

                            result.AddSpouse(resParent);
                            resParent.BaseSpouse = result;
                            resParent.BaseFamily = family;

                            if (resParent.Rec != null) {
                                if (fOptions.MarriagesDates) {
                                    //DateFormat dateFormat = (fOptions.OnlyYears) ? DateFormat.dfYYYY : DateFormat.dfDD_MM_YYYY;
                                    DateFormat dateFormat = DateFormat.dfYYYY;
                                    var marDate = GKUtils.GetMarriageDateStr(family, dateFormat);
                                    resParent.MarriageDate = marDate;
                                }

                                if (resParent.Rec.ChildToFamilyLinks.Count > 0) {
                                    resParent.SetFlag(PersonFlag.pfHasInvAnc);
                                }

                                if (resParent.Rec.SpouseToFamilyLinks.Count > 1) {
                                    resParent.SetFlag(PersonFlag.pfHasInvDesc);
                                }
                            }
                        } else {
                            resParent = result;
                        }

                        if ((fDepthLimit <= -1 || level != fDepthLimit) && (!isDup))
                        {
                            int num2 = family.Children.Count;
                            for (int j = 0; j < num2; j++)
                            {
                                var childRec = fTree.GetPtrValue(family.Children[j]);

                                // protection against invalid third-party files
                                if (childRec == null) {
                                    Logger.WriteError("TreeChartModel.DoDescendantsStep(): null pointer to child");
                                    continue;
                                }

                                if (!fBase.Context.IsRecordAccess(childRec.Restriction)) continue;

                                TreeChartPerson child = DoDescendantsStep(resParent, childRec, level + 1);
                                if (child == null) continue;

                                child.Father = ft;
                                child.Mother = mt;
                                child.SetFlag(descFlag);

                                GDMChildToFamilyLink childLink = childRec.FindChildToFamilyLink(family);
                                if (childLink != null) {
                                    child.Adopted = (childLink.PedigreeLinkageType == GDMPedigreeLinkageType.plAdopted);
                                }

                                if (fOptions.Kinship) {
                                    if (ft != null) {
                                        fGraph.AddRelation(child.Node, ft.Node, RelationKind.rkParent, RelationKind.rkChild);
                                    }
                                    if (mt != null) {
                                        fGraph.AddRelation(child.Node, mt.Node, RelationKind.rkParent, RelationKind.rkChild);
                                    }
                                }
                            }
                        } else {
                            if (family.Children.Count > 0) {
                                if (ft != null) {
                                    ft.SetFlag(PersonFlag.pfHasInvDesc);
                                }
                                if (mt != null) {
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
                Logger.WriteError("TreeChartModel.DoDescendantsStep()", ex);
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
                string kinship = fGraph.GetRelationship(target.Rec);
                if (kinship == "?") {
                    kinship = "-";
                }
                target.Kinship = "[" + kinship + "]";

                if (fPathDebug) {
                    target.PathDebug = fGraph.IndividualsPath;
                }
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

        private void Predef()
        {
            fBranchDistance = (int)Math.Round(fOptions.BranchDistance * fScale);
            fLevelDistance = (int)Math.Round(fOptions.LevelDistance * fScale);
            fMargins = (int)Math.Round(fOptions.Margins * fScale);
            fNodePadding = (int)(DEF_PERSON_NODE_PADDING * fScale);
            fSpouseDistance = (int)Math.Round(fOptions.SpouseDistance * fScale);
        }

        public void RecalcChart(bool noRedraw = false)
        {
            float fsz = (float)Math.Round(fOptions.DefFontSize * fScale);
            fDrawFont = AppHost.GfxProvider.CreateFont(fOptions.DefFontName, fsz, false);

            Predef();

            if (fOptions.Kinship && fKinRoot != null) {
                fGraph.SetTreeRoot(fKinRoot.Rec);
            }

            int lines = InitInfoSize();

            int num = fPersons.Count;
            for (int i = 0; i < num; i++) {
                TreeChartPerson p = fPersons[i];

                if (fOptions.Kinship) {
                    FindRelationship(p);
                }

                p.IsVisible = false;
                p.CalcBounds(lines, fRenderer);
            }

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

            fImageHeight = fTreeBounds.GetHeight() + fMargins * 2;
            fImageWidth = fTreeBounds.GetWidth() + fMargins * 2;
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

        private void ShiftAnc(TreeChartPerson person, int offset)
        {
            TreeChartPerson pp = person;
            if (pp == null) return;

            do
            {
                pp.PtX += offset;
                fEdges[pp.Generation] = pp.Rect.Right;

                pp = (pp.GetChildsCount() < 1) ? null : pp.GetChild(0);
            }
            while (pp != null);
        }

        private void RecalcAnc(ExtList<TreeChartPerson> prev, TreeChartPerson person, int ptX, int ptY)
        {
            if (person == null) return;

            person.PtX = ptX;
            person.PtY = ptY;

            int gen = person.Generation;

            int offset = (fEdges[gen] > 0) ? fBranchDistance : fMargins;
            int bound = fEdges[gen] + offset;
            if (person.Rect.Left <= bound) {
                ShiftAnc(person, bound - person.Rect.Left);
            }

            fEdges[gen] = person.Rect.Right;

            prev.Add(person);
            if (person.Rect.Top < 0) {
                offset = 0 - person.Rect.Top + fMargins;
                int num = prev.Count;
                for (int i = 0; i < num; i++) {
                    prev[i].PtY += offset;
                }
            }

            person.IsVisible = true;

            if (person.IsCollapsed) {
                return;
            }

            if (person.Father != null && person.Mother != null) {
                RecalcAnc(prev, person.Father, person.PtX - (fSpouseDistance + person.Father.Width / 2), NextGenY(person, true));
                RecalcAnc(prev, person.Mother, person.PtX + (fSpouseDistance + person.Mother.Width / 2), NextGenY(person, true));

                person.PtX = (person.Father.PtX + person.Mother.PtX) / 2;
                fEdges[gen] = person.Rect.Right;
            } else {
                TreeChartPerson anc = null;
                if (person.Father != null) {
                    anc = person.Father;
                } else if (person.Mother != null) {
                    anc = person.Mother;
                }

                if (anc != null) {
                    RecalcAnc(prev, anc, person.PtX, NextGenY(person, true));
                }
            }
        }

        private int NextGenY(TreeChartPerson person, bool ancestors)
        {
            int sign = (ancestors) ? -1 : +1;
            int sign2 = (!fOptions.InvertedTree) ? +1 : -1;
            int offset = (fLevelDistance + person.Height) * sign * sign2;
            return person.PtY + offset;
        }

        private void RecalcAncestorsChart()
        {
            Array.Clear(fEdges, 0, fEdges.Length);

            var prev = new ExtList<TreeChartPerson>();
            try {
                RecalcAnc(prev, fRoot, fMargins, fMargins);
            } finally {
                prev.Dispose();
            }
        }

        private bool ShiftDesc(TreeChartPerson person, int offset, bool isSingle, bool verify = false)
        {
            if (person == null) return true;

            if (person == fRoot) {
                isSingle = false;
            }

            // fix #189
            if (verify && (person.Rect.Left + offset < fEdges[person.Generation] + fBranchDistance)) {
                return false;
            }

            bool res = true;
            if (person.BaseSpouse != null && (person.BaseSpouse.Sex == GDMSex.svFemale || person.BaseSpouse.GetSpousesCount() == 1)) {
                res = ShiftDesc(person.BaseSpouse, offset, isSingle, verify);
                if (!res) return false;
            } else {
                if (!isSingle) {
                    res = ShiftDesc(person.Father, offset, false, verify);
                    if (!res) return false;

                    res = ShiftDesc(person.Mother, offset, false, verify);
                    if (!res) return false;
                } else {
                    if (person.HasFlag(PersonFlag.pfDescByFather)) {
                        res = ShiftDesc(person.Father, offset, true, verify);
                        if (!res) return false;
                    } else if (person.HasFlag(PersonFlag.pfDescByMother)) {
                        res = ShiftDesc(person.Mother, offset, true, verify);
                        if (!res) return false;
                    }
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

            curX = person.GetChild(0).PtX;
            if (childrenCount > 1) {
                curX += (person.GetChild(childrenCount - 1).PtX - curX) / 2;
            }

            // This code is designed to align parents in the center of the location of children (across width),
            // because in the process of drawing children, various kinds of displacement are formed, 
            // and the initial arrangement of the parents can be very laterally, 
            // after the formation of a complete tree of their descendants.
            // However, this may be a problem (reason of #189) in the case if a shift initiated from descendants, 
            // must be performed to the left with an overlay on an already formed side branch.

            if (!fOptions.AutoAlign) {
                return;
            }

            if (alignPair) {
                int offset;
                switch (person.Sex) {
                    case GDMSex.svMale:
                        // fix #189
                        offset = curX - (fBranchDistance + person.Width) / 2 + 1 - person.PtX;
                        if (person.Rect.Left + offset < fEdges[person.Generation]) {
                            return;
                        }

                        ShiftDesc(person, curX - (fBranchDistance + person.Width) / 2 + 1 - person.PtX, true);
                        ShiftDesc(person.BaseSpouse, curX + (fBranchDistance + person.BaseSpouse.Width) / 2 - person.BaseSpouse.PtX, true);
                        break;

                    case GDMSex.svFemale:
                        // fix #189
                        offset = curX - (fBranchDistance + person.BaseSpouse.Width) / 2 + 1 - person.BaseSpouse.PtX;
                        if (person.BaseSpouse.Rect.Left + offset < fEdges[person.BaseSpouse.Generation]) {
                            return;
                        }

                        ShiftDesc(person, curX + (fBranchDistance + person.Width) / 2 - person.PtX, true);
                        ShiftDesc(person.BaseSpouse, curX - (fBranchDistance + person.BaseSpouse.Width) / 2 + 1 - person.BaseSpouse.PtX, true);
                        break;
                }
            } else {
                ShiftDesc(person, curX - person.PtX, true, true);
            }
        }

        private void RecalcDesc(TreeChartPerson person, int ptX, int ptY, bool predef)
        {
            if (person == null) return;

            int gen = person.Generation;
            if (predef) {
                person.PtX = ptX;
                person.PtY = ptY;
            }

            int offset = (fEdges[gen] > 0) ? fBranchDistance : fMargins;
            int bound = fEdges[gen] + offset;
            if (person.Rect.Left < bound) {
                ShiftDesc(person, bound - person.Rect.Left, true);
            }

            if (person.Sex == GDMSex.svMale) {
                RecalcDescChilds(person);
                fEdges[gen] = person.Rect.Right;
            }

            person.IsVisible = true;

            int spousesCount = person.GetSpousesCount();
            if (spousesCount > 0) {
                TreeChartPerson prev = person;
                for (int i = 0; i < spousesCount; i++) {
                    TreeChartPerson sp = person.GetSpouse(i);
                    int spOffset = (fBranchDistance + sp.Width / 2);
                    int spX = 0;

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

            if (person.Sex == GDMSex.svFemale) {
                RecalcDescChilds(person);
                fEdges[gen] = person.Rect.Right;
            }

            // FIXME: Temporary hack: if this person does not specify a particular sex,
            // then breaks the normal sequence of formation of coordinates.
            if (person.Sex == GDMSex.svUnknown || person.Sex == GDMSex.svIntersex) {
                fEdges[gen] = person.Rect.Right;
            }

            // Fix of long-distance displacement of male nodes in the presence of more than 
            // one marriage and a large tree of descendants from the first wife
            if (person.Sex == GDMSex.svMale && spousesCount >= 2) {
                var firstWife = person.GetSpouse(0);
                if (firstWife.GetChildsCount() > 0) {
                    int d = firstWife.Rect.Left - person.Rect.Right;
                    if (d > fBranchDistance * 1.5f) {
                        //person.SetFlag(PersonFlag.pfSpecialMark);
                        offset = (d - fBranchDistance);
                        ShiftDesc(person, offset, true);
                    }
                }
            }
        }

        private void RecalcDescendantsChart(bool predef)
        {
            Array.Clear(fEdges, 0, fEdges.Length);
            RecalcDesc(fRoot, fMargins, fMargins, predef);
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

            ChartFilter.BranchCutType branchCut = fFilter.BranchCut;
            switch (branchCut) {
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
            return result;
        }

        public IList<ISearchResult> FindAll(string searchPattern)
        {
            var result = new List<ISearchResult>();

            Regex regex = GKUtils.InitMaskRegex(searchPattern);

            int num = fPersons.Count;
            for (int i = 0; i < num; i++) {
                TreeChartPerson person = fPersons[i];
                GDMIndividualRecord iRec = person.Rec;
                if (iRec == null) continue;

                string fullname = GKUtils.GetNameString(iRec, true, false);
                if (GKUtils.MatchesRegex(fullname, regex)) {
                    result.Add(new SearchResult(iRec));
                }
            }

            return result;
        }

        #endregion

        #region Navigation

        public TreeChartPerson FindPersonByRec(GDMIndividualRecord iRec)
        {
            if (iRec != null) {
                int num = fPersons.Count;
                for (int i = 0; i < num; i++) {
                    TreeChartPerson p = fPersons[i];
                    if (p.Rec == iRec) {
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

            InitSigns();
            InitGraphics();
        }

        public void InitGraphics()
        {
            DoneGraphics();

            fLinePen = fRenderer.CreatePen(ChartRenderer.GetColor(BSDColors.Black), 1f);
            fDottedLinePen = fRenderer.CreatePen(ChartRenderer.GetColor(BSDColors.Black), 1f, new float[] {4.0F, 2.0F});
            fDecorativeLinePen = fRenderer.CreatePen(ChartRenderer.GetColor(BSDColors.Silver), 1f);
            fDottedDecorativeLinePen = fRenderer.CreatePen(ChartRenderer.GetColor(BSDColors.Silver), 1f, new float[] {4.0F, 2.0F});
            fSolidBlack = fRenderer.CreateSolidBrush(ChartRenderer.GetColor(BSDColors.Black));
        }

        private void DoneGraphics()
        {
            if (fLinePen != null) fLinePen.Dispose();
            if (fDecorativeLinePen != null) fDecorativeLinePen.Dispose();
            if (fSolidBlack != null) fSolidBlack.Dispose();
        }

        public static ExtRect GetExpanderRect(ExtRect personRect)
        {
            ExtRect expRt = ExtRect.Create(personRect.Left, personRect.Top - 18, personRect.Left + 16 - 1, personRect.Top - 2);
            return expRt;
        }

        public static ExtRect GetInfoRect(ExtRect personRect)
        {
            ExtRect expRt = ExtRect.Create(personRect.Right - 16, personRect.Top - 18, personRect.Right, personRect.Top - 2);
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

            return rangeX.IsOverlapped(new Range<int>(x1, x2)) && rangeY.IsOverlapped(new Range<int>(y1, y2));
        }

        private void DrawLine(int x1, int y1, int x2, int y2)
        {
            DrawLine(x1, y1, x2, y2, fLinePen, fDecorativeLinePen);
        }

        private void DrawLine(int x1, int y1, int x2, int y2, IPen linePen, IPen decorativeLinePen)
        {
            if (!IsLineVisible(x1, y1, x2, y2)) return;

            int sX = fOffsetX + x1;
            int sX2 = fOffsetX + x2;
            int sY = fOffsetY + y1;
            int sY2 = fOffsetY + y2;
            fRenderer.DrawLine(linePen, sX, sY, sX2, sY2);

            if (fOptions.Decorative) {
                if (sX == sX2) {
                    fRenderer.DrawLine(decorativeLinePen, sX + 1, sY + 1, sX2 + 1, sY2 - 1);
                } else if (sY == sY2) {
                    fRenderer.DrawLine(decorativeLinePen, sX + 1, sY + 1, sX2 + 0, sY2 + 1);
                }
            }
        }

        private void DrawBorder(IPen xpen, ExtRect rt, bool dead, TreeChartPerson person)
        {
            IColor bColor = person.GetFillColor(dead);
            if (fHighlightedPerson == person) {
                bColor = bColor.Lighter(HIGHLIGHTED_VAL);
            }

            if (person.Sex == GDMSex.svFemale) {
                fRenderer.DrawRoundedRectangle(xpen, bColor, rt.Left, rt.Top, rt.GetWidth(), rt.GetHeight(), 6);
            } else {
                fRenderer.DrawRectangle(xpen, bColor, rt.Left, rt.Top, rt.GetWidth(), rt.GetHeight());
            }
        }

        private void DrawPerson(TreeChartPerson person, ChartDrawMode drawMode)
        {
            try {
                ExtRect prt = person.Rect;
                if (drawMode == ChartDrawMode.dmInteractive && !IsPersonVisible(prt))
                    return;

                prt.Offset(fOffsetX, fOffsetY);

                if (person.IsDead) {
                    ExtRect dt = prt.GetOffset(-2, -2);
                    DrawBorder(null, dt, true, person);
                }

                IPen xpen = null;
                try {
                    if (drawMode == ChartDrawMode.dmInteractive && person.Selected) {
                        IColor penColor = person.GetSelectedColor();
                        xpen = fRenderer.CreatePen(penColor, 2.0f);
                    } else {
                        xpen = fRenderer.CreatePen(ChartRenderer.GetColor(BSDColors.Black), 1.0f);
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

                        IImage pic = fSignsPic[(int)cps];
                        fRenderer.DrawImage(pic, brt.Right, brt.Top - 21 + i * pic.Height);
                        i++;
                    }
                }

                // only interactive mode
                if (drawMode == ChartDrawMode.dmInteractive) {
                    if (person.CanExpand) {
                        ExtRect expRt = GetExpanderRect(brt);
                        fRenderer.DrawImage(fExpPic, expRt.Left, expRt.Top);
                    }

                    if (person.IsCollapsed) {
                        ExtRect expRt = GetPersonExpandRect(brt);
                        fRenderer.DrawImage(fPersExpPic, expRt.Left, expRt.Top);
                    }

                    if (person.Selected) {
                        ExtRect infoRt = GetInfoRect(brt);
                        fRenderer.DrawImage(fInfoPic, infoRt.Left, infoRt.Top);
                    }

                    // draw CI only for existing individuals
                    if (fCertaintyIndex && person.Rec != null) {
                        string cas = string.Format("{0:0.00}", person.CertaintyAssessment);
                        fRenderer.DrawString(cas, fDrawFont, fSolidBlack, brt.Left, brt.Bottom);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("TreeChartModel.DrawPerson()", ex);
            }
        }

        private bool IsDottedLines(TreeChartPerson person)
        {
            return (person != null && person.Adopted && fOptions.DottedLinesOfAdoptedChildren);
        }

        private void DrawAncestors(TreeChartPerson person, ChartDrawMode drawMode)
        {
            if (person.IsCollapsed || (person.Father == null && person.Mother == null)) {
                return;
            }

            Draw(person.Father, TreeChartKind.ckAncestors, drawMode);
            Draw(person.Mother, TreeChartKind.ckAncestors, drawMode);

            int crY, parY;
            if (!fOptions.InvertedTree) {
                crY = person.PtY - fLevelDistance / 2;
                parY = (person.Father != null) ? person.Father.PtY + person.Father.Height : person.Mother.PtY + person.Mother.Height;
            } else {
                crY = person.PtY + person.Height + fLevelDistance / 2;
                parY = (person.Father != null) ? person.Father.PtY : person.Mother.PtY;
            }

            bool dotted = IsDottedLines(person);
            var linePen = (!dotted) ? fLinePen : fDottedLinePen;
            var decorativeLinePen = (!dotted) ? fDecorativeLinePen : fDottedDecorativeLinePen;

            DrawLine(person.PtX, crY, person.PtX, person.PtY, linePen, decorativeLinePen); // v

            string marrDate = null;

            if (person.Father != null) {
                DrawLine(person.Father.PtX, crY, person.PtX, crY, linePen, decorativeLinePen); // h
                DrawLine(person.Father.PtX, parY, person.Father.PtX, crY, linePen, decorativeLinePen); // v

                if (!string.IsNullOrEmpty(person.Father.MarriageDate) && marrDate == null) {
                    marrDate = person.Father.MarriageDate;
                }
            }

            if (person.Mother != null) {
                DrawLine(person.PtX, crY, person.Mother.PtX, crY, linePen, decorativeLinePen); // h
                DrawLine(person.Mother.PtX, parY, person.Mother.PtX, crY, linePen, decorativeLinePen); // v

                if (!string.IsNullOrEmpty(person.Mother.MarriageDate) && marrDate == null) {
                    marrDate = person.Mother.MarriageDate;
                }
            }

            if (!string.IsNullOrEmpty(marrDate)) {
                int q = (!fOptions.InvertedTree) ? 1 : 2;
                DrawText(marrDate, person.PtX, crY, q);
            }
        }

        private void DrawText(string text, float x, float y, int quad = 2)
        {
            // quadrant clockwise from 00 hours
            x += fOffsetX;
            y += fOffsetY;
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

            fRenderer.DrawString(text, fDrawFont, fSolidBlack, x, y);
        }

        private void DrawDescendants(TreeChartPerson person, ChartDrawMode drawMode)
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

                        int spbV = spbBeg + spbOfs * i;
                        DrawLine(person.Rect.Right + 1, spbV, spouse.Rect.Left, spbV); // h

                        if (!string.IsNullOrEmpty(spouse.MarriageDate)) {
                            int q = (!fOptions.InvertedTree) ? 4 : 3;
                            DrawText(spouse.MarriageDate, spouse.Rect.Left, spbV, q);
                        }
                    }
                    break;

                case GDMSex.svFemale:
                    for (int i = 0; i < spousesCount; i++) {
                        TreeChartPerson spouse = person.GetSpouse(i);

                        int spbV = spbBeg + spbOfs * i;
                        DrawLine(spouse.Rect.Right + 1, spbV, person.Rect.Left, spbV); // h

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
                // draw children
                for (int i = 0; i < childrenCount; i++) {
                    Draw(person.GetChild(i), TreeChartKind.ckDescendants, drawMode);
                }

                int crY;
                if (!fOptions.InvertedTree) {
                    crY = person.PtY + person.Height + fLevelDistance / 2;
                } else {
                    crY = person.PtY - fLevelDistance / 2;
                }

                int cx = 0;
                if (person.BaseSpouse == null || (person.BaseSpouse.GetSpousesCount() > 1)) {
                    cx = person.PtX;
                    spbBeg = person.PtY + person.Height - 1;
                } else {
                    switch (person.Sex) {
                        case GDMSex.svMale:
                            cx = (person.Rect.Right + person.BaseSpouse.Rect.Left) / 2;
                            break;

                        case GDMSex.svFemale:
                            cx = (person.BaseSpouse.Rect.Right + person.Rect.Left) / 2;
                            break;
                    }

                    spbBeg -= spbOfs / 2;
                }

                DrawLine(cx, spbBeg, cx, crY); // v

                TreeChartPerson child0 = person.GetChild(0);
                int chY = (!fOptions.InvertedTree) ? child0.PtY : child0.PtY + child0.Height;

                if (childrenCount > 1) {
                    int bpx = person.GetChild(0).PtX;
                    int epx = person.GetChild(childrenCount - 1).PtX;
                    DrawLine(bpx, crY, epx, crY); // h
                }

                for (int i = 0; i < childrenCount; i++) {
                    TreeChartPerson child = person.GetChild(i);

                    bool dotted = IsDottedLines(child);
                    var linePen = (!dotted) ? fLinePen : fDottedLinePen;
                    var decorativeLinePen = (!dotted) ? fDecorativeLinePen : fDottedDecorativeLinePen;
                    DrawLine(child.PtX, crY, child.PtX, chY, linePen, decorativeLinePen); // v
                }
            }
        }

        public void Draw(ChartDrawMode drawMode)
        {
            InitGraphics();
            Draw(fRoot, fKind, drawMode);
        }

        private void Draw(TreeChartPerson person, TreeChartKind dirKind, ChartDrawMode drawMode)
        {
            if (person == null) return;

            switch (dirKind)
            {
                case TreeChartKind.ckAncestors:
                    DrawAncestors(person, drawMode);
                    break;

                case TreeChartKind.ckDescendants:
                    DrawDescendants(person, drawMode);
                    break;

                case TreeChartKind.ckBoth:
                    if (person == fRoot || dirKind == TreeChartKind.ckAncestors) DrawAncestors(person, drawMode);
                    if (person == fRoot || dirKind == TreeChartKind.ckDescendants) DrawDescendants(person, drawMode);
                    break;
            }

            DrawPerson(person, drawMode);
        }

        private void InternalDraw(ChartDrawMode drawMode, BackgroundMode background, int fSPX, int fSPY)
        {
            // dummy
        }

        #endregion

        public static bool CheckTreeChartSize(GDMTree tree, GDMIndividualRecord iRec, TreeChartKind chartKind)
        {
            bool result = true;
            if (!GlobalOptions.Instance.CheckTreeSize) return result;

            if (chartKind == TreeChartKind.ckAncestors || chartKind == TreeChartKind.ckBoth) {
                int ancCount = GKUtils.GetAncestorsCount(tree, iRec);
                if (ancCount > 2048) {
                    AppHost.StdDialogs.ShowMessage(string.Format(LangMan.LS(LSID.LSID_AncestorsNumberIsInvalid), ancCount.ToString()));
                    return false;
                }
            }

            if (chartKind >= TreeChartKind.ckDescendants && chartKind <= TreeChartKind.ckBoth) {
                int descCount = GKUtils.GetDescendantsCount(tree, iRec);
                if (descCount > 2048) {
                    AppHost.StdDialogs.ShowMessage(string.Format(LangMan.LS(LSID.LSID_DescendantsNumberIsInvalid), descCount.ToString()));
                    result = false;
                }
            }

            return result;
        }
    }
}
