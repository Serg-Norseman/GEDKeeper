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

using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Kinships;
using GKCore.Options;
using GKCore.Types;

namespace GKCore.Charts
{
    public delegate void PersonModifyEventHandler(object sender, PersonModifyEventArgs eArgs);

    public delegate void RootChangedEventHandler(object sender, TreeChartPerson person);

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

        // Specifies the interior spacing of a node.
        public const int DEF_PERSON_NODE_PADDING = 10;

        private const float HIGHLIGHTED_VAL = 0.1f;

        private readonly ChartFilter fFilter;
        private readonly PersonList fPersons;

        private IBaseWindow fBase;
        private int fBranchDistance;
        private bool fCertaintyIndex;
        private IPen fDecorativeLinePen;
        private int fDepthLimit;
        private IFont fDrawFont;
        private int[] fEdges;
        private IImage fExpPic;
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
        private readonly IList<string> fPreparedFamilies;
        private readonly IList<string> fPreparedIndividuals;
        private TreeChartPerson fRoot;
        private float fScale;
        private IImage[] fSignsPic;
        private IBrush fSolidBlack;
        private int fSpouseDistance;
        private int fSPX;
        private int fSPY;
        private GEDCOMTree fTree;
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
            fFilter = new ChartFilter();
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
            if (disposing)
            {
                if (fGraph != null) fGraph.Dispose();
                fFilter.Dispose();
                fPersons.Dispose();

                DoneGraphics();
                if (fDrawFont != null) fDrawFont.Dispose();
            }
            base.Dispose(disposing);
        }

        private static IImage PrepareImage(string name, bool makeTransp)
        {
            if (name == null) return null;

            try {
                var result = AppHost.GfxProvider.GetResourceImage(name, makeTransp);
                return result;
            } catch (Exception ex) {
                Logger.LogWrite("TreeChartModel.PrepareImage(): " + ex.Message);
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
            } catch (Exception ex) {
                Logger.LogWrite("TreeChartModel.InitSigns(): " + ex.Message);
            }
        }

        // FIXME
        private void DoneSigns()
        {
            // dummy
        }

        public void GenChart(GEDCOMIndividualRecord iRec, TreeChartKind kind, bool rootCenter)
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
                    result.BuildBy(iRec);
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
                Logger.LogWrite("TreeChartModel.AddDescPerson(): " + ex.Message);
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
                    result.BuildBy(aPerson);
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

                        if (fBase.Context.IsRecordAccess(family.Restriction))
                        {
                            GEDCOMIndividualRecord iFather = family.GetHusband();
                            GEDCOMIndividualRecord iMother = family.GetWife();

                            bool divorced = (family.GetTagStringValue("_STAT") == "NOTMARR");

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
                Logger.LogWrite("TreeChartModel.DoAncestorsStep(): " + ex.Message);
                throw;
            }
        }

        private bool CheckDescendantFilter(GEDCOMIndividualRecord person, int level)
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
                    GEDCOMSourceRecord filterSource;
                    if (fFilter.SourceRef == "") {
                        filterSource = null;
                    } else {
                        filterSource = fTree.XRefIndex_Find(fFilter.SourceRef) as GEDCOMSourceRecord;
                    }
                    if (person.IndexOfSource(filterSource) < 0) {
                        result = false;
                    }
                    break;
            }

            if ((fFilter.BranchCut != ChartFilter.BranchCutType.None) && (!(bool)person.ExtData)) {
                result = false;
            }

            return result;
        }

        private TreeChartPerson DoDescendantsStep(TreeChartPerson parent, GEDCOMIndividualRecord person, int level)
        {
            try
            {
                TreeChartPerson result = null;
                if (person != null && (!fOptions.ChildlessExclude || level <= 1 || person.SpouseToFamilyLinks.Count != 0 || !fBase.Context.IsChildless(person)))
                {
                    if (!CheckDescendantFilter(person, level))
                        return null;

                    // Kevin Routley (aka fire-eggs, aka KBR). Partially address issue #147.
                    // Here, multiple instances of the same person _in_a_generation_ are
                    // combined together. The subsequent changes to the drawing code don't
                    // cope with connecting people across generations.
                    if (person.SpouseToFamilyLinks.Count == 1 &&
                        fPreparedFamilies.IndexOf(person.SpouseToFamilyLinks[0].Family.XRef) >= 0)
                    {
                        for (int i = 0; i < fPersons.Count; i++)
                        {
                            TreeChartPerson p = fPersons[i];
                            if (p != null && p.Rec != null && p.Rec.XRef == person.XRef && p.Generation == level)
                            {
                                p.Parent = parent;
                                parent.AddChild(p);
                                return null;
                            }
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
                            Logger.LogWrite("TreeChartModel.DoDescendantsStep(): null pointer to family");
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

                        switch (person.Sex) {
                            case GEDCOMSex.svFemale:
                                {
                                    GEDCOMIndividualRecord sp = family.GetHusband();
                                    resParent = AddDescPerson(null, sp, true, level);
                                    resParent.Sex = GEDCOMSex.svMale;

                                    ft = resParent;
                                    ft.IsDup = isDup;

                                    mt = res;
                                    mt.IsDup = isDup;

                                    descFlag = PersonFlag.pfDescByFather;
                                    break;
                                }

                            case GEDCOMSex.svMale:
                                {
                                    GEDCOMIndividualRecord sp = family.GetWife();
                                    resParent = AddDescPerson(null, sp, true, level);
                                    resParent.Sex = GEDCOMSex.svFemale;

                                    ft = res;
                                    ft.IsDup = isDup;

                                    mt = resParent;
                                    mt.IsDup = isDup;

                                    descFlag = PersonFlag.pfDescByMother;
                                    break;
                                }

                            default:
                                invalidSpouse = true;
                                Logger.LogWrite("TreeChartModel.DoDescendantsStep(): sex of spouse is undetermined");
                                break;
                        }

                        if (invalidSpouse) {
                            continue;
                        }

                        if (resParent != null) {
                            if (fOptions.Kinship) {
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
                        } else {
                            resParent = res;
                        }

                        if ((fDepthLimit <= -1 || level != fDepthLimit) && (!isDup))
                        {
                            int num2 = family.Children.Count;
                            for (int j = 0; j < num2; j++)
                            {
                                var childRec = family.Children[j].Value as GEDCOMIndividualRecord;

                                // protection against invalid third-party files
                                if (childRec == null) {
                                    Logger.LogWrite("TreeChartModel.DoDescendantsStep(): null pointer to child");
                                    continue;
                                }

                                if (!fBase.Context.IsRecordAccess(childRec.Restriction)) continue;

                                TreeChartPerson child = DoDescendantsStep(resParent, childRec, level + 1);
                                if (child == null) continue;

                                child.Father = ft;
                                child.Mother = mt;
                                child.SetFlag(descFlag);
                                if (fOptions.Kinship)
                                {
                                    fGraph.AddRelation(child.Node, ft.Node, RelationKind.rkParent, RelationKind.rkChild);
                                    fGraph.AddRelation(child.Node, mt.Node, RelationKind.rkParent, RelationKind.rkChild);
                                }
                            }
                        } else {
                            if (family.Children.Count > 0) {
                                ft.SetFlag(PersonFlag.pfHasInvDesc);
                                mt.SetFlag(PersonFlag.pfHasInvDesc);
                            }
                        }
                    }
                }

                return result;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TreeChartModel.DoDescendantsStep(): " + ex.Message);
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
                target.Kinship = "[" + fGraph.GetRelationship(target.Rec) + "]";
                if (fPathDebug) {
                    target.PathDebug = fGraph.IndividualsPath;
                }
            }
        }

        #endregion

        #region Sizes and adjustment routines

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
            fBranchDistance = (int)Math.Round(DEF_BRANCH_DISTANCE * fScale);
            fLevelDistance = (int)Math.Round(DEF_LEVEL_DISTANCE * fScale);
            fMargins = (int)Math.Round(DEF_MARGINS * fScale);
            fNodePadding = (int)(DEF_PERSON_NODE_PADDING * fScale);
            fSpouseDistance = (int)Math.Round(DEF_SPOUSE_DISTANCE * fScale);
        }

        public void RecalcChart(bool noRedraw = false)
        {
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
                RecalcAnc(prev, person.Father, person.PtX - (fSpouseDistance + person.Father.Width / 2), person.PtY - fLevelDistance - person.Height);
                RecalcAnc(prev, person.Mother, person.PtX + (fSpouseDistance + person.Mother.Width / 2), person.PtY - fLevelDistance - person.Height);

                person.PtX = (person.Father.PtX + person.Mother.PtX) / 2;
                fEdges[gen] = person.Rect.Right;
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
                    RecalcAnc(prev, anc, person.PtX, person.PtY - fLevelDistance - person.Height);
                }
            }
        }

        private void RecalcAncestorsChart()
        {
            fEdges = new int[256];
            Array.Clear(fEdges, 0, fEdges.Length);

            var prev = new ExtList<TreeChartPerson>();
            try
            {
                RecalcAnc(prev, fRoot, fMargins, fMargins);
            }
            finally
            {
                prev.Dispose();
                fEdges = new int[0];
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
            }
            else
            {
                if (!isSingle) {
                    ShiftDesc(person.Father, offset, false);
                    ShiftDesc(person.Mother, offset, false);
                } else {
                    if (person.HasFlag(PersonFlag.pfDescByFather)) {
                        ShiftDesc(person.Father, offset, true);
                    } else if (person.HasFlag(PersonFlag.pfDescByMother)) {
                        ShiftDesc(person.Mother, offset, true);
                    }
                }
            }
        }

        private void RecalcDescChilds(TreeChartPerson person)
        {
            int childrenCount = person.GetChildsCount();
            if (childrenCount == 0) return;

            bool fixPair = person.BaseSpouse != null && person.BaseSpouse.GetSpousesCount() == 1;
            int centX = 0;

            if (fixPair) {
                GEDCOMSex sex = person.Sex;
                switch (sex)
                {
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

            int childrenWidth = 0;
            for (int i = 0; i < childrenCount; i++) {
                childrenWidth += person.GetChild(i).Width;
            }
            if (childrenCount > 1) {
                childrenWidth += (childrenCount - 1) * fBranchDistance;
            }

            int curX = centX - childrenWidth / 2;
            int curY = person.PtY + fLevelDistance + person.Height;

            for (int i = 0; i < childrenCount; i++) {
                TreeChartPerson child = person.GetChild(i);
                RecalcDesc(child, new ExtPoint(curX + child.Width / 2, curY), true);
                curX = child.Rect.Right + fBranchDistance;
            }

            curX = person.GetChild(0).PtX;
            if (childrenCount > 1) {
                curX += (person.GetChild(childrenCount - 1).PtX - curX) / 2;
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

        private void RecalcDesc(TreeChartPerson person, ExtPoint aPt, bool predef)
        {
            if (person == null) return;

            int gen = person.Generation;
            if (predef) {
                person.PtX = aPt.X;
                person.PtY = aPt.Y;
            }

            int offset = (fEdges[gen] > 0) ? fBranchDistance : fMargins;
            int bound = fEdges[gen] + offset;
            if (person.Rect.Left <= bound) {
                ShiftDesc(person, bound - person.Rect.Left, true);
            }

            if (person.Sex == GEDCOMSex.svMale) {
                RecalcDescChilds(person);
                fEdges[gen] = person.Rect.Right;
            }

            if (person.GetSpousesCount() > 0) {
                TreeChartPerson prev = person;
                int num = person.GetSpousesCount();
                for (int i = 0; i < num; i++) {
                    TreeChartPerson sp = person.GetSpouse(i);
                    var spPt = new ExtPoint();

                    switch (person.Sex) {
                        case GEDCOMSex.svMale:
                            spPt = new ExtPoint(prev.Rect.Right + (fBranchDistance + sp.Width / 2), person.PtY);
                            break;

                        case GEDCOMSex.svFemale:
                            spPt = new ExtPoint(prev.Rect.Left - (fBranchDistance + sp.Width / 2), person.PtY);
                            break;
                    }

                    RecalcDesc(sp, spPt, true);

                    if (sp.Sex != GEDCOMSex.svMale) {
                        prev = sp;
                    }
                }
            }

            if (person.Sex == GEDCOMSex.svFemale) {
                RecalcDescChilds(person);
                fEdges[gen] = person.Rect.Right;
            }

            // FIXME: Temporary hack: if this person does not specify a particular sex,
            // then breaks the normal sequence of formation of coordinates.
            if (person.Sex == GEDCOMSex.svNone || person.Sex == GEDCOMSex.svUndetermined) {
                fEdges[gen] = person.Rect.Right;
            }

            // Kevin Routley (aka fire-eggs, aka KBR). Partially address issue #147.
            // As a result of combining duplicate people together, there may be overlapping
            // boxes. Here, these overlaps are corrected. NOTE: duplicate people _across_generations_
            // have not been combined as no adjustment is being made to shift a box down as 
            // would be necessary.
            FixOverlap(person);
        }

        private void AdjustChildren(TreeChartPerson person, int offset)
        {
            // Kevin Routley (aka fire-eggs, aka KBR). Partially address issue #147.
            // a person has been shifted because of collision. Recursively adjust 
            // _descendants_ of that person
            if (person.GetChildsCount() < 1)
                return;
            int cMax = person.GetChildsCount();
            for (int i = 0; i < cMax; i++)
            {
                var pp = person.GetChild(i);
                pp.PtX += offset;
                AdjustChildren(pp, offset);
            }
        }

        private int FixOverlap(TreeChartPerson person)
        {
            // Kevin Routley (aka fire-eggs, aka KBR). Partially address issue #147.
            // Check for, and correct, collisions of boxes in this person's 
            // generational row.
            int shiftAmount = 0;
            for (int m = 0; m <= fPersons.Count; m++)
            {
                if (fPersons[m].Generation != person.Generation)
                    continue;
                if (person.Rec == null || fPersons[m].Rec.XRef == person.Rec.XRef)
                    break;

                if (!person.Rect.IntersectsWith(fPersons[m].Rect))
                    continue; // No overlap

                int thisShift = fPersons[m].Rect.Right + 25 - person.Rect.Left; // Add a little extra spacing
                ShiftDesc(person, thisShift, true); // Note that ShiftDesc moves this person and ANCESTORS; descendants need to be fixed
                shiftAmount += thisShift;
            }

            if (shiftAmount != 0)
                AdjustChildren(person, shiftAmount);
            return shiftAmount;
        }

        private void RecalcDescendantsChart(bool predef)
        {
            fEdges = new int[256];
            Array.Clear(fEdges, 0, fEdges.Length);

            try
            {
                RecalcDesc(fRoot, new ExtPoint(fMargins, fMargins), predef);
            }
            finally
            {
                fEdges = new int[0];
            }
        }

        #endregion

        #region Filtering and search

        public void DoFilter(GEDCOMIndividualRecord root)
        {
            if (root == null)
                throw new ArgumentNullException("root");

            if (fFilter.BranchCut == ChartFilter.BranchCutType.None) return;

            GKUtils.InitExtCounts(fTree, 0);
            DoDescendantsFilter(root);
            root.ExtData = true;
        }

        private bool DoDescendantsFilter(GEDCOMIndividualRecord person)
        {
            bool result = false;
            if (person == null) return result;

            ChartFilter.BranchCutType branchCut = fFilter.BranchCut;
            switch (branchCut) {
                case ChartFilter.BranchCutType.Years:
                    int birthYear = person.GetChronologicalYear("BIRT");
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

                int num2 = family.Children.Count;
                for (int j = 0; j < num2; j++)
                {
                    GEDCOMIndividualRecord child = family.Children[j].Value as GEDCOMIndividualRecord;
                    bool resChild = DoDescendantsFilter(child);
                    result |= resChild;
                }
            }

            person.ExtData = result;
            return result;
        }

        public IList<ISearchResult> FindAll(string searchPattern)
        {
            var result = new List<ISearchResult>();

            Regex regex = SysUtils.InitMaskRegex(searchPattern);

            int num = fPersons.Count;
            for (int i = 0; i < num; i++) {
                TreeChartPerson person = fPersons[i];
                GEDCOMIndividualRecord iRec = person.Rec;
                if (iRec == null) continue;

                string fullname = GKUtils.GetNameString(iRec, true, false);
                if (SysUtils.MatchesRegex(fullname, regex)) {
                    result.Add(new SearchResult(iRec));
                }
            }

            return result;
        }

        #endregion

        #region Navigation

        public TreeChartPerson FindPersonByCoords(int aX, int aY)
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

        #endregion

        #region Drawing routines

        public override void SetRenderer(ChartRenderer renderer)
        {
            base.SetRenderer(renderer);

            InitGraphics();
        }

        private void InitGraphics()
        {
            InitSigns();
            fLinePen = fRenderer.CreatePen(ChartRenderer.GetColor(ChartRenderer.Black), 1f);
            fDecorativeLinePen = fRenderer.CreatePen(ChartRenderer.GetColor(ChartRenderer.Silver), 1f);
            fSolidBlack = fRenderer.CreateSolidBrush(ChartRenderer.GetColor(ChartRenderer.Black));
        }

        private void DoneGraphics()
        {
            if (fLinePen != null) fLinePen.Dispose();
            if (fDecorativeLinePen != null) fDecorativeLinePen.Dispose();
            if (fSolidBlack != null) fSolidBlack.Dispose();
            DoneSigns();
        }

        public static ExtRect GetExpanderRect(ExtRect personRect)
        {
            ExtRect expRt = ExtRect.Create(personRect.Left, personRect.Top - 18, personRect.Left + 16 - 1, personRect.Top - 2);
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

        private void DrawBorder(IPen xpen, ExtRect rt, bool dead, TreeChartPerson person)
        {
            IColor bColor = person.GetFillColor(dead);
            if (fHighlightedPerson == person) {
                bColor = bColor.Lighter(HIGHLIGHTED_VAL);
            }

            if (person.Sex == GEDCOMSex.svFemale) {
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

                prt.Offset(fSPX, fSPY);

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
                        xpen = fRenderer.CreatePen(ChartRenderer.GetColor(ChartRenderer.Black), 1.0f);
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

                    // draw CI only for existing individuals
                    if (fCertaintyIndex && person.Rec != null) {
                        string cas = string.Format("{0:0.00}", person.CertaintyAssessment);
                        fRenderer.DrawString(cas, fDrawFont, fSolidBlack, brt.Left, brt.Bottom);
                    }
                }
            } catch (Exception ex) {
                Logger.LogWrite("TreeChartBox.DrawPerson(): " + ex.Message);
            }
        }

        private void DrawAncestors(TreeChartPerson person, ChartDrawMode drawMode)
        {
            Draw(person.Father, TreeChartKind.ckAncestors, drawMode);
            Draw(person.Mother, TreeChartKind.ckAncestors, drawMode);
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

        private void DrawDescendants(TreeChartPerson person, ChartDrawMode drawMode)
        {
            int spousesCount = person.GetSpousesCount();
            int childrenCount = person.GetChildsCount();

            for (int i = 0; i < childrenCount; i++) {
                Draw(person.GetChild(i), TreeChartKind.ckDescendants, drawMode);
            }

            int spbOfs = (person.Height - 10) / (spousesCount + 1);
            int spbBeg = person.PtY + (person.Height - spbOfs * (spousesCount - 1)) / 2;

            switch (person.Sex) {
                case GEDCOMSex.svMale:
                    for (int i = 0; i < spousesCount; i++) {
                        int spbV = spbBeg + spbOfs * i;
                        DrawLine(person.Rect.Right + 1, spbV, person.GetSpouse(i).Rect.Left, spbV);
                    }
                    break;

                case GEDCOMSex.svFemale:
                    for (int i = 0; i < spousesCount; i++) {
                        int spbV = spbBeg + spbOfs * i;
                        DrawLine(person.GetSpouse(i).Rect.Right + 1, spbV, person.Rect.Left, spbV);
                    }
                    break;
            }

            for (int i = 0; i < spousesCount; i++) {
                Draw(person.GetSpouse(i), TreeChartKind.ckDescendants, drawMode);
            }

            int crY = person.PtY + person.Height + fLevelDistance / 2;
            int cx = 0;
            if (person.BaseSpouse == null || (person.BaseSpouse.GetSpousesCount() > 1))
            {
                cx = person.PtX;
                spbBeg = person.PtY + person.Height - 1;
            }
            else
            {
                switch (person.Sex)
                {
                    case GEDCOMSex.svMale:
                        cx = (person.Rect.Right + person.BaseSpouse.Rect.Left) / 2;
                        break;

                    case GEDCOMSex.svFemale:
                        cx = (person.BaseSpouse.Rect.Right + person.Rect.Left) / 2;
                        break;
                }

                spbBeg -= spbOfs / 2;
            }

            if (childrenCount != 0)
            {
                DrawLine(cx, spbBeg, cx, crY);
                if (childrenCount == 1)
                {
                    TreeChartPerson child = person.GetChild(0);
                    DrawLine(child.PtX, crY, child.PtX, child.PtY);
                }
                else
                {
                    int bpx = person.GetChild(0).PtX;
                    int epx = person.GetChild(childrenCount - 1).PtX;
                    DrawLine(bpx, crY, epx, crY);

                    for (int i = 0; i < childrenCount; i++) {
                        TreeChartPerson child = person.GetChild(i);
                        DrawLine(child.PtX, crY, child.PtX, child.PtY);
                    }
                }
            }
        }

        public void Draw(TreeChartPerson person, TreeChartKind dirKind, ChartDrawMode drawMode)
        {
            if (person == null) return;

            switch (fKind)
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
            // FIXME: dummy
        }

        public void SetOffsets(int spx, int spy)
        {
            fSPX = spx;
            fSPY = spy;
        }

        public ExtPoint GetOffsets()
        {
            return new ExtPoint(fSPX, fSPY);
        }

        #endregion

        public static bool CheckTreeChartSize(GEDCOMTree tree, GEDCOMIndividualRecord iRec, TreeChartKind chartKind)
        {
            bool result = true;

            if (chartKind == TreeChartKind.ckAncestors || chartKind == TreeChartKind.ckBoth)
            {
                GKUtils.InitExtCounts(tree, -1);
                int ancCount = GKUtils.GetAncestorsCount(iRec);
                if (ancCount > 2048)
                {
                    AppHost.StdDialogs.ShowMessage(string.Format(LangMan.LS(LSID.LSID_AncestorsNumberIsInvalid), ancCount.ToString()));
                    return false;
                }
            }

            if (chartKind >= TreeChartKind.ckDescendants && chartKind <= TreeChartKind.ckBoth)
            {
                GKUtils.InitExtCounts(tree, -1);
                int descCount = GKUtils.GetDescendantsCount(iRec);
                if (descCount > 2048)
                {
                    AppHost.StdDialogs.ShowMessage(string.Format(LangMan.LS(LSID.LSID_DescendantsNumberIsInvalid), descCount.ToString()));
                    result = false;
                }
            }

            return result;
        }
    }
}
