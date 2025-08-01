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

using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using BSLib;
using BSLib.DataViz.SmartGraph;
using GDModel;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Utilities;

namespace GKCore.Kinships
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class KinshipsGraph : BaseObject
    {
        private readonly IBaseContext fContext;
        private readonly Graph fGraph;

        public KinshipsGraph(IBaseContext context)
        {
            LoadCulture();

            fContext = context;
            fGraph = new Graph();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fGraph.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Filling the relationship graph

        public bool IsEmpty()
        {
            return fGraph.IsEmpty();
        }

        public void Clear()
        {
            fGraph.Clear();
        }

        public Vertex AddIndividual(GDMIndividualRecord iRec)
        {
            return (iRec == null) ? null : fGraph.AddVertex(iRec.XRef, iRec);
        }

        public Vertex FindIndividual(string xref)
        {
            return fGraph.FindVertex(xref);
        }

        /// <summary>
        /// Adds a bidirectional relationship (edge) between two persons (graph vertices).
        /// </summary>
        /// <param name="source"></param>
        /// <param name="target"></param>
        /// <param name="tsRel">target to source relation (if target is parent of source = Parent)</param>
        /// <param name="stRel">source to target relation (if target is parent of source = Child)</param>
        public void AddRelation(Vertex source, Vertex target, KinshipType tsRel, KinshipType stRel, KinshipExt ext)
        {
            if (source == null || target == null || source == target)
                return;

            var tsExt = (ext != KinshipExt.Adoption) ? ext : ((tsRel == KinshipType.ktParent) ? KinshipExt.Adoptive : KinshipExt.Adopted);
            var irTgt = (GDMIndividualRecord)target.Value;
            var tsProps = new KinshipProps(FixLink(irTgt.Sex, tsRel), tsExt);

            var stExt = (ext != KinshipExt.Adoption) ? ext : ((stRel == KinshipType.ktParent) ? KinshipExt.Adoptive : KinshipExt.Adopted);
            var irSrc = (GDMIndividualRecord)source.Value;
            var stProps = new KinshipProps(FixLink(irSrc.Sex, stRel), stExt);

            fGraph.AddUndirectedEdge(source, target, 1, tsProps, stProps);
        }

        #endregion

        #region Determination of the kinships

        /// <summary>
        /// Sets the person as the starting point for determining relationships in a tree diagram or calculator.
        /// </summary>
        public void SetTreeRoot(GDMIndividualRecord rootRec)
        {
            if (rootRec == null) return;
            Vertex root = fGraph.FindVertex(rootRec.XRef);
            if (root == null) return;

            fGraph.FindPathTree(root);
        }

        public string DetermineKinship(GDMIndividualRecord targetRec, bool fullFormat = false, bool shortForm = false)
        {
            if (targetRec == null) return "???";
            Vertex target = fGraph.FindVertex(targetRec.XRef);
            if (target == null) return "???";

            try {
                var edgesPath = fGraph.GetPath(target);

                int prevprevRel = (int)KinshipType.ktNone, prevRel = prevprevRel, finRel = prevprevRel, great = 0, degree = 0;
                KinshipExt finExt = KinshipExt.None;
                GDMIndividualRecord starting = null, ending = null;
                Vertex vtxStarting = null, vtxPrevTarget = null;
                var relPartsStack = new Stack<string>();

                foreach (Edge edge in edgesPath) {
                    var src = (GDMIndividualRecord)edge.Source.Value;
                    var tgt = (GDMIndividualRecord)edge.Target.Value;
                    var relProps = (KinshipProps)edge.Value;
                    int curRel = (int)relProps.Type;
                    KinshipExt curExt = relProps.Ext;

                    if (starting == null) {
                        starting = src;
                        vtxStarting = edge.Source;
                    }

                    if (prevRel != (int)KinshipType.ktUndefined) {
                        finRel = FindKinship(prevprevRel, prevRel, curRel, ref great, ref degree);
                        finExt = FixExt(vtxStarting, edge.Target, finRel, curExt, degree);

                        if (finRel == (int)KinshipType.ktUndefined && fullFormat) {
                            string relPart = GetRelationPart(starting, src, prevRel, great, degree, shortForm, finExt);
                            relPartsStack.Push(relPart);

                            src = ending;
                            starting = ending;
                            vtxStarting = vtxPrevTarget;
                            great = 0;
                            degree = 0;
                            prevprevRel = prevRel;
                            prevRel = (int)KinshipType.ktNone;

                            finRel = FindKinship(prevprevRel, prevRel, curRel, ref great, ref degree);
                            finExt = FixExt(vtxStarting, edge.Target, finRel, curExt, degree);
                        }

                        prevprevRel = prevRel;
                        prevRel = finRel;
                    }

                    ending = tgt;
                    vtxPrevTarget = edge.Target;
                }

                if (!fullFormat) {
                    string relRes = GetRelationName(targetRec, finRel, great, degree, shortForm, finExt);
                    return relRes;
                } else {
                    string relPart = GetRelationPart(starting, ending, finRel, great, degree, shortForm, finExt);
                    relPartsStack.Push(relPart);

                    var result = new StringBuilder();
                    while (relPartsStack.Count > 0) {
                        result.Append(relPartsStack.Pop());
                        if (relPartsStack.Count != 0) {
                            result.Append(", ");
                        }
                    }
                    return result.ToString();
                }
            } catch (Exception ex) {
                Logger.WriteError("KinshipsGraph.DetermineKinship()", ex);
                return "";
            }
        }

        private string GetRelationPart(GDMIndividualRecord ind1, GDMIndividualRecord ind2, int xrel, int great, int degree, bool shortForm, KinshipExt ext)
        {
            if (ind1 == null || ind2 == null)
                return "???";

            string rel = GetRelationName(ind2, xrel, great, degree, shortForm, ext);
            string name1 = fContext.Culture.GetPossessiveName(ind1);
            string name2 = GKUtils.GetNameString(ind2, true, false);

            rel = string.Format(LangMan.LS(LSID.RelationshipMask), name2, rel, name1);
            return rel;
        }

        private static KinshipType FixLink(GDMSex targetSex, KinshipType rel)
        {
            KinshipType resRel = rel;

            switch (rel) {
                case KinshipType.ktParent:
                    switch (targetSex) {
                        case GDMSex.svMale:
                            resRel = KinshipType.ktFather;
                            break;
                        case GDMSex.svFemale:
                            resRel = KinshipType.ktMother;
                            break;
                    }
                    break;

                case KinshipType.ktSpouse:
                    switch (targetSex) {
                        case GDMSex.svMale:
                            resRel = KinshipType.ktHusband;
                            break;
                        case GDMSex.svFemale:
                            resRel = KinshipType.ktWife;
                            break;
                    }
                    break;

                case KinshipType.ktChild:
                    switch (targetSex) {
                        case GDMSex.svMale:
                            resRel = KinshipType.ktSon;
                            break;
                        case GDMSex.svFemale:
                            resRel = KinshipType.ktDaughter;
                            break;
                    }
                    break;

                default:
                    resRel = rel;
                    break;
            }

            return resRel;
        }

        private static KinshipExt FixExt(Vertex source, Vertex target, int finRel, KinshipExt curExt, int degree)
        {
            if (!GlobalOptions.Instance.ExtendedKinships)
                return curExt;

            if (curExt == KinshipExt.None && (finRel == (int)KinshipType.ktBrother || finRel == (int)KinshipType.ktSister) && (degree == 0)) {
                var srcFather = FindEdgeTargetByRelation(source, KinshipType.ktFather);
                var srcMother = FindEdgeTargetByRelation(source, KinshipType.ktMother);
                var tgtFather = FindEdgeTargetByRelation(target, KinshipType.ktFather);
                var tgtMother = FindEdgeTargetByRelation(target, KinshipType.ktMother);

                bool sameFather = (srcFather == tgtFather);
                bool sameMother = (srcMother == tgtMother);

                if (sameFather && sameMother) {
                    curExt = KinshipExt.None;
                } else if (sameFather) {
                    curExt = KinshipExt.Blood;
                } else if (sameMother) {
                    curExt = KinshipExt.Uterine;
                }
            }
            return curExt;
        }

        private static Vertex FindEdgeTargetByRelation(Vertex vertex, KinshipType relKind)
        {
            foreach (Edge edge in vertex.EdgesOut) {
                var relProps = (KinshipProps)edge.Value;
                if (relProps.Type == relKind) {
                    return edge.Target;
                }
            }
            return null;
        }

        /// <summary>
        /// Relationship type identifiers are used as integers to allow extensibility of types via the configuration file.
        /// </summary>
        public static int FindKinship(int prevprev, int prev, int cur, ref int great, ref int degree)
        {
            int finRel = (int)KinshipType.ktUndefined;

            var kinshipDefs = fKinshipsCulture.Definitions;
            for (int i = 0, num = kinshipDefs.Length; i < num; i++) {
                var kinDef = kinshipDefs[i];
                if (!kinDef.Enable) continue;

                bool baseCond = kinDef.PrevRels.Contains(prev) && kinDef.CurrRels.Contains(cur) && (kinDef.PrePrevRels.Count == 0 || kinDef.PrePrevRels.Contains(prevprev));
                if (baseCond) {
                    bool specCond = CheckSpecConditions(kinDef.SpecConditions, great, degree);
                    if (specCond) {
                        great += kinDef.Great;
                        degree += kinDef.Degree;
                        finRel = (kinDef.FinRel == (int)KinshipType.ktSame) ? cur : kinDef.FinRel;
                        break;
                    }
                }
            }

            return finRel;
        }

        public static string GetExt(int n, GDMSex sex)
        {
            string result = (n == 0 || n == 3) ? string.Empty : (LangMan.LSS(KinExts[n], (int)sex - 1) + " ");
            return result;
        }

        public static string GetDegree(int n, GDMSex sex)
        {
            string result = (n == 0) ? string.Empty : (LangMan.LSS(KinDegrees[n], (int)sex - 1) + " ");
            return result;
        }

        public static string GetGreat(int n, bool shortForm)
        {
            string result = "";
            if (n > 0) {
                if (!shortForm) {
                    for (int i = 1; i <= n; i++) {
                        result += fKinshipsCulture.GreatPrefix;
                    }
                } else {
                    result = fKinshipsCulture.GreatPrefix;
                    if (n > 1) {
                        result += string.Format("({0})", n);
                    }
                }
            }
            return result;
        }

        public static string GetRelationName(GDMIndividualRecord target, int rel, int great, int degree, bool shortForm, KinshipExt ext)
        {
            string tmp = string.Empty;

            if (rel != (int)KinshipType.ktUndefined) {
                Substitute(ref rel, ref great, ref degree);

                tmp = GetDegree(degree, target.Sex);

                if (fKinshipsCulture.GreatUsed) {
                    tmp += GetGreat(great, shortForm);
                }
            }

            var kinType = fKinshipsCulture.GetTypeEntry(rel);
            if (GlobalOptions.Instance.ExtendedKinships && kinType.HasExt) {
                tmp = GetExt((int)ext, target.Sex) + tmp;
            }

            return tmp + kinType.Name;
        }

        private static void Substitute(ref int rel, ref int great, ref int degree)
        {
            var kinshipSubsts = fKinshipsCulture.Substitutions;
            for (int i = 0, num = kinshipSubsts.Length; i < num; i++) {
                var kinSubst = kinshipSubsts[i];
                if (!kinSubst.Enable) continue;

                bool cond = kinSubst.CurrRels.Contains(rel) && CheckSpecConditions(kinSubst.SpecConditions, great, degree);
                if (cond) {
                    great += kinSubst.Great;
                    degree += kinSubst.Degree;
                    rel = (kinSubst.FinRel == (int)KinshipType.ktSame) ? rel : kinSubst.FinRel;
                    break;
                }
            }
        }

        private static bool CheckSpecConditions(string specConditions, int great, int degree)
        {
            if (string.IsNullOrEmpty(specConditions))
                return true;

            char param = specConditions[0];
            char cond = specConditions[1];
            int val = int.Parse(specConditions.Substring(2));

            bool result = true;
            switch (param) {
                case 'G':
                    result = (cond == '<' && great < val) || (cond == '=' && great == val) || (cond == '>' && great > val);
                    break;

                case 'D':
                    result = (cond == '<' && degree < val) || (cond == '=' && degree == val) || (cond == '>' && degree > val);
                    break;
            }
            return result;
        }

        #endregion

        #region Kinships culture configuration

        public static readonly LSID[] KinDegrees;
        public static readonly LSID[] KinExts;

        private static KinshipsCulture fKinshipsCulture;

        static KinshipsGraph()
        {
            fKinshipsCulture = new KinshipsCulture();

            KinDegrees = new LSID[] {
                LSID.KinshipDegree_01,
                LSID.KinshipDegree_02,
                LSID.KinshipDegree_03,
                LSID.KinshipDegree_04,
                LSID.KinshipDegree_05,
                LSID.KinshipDegree_06,
                LSID.KinshipDegree_07,
                LSID.KinshipDegree_08,
                LSID.KinshipDegree_09,
                LSID.KinshipDegree_10,
            };

            KinExts = new LSID[] {
                LSID.None,
                LSID.RE_Blood,
                LSID.RE_Uterine,
                LSID.None,
                LSID.RE_Adoptive,
                LSID.RE_Adopted,
                LSID.RE_CommonLaw,
            };
        }

        private static void LoadCulture()
        {
            string fileName = AppHost.GetKinshipsCultureFileName();

            if (!File.Exists(fileName)) return;

            try {
                // loading file
                using (var reader = new StreamReader(fileName)) {
                    string content = reader.ReadToEnd();
                    fKinshipsCulture = YamlHelper.Deserialize<KinshipsCulture>(content);
                }

                // processing
                fKinshipsCulture.Prepare();
            } catch (Exception ex) {
                Logger.WriteError("KinshipsGraph.Load()", ex);
            }
        }

        public static void InitDefaults()
        {
            fKinshipsCulture.InitDefaults();
        }

        #endregion

        #region Search graph

        public static KinshipsGraph SearchGraph(IBaseContext context, GDMIndividualRecord iRec)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            KinshipsGraph graph = new KinshipsGraph(context);
            graph.SearchKGInt(null, iRec, KinshipType.ktUndefined, KinshipType.ktUndefined, KinshipExt.None);
            return graph;
        }

        private void SearchKGInt(Vertex prevNode, GDMIndividualRecord iRec, KinshipType relation, KinshipType inverseRelation, KinshipExt ext)
        {
            if (iRec == null) return;

            GDMTree tree = fContext.Tree;

            Vertex currNode = FindIndividual(iRec.XRef);
            if (currNode != null) {
                if (prevNode != null) {
                    AddRelation(prevNode, currNode, relation, inverseRelation, ext);
                }

                return;
            } else {
                currNode = AddIndividual(iRec);

                if (prevNode != null) {
                    AddRelation(prevNode, currNode, relation, inverseRelation, ext);
                }
            }

            if (iRec.ChildToFamilyLinks.Count > 0) {
                var childLink = iRec.ChildToFamilyLinks[0];
                bool adopted = (childLink.PedigreeLinkageType == GDMPedigreeLinkageType.plAdopted);

                GDMFamilyRecord fam = tree.GetPtrValue(childLink);
                if (fam != null) {
                    GDMIndividualRecord father, mother;
                    tree.GetSpouses(fam, out father, out mother);

                    var kinExt = !adopted ? KinshipExt.None : KinshipExt.Adoption;
                    SearchKGInt(currNode, father, KinshipType.ktParent, KinshipType.ktChild, kinExt);
                    SearchKGInt(currNode, mother, KinshipType.ktParent, KinshipType.ktChild, kinExt);
                }
            }

            for (int i = 0, num = iRec.SpouseToFamilyLinks.Count; i < num; i++) {
                GDMFamilyRecord family = tree.GetPtrValue(iRec.SpouseToFamilyLinks[i]);
                GDMIndividualRecord spouse = (iRec.Sex == GDMSex.svMale) ? tree.GetPtrValue(family.Wife) : tree.GetPtrValue(family.Husband);
                bool commonLaw = (family.Status == GDMMarriageStatus.MarrNotRegistered);

                var kinExt = !commonLaw ? KinshipExt.None : KinshipExt.CommonLaw;
                SearchKGInt(currNode, spouse, KinshipType.ktSpouse, KinshipType.ktSpouse, kinExt);

                for (int j = 0, num2 = family.Children.Count; j < num2; j++) {
                    GDMIndividualRecord child = tree.GetPtrValue(family.Children[j]);
                    GDMChildToFamilyLink childLink = child.FindChildToFamilyLink(family);
                    var adopted = (childLink != null && (childLink.PedigreeLinkageType == GDMPedigreeLinkageType.plAdopted));

                    kinExt = !adopted ? KinshipExt.None : KinshipExt.Adoption;
                    SearchKGInt(currNode, child, KinshipType.ktChild, KinshipType.ktParent, kinExt);
                }
            }
        }

        #endregion
    }
}
