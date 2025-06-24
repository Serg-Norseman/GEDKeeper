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

namespace GKCore.Kinships
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class KinshipSolver : BaseObject
    {
        private readonly IBaseContext fContext;
        private readonly Graph fGraph;

        public KinshipSolver(IBaseContext context)
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

        public Vertex FindVertex(string sign)
        {
            return fGraph.FindVertex(sign);
        }

        /// <summary>
        /// Adds a bidirectional relationship (edge) between two persons (graph vertices).
        /// </summary>
        /// <param name="source"></param>
        /// <param name="target"></param>
        /// <param name="tsRel">target to source relation (if target is parent of source = Parent)</param>
        /// <param name="stRel">source to target relation (if target is parent of source = Child)</param>
        /// <returns></returns>
        public bool AddRelation(Vertex source, Vertex target, KinshipType tsRel, KinshipType stRel, KinshipExt ext)
        {
            if (source == null || target == null || source == target) {
                return false;
            }

            var tsExt = (ext != KinshipExt.Adoption) ? ext : ((tsRel == KinshipType.ktParent) ? KinshipExt.Adoptive : KinshipExt.Adopted);
            var irTgt = (GDMIndividualRecord)target.Value;
            var tsProps = new KinshipProps(FixLink(irTgt.Sex, tsRel), tsExt);

            var stExt = (ext != KinshipExt.Adoption) ? ext : ((stRel == KinshipType.ktParent) ? KinshipExt.Adoptive : KinshipExt.Adopted);
            var irSrc = (GDMIndividualRecord)source.Value;
            var stProps = new KinshipProps(FixLink(irSrc.Sex, stRel), stExt);

            return fGraph.AddUndirectedEdge(source, target, 1, tsProps, stProps);
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

        public string GetRelationship(GDMIndividualRecord targetRec, bool fullFormat = false, bool shortForm = false)
        {
            if (targetRec == null) return "???";
            Vertex target = fGraph.FindVertex(targetRec.XRef);
            if (target == null) return "???";

            try {
                GDMIndividualRecord src = null, tgt = null;
                KinshipProps relProps = null;

                var edgesPath = fGraph.GetPath(target);

                int prevprevRel = (int)KinshipType.ktNone;
                int prevRel = (int)KinshipType.ktNone;
                int finRel = (int)KinshipType.ktNone;
                int great = 0;
                int degree = 0;
                KinshipExt finExt = KinshipExt.None;
                GDMIndividualRecord prev_tgt = null;
                GDMIndividualRecord starting = null;
                Vertex vtxStarting = null, vtxTarget = null, vtxPrevTarget = null;
                var relPartsStack = new Stack<string>();

                foreach (Edge edge in edgesPath) {
                    prev_tgt = tgt;
                    vtxPrevTarget = vtxTarget;

                    src = (GDMIndividualRecord)edge.Source.Value;
                    tgt = (GDMIndividualRecord)edge.Target.Value;
                    relProps = (KinshipProps)edge.Value;

                    vtxTarget = edge.Target;

                    if (starting == null) {
                        starting = src;
                        vtxStarting = edge.Source;
                    }

                    int curRel = (int)relProps.Type;
                    KinshipExt curExt = relProps.Ext;

                    if (prevRel != (int)KinshipType.ktUndefined) {
                        int g, deg;

                        finRel = FindKinship(prevprevRel, prevRel, curRel, great, out g, out deg);
                        finExt = FixExt(vtxStarting, edge.Target, finRel, curExt, deg);

                        great += g;
                        degree += deg;

                        if (finRel == (int)KinshipType.ktUndefined && fullFormat) {
                            string relPart = GetRelationPart(starting, src, prevRel, great, degree, shortForm, finExt);
                            relPartsStack.Push(relPart);

                            src = prev_tgt;
                            starting = prev_tgt;
                            vtxStarting = vtxPrevTarget;
                            great = 0;
                            degree = 0;

                            prevprevRel = prevRel;
                            prevRel = (int)KinshipType.ktNone;

                            finRel = FindKinship(prevprevRel, prevRel, curRel, great, out g, out deg);
                            finExt = FixExt(vtxStarting, edge.Target, finRel, curExt, deg);
                            great += g;
                            degree += deg;
                        }

                        prevprevRel = prevRel;
                        prevRel = finRel;
                    }
                }

                if (!fullFormat) {
                    string relRes = GetRelationName(targetRec, finRel, great, degree, shortForm, finExt);
                    return relRes;
                } else {
                    string relPart = GetRelationPart(starting, tgt, finRel, great, degree, shortForm, finExt);
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
                Logger.WriteError("KinshipsGraph.GetRelationship()", ex);
                return "";
            }
        }

        private string GetRelationPart(GDMIndividualRecord ind1, GDMIndividualRecord ind2, int xrel, int great, int degree, bool shortForm, KinshipExt ext)
        {
            if (ind1 == null || ind2 == null)
                return "???";

            string rel;
            rel = GetRelationName(ind2, xrel, great, degree, shortForm, ext);
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

        public static int FindKinship(int prevprev, int prev, int cur, int great, out int dGreat, out int dDegree)
        {
            int finRel = (int)KinshipType.ktUndefined;
            dGreat = 0;
            dDegree = 0;

            var kinshipDefs = fKinshipsCulture.Definitions;
            for (int i = 0, num = kinshipDefs.Length; i < num; i++) {
                var kinDef = kinshipDefs[i];
                if (!kinDef.Enable) continue;

                bool baseCond = kinDef.PrevRels.Contains(prev) && kinDef.CurrRels.Contains(cur) && (kinDef.PrevPrevRels.Count == 0 || kinDef.PrevPrevRels.Contains(prevprev));
                if (baseCond) {
                    bool specCond = string.IsNullOrEmpty(kinDef.SpecConditions) || (kinDef.SpecConditions == "G>0" && great > 0);
                    if (specCond) {
                        dGreat = kinDef.Great;
                        dDegree = kinDef.Degree;
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
                if (degree == 1) {
                    if (rel == (int)KinshipType.ktSister) {
                        rel = (int)KinshipType.ktCousinF;
                        degree = 0;
                    }
                    if (rel == (int)KinshipType.ktBrother) {
                        rel = (int)KinshipType.ktCousinM;
                        degree = 0;
                    }
                }

                tmp = GetDegree(degree, target.Sex);
                tmp += GetGreat(great, shortForm);
            }

            var kinTypeEntry = fKinshipsCulture.GetTypeEntry((int)rel);

            if (GlobalOptions.Instance.ExtendedKinships && kinTypeEntry.HasExt) {
                tmp = GetExt((int)ext, target.Sex) + tmp;
            }

            return tmp + kinTypeEntry.Name;
        }

        #endregion

        #region Kinships culture configuration

        public static readonly LSID[] KinDegrees;
        public static readonly LSID[] KinExts;

        private static KinshipsCulture fKinshipsCulture;

        static KinshipSolver()
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

        #endregion

        #region Search graph

        public static KinshipSolver SearchGraph(IBaseContext context, GDMIndividualRecord iRec)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            KinshipSolver graph = new KinshipSolver(context);

            SearchKGInt(context, null, iRec, graph, KinshipType.ktUndefined, KinshipType.ktUndefined, KinshipExt.None);

            return graph;
        }

        private static void SearchKGInt(IBaseContext context, Vertex prevNode, GDMIndividualRecord iRec,
                                        KinshipSolver graph, KinshipType relation, KinshipType inverseRelation,
                                        KinshipExt ext)
        {
            if (iRec == null) return;

            Vertex currNode = graph.FindVertex(iRec.XRef);
            if (currNode != null) {
                if (prevNode != null) {
                    graph.AddRelation(prevNode, currNode, relation, inverseRelation, ext);
                }

                return;
            } else {
                currNode = graph.AddIndividual(iRec);

                if (prevNode != null) {
                    graph.AddRelation(prevNode, currNode, relation, inverseRelation, ext);
                }
            }

            if (iRec.ChildToFamilyLinks.Count > 0) {
                var childLink = iRec.ChildToFamilyLinks[0];
                bool adopted = (childLink.PedigreeLinkageType == GDMPedigreeLinkageType.plAdopted);

                GDMFamilyRecord fam = context.Tree.GetPtrValue(childLink);
                if (fam != null) {
                    GDMIndividualRecord father, mother;
                    context.Tree.GetSpouses(fam, out father, out mother);

                    SearchKGInt(context, currNode, father, graph, KinshipType.ktParent, KinshipType.ktChild, (!adopted ? KinshipExt.None : KinshipExt.Adoption));
                    SearchKGInt(context, currNode, mother, graph, KinshipType.ktParent, KinshipType.ktChild, (!adopted ? KinshipExt.None : KinshipExt.Adoption));
                }
            }

            int num = iRec.SpouseToFamilyLinks.Count;
            for (int i = 0; i < num; i++) {
                GDMFamilyRecord family = context.Tree.GetPtrValue(iRec.SpouseToFamilyLinks[i]);
                GDMIndividualRecord spouse = (iRec.Sex == GDMSex.svMale) ? context.Tree.GetPtrValue(family.Wife) : context.Tree.GetPtrValue(family.Husband);
                bool commonLaw = (family.Status == GDMMarriageStatus.MarrNotRegistered);

                SearchKGInt(context, currNode, spouse, graph, KinshipType.ktSpouse, KinshipType.ktSpouse, (!commonLaw ? KinshipExt.None : KinshipExt.CommonLaw));

                int num2 = family.Children.Count;
                for (int j = 0; j < num2; j++) {
                    GDMIndividualRecord child = context.Tree.GetPtrValue(family.Children[j]);
                    GDMChildToFamilyLink childLink = child.FindChildToFamilyLink(family);
                    var adopted = (childLink != null && (childLink.PedigreeLinkageType == GDMPedigreeLinkageType.plAdopted));

                    SearchKGInt(context, currNode, child, graph, KinshipType.ktChild, KinshipType.ktParent, (!adopted ? KinshipExt.None : KinshipExt.Adoption));
                }
            }
        }

        #endregion
    }
}
