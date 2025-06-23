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

//#define DEBUG_KINSHIPS

using System;
using System.Collections.Generic;
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
    public sealed class KinshipsGraph : BaseObject
    {
        private readonly IBaseContext fContext;
        private readonly Graph fGraph;

        public KinshipsGraph(IBaseContext context)
        {
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
        /// 
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
            var tsProps = new KinshipProps(KinshipsMan.FixLink(irTgt.Sex, tsRel), tsExt);

            var stExt = (ext != KinshipExt.Adoption) ? ext : ((stRel == KinshipType.ktParent) ? KinshipExt.Adoptive : KinshipExt.Adopted);
            var irSrc = (GDMIndividualRecord)source.Value;
            var stProps = new KinshipProps(KinshipsMan.FixLink(irSrc.Sex, stRel), stExt);

            return fGraph.AddUndirectedEdge(source, target, 1, tsProps, stProps);
        }

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

                var result = new StringBuilder();

                var edgesPath = fGraph.GetPath(target);

#if DEBUG_KINSHIPS
                result.AppendLine("PATH:");
                foreach (Edge edge in edgesPath) {
                    src = (GDMIndividualRecord)edge.Source.Value;
                    tgt = (GDMIndividualRecord)edge.Target.Value;
                    relProps = (RelationProps)edge.Value;
                    RelationKind curRel = KinshipsMan.FixLink(tgt.Sex, relProps.Kind);
                    result.AppendLine("  " + GetRelationPart(src, tgt, curRel, 0, 0, false, relProps.Ext));
                }
                result.AppendLine();
                result.AppendLine("SOLVE:");
#endif

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

                        if ((prevRel == (int)KinshipType.ktGrandfather || prevRel == (int)KinshipType.ktGrandmother) &&
                            (curRel == (int)KinshipType.ktSon || curRel == (int)KinshipType.ktDaughter) && (great > 0)) {
                            if (curRel == (int)KinshipType.ktSon) {
                                finRel = (int)KinshipType.ktGrandfather;
                            } else if (curRel == (int)KinshipType.ktDaughter) {
                                finRel = (int)KinshipType.ktGrandmother;
                            }
                            g = -1;
                            deg = +1;
                        } else {
                            finRel = KinshipsMan.FindKinship(prevprevRel, prevRel, curRel, out g, out deg);
                            finExt = FixExt(vtxStarting, edge.Target, finRel, curExt, deg);
                        }

                        great += g;
                        degree += deg;

#if DEBUG_KINSHIPS
                        result.AppendLine("  " + GetRelationPart(src, tgt, curRel, 0, 0, true, curExt) + " -> " + finRel.ToString() + " " + fContext.Culture.GetPossessiveName(starting));
                        if (finRel == RelationKind.rkBrotherInLaw_H || finRel == RelationKind.rkBrotherInLaw_W) {
                            // for breakpoint
                            SysUtils.DoNotInline(finRel);
                        }
#endif

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

                            finRel = KinshipsMan.FindKinship(prevprevRel, prevRel, curRel, out g, out deg);
                            finExt = FixExt(vtxStarting, edge.Target, finRel, curExt, deg);
                            great += g;
                            degree += deg;
                        }

                        prevprevRel = prevRel;
                        prevRel = finRel;
                    }
                }

#if DEBUG_KINSHIPS
                result.AppendLine();
                result.AppendLine("RESULT:");
#endif

                if (!fullFormat) {
                    string relRes = KinshipsMan.GetRelationName(targetRec, finRel, great, degree, shortForm, finExt);
                    return relRes;
                } else {
                    string relPart = GetRelationPart(starting, tgt, finRel, great, degree, shortForm, finExt);
                    relPartsStack.Push(relPart);

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
#if DEBUG_KINSHIPS
            rel = string.Format("{0} [g={1}, d={2}]", xrel.ToString(), great, degree);
#else
            rel = KinshipsMan.GetRelationName(ind2, xrel, great, degree, shortForm, ext);
#endif
            string name1 = fContext.Culture.GetPossessiveName(ind1);
            string name2 = GKUtils.GetNameString(ind2, true, false);

            rel = string.Format(LangMan.LS(LSID.RelationshipMask), name2, rel, name1);
            return rel;
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

        #region Search graph

        public static KinshipsGraph SearchGraph(IBaseContext context, GDMIndividualRecord iRec)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            KinshipsGraph graph = new KinshipsGraph(context);

            SearchKGInt(context, null, iRec, graph, KinshipType.ktUndefined, KinshipType.ktUndefined, KinshipExt.None);

            return graph;
        }

        private static void SearchKGInt(IBaseContext context, Vertex prevNode, GDMIndividualRecord iRec,
                                        KinshipsGraph graph, KinshipType relation, KinshipType inverseRelation,
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
