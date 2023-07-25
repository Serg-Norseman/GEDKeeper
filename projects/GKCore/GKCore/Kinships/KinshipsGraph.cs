/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GKCore.Types;

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
        public bool AddRelation(Vertex source, Vertex target, RelationKind tsRel, RelationKind stRel)
        {
            return fGraph.AddUndirectedEdge(source, target, 1, (int)tsRel, (int)stRel);
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
                RelationKind prevprevRel = RelationKind.rkNone;
                RelationKind prevRel = RelationKind.rkNone;
                RelationKind finRel = RelationKind.rkNone;
                int great = 0;
                int degree = 0;
                GDMIndividualRecord src = null, tgt = null, prev_tgt = null;
                string relPart = "";

                GDMIndividualRecord starting = null;

                var relPartsStack = new Stack<string>();
                var result = new StringBuilder();

                var edgesPath = fGraph.GetPath(target);

#if DEBUG_KINSHIPS
                result.AppendLine("PATH:");
                foreach (Edge edge in edgesPath) {
                    src = (GDMIndividualRecord)edge.Source.Value;
                    tgt = (GDMIndividualRecord)edge.Target.Value;
                    RelationKind curRel = KinshipsMan.FixLink(tgt.Sex, (RelationKind)((int)edge.Value));
                    result.AppendLine("  " + GetRelationPart(src, tgt, curRel, 0, 0, false));
                }
                result.AppendLine();
                result.AppendLine("SOLVE:");
#endif

                foreach (Edge edge in edgesPath) {
                    prev_tgt = tgt;
                    src = (GDMIndividualRecord)edge.Source.Value;
                    tgt = (GDMIndividualRecord)edge.Target.Value;

                    if (starting == null) { starting = src; }

                    RelationKind curRel = KinshipsMan.FixLink(tgt.Sex, (RelationKind)((int)edge.Value));

                    if (prevRel != RelationKind.rkUndefined) {
                        int g, deg;

                        if ((prevRel == RelationKind.rkGrandfather || prevRel == RelationKind.rkGrandmother) &&
                            (curRel == RelationKind.rkSon || curRel == RelationKind.rkDaughter) && (great > 0)) {
                            if (curRel == RelationKind.rkSon) {
                                finRel = RelationKind.rkGrandfather;
                            } else if (curRel == RelationKind.rkDaughter) {
                                finRel = RelationKind.rkGrandmother;
                            }
                            g = -1;
                            deg = +1;
                        } else {
                            finRel = KinshipsMan.FindKinship(prevprevRel, prevRel, curRel, out g, out deg);
                        }

                        great += g;
                        degree += deg;

#if DEBUG_KINSHIPS
                        result.AppendLine("  " + GetRelationPart(src, tgt, curRel, 0, 0, true) + " -> " + finRel.ToString() + " " + fContext.Culture.GetPossessiveName(starting));
                        if (finRel == RelationKind.rkBrotherInLaw_H || finRel == RelationKind.rkBrotherInLaw_W) {
                            // for breakpoint
                            SysUtils.DoNotInline(finRel);
                        }
#endif

                        if (finRel == RelationKind.rkUndefined && fullFormat) {
                            relPart = GetRelationPart(starting, src, prevRel, great, degree, shortForm);
                            src = prev_tgt;

                            starting = prev_tgt;
                            great = 0;
                            degree = 0;

                            prevprevRel = prevRel;
                            prevRel = RelationKind.rkNone;

                            relPartsStack.Push(relPart);

                            finRel = KinshipsMan.FindKinship(prevprevRel, prevRel, curRel, out g, out deg);
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
                    string relRes = GetRelationName(targetRec, finRel, great, degree, shortForm);
                    return relRes;
                } else {
                    relPart = GetRelationPart(starting, tgt, finRel, great, degree, shortForm);

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

        private string GetRelationPart(GDMIndividualRecord ind1, GDMIndividualRecord ind2, RelationKind xrel, int great, int degree, bool shortForm)
        {
            if (ind1 == null || ind2 == null)
                return "???";

            string rel;
#if DEBUG_KINSHIPS
            rel = string.Format("{0} [g={1}, d={2}]", xrel.ToString(), great, degree);
#else
            rel = GetRelationName(ind2, xrel, great, degree, shortForm);
#endif
            string name1 = fContext.Culture.GetPossessiveName(ind1);
            string name2 = GKUtils.GetNameString(ind2, true, false);

            rel = string.Format(LangMan.LS(LSID.LSID_RelationshipMask), name2, rel, name1);
            return rel;
        }

        private static string GetRelationName(GDMIndividualRecord target, RelationKind rel, int great, int degree, bool shortForm)
        {
            string tmp = string.Empty;

            if (rel != RelationKind.rkUndefined) {
                if (degree == 1) {
                    if (rel == RelationKind.rkSister) {
                        rel = RelationKind.rkCousinF;
                        degree = 0;
                    }
                    if (rel == RelationKind.rkBrother) {
                        rel = RelationKind.rkCousinM;
                        degree = 0;
                    }
                }

                tmp = KinshipsMan.GetDegree(degree, target.Sex);
                tmp += KinshipsMan.GetGreat(great, shortForm);
            }

            return tmp + LangMan.LS(GKData.RelationKinds[(int)rel]);
        }

        #region Search graph

        public static KinshipsGraph SearchGraph(IBaseContext context, GDMIndividualRecord iRec)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            KinshipsGraph graph = new KinshipsGraph(context);

            SearchKGInt(context, null, iRec, graph, RelationKind.rkUndefined, RelationKind.rkUndefined);

            return graph;
        }

        private static void SearchKGInt(IBaseContext context, Vertex prevNode, GDMIndividualRecord iRec,
                                        KinshipsGraph graph, RelationKind relation, RelationKind inverseRelation)
        {
            if (iRec == null) return;

            Vertex currNode = graph.FindVertex(iRec.XRef);
            if (currNode != null) {
                if (prevNode != null) {
                    graph.AddRelation(prevNode, currNode, relation, inverseRelation);
                }

                return;
            } else {
                currNode = graph.AddIndividual(iRec);

                if (prevNode != null) {
                    graph.AddRelation(prevNode, currNode, relation, inverseRelation);
                }
            }

            if (iRec.ChildToFamilyLinks.Count > 0) {
                var childLink = iRec.ChildToFamilyLinks[0];
                bool adopted = (childLink.PedigreeLinkageType == GDMPedigreeLinkageType.plAdopted);

                GDMFamilyRecord fam = context.Tree.GetPtrValue(childLink);
                if (fam != null) {
                    GDMIndividualRecord father, mother;
                    context.Tree.GetSpouses(fam, out father, out mother);

                    SearchKGInt(context, currNode, father, graph, RelationKind.rkParent, RelationKind.rkChild);
                    SearchKGInt(context, currNode, mother, graph, RelationKind.rkParent, RelationKind.rkChild);
                }
            }

            int num = iRec.SpouseToFamilyLinks.Count;
            for (int i = 0; i < num; i++) {
                GDMFamilyRecord family = context.Tree.GetPtrValue(iRec.SpouseToFamilyLinks[i]);
                GDMIndividualRecord spouse = (iRec.Sex == GDMSex.svMale) ? context.Tree.GetPtrValue(family.Wife) : context.Tree.GetPtrValue(family.Husband);
                bool commonLaw = (family.Status == GDMMarriageStatus.MarrNotRegistered);

                SearchKGInt(context, currNode, spouse, graph, RelationKind.rkSpouse, RelationKind.rkSpouse);

                int num2 = family.Children.Count;
                for (int j = 0; j < num2; j++) {
                    GDMIndividualRecord child = context.Tree.GetPtrValue(family.Children[j]);
                    SearchKGInt(context, currNode, child, graph, RelationKind.rkChild, RelationKind.rkParent);
                }
            }
        }

        #endregion
    }
}
