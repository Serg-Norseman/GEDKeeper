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

using System;
using System.Collections.Generic;
using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.SmartGraph;
using GKCore.Types;

namespace GKCore.Kinships
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class KinshipsGraph : BaseObject
    {
        private readonly Graph fGraph;

        public string IndividualsPath;

        public KinshipsGraph()
        {
            this.fGraph = new Graph();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                this.fGraph.Dispose();
            }
            base.Dispose(disposing);
        }

        public bool IsEmpty()
        {
            return this.fGraph.IsEmpty();
        }

        public void Clear()
        {
            this.fGraph.Clear();
        }

        public IVertex AddIndividual(GEDCOMIndividualRecord iRec)
        {
            if (iRec == null) return null;

            return this.fGraph.AddVertex(iRec.XRef, iRec);
        }

        public IVertex FindVertex(string sign)
        {
            return this.fGraph.FindVertex(sign);
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="source"></param>
        /// <param name="target"></param>
        /// <param name="tsRel">target to source relation (if target is parent of source = Parent)</param>
        /// <param name="stRel">source to target relation (if target is parent of source = Child)</param>
        /// <returns></returns>
        public bool AddRelation(IVertex source, IVertex target, RelationKind tsRel, RelationKind stRel)
        {
            return this.fGraph.AddUndirectedEdge(source, target, 1, (int)tsRel, (int)stRel);
        }

        public void SetTreeRoot(GEDCOMIndividualRecord rootRec)
        {
            IVertex root = this.fGraph.FindVertex(rootRec.XRef);
            if (root == null) return;

            this.fGraph.FindPathTree(root);
        }

        public string GetRelationship(GEDCOMIndividualRecord targetRec)
        {
            IVertex target = this.fGraph.FindVertex(targetRec.XRef);
            if (target == null) return "???";

            try
            {
                GEDCOMIndividualRecord targetIndividual = target.Value as GEDCOMIndividualRecord;

                IEnumerable<IEdge> edgesPath = this.fGraph.GetPath(target);

                string tmp = "";
                RelationKind prevRel = RelationKind.rkNone;
                RelationKind finRel = RelationKind.rkNone;
                int great = 0;

                foreach (Edge edge in edgesPath)
                {
                    GEDCOMIndividualRecord xFrom = (GEDCOMIndividualRecord)edge.Source.Value;
                    GEDCOMIndividualRecord xTo = (GEDCOMIndividualRecord)edge.Target.Value;
                    RelationKind curRel = FixLink(xFrom, xTo, (RelationKind)((int)edge.Value));

                    if (tmp != "") tmp += ", ";
                    tmp += xFrom.XRef + ">" + GKData.RelationSigns[(int)curRel] + ">" + xTo.XRef;

                    if (prevRel != RelationKind.rkUndefined)
                    {
                        int g, lev;

                        finRel = KinshipsMan.FindKinship(prevRel, curRel, out g, out lev);
                        great += g;
                        prevRel = finRel;
                    }
                }

                IndividualsPath = targetIndividual.XRef + " [" + tmp + "]";

                string result = FixRelation(targetIndividual, finRel, great);
                return result;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("KinshipsGraph.GetRelationship(): " + ex.Message);
                return "";
            }
        }

        private static RelationKind FixLink(GEDCOMIndividualRecord xFrom, GEDCOMIndividualRecord xTo, RelationKind rel)
        {
            RelationKind resRel = rel;

            switch (rel)
            {
                case RelationKind.rkParent:
                    switch (xTo.Sex)
                    {
                        case GEDCOMSex.svMale:
                            resRel = RelationKind.rkFather;
                            break;
                        case GEDCOMSex.svFemale:
                            resRel = RelationKind.rkMother;
                            break;
                    }
                    break;

                case RelationKind.rkSpouse:
                    switch (xTo.Sex)
                    {
                        case GEDCOMSex.svMale:
                            resRel = RelationKind.rkHusband;
                            break;
                        case GEDCOMSex.svFemale:
                            resRel = RelationKind.rkWife;
                            break;
                    }
                    break;

                case RelationKind.rkChild:
                    switch (xTo.Sex)
                    {
                        case GEDCOMSex.svMale:
                            resRel = RelationKind.rkSon;
                            break;
                        case GEDCOMSex.svFemale:
                            resRel = RelationKind.rkDaughter;
                            break;
                    }
                    break;

                default:
                    resRel = rel;
                    break;
            }

            return resRel;
        }

        private static string FixRelation(GEDCOMIndividualRecord target, RelationKind rel, int great)
        {
            string tmp = "";
            if (great != 0)
            {
                if (rel == RelationKind.rkUncle || rel == RelationKind.rkAunt)
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
                else if (rel == RelationKind.rkNephew || rel == RelationKind.rkNiece)
                {
                    tmp = GKData.Numerals[great] + GKData.NumKinship[(int)target.Sex] + " ";
                    if (rel == RelationKind.rkNephew)
                    {
                        rel = RelationKind.rkBrother;
                    }
                    if (rel == RelationKind.rkNiece)
                    {
                        rel = RelationKind.rkSister;
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
    }
}
