/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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

using System.Collections.Generic;
using BSLib;
using GKCore.Types;

namespace GKCore.Kinships
{
    using RelationSet = EnumSet<RelationKind>;

    /// <summary>
    /// 
    /// </summary>
    public static class KinshipsMan
    {
        private static List<KinshipRec> fKinships;

        static KinshipsMan()
        {
            fKinships = new List<KinshipRec>();

            RegisterKinship(
                RelationSet.Create(RelationKind.rkNone),
                RelationSet.Create(RelationKind.rkFather, RelationKind.rkMother, RelationKind.rkHusband, RelationKind.rkWife, RelationKind.rkSon, RelationKind.rkDaughter),
                RelationKind.rkSame, 0, 0);

            /*
             *  __________         _____________
             * |          |       |             |
             * | wife (0) | ----> | husband (1) |
             * |__________|       |_____________|
             *                       |
             *                       |
             *               ________v_________
             *              |                  |
             *              | son/daughter (2) |
             *              |__________________|
             * 
             * 1) prevRelation: none, curRelation: husband
             * 2) prevRelation: husband, curRelation: son -> finRelation from (2) to (0) = same(son)
             */
            RegisterKinship(
                RelationSet.Create(RelationKind.rkHusband, RelationKind.rkWife),
                RelationSet.Create(RelationKind.rkSon, RelationKind.rkDaughter),
                RelationKind.rkSame, 0, 1);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkMother),
                RelationSet.Create(RelationKind.rkHusband),
                RelationKind.rkFather, 0, 0);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkFather),
                RelationSet.Create(RelationKind.rkWife),
                RelationKind.rkMother, 0, 0);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkGrandfather, RelationKind.rkGrandmother),
                RelationSet.Create(RelationKind.rkSon),
                RelationKind.rkUncle, 0, 1);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkGrandfather, RelationKind.rkGrandmother),
                RelationSet.Create(RelationKind.rkDaughter),
                RelationKind.rkAunt, 0, 1);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkBrother, RelationKind.rkSister),
                RelationSet.Create(RelationKind.rkSon),
                RelationKind.rkNephew, 0, 1);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkBrother, RelationKind.rkSister),
                RelationSet.Create(RelationKind.rkDaughter),
                RelationKind.rkNiece, 0, 1);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkSon, RelationKind.rkBrother),
                RelationSet.Create(RelationKind.rkWife),
                RelationKind.rkDaughterInLaw, 0, 0);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkDaughter, RelationKind.rkSister),
                RelationSet.Create(RelationKind.rkHusband),
                RelationKind.rkSonInLaw, 0, 0);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkWife),
                RelationSet.Create(RelationKind.rkFather),
                RelationKind.rkWifeFather, 0, -1);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkWife),
                RelationSet.Create(RelationKind.rkMother),
                RelationKind.rkWifeMother, 0, -1);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkHusband),
                RelationSet.Create(RelationKind.rkFather),
                RelationKind.rkHusbandFather, 0, -1);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkHusband),
                RelationSet.Create(RelationKind.rkMother),
                RelationKind.rkHusbandMother, 0, -1);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkFather, RelationKind.rkMother),
                RelationSet.Create(RelationKind.rkFather),
                RelationKind.rkGrandfather, 0, -1);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkFather, RelationKind.rkMother),
                RelationSet.Create(RelationKind.rkMother),
                RelationKind.rkGrandmother, 0, -1);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkFather, RelationKind.rkMother),
                RelationSet.Create(RelationKind.rkSon),
                RelationKind.rkBrother, 0, 1);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkFather, RelationKind.rkMother),
                RelationSet.Create(RelationKind.rkDaughter),
                RelationKind.rkSister, 0, 1);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkGrandfather, RelationKind.rkGrandmother),
                RelationSet.Create(RelationKind.rkFather),
                RelationKind.rkGrandfather, 1, -1);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkGrandfather, RelationKind.rkGrandmother),
                RelationSet.Create(RelationKind.rkMother),
                RelationKind.rkGrandmother, 1, -1);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkSon, RelationKind.rkDaughter, RelationKind.rkSonInLaw, RelationKind.rkDaughterInLaw),
                RelationSet.Create(RelationKind.rkSon), RelationKind.rkGrandson, 0, 1);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkSon, RelationKind.rkDaughter, RelationKind.rkSonInLaw, RelationKind.rkDaughterInLaw),
                RelationSet.Create(RelationKind.rkDaughter),
                RelationKind.rkGranddaughter, 0, 1);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkGrandson, RelationKind.rkGranddaughter),
                RelationSet.Create(RelationKind.rkSon),
                RelationKind.rkGrandson, 1, 1);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkGrandson, RelationKind.rkGranddaughter),
                RelationSet.Create(RelationKind.rkDaughter),
                RelationKind.rkGranddaughter, 1, 1);


            RegisterKinship(
                RelationSet.Create(RelationKind.rkUncle, RelationKind.rkAunt),
                RelationSet.Create(RelationKind.rkDaughter),
                RelationKind.rkNiece, 1, 1);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkUncle, RelationKind.rkAunt),
                RelationSet.Create(RelationKind.rkSon),
                RelationKind.rkNephew, 1, 1);


            RegisterKinship(
                RelationSet.Create(RelationKind.rkHusband),
                RelationSet.Create(RelationKind.rkBrother),
                RelationKind.rkBrotherInLaw_H, 0, 0);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkWife),
                RelationSet.Create(RelationKind.rkBrother),
                RelationKind.rkBrotherInLaw_W, 0, 0);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkHusbandFather, RelationKind.rkHusbandMother),
                RelationSet.Create(RelationKind.rkSon),
                RelationKind.rkBrotherInLaw_H, 0, 1);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkWifeFather, RelationKind.rkWifeMother),
                RelationSet.Create(RelationKind.rkSon),
                RelationKind.rkBrotherInLaw_W, 0, 1);


            RegisterKinship(
                RelationSet.Create(RelationKind.rkHusband),
                RelationSet.Create(RelationKind.rkSister),
                RelationKind.rkSisterInLaw_H, 0, 0);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkWife),
                RelationSet.Create(RelationKind.rkSister),
                RelationKind.rkSisterInLaw_W, 0, 0);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkHusbandFather, RelationKind.rkHusbandMother),
                RelationSet.Create(RelationKind.rkDaughter),
                RelationKind.rkSisterInLaw_H, 0, 1);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkWifeFather, RelationKind.rkWifeMother),
                RelationSet.Create(RelationKind.rkDaughter),
                RelationKind.rkSisterInLaw_W, 0, 1);
        }

        private static void RegisterKinship(RelationSet prevRels, RelationSet currRels, RelationKind finRel, sbyte great, sbyte level)
        {
            fKinships.Add(new KinshipRec(prevRels, currRels, finRel, great, level));
        }

        public static RelationKind FindKinship(RelationKind prev, RelationKind cur, out int great, out int level)
        {
            RelationKind finRel = RelationKind.rkUndefined;
            great = 0;
            level = 0;

            int num = fKinships.Count;
            for (int i = 0; i < num; i++) {
                KinshipRec kinship = fKinships[i];

                if (kinship.PrevRels.Contains(prev) && kinship.CurrRels.Contains(cur)) {
                    RelationKind rel = kinship.FinRel;
                    great = kinship.Great;
                    level = kinship.Level;

                    if (rel == RelationKind.rkSame) {
                        rel = cur;
                    }

                    finRel = rel;
                    break;
                }
            }

            return finRel;
        }
    }
}
