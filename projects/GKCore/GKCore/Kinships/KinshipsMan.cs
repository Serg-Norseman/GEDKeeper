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

using System.Collections.Generic;
using BSLib;
using GDModel;
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
                RelationSet.Create(),
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
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkHusband, RelationKind.rkWife),
                RelationSet.Create(RelationKind.rkSon, RelationKind.rkDaughter),
                RelationKind.rkSame, 0, 0);


            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkMother),
                RelationSet.Create(RelationKind.rkHusband),
                /*RelationKind.rkFather*/ RelationKind.rkMothersHusband, 0, 0);

            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkFather),
                RelationSet.Create(RelationKind.rkWife),
                /*RelationKind.rkMother*/ RelationKind.rkFathersWife, 0, 0);


            RegisterKinship(
                RelationSet.Create(RelationKind.rkFather),
                RelationSet.Create(RelationKind.rkGrandfather, RelationKind.rkGrandmother),
                RelationSet.Create(RelationKind.rkSon),
                RelationKind.rkUncle_FatherBrother, -1, 0);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkFather),
                RelationSet.Create(RelationKind.rkGrandfather, RelationKind.rkGrandmother),
                RelationSet.Create(RelationKind.rkDaughter),
                RelationKind.rkAunt_FatherSister, -1, 0);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkMother),
                RelationSet.Create(RelationKind.rkGrandfather, RelationKind.rkGrandmother),
                RelationSet.Create(RelationKind.rkSon),
                RelationKind.rkUncle_MotherBrother, -1, 0);

            RegisterKinship(
                RelationSet.Create(RelationKind.rkMother),
                RelationSet.Create(RelationKind.rkGrandfather, RelationKind.rkGrandmother),
                RelationSet.Create(RelationKind.rkDaughter),
                RelationKind.rkAunt_MotherSister, -1, 0);


            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkBrother, RelationKind.rkSister),
                RelationSet.Create(RelationKind.rkSon),
                RelationKind.rkNephew, 0, 0);

            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkBrother, RelationKind.rkSister),
                RelationSet.Create(RelationKind.rkDaughter),
                RelationKind.rkNiece, 0, 0);


            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkSon, RelationKind.rkBrother),
                RelationSet.Create(RelationKind.rkWife),
                RelationKind.rkDaughterInLaw, 0, 0);

            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkDaughter, RelationKind.rkSister),
                RelationSet.Create(RelationKind.rkHusband),
                RelationKind.rkSonInLaw, 0, 0);


            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkWife),
                RelationSet.Create(RelationKind.rkFather),
                RelationKind.rkWifeFather, 0, 0);

            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkWife),
                RelationSet.Create(RelationKind.rkMother),
                RelationKind.rkWifeMother, 0, 0);

            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkHusband),
                RelationSet.Create(RelationKind.rkFather),
                RelationKind.rkHusbandFather, 0, 0);

            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkHusband),
                RelationSet.Create(RelationKind.rkMother),
                RelationKind.rkHusbandMother, 0, 0);


            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkFather, RelationKind.rkMother),
                RelationSet.Create(RelationKind.rkFather),
                RelationKind.rkGrandfather, 0, 0);

            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkFather, RelationKind.rkMother),
                RelationSet.Create(RelationKind.rkMother),
                RelationKind.rkGrandmother, 0, 0);


            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkGrandfather, RelationKind.rkGrandmother),
                RelationSet.Create(RelationKind.rkFather),
                RelationKind.rkGrandfather, +1, 0);

            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkGrandfather, RelationKind.rkGrandmother),
                RelationSet.Create(RelationKind.rkMother),
                RelationKind.rkGrandmother, +1, 0);


            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkFather, RelationKind.rkMother),
                RelationSet.Create(RelationKind.rkSon),
                RelationKind.rkBrother, 0, 0);

            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkFather, RelationKind.rkMother),
                RelationSet.Create(RelationKind.rkDaughter),
                RelationKind.rkSister, 0, 0);


            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkSon, RelationKind.rkDaughter, RelationKind.rkSonInLaw, RelationKind.rkDaughterInLaw),
                RelationSet.Create(RelationKind.rkSon),
                RelationKind.rkGrandson, 0, 0);

            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkSon, RelationKind.rkDaughter, RelationKind.rkSonInLaw, RelationKind.rkDaughterInLaw),
                RelationSet.Create(RelationKind.rkDaughter),
                RelationKind.rkGranddaughter, 0, 0);


            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkGrandson, RelationKind.rkGranddaughter),
                RelationSet.Create(RelationKind.rkSon),
                RelationKind.rkGrandson, +1, 0);

            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkGrandson, RelationKind.rkGranddaughter),
                RelationSet.Create(RelationKind.rkDaughter),
                RelationKind.rkGranddaughter, +1, 0);


            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkUncle_FatherBrother, RelationKind.rkAunt_FatherSister, RelationKind.rkUncle_MotherBrother, RelationKind.rkAunt_MotherSister),
                RelationSet.Create(RelationKind.rkSon),
                RelationKind.rkBrother, 0, +1);

            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkUncle_FatherBrother, RelationKind.rkAunt_FatherSister, RelationKind.rkUncle_MotherBrother, RelationKind.rkAunt_MotherSister),
                RelationSet.Create(RelationKind.rkDaughter),
                RelationKind.rkSister, 0, +1);


            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkNephew, RelationKind.rkNiece),
                RelationSet.Create(RelationKind.rkSon),
                RelationKind.rkGrandson, +1, +1);

            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkNephew, RelationKind.rkNiece),
                RelationSet.Create(RelationKind.rkDaughter),
                RelationKind.rkGranddaughter, +1, +1);


            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkHusband),
                RelationSet.Create(RelationKind.rkBrother),
                RelationKind.rkBrotherInLaw_H, 0, 0);

            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkWife),
                RelationSet.Create(RelationKind.rkBrother),
                RelationKind.rkBrotherInLaw_W, 0, 0);

            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkHusbandFather, RelationKind.rkHusbandMother),
                RelationSet.Create(RelationKind.rkSon),
                RelationKind.rkBrotherInLaw_H, 0, 0);

            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkWifeFather, RelationKind.rkWifeMother),
                RelationSet.Create(RelationKind.rkSon),
                RelationKind.rkBrotherInLaw_W, 0, 0);


            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkHusband),
                RelationSet.Create(RelationKind.rkSister),
                RelationKind.rkSisterInLaw_H, 0, 0);

            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkWife),
                RelationSet.Create(RelationKind.rkSister),
                RelationKind.rkSisterInLaw_W, 0, 0);

            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkHusbandFather, RelationKind.rkHusbandMother),
                RelationSet.Create(RelationKind.rkDaughter),
                RelationKind.rkSisterInLaw_H, 0, 0);

            RegisterKinship(
                RelationSet.Create(),
                RelationSet.Create(RelationKind.rkWifeFather, RelationKind.rkWifeMother),
                RelationSet.Create(RelationKind.rkDaughter),
                RelationKind.rkSisterInLaw_W, 0, 0);
        }

        private static void RegisterKinship(RelationSet prevprevRels, RelationSet prevRels, RelationSet currRels, RelationKind finRel, sbyte great, sbyte degree)
        {
            fKinships.Add(new KinshipRec(prevprevRels, prevRels, currRels, finRel, great, degree));
        }

        public static RelationKind FindKinship(RelationKind prevprev, RelationKind prev, RelationKind cur, out int great, out int degree)
        {
            RelationKind finRel = RelationKind.rkUndefined;
            great = 0;
            degree = 0;

            int num = fKinships.Count;
            for (int i = 0; i < num; i++) {
                KinshipRec kinship = fKinships[i];

                if (kinship.PrevRels.Contains(prev) && kinship.CurrRels.Contains(cur) && (kinship.PrevPrevRels.IsEmpty() || kinship.PrevPrevRels.Contains(prevprev))) {
                    great = kinship.Great;
                    degree = kinship.Degree;
                    finRel = (kinship.FinRel == RelationKind.rkSame) ? cur : kinship.FinRel;
                    break;
                }
            }

            return finRel;
        }

        public static RelationKind FixLink(GDMSex targetSex, RelationKind rel)
        {
            RelationKind resRel = rel;

            switch (rel) {
                case RelationKind.rkParent:
                    switch (targetSex) {
                        case GDMSex.svMale:
                            resRel = RelationKind.rkFather;
                            break;
                        case GDMSex.svFemale:
                            resRel = RelationKind.rkMother;
                            break;
                    }
                    break;

                case RelationKind.rkSpouse:
                    switch (targetSex) {
                        case GDMSex.svMale:
                            resRel = RelationKind.rkHusband;
                            break;
                        case GDMSex.svFemale:
                            resRel = RelationKind.rkWife;
                            break;
                    }
                    break;

                case RelationKind.rkChild:
                    switch (targetSex) {
                        case GDMSex.svMale:
                            resRel = RelationKind.rkSon;
                            break;
                        case GDMSex.svFemale:
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

        public static string GetDegree(int n, GDMSex sex)
        {
            string result = (n == 0) ? string.Empty : (GKData.Numerals[n] + GKData.NumKinship[(int)sex] + " ");
            return result;
        }

        public static string GetGreat(int n, bool shortForm)
        {
            string result = "";
            if (n > 0) {
                if (!shortForm) {
                    for (int i = 1; i <= n; i++) {
                        result += LangMan.LS(GKData.GreatPrefix);
                    }
                } else {
                    result = LangMan.LS(GKData.GreatPrefix);
                    if (n > 1) {
                        result += string.Format("({0})", n);
                    }
                }
            }
            return result;
        }
    }
}
