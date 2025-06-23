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

namespace GKCore.Kinships
{
    public enum RelationKind
    {
        rkNone                  = 00,

        rkUndefined             = 01,
        rkSame                  = 02,

        rkParent                = 03,
        rkSpouse                = 04,
        rkChild                 = 05,

        rkFather                = 06,
        rkMother                = 07,

        rkHusband               = 08,
        rkWife                  = 09,

        rkSon                   = 10,
        rkDaughter              = 11,

        rkGrandfather           = 12,
        rkGrandmother           = 13,

        rkGrandson              = 14,
        rkGranddaughter         = 15,

        rkBrother               = 16,
        rkSister                = 17,

        rkSonInLaw              = 18,   // husband of daughter or sister
        rkDaughterInLaw         = 19,   // wife of son or brother

        rkHusbandFather         = 20,
        rkHusbandMother         = 21,

        rkWifeFather            = 22,
        rkWifeMother            = 23,

        rkUncle_FatherBrother   = 24,
        rkAunt_FatherSister     = 25,

        rkUncle_MotherBrother   = 26,
        rkAunt_MotherSister     = 27,

        rkNephew                = 28,
        rkNiece                 = 29,

        rkCousinM               = 30,
        rkCousinF               = 31,

        rkBrotherInLaw_H        = 32,   // husband's brother
        rkSisterInLaw_H         = 33,   // husband's sister

        rkBrotherInLaw_W        = 34,   // wife's brother
        rkSisterInLaw_W         = 35,   // wife's sister

        rkStepfather            = 36,   // unused yet
        rkStepmother            = 37,   // unused yet

        rkFathersWife           = 38,
        rkMothersHusband        = 39,

        rkUncle_AuntHusband     = 40,
        rkAunt_UncleWife        = 41,

        rkLast = rkAunt_UncleWife
    }


    public enum RelationExt
    {
        None        = 0,
        Blood       = 1,    // brother/sister   [ stage: kinship analysis ]
        Uterine     = 2,    // brother/sister   [ stage: kinship analysis ]
        Adoption    = 3,    // internal
        Adoptive    = 4,    // father/mother    [ stage: graph generation ]
        Adopted     = 5,    // son/daughter     [ stage: graph generation ]
        CommonLaw   = 6,    // husband/wife     [ stage: graph generation ]
    }


    public sealed class RelationProps
    {
        public RelationKind Kind { get; private set; }
        public RelationExt Ext { get; private set; }

        public RelationProps(RelationKind kind, RelationExt ext)
        {
            Kind = kind;
            Ext = ext;
        }
    }
}
