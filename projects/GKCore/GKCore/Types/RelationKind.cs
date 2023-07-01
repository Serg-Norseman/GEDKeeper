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

namespace GKCore.Types
{
    public enum RelationKind
    {
        rkNone                  = 0,

        rkParent                = 1,
        rkSpouse                = 2,
        rkChild                 = 3,

        rkFather                = 4,
        rkMother                = 5,

        rkHusband               = 6,
        rkWife                  = 7,

        rkSon                   = 8,
        rkDaughter              = 9,

        rkGrandfather           = 10,
        rkGrandmother           = 11,

        rkGrandson              = 12,
        rkGranddaughter         = 13,

        rkBrother               = 14,
        rkSister                = 15,

        rkSonInLaw              = 16,
        rkDaughterInLaw         = 17,

        rkHusbandFather         = 18,
        rkHusbandMother         = 19,

        rkWifeFather            = 20,
        rkWifeMother            = 21,

        rkUncle_FatherBrother   = 22,
        rkAunt_FatherSister     = 23,

        rkUncle_MotherBrother   = 24,
        rkAunt_MotherSister     = 25,

        rkNephew                = 26,
        rkNiece                 = 27,

        rkCousinM               = 28,
        rkCousinF               = 29,

        rkBrotherInLaw_H        = 30,
        rkSisterInLaw_H         = 31,

        rkBrotherInLaw_W        = 32,
        rkSisterInLaw_W         = 33,

        rkStepfather            = 34,
        rkStepmother            = 35,

        rkFathersWife           = 36,
        rkMothersHusband        = 37,

        rkSame                  = 38,
        rkUndefined             = 39
    }
}
