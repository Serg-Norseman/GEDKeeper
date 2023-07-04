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

        rkSonInLaw              = 18,
        rkDaughterInLaw         = 19,

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

        rkBrotherInLaw_H        = 32,
        rkSisterInLaw_H         = 33,

        rkBrotherInLaw_W        = 34,
        rkSisterInLaw_W         = 35,

        rkStepfather            = 36,
        rkStepmother            = 37,

        rkFathersWife           = 38,
        rkMothersHusband        = 39,

        rkUncle_AuntHusband     = 40,
        rkAunt_UncleWife        = 41,
    }
}
