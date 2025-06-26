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
    /// <summary>
    /// Built-in relationship types that are basic to the functionality.
    /// </summary>
    public enum KinshipType
    {
        ktNone                  = 00,

        ktUndefined             = 01,
        ktSame                  = 02,

        ktParent                = 03,
        ktSpouse                = 04,
        ktChild                 = 05,

        ktFather                = 06,
        ktMother                = 07,

        ktHusband               = 08,
        ktWife                  = 09,

        ktSon                   = 10,
        ktDaughter              = 11,

        ktGrandfather           = 12,
        ktGrandmother           = 13,

        ktGrandson              = 14,
        ktGranddaughter         = 15,

        ktBrother               = 16,
        ktSister                = 17,

        ktSonInLaw              = 18,   // husband of daughter or sister
        ktDaughterInLaw         = 19,   // wife of son or brother

        ktHusbandFather         = 20,
        ktHusbandMother         = 21,

        ktWifeFather            = 22,
        ktWifeMother            = 23,

        ktUncle_FatherBrother   = 24,
        ktAunt_FatherSister     = 25,

        ktUncle_MotherBrother   = 26,
        ktAunt_MotherSister     = 27,

        ktNephew                = 28,
        ktNiece                 = 29,

        ktCousinM               = 30,
        ktCousinF               = 31,

        ktBrotherInLaw_H        = 32,   // husband's brother
        ktSisterInLaw_H         = 33,   // husband's sister

        ktBrotherInLaw_W        = 34,   // wife's brother
        ktSisterInLaw_W         = 35,   // wife's sister

        ktStepfather            = 36,   // unused yet
        ktStepmother            = 37,   // unused yet

        ktFathersWife           = 38,
        ktMothersHusband        = 39,

        ktUncle_AuntHusband     = 40,
        ktAunt_UncleWife        = 41,

        ktLast = ktAunt_UncleWife
    }


    public enum KinshipExt
    {
        None        = 0,
        Blood       = 1,    // brother/sister   [ stage: kinship analysis ]
        Uterine     = 2,    // brother/sister   [ stage: kinship analysis ]
        Adoption    = 3,    // internal
        Adoptive    = 4,    // father/mother    [ stage: graph generation ]
        Adopted     = 5,    // son/daughter     [ stage: graph generation ]
        CommonLaw   = 6,    // husband/wife     [ stage: graph generation ]
    }


    public sealed class KinshipProps
    {
        public KinshipType Type { get; private set; }
        public KinshipExt Ext { get; private set; }

        public KinshipProps(KinshipType type, KinshipExt ext)
        {
            Type = type;
            Ext = ext;
        }
    }
}
