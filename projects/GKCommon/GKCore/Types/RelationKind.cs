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

namespace GKCore.Types
{
    public enum RelationKind
    {
        rkNone,		/* 0 */
        rkParent,	/* 1 */
        rkSpouse,	/* 2 */
        rkChild,	/* 3 */
        rkFather,	/* 4 */
        rkMother,	/* 5 */
        rkHusband,	/* 6 */
        rkWife,		/* 7 */
        rkSon,		/* 8 */
        rkDaughter,	/* 9 */
        rkGrandfather,
        rkGrandmother,
        rkGrandson,
        rkGranddaughter,
        rkBrother,
        rkSister,
        rkSonInLaw,
        rkDaughterInLaw,
        rkHusbandFather,
        rkHusbandMother,
        rkWifeFather,
        rkWifeMother,
        rkUncle,
        rkAunt,
        rkNephew,
        rkNiece,
        rkCousinM,
        rkCousinF,
        rkSame,
        rkUndefined
    }
}