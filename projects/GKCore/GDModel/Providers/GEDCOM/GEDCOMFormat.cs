/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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

namespace GDModel.Providers.GEDCOM
{
    public enum GEDCOMFormat
    {
        gf_Unknown,
        gf_Native,
        gf_Genealogy_RusOld,

        gf_Ahnenblatt,
        gf_AncestQuest,
        gf_Ancestry,
        gf_AGES,
        gf_ALTREE,
        gf_EasyTree,
        gf_FamilyHistorian,
        gf_FamilyTreeMaker,
        gf_FTB,
        gf_GeneWeb,
        gf_Geni,
        gf_Genney,
        gf_GenoPro,
        gf_Gramps,
        gf_GENBOX,
        gf_GENJ,
        gf_Heredis,
        gf_Legacy,
        gf_Lifelines,
        gf_PAF,
        gf_Reunion,
        gf_RootsMagic,
        gf_WikiTree,

        gf_Last = gf_WikiTree
    }
}
