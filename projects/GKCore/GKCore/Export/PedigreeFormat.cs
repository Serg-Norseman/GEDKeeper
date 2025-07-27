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

namespace GKCore.Export
{
    public enum PedigreeKind
    {
        Ascend,
        Descend_dAboville,
        Descend_Konovalov
    }


    public enum PedigreeType
    {
        Ascend,
        Descend
    }


    public enum PedigreeNumbering
    {
        Aboville,
        Kobrin_Konovalov_A,
        Kobrin_Konovalov_D,
        Sosa_Stradonitz,
    }


    public enum PedigreeFormat
    {
        Excess,
        Compact
    }


    public static class PedigreeData
    {
        public sealed class NumberingStruct
        {
            public LSID Name;
            public PedigreeType Type;

            public NumberingStruct(LSID name, PedigreeType type)
            {
                Name = name;
                Type = type;
            }
        }


        public static readonly NumberingStruct[] Numberings = new NumberingStruct[] {
            new NumberingStruct(LSID.PN_Aboville, PedigreeType.Descend),           // Aboville
            new NumberingStruct(LSID.PN_Kobrin_Konovalov, PedigreeType.Ascend),    // Kobrin_Konovalov_A
            new NumberingStruct(LSID.PN_Kobrin_Konovalov, PedigreeType.Descend),   // Kobrin_Konovalov_D
            new NumberingStruct(LSID.PN_Sosa_Stradonitz, PedigreeType.Ascend),     // Sosa_Stradonitz
        };
    }
}
