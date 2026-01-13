/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Locales;

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
