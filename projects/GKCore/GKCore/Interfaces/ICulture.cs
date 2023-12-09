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

using System.Threading.Tasks;
using GDModel;
using GKCore.Types;

namespace GKCore.Interfaces
{
    /// <summary>
    /// The main interface to culture-dependent processing of names.
    /// <br/>
    /// In most languages, where there is a middle name, it consists of several parts.
    /// Since we do not introduce a separate field for storage of these parts - 
    /// we are also not going to control the middle names by the specific flag.
    /// Similarly for the second surname. Therefore, the corresponding flags are 
    /// removed and will no longer be used.
    /// </summary>
    public interface ICulture
    {
        GDMLanguageID Language { get; set; }

        bool HasPatronymic { get; }
        bool HasSurname { get; }

        string SysCulture { get; }

        string NormalizeSurname(string sn, bool aFemale);
        string GetMarriedSurname(string husbSurname);
        Task<GDMSex> GetSex(string iName, string iPat, bool canQuery);

        string[] GetSurnames(string surname, bool female);
        string[] GetSurnames(GDMIndividualRecord iRec);

        string GetPossessiveName(string name);
        string GetPossessiveName(GDMIndividualRecord iRec);

        NamePartsRet GetNameParts(GDMPersonalName personalName);
    }
}
