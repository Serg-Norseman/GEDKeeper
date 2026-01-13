/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Threading.Tasks;
using GDModel;
using GKCore.Names;

namespace GKCore.Cultures
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
