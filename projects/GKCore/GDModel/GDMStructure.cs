/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;

namespace GDModel
{
    /// <summary>
    /// Some names are temporary.
    /// </summary>
    [Flags]
    public enum GDMStructureType
    {
        None = 0,

        // Std structures
        Association         = 1 << 0,
        ChildLink           = 1 << 1,
        Event               = 1 << 2,
        MediaFile           = 1 << 3,
        MultimediaLink      = 1 << 4,
        NoteLink            = 1 << 5,
        ParentLink          = 1 << 6, /* ChildToFamilyLink */
        PersonalName        = 1 << 7,
        RepositoryCitation  = 1 << 8,
        SourceCallNumber    = 1 << 9,
        SourceCitation      = 1 << 10,
        SpouseLink          = 1 << 11, /* SpouseToFamilyLink */
        UserReference       = 1 << 12,

        // GK structures
        CommunicationLink   = 1 << 13,
        DNATest             = 1 << 14,
        GroupLink           = 1 << 15,
        LocName             = 1 << 16,
        LocLink             = 1 << 17,
        MemberLink          = 1 << 18,
        TaskLink            = 1 << 19,
    }


    public interface IGDMStructure
    {
        GDMStructureType GetAccessibleSubstructures();
    }
}
