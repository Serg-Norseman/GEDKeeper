/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

namespace GKCore.Media
{
    /// <summary>
    /// The type of the media object when stored in a storage or archive
    /// (specifies the structure of sub-folders).
    /// </summary>
    public enum MultimediaKind
    {
        mkNone,
        mkImage,
        mkVideo,
        mkAudio,
        mkText,
        mkOffice,
        mkArchive
    }
}
