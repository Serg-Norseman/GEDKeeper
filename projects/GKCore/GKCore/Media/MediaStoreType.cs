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
    /// Types of store multimedia files.
    /// </summary>
    public enum MediaStoreType
    {
        /// <summary>
        /// Direct absolute file reference.
        /// </summary>
        mstReference,

        /// <summary>
        /// File reference relative to the database file.
        /// </summary>
        mstRelativeReference,

        /// <summary>
        /// The archive file (zip) next to the database file.
        /// </summary>
        mstArchive,

        /// <summary>
        /// File reference relative to the web address of file.
        /// </summary>
        mstURL,

        /// <summary>
        /// Storage's folder next to the database file. Deprecated.
        /// </summary>
        mstStorage_Old,
    }
}
