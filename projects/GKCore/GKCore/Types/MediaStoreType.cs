/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
        /// Storage's folder next to the database file.
        /// </summary>
        mstStorage,

        /// <summary>
        /// The archive file (zip) next to the database file.
        /// </summary>
        mstArchive,

        /// <summary>
        /// File reference relative to the database file.
        /// </summary>
        mstRelativeReference,

        /// <summary>
        /// File reference relative to the web address of file.
        /// </summary>
        mstURL
    }
}
