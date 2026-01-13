/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;

namespace GKCore.Design
{
    /// <summary>
    /// This class is designed to isolate common type names from platform-specific implementations.
    /// Not the best name for the class, it may be renamed in the future.
    /// </summary>
    [Flags]
    public enum GKFontStyle
    {
        None = 0,
        Bold = 1,
        Italic = 2,
        Underline = 4,
        Strikeout = 8
    }


    /// <summary>
    /// This class is designed to isolate common type names from platform-specific implementations.
    /// Not the best name for the class, it may be renamed in the future.
    /// </summary>
    public enum GKHorizontalAlignment
    {
        Left,
        Right,
        Center
    }


    /// <summary>
    /// This class is designed to isolate common type names from platform-specific implementations.
    /// Not the best name for the class, it may be renamed in the future.
    /// </summary>
    public enum GKSortOrder
    {
        None,
        Ascending,
        Descending
    }
}
