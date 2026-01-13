/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

namespace GKCore.Search
{
    public enum FARPropertyType
    {
        ptNone,

        ptName,
        ptPlace,
        ptAddress,
        ptFact,
        ptEvent,
        ptAssociation,
        ptText,
        ptGoal,
        ptTheme,
        ptFilePath,

        ptLast = ptFilePath
    }
}
