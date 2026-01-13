/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Reflection;

namespace GKCore.Locales
{
    /// <summary>
    /// The interface for objects that provide localized resources.
    /// </summary>
    public interface ILangMan
    {
        string LS(Enum lsid);
        bool LoadFromFile(string fileName, Assembly resAssembly);
    }
}
