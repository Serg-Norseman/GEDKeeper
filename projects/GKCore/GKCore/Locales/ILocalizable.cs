/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

namespace GKCore.Locales
{
    /// <summary>
    /// The interface for objects (dialogs, windows, components),
    /// which should change content based on selected language of UI.
    /// </summary>
    public interface ILocalizable
    {
        /// <summary>
        /// The implementation of this method is required to replace
        /// the all localizable resources of object.
        /// </summary>
        void SetLocale();
    }
}
