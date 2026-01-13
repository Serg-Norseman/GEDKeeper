/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Graphics;

namespace GKCore.Options
{
    public interface IOptionsSection
    {
        void Accept();
        void Cancel();
        bool HasValidationErrors();
        string DisplayName { get; }
        string TreePosition { get; }
        IImage MenuIcon { get; }
    }
}
