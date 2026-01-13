/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

namespace GKCore.Design.Graphics
{
    /// <summary>
    /// Interface for platform-independent class of color representation.
    /// </summary>
    public interface IColor
    {
        IColor Darker(float fraction);
        IColor Lighter(float fraction);

        string GetCode();
        string GetName();

        byte GetR();
        byte GetG();
        byte GetB();
        byte GetA();

        bool IsTransparent();

        int ToArgb();
    }
}
