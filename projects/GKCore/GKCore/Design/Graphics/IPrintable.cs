/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;

namespace GKCore.Design.Graphics
{
    /// <summary>
    /// The interface of objects are permitted to print.
    /// </summary>
    public interface IPrintable
    {
        bool IsLandscape();
        IImage GetPrintableImage();
    }
}
