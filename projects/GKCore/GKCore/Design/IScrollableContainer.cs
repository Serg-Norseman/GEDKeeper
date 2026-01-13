/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;

namespace GKCore.Design
{
    public interface IScrollableContainer
    {
        ExtRect ImageViewport { get; }

        ExtRect Viewport { get; }

        /// <summary>
        /// Virtual canvas - if the size of the drawing canvas is based on the client area of ​​the scrollable container.
        /// </summary>
        bool VirtualCanvas { get; }
    }
}
