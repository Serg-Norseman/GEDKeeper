/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;
using GKCore.Design.Graphics;

namespace GKUI.Platform.Handlers
{
    public sealed class BrushHandler : TypeHandler<object>, IBrush
    {
        private static readonly object fFake = new object();

        public IColor Color { get; set; }

        public BrushHandler(IColor color) : base(fFake)
        {
            Color = color;
        }
    }
}
