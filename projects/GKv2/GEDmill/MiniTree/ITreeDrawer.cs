/* 
 * Copyright 2009 Alexander Curtis <alex@logicmill.com>
 * This file is part of GEDmill - A family history website creator
 * 
 * GEDmill is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GEDmill is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GEDmill.  If not, see <http://www.gnu.org/licenses/>.
 */

using System.Collections.Generic;
using System.IO;
using BSLib;

namespace GEDmill.MiniTree
{
    public interface ITreeDrawer
    {
        void InitTempGfx(int width, int height, int pixelFormat, bool background);
        void DoneTempGfx();
        void SaveTempGfx(Stream fileStream);

        void DrawGroup(MTGroup group, List<MTMap> map);
        ExtSizeF MeasureString(string text);
    }
}
