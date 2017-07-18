/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.IO;
using GKCommon;

namespace GKCore.Interfaces
{
    /// <summary>
    /// 
    /// </summary>
    public interface IGraphicsProvider
    {
        IColor CreateColor(int argb);
        IColor CreateColor(int r, int g, int b);
        IColor CreateColor(string signature);
        IFont CreateFont(string fontName, float size, bool bold);
        IImage CreateImage(Stream stream);
        IImage CreateImage(Stream stream, int thumbWidth, int thumbHeight, ExtRect cutoutArea);
        IGfxPath CreatePath();
        IPen CreatePen(IColor color, float width);
        IBrush CreateSolidBrush(IColor color);

        IImage GetResourceImage(string resName, bool makeTransp);
        IImage LoadImage(string fileName);
        void SaveImage(IImage image, string fileName);
    }
}
