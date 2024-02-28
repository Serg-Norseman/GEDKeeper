/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using BSLib;

namespace GKCore.Design.Graphics
{
    public enum ImageTarget
    {
        UI,
        Chart
    }


    /// <summary>
    /// Interface for platform-independent graphics rendering providers.
    /// </summary>
    public interface IGraphicsProvider
    {
        /// <summary>
        /// Check photo orientation.
        /// </summary>
        /// <param name="inputStream"></param>
        /// <returns>Returns true if normal, false if transformations are needed.</returns>
        Stream CheckOrientation(Stream inputStream);

        IColor CreateColor(int argb);
        IColor CreateColor(string signature);

        IFont CreateFont(string fontName, float size, bool bold);

        void FreeImage(ref IImage image);

        IImage LoadImage(Stream stream, int thumbWidth, int thumbHeight, ExtRect cutoutArea, string cachedFile);

        /// <summary>
        /// Loading an image from a file (already cached by other functions or service images/icons of application).
        /// That is, loading without any additional processing procedures.
        /// </summary>
        IImage LoadImage(string fileName);

        IImage LoadResourceImage(Type baseType, string resName, ImageTarget target);
        IImage LoadResourceImage(string resName, ImageTarget target, bool makeTransp = false);

        /// <summary>
        /// Saving portrait images for caching purposes.
        /// </summary>
        void SaveImage(IImage image, string fileName);

        ExtSizeF GetTextSize(string text, IFont font, object target);

        string GetDefaultFontName();

        float GetDefaultFontSize();
    }
}
