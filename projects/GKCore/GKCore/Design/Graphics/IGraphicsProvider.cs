/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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

        /// <summary>
        /// Loading an image from a file (already cached by other functions or service images/icons of application).
        /// That is, loading without any additional processing procedures.
        /// </summary>
        IImage LoadImage(Stream stream, int thumbWidth, int thumbHeight, ExtRect cutoutArea, bool reduce);

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
