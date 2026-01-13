/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.IO;

namespace GKCore.Design.Graphics
{
    /// <summary>
    /// 
    /// </summary>
    public interface IImage : IDisposable
    {
        int Height { get; }
        int Width { get; }

        /// <summary>
        /// Default format: bmp.
        /// </summary>
        /// <returns></returns>
        byte[] GetBytes();

        /// <summary>
        /// Supported formats: bmp, gif, jpeg, png, tiff.
        /// </summary>
        byte[] GetBytes(string format);

        MemoryStream GetStream(string format);

        IImage Resize(int newWidth, int newHeight);
    }
}
