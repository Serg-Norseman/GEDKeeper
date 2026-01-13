/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.IO;
using GDModel;
using GKCore.Design.Graphics;

namespace GKCore.Design.Views
{
    public interface IMediaViewerWin : IWindow, IWindowDependent
    {
        GDMMultimediaRecord MultimediaRecord { get; set; }
        GDMFileReferenceWithTitle FileReference { get; set; }

        object ViewControl { get; }

        void SetViewImage(IImage img);
        void SetViewMedia(string mediaFile);
        void SetViewText(string text);
        void SetViewRTF(string text);
        void SetViewHTML(Stream stm);
        void DisposeViewControl();
    }
}
