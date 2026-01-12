/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using Eto.Drawing;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKUI.Platform.Handlers;
using EFImageListItem = Eto.Forms.IImageListItem;
using EFListItem = Eto.Forms.IListItem;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class GKComboItem<T> : ComboItem<T>, EFListItem, EFImageListItem
    {
        public string Key
        {
            get { return Text; }
        }

        string EFListItem.Text
        {
            get { return base.Text; }
            set {  }
        }

        Image EFImageListItem.Image
        {
            get {
                var image = base.Image;
                var efImage = (image == null) ? null : ((ImageHandler) image).Handle;
                return efImage;
            }
        }

        public GKComboItem(string text) : base(text)
        {
        }

        public GKComboItem(string text, T tag) : base(text, tag)
        {
        }

        public GKComboItem(string text, T tag, IImage image) : base(text, tag, image)
        {
        }
    }
}
