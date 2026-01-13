/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKUI.Components;
using Xamarin.Forms;
using Xamarin.Forms.Xaml;

namespace GKUI.Platform
{
    [ContentProperty("Source")]
    public class ImageResourceExt : IMarkupExtension
    {
        public string Source { get; set; }

        public object ProvideValue(IServiceProvider serviceProvider)
        {
            return UIHelper.LoadResourceImage(Source);
        }
    }
}
