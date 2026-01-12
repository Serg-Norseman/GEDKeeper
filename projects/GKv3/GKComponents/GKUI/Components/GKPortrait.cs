/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih, Igor Tyulyakov.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using Eto.Forms;
using GKCore.Design.Controls;
using GKUI.Platform;

namespace GKUI.Components
{
    /// <summary>
    /// Image with the pop-up panel.
    /// </summary>
    public class GKPortrait : PictureBox, IPortraitControl
    {
        public GKPortrait()
        {
        }

        public void Activate()
        {
            Focus();
        }

        public void AddButton(Button b)
        {
        }

        protected override void OnSizeChanged(EventArgs e)
        {
            base.OnSizeChanged(e);
        }

        protected override void OnShown(EventArgs e)
        {
            base.OnShown(e);
        }
    }
}
